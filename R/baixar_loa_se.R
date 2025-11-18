# ==========================================================
# --- 1. CONFIGURAÇÃO E PACOTES ---
# ==========================================================
library(rvest)
library(stringr)
library(dplyr)
library(stringi)
library(purrr)
library(curl)
library(httr2)

# ==========================================================
# --- 2. UTILITÁRIOS: TEXTO E SISTEMA DE ARQUIVOS ---
# ==========================================================

#' Limpa e padroniza nomes de arquivos para salvar no disco
#' @param nome String com o nome original
#' @return String sanitizada e segura para sistema de arquivos
limpar_nome_arquivo <- function(nome) {
  nome %>%
    enc2utf8() %>%
    stringi::stri_trans_general("Latin-ASCII") %>%
    str_replace_all("[^[:alnum:]_\\-\\. ]", "") %>% # Remove caracteres especiais
    str_squish() %>%
    str_replace_all(" ", "_") %>%
    str_remove("file_open_") %>%
    str_sub(1, 100) %>% # Limita tamanho para evitar erros de OS
    { 
      # Usa tools::file_ext (Base R) para verificar extensão
      ext <- tools::file_ext(.)
      if (ext == "") paste0(., ".pdf") else . 
    }
}

#' Garante que o diretório de destino exista (Versão Base R)
#' @param base_path Caminho base
#' @param subfolder Subpasta (geralmente o ano)
#' @return Caminho completo criado
preparar_diretorio <- function(base_path, subfolder) {
  # file.path substitui path()
  caminho <- file.path(base_path, subfolder)
  
  # dir.create com recursive = TRUE substitui dir_create()
  if (!dir.exists(caminho)) {
    dir.create(caminho, recursive = TRUE, showWarnings = FALSE)
  }
  caminho
}

# ==========================================================
# --- 3. UTILITÁRIOS: REDE E DOWNLOAD ---
# ==========================================================

#' Tenta obter o nome do arquivo via Headers HTTP (Content-Disposition)
obter_nome_via_header <- function(url) {
  req <- request(url) %>% req_timeout(10)
  resp <- tryCatch(req_perform(req, verbosity = 0), error = function(e) NULL)
  
  if (is.null(resp)) return(basename(url))
  
  cd <- resp_header(resp, "content-disposition")
  nome <- if (!is.null(cd)) str_match(cd, 'filename="?([^";]+)"?')[, 2] else NA
  
  if (is.na(nome)) {
    nome <- basename(url) %>% 
      URLdecode() %>% 
      iconv(from = "latin1", to = "UTF-8", sub = "") %>%
      str_remove("\\?.*$")
  }
  return(nome)
}

#' Executa o download de um arquivo com mecanismo de retentativa
download_com_retry <- function(url, destino, tentativas = 3) {
  # Verificações usando Base R
  if (file.exists(destino) && file.info(destino)$size > 1000) return(TRUE)
  
  for (i in 1:tentativas) {
    sucesso <- tryCatch({
      curl_download(url, destfile = destino, mode = "wb", quiet = TRUE, 
                    handle = new_handle(followlocation = 1))
      TRUE
    }, error = function(e) FALSE)
    
    if (sucesso && file.exists(destino) && file.info(destino)$size > 1000) {
      return(TRUE)
    }
    
    # Limpa arquivo corrompido usando file.remove (Base R)
    if (file.exists(destino)) file.remove(destino)
    Sys.sleep(1)
  }
  return(FALSE)
}

# ==========================================================
# --- 4. LÓGICA DE EXTRAÇÃO (PARSER) ---
# ==========================================================

#' Analisa a tabela HTML e associa arquivos aos seus respectivos anos
parsear_tabela_estruturada <- function(pagina, seletor) {
  linhas <- pagina %>% html_element(seletor) %>% html_elements("tr")
  
  if (length(linhas) == 0) stop("Nenhuma linha encontrada com o seletor fornecido.")
  
  resultados <- list()
  ano_atual <- NA_character_
  
  for (linha in linhas) {
    celula_header <- html_element(linha, "td[colspan='3']")
    
    if (!is.na(celula_header)) {
      texto <- html_text(celula_header, trim = TRUE)
      ano_detectado <- str_extract(texto, "\\d{4}")
      if (!is.na(ano_detectado)) ano_atual <- ano_detectado
      next
    }
    
    link <- html_element(linha, "a")
    if (!is.na(link) && !is.na(ano_atual)) {
      href <- html_attr(link, "href")
      if (!is.na(href) && href != "#") {
        resultados[[length(resultados) + 1]] <- tibble(
          ano = ano_atual,
          nome_link = html_text(link, trim = TRUE),
          url = href 
        )
      }
    }
  }
  
  bind_rows(resultados)
}

# ==========================================================
# --- 5. ORQUESTRADOR PRINCIPAL ---
# ==========================================================

executar_scrapper <- function(config) {
  # Nota: cli::col_cyan é do pacote cli, mas não requer carregamento explícito se instalado
  message(paste0("\n🚀 Iniciando: ", config$pasta_base))
  
  tryCatch({
    pagina <- read_html(config$url_pagina)
    df_arquivos <- parsear_tabela_estruturada(pagina, config$seletor_tabela)
    
    df_arquivos$url <- url_absolute(df_arquivos$url, config$url_pagina)
    
    if (!is.null(config$anos_interesse)) {
      df_arquivos <- df_arquivos %>% filter(ano %in% config$anos_interesse)
    }
    
  }, error = function(e) {
    message(paste("❌ Erro fatal ao ler página: ", e$message))
    return(NULL)
  })
  
  if (is.null(df_arquivos) || nrow(df_arquivos) == 0) {
    message("⚠️ Nenhum arquivo encontrado para os critérios.")
    return(NULL)
  }
  
  message(sprintf("📦 Total de arquivos mapeados: %d", nrow(df_arquivos)))
  
  pwalk(list(df_arquivos$url, df_arquivos$nome_link, df_arquivos$ano, seq_len(nrow(df_arquivos))), 
        function(url, nome_bruto, ano, idx) {
          
          nome_final <- if (nchar(nome_bruto) > 0) nome_bruto else obter_nome_via_header(url)
          nome_arquivo <- limpar_nome_arquivo(nome_final)
          
          pasta_destino <- preparar_diretorio(config$pasta_base, ano)
          caminho_completo <- file.path(pasta_destino, nome_arquivo)
          
          msg_prefix <- sprintf("(%03d/%03d) [%s]", idx, nrow(df_arquivos), ano)
          
          if (download_com_retry(url, caminho_completo)) {
            message(paste("✅", msg_prefix, nome_arquivo))
          } else {
            message(paste("❌", msg_prefix, "Falha no download:", nome_arquivo))
          }
        })
  
  message(paste("\n🏁 Processo finalizado para: ", config$pasta_base))
}

# ==========================================================
# --- 6. EXECUÇÃO ---
# ==========================================================

configs_loa <- list(
  list(
    url_pagina = "https://www.transparencia.se.gov.br/Planejamento/LOA.xhtml",
    seletor_tabela = ".ui-datatable-data", 
    pasta_base = "LOA-SE",
    anos_interesse = as.character(2003:2024)
  )
)

walk(configs_loa, executar_scrapper)

# ==========================================================
# --- 7. PÓS-PROCESSAMENTO: DESCOMPACTAR ZIPS ---
# ==========================================================

rodar_descompactacao <- function(config_list) {
  
  pastas_raiz <- map_chr(config_list, "pasta_base") %>% unique()
  
  walk(pastas_raiz, function(pasta) {
    if (!dir.exists(pasta)) return()
    
    message(paste0("\n🔍 Buscando arquivos ZIP em: ", pasta))
    
    arquivos_zip <- list.files(
      path = pasta, 
      pattern = "\\.zip$", 
      recursive = TRUE, 
      full.names = TRUE, 
      ignore.case = TRUE
    )
    
    if (length(arquivos_zip) == 0) {
      message("   Nenhum arquivo zip encontrado.")
      return()
    }
    
    message(sprintf("   Encontrados %d arquivos. Iniciando extração segura...", length(arquivos_zip)))
    
    walk(arquivos_zip, function(arquivo_zip) {
      
      pasta_pai <- dirname(arquivo_zip)
      nome_zip_sem_extensao <- tools::file_path_sans_ext(basename(arquivo_zip))
      pasta_destino_final <- file.path(pasta_pai, nome_zip_sem_extensao)
      
      # 1. Verifica se é lixo (HTML salvo como zip)
      if (file.info(arquivo_zip)$size < 2000) {
        message(paste("   ⚠️ Pulei (arquivo muito pequeno/HTML):", basename(arquivo_zip)))
        return()
      }
      
      # 2. Tenta listar o conteúdo interno
      # 'unzip -l' geralmente funciona mesmo com encoding ruim, apenas exibe caracteres estranhos
      lista_conteudo <- try(unzip(arquivo_zip, list = TRUE), silent = TRUE)
      
      if (inherits(lista_conteudo, "try-error")) {
        message(paste("   ❌ ARQUIVO CORROMPIDO (ZIP inválido):", basename(arquivo_zip)))
        return()
      }
      
      # Cria a pasta de destino
      if (!dir.exists(pasta_destino_final)) {
        dir.create(pasta_destino_final, recursive = TRUE)
      }
      
      message(paste0("   📂 Processando: ", basename(arquivo_zip)))
      
      # 3. Itera sobre cada arquivo DENTRO do zip sem extrair direto
      sucesso_extracao <- TRUE
      
      tryCatch({
        pwalk(lista_conteudo, function(Name, Length, ...) {
          # Ignora diretórios (tamanho 0 ou termina com /)
          if (Length == 0 || str_ends(Name, "/")) return()
          
          # --- TRATAMENTO DE CODIFICAÇÃO ---
          # Tenta converter de Latin1 (padrão Windows antigo BR) para UTF-8
          nome_corrigido <- iconv(Name, from = "Latin1", to = "UTF-8", sub = "")
          
          # Se falhar ou ficar vazio, usa o original
          if (is.na(nome_corrigido) || nchar(nome_corrigido) == 0) nome_corrigido <- Name
          
          # Aplica sua função de limpeza para garantir nome seguro no sistema de arquivos
          nome_final <- limpar_nome_arquivo(basename(nome_corrigido))
          caminho_saida <- file.path(pasta_destino_final, nome_final)
          
          # --- EXTRAÇÃO MANUAL (STREAM) ---
          # Abre conexão direta com o arquivo dentro do zip
          con <- unz(arquivo_zip, Name, open = "rb")
          on.exit(close(con), add = TRUE) # Garante fechamento
          
          # Lê os bytes
          dados_binarios <- readBin(con, what = "raw", n = Length)
          
          # Salva no disco com o nome limpo
          writeBin(dados_binarios, caminho_saida)
        })
        
        message(paste("      ✅ Extraído com sucesso."))
        
      }, error = function(e) {
        sucesso_extracao <<- FALSE
        message(paste("      ❌ Erro ao processar conteúdo interno:", e$message))
      })
      
      # 4. Remove o ZIP apenas se extraiu sem erros fatais no loop
      if (sucesso_extracao) {
        file.remove(arquivo_zip)
      }
    })
  })
}

# Executa a rotina
rodar_descompactacao(configs_loa)
