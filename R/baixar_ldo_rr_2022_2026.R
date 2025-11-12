# --- 1. Carregar Bibliotecas ---
message("Carregando pacotes necessários...")
suppressPackageStartupMessages({
  library(RSelenium)
  library(httr) 
  library(stringr) 
  # library(tools) # tools::file_ext é do 'base' R, não precisa carregar
})

# --- 2. Configurações Principais ---
# Centralizar as configurações aqui facilita a manutenção futura
DIRETORIO_DESTINO <- "LDO-RR" # Esta agora é a pasta BASE
PAUSA_MIN <- 0.5
PAUSA_MAX <- 2.0

# Lista de tarefas (URLs e seletores)
TAREFAS_SCRAPING <- list(
    list(url = "https://seplan.rr.gov.br/exercicio-financeiro-2022-cgop/", 
       selector = "#post-1947 > div > div.wp-block-uagb-container.uagb-block-560737af.alignfull.uagb-is-root-container > div > div > div > div.wp-block-uagb-tabs-child.uagb-tabs__body-container.uagb-inner-tab-0.uagb-tabs-body__active a", 
       ano = "2022"),

    list(url = "https://seplan.rr.gov.br/exercicio-financeiro-2023/", 
       selector = "#post-1957 > div > div.wp-block-uagb-container.uagb-block-1ffd98a6.alignfull.uagb-is-root-container > div > div.wp-block-uagb-container.uagb-block-d446ee2b > div > div > div.wp-block-uagb-tabs-child.uagb-tabs__body-container.uagb-inner-tab-0.uagb-tabs-body__active a", 
       ano = "2023"),

    list(url = "https://seplan.rr.gov.br/exercicio-financeiro-2024/", 
       selector = "#post-1963 > div > div.wp-block-uagb-container.uagb-block-d446ee2b.alignfull.uagb-is-root-container > div > div > div > div.wp-block-uagb-tabs-child.uagb-tabs__body-container.uagb-inner-tab-0.uagb-tabs-body__active a", 
       ano = "2024"),

    list(url = "https://seplan.rr.gov.br/exercicio-financeiro-2025/", 
       selector = "#post-8208 > div > div.wp-block-uagb-container.uagb-block-d446ee2b.alignfull.uagb-is-root-container > div > div > div > div.wp-block-uagb-tabs-child.uagb-tabs__body-container.uagb-inner-tab-0.uagb-tabs-body__active a", 
       ano = "2025"), 
    
    list(url = "https://seplan.rr.gov.br/exercicio-financeiro-2026/", 
       selector = "#post-13493 > div > div.wp-block-uagb-container.uagb-block-d446ee2b.alignfull.uagb-is-root-container > div > div > div > div.wp-block-uagb-tabs-child.uagb-tabs__body-container.uagb-inner-tab-0.uagb-tabs-body__active a", 
       ano = "2026") 
)


# --- 3. Funções Auxiliares (Setup e Teardown) ---

#' @title Prepara o diretório de destino
#' @param path O caminho da pasta onde os arquivos serão salvos
setup_diretorio <- function(path) {
  message(paste("📁 Verificando pasta de destino:", path))
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE)
    message("   -> Pasta criada com sucesso!")
  } else {
    message("   -> Pasta já existe.")
  }
}

#' @title Inicia o RSelenium
#' @return Uma lista contendo o objeto 'driver' e o 'client'
iniciar_selenium <- function() {
  message("🚀 Iniciando o navegador (Selenium)...")
  
  message("   -> Verificando e encerrando processos Java/Selenium antigos...")
  if (.Platform$OS.type == "unix") {
    system("pkill -f 'selenium-standalone'")
  } else {
    try(system("taskkill /F /IM java.exe /T"), silent = TRUE)
  }
  
  rs_driver_object <- RSelenium::rsDriver(
    browser = "firefox",
    chromever = NULL,   # Não checar o Chrome
    phantomver = NULL,  # NÃO checar o PhantomJS (causa do erro 402)
    check = FALSE,      # 🚫 NÃO checar versões de drivers online
    verbose = FALSE
  )
  
  remDr <- rs_driver_object$client
  remDr$close() # Fechar a aba 'about:blank'
  remDr$open()
  
  message("   -> Navegador pronto!")
  return(list(driver = rs_driver_object, client = remDr))
}

#' @title Encerra o RSelenium
#' @param selenium_objs A lista retornada por iniciar_selenium()
encerrar_selenium <- function(selenium_objs) {
  message("🛑 Encerrando o navegador e o servidor Selenium...")
  try(selenium_objs$client$close(), silent = TRUE)
  try(selenium_objs$driver$server$stop(), silent = TRUE)
  message("   -> Sessão encerrada.")
}


# --- 4. Funções Principais (Coleta e Download) ---

#' @title [MODIFICADO] Extrai os links <a> e seus TÍTULOS (TEXTOS)
#' @param client O objeto 'remDr' do Selenium
#' @param url A URL da página para navegar
#' @param selector O seletor CSS para encontrar os links
#' @return Um data.frame com colunas 'url' e 'titulo'
extrair_links_da_pagina <- function(client, url, selector) {
  
  message(paste("🌐 Navegando para:", url))
  tryCatch({
    client$navigate(url)
    Sys.sleep(5) # Espera a página carregar
  }, error = function(e) {
    message(paste("   ❌ ERRO ao navegar para", url, ":", e$message))
    return(data.frame(url = character(0), titulo = character(0))) # Retorna DF vazio
  })
  
  message("🔗 Buscando links na página...")
  links_elementos <- client$findElements(using = "css selector", value = selector)
  
  if (length(links_elementos) == 0) {
    message("   ⚠️ Nenhum link encontrado com este seletor.")
    return(data.frame(url = character(0), titulo = character(0)))
  }
  
  message(paste("   ->", length(links_elementos), "elementos <a> encontrados. Extraindo URLs e Títulos..."))
  
  # Usar lapply para extrair ambos os atributos de cada elemento
  dados_extraidos <- lapply(links_elementos, function(el) {
    url <- tryCatch(unlist(el$getElementAttribute("href")), error = function(e) NA_character_)
    
    # [MODIFICAÇÃO 1]
    # Mudar de getElementAttribute("title") para getElementText()
    titulo <- tryCatch(unlist(el$getElementText()), error = function(e) NA_character_) 
    
    # Limpar o texto de espaços em branco excessivos ou quebras de linha
    titulo <- stringr::str_trim(gsub("\\s+", " ", titulo))
    
    # Retorna um data.frame de uma linha
    return(data.frame(url = url, titulo = titulo, stringsAsFactors = FALSE))
  })
  
  # Combinar a lista de data.frames em um único data.frame
  df_links <- do.call(rbind, dados_extraidos)
  
  # Remover linhas onde a URL ou o Título falharam
  df_links <- na.omit(df_links)
  
  # [NOVO] Remover linhas onde o título está vazio
  df_links <- df_links[nchar(df_links$titulo) > 0, ]
  
  message(paste("   ✅", nrow(df_links), "links/títulos válidos extraídos!"))
  return(df_links) # Retorna o data.frame
}


#' @title [MODIFICADO] Baixa um arquivo via HTTR
#' @description Agora prioriza o 'nome_base' (do title) se ele for fornecido.
#' @param url O link direto para o arquivo
#' @param pasta_destino A pasta para salvar o arquivo
#' @param nome_base O nome de arquivo desejado (sem extensão), vindo do 'title'
#' @return O caminho final do arquivo salvo, ou NA em caso de falha
baixar_arquivo_http <- function(url, pasta_destino, nome_base = NULL) {
  
  tryCatch({
    ua <- httr::user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/91.0.4472.124 Safari/537.36")
    
    # [MODIFICAÇÃO 2]
    # Aumentar o timeout de 60 para 300 segundos (5 minutos)
    resp <- httr::GET(url, ua, httr::timeout(300))
    
    httr::stop_for_status(resp) 
    
    nome_arquivo <- NULL
    
    # --- [NOVO] Plano A: Usar o nome do 'title' (nome_base) ---
    if (!is.null(nome_base) && nchar(nome_base) > 0) {
      message("   -> 💡 Info: Usando nome do TEXTO do link fornecido.")
      
      # Vamos extrair a extensão (ex: ".pdf") da URL final
      url_final <- resp$url
      nome_url_decodado <- utils::URLdecode(basename(url_final))
      
      # Pega a extensão, incluindo o ponto (ex: ".pdf")
      extensao <- stringr::str_extract(nome_url_decodado, "\\.[^\\.]+$") 
      
      if (is.na(extensao)) {
        extensao <- "" # Sem extensão encontrada
        message("   -> ⚠️ Aviso: Não foi possível determinar a extensão pela URL. Salvando sem extensão.")
      }
      
      # Combina o nome do title com a extensão da URL
      nome_arquivo <- paste0(nome_base, extensao)
    }

    # --- Plano B: Cabeçalho 'content-disposition' (Antigo Plano A) ---
    if (is.null(nome_arquivo)) {
      message("   -> 💡 Info: 'title' não fornecido. Tentando cabeçalho HTTP...")
      header_cd <- httr::headers(resp)$`content-disposition`
      
      if (!is.null(header_cd)) {
        match_quoted <- regmatches(header_cd, regexec('filename="([^"]+)"', header_cd))
        match_unquoted <- regmatches(header_cd, regexec("filename=([^;]+)", header_cd))
        
        if (length(match_quoted[[1]]) > 1) {
          nome_arquivo <- match_quoted[[1]][2]
        } else if (length(match_unquoted[[1]]) > 1) {
          nome_arquivo <- trimws(match_unquoted[[1]][2])
        }
      }
    }
    
    # --- Plano C: Nome da URL (Antigo Plano B) ---
    if (is.null(nome_arquivo)) {
      message("   -> 💡 Info: Cabeçalho não encontrado. Usando nome da URL final.")
      url_final <- resp$url 
      nome_arquivo_encoded <- basename(url_final) 
      nome_arquivo <- utils::URLdecode(nome_arquivo_encoded)
      
      if (nchar(nome_arquivo) < 5 || grepl("\\?", nome_arquivo)) {
        nome_fallback <- tail(strsplit(url_final, "/")[[1]], 1)
        nome_arquivo <- paste0("download_", gsub("[^a-zA-Z0-9]", "_", nome_fallback), ".pdf") # Assumir PDF
        message(paste("   -> 💡 Usando nome de fallback:", nome_arquivo))
      }
    }
    
    # --- Salvando o Arquivo ---
    nome_arquivo_limpo <- gsub("[<>:\"/\\|?*]", "_", nome_arquivo) # Limpa caracteres inválidos
    caminho_final <- file.path(pasta_destino, nome_arquivo_limpo)
    
    writeBin(httr::content(resp, "raw"), con = caminho_final)
    
    message(paste("   ✅ Sucesso! Arquivo salvo como:", nome_arquivo_limpo))
    return(caminho_final)
    
  }, error = function(e) {
    message(paste0("   ❌ FALHA ao baixar: ", url))
    message(paste("           Motivo:", e$message)) 
    return(NA_character_) 
  })
}


# --- 5. Execução Principal (Main) ---

#' @title Orquestrador principal do script
main <- function() {
  
  message("\n✨ Olá! Iniciando a busca pelos arquivos LDO de Roraima... ✨")
  
  selenium_objs <- NULL 
  
  tryCatch(
    {
      # --- Setup ---
      # Garante que a pasta BASE (ex: "LDO-RR") exista
      setup_diretorio(DIRETORIO_DESTINO)
      
      selenium_objs <- iniciar_selenium()
      remDr <- selenium_objs$client
      
      message("\n--- 🚀 Começando a coleta nas páginas ---")
      
      # --- Loop Principal (Iterar sobre as páginas) ---
      for (tarefa in TAREFAS_SCRAPING) {
        
        message(paste0("\n--- 🔎 Processando: LDO ", tarefa$ano, " ---"))
        
        # [NOVA MODIFICAÇÃO] Criar e verificar a subpasta específica do ano
        pasta_do_ano <- file.path(DIRETORIO_DESTINO, tarefa$ano)
        setup_diretorio(pasta_do_ano) # Garante que "LDO-RR/2022" exista
        
        # 1. Extrair os links e títulos (agora retorna um data.frame)
        df_links_para_baixar <- extrair_links_da_pagina(remDr, tarefa$url, tarefa$selector)
        
        if (nrow(df_links_para_baixar) == 0) {
          message("   -> Nenhum link para baixar nesta página. Pulando para a próxima.")
          next
        }
        
        # 2. Baixar os links encontrados
        message(paste("--- 📥 Iniciando downloads para", tarefa$ano, "---"))
        
        # Loop sobre as LINHAS do data.frame
        for (i in 1:nrow(df_links_para_baixar)) {
          
          url_arquivo <- df_links_para_baixar$url[i] 
          titulo_arquivo <- df_links_para_baixar$titulo[i]
          
          message(paste0("[", i, "/", nrow(df_links_para_baixar), "] Baixando: ", titulo_arquivo))
          
          # [NOVA MODIFICAÇÃO] Mudar o destino para a 'pasta_do_ano'
          baixar_arquivo_http(url = url_arquivo, 
                              pasta_destino = pasta_do_ano, # <--- MUDANÇA AQUI
                              nome_base = titulo_arquivo)
          
          # Pausa amigável (Rate Limiting)
          if (i < nrow(df_links_para_baixar)) {
            pausa_atual <- runif(1, min = PAUSA_MIN, max = PAUSA_MAX)
            message(paste0("   -> ⏸️  Pausando por ", round(pausa_atual, 2), " segundos..."))
            Sys.sleep(pausa_atual)
          }
        }
        message(paste("--- ✅ Downloads para", tarefa$ano, "finalizados. ---"))
        
        # Pausa entre as páginas
        message("   -> Pausando antes de processar a próxima página...")
        Sys.sleep(runif(1, 2.0, 4.0))
      }
      
    }, # Fim do bloco 'try'
    
    finally = {
      # --- Bloco de Limpeza (FINALLY) ---
      message("\n--- 🧹 Iniciando limpeza ---")
      if (!is.null(selenium_objs)) {
        encerrar_selenium(selenium_objs)
      }
      message("\n✅ Missão cumprida! O processo foi concluído. Até a próxima! 👋")
    }
  ) # Fim do tryCatch
}

# --- 6. Rodar o script ---
main()