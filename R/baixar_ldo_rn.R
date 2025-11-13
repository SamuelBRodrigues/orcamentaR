# ==========================================================
# --- 1. Pacotes necessários ---
# ==========================================================
library(httr2)
library(rvest)
library(stringr)
library(dplyr)
library(stringi)

# ==========================================================
# --- 2. Funções utilitárias ---
# ==========================================================

# 📂 Cria as pastas base e de períodos
criar_pastas_periodos <- function(pasta_base, periodos) {
  if (!dir.exists(pasta_base)) {
    dir.create(pasta_base, recursive = TRUE)
    message("📁 Pasta base criada: ", pasta_base)
  }
  
  for (p in c(periodos, "Outros")) {
    dir.create(file.path(pasta_base, p), recursive = TRUE, showWarnings = FALSE)
  }
  message("📚 Estrutura de pastas pronta!\n")
}

# 🔢 Identifica o período com base nos anos encontrados no nome do arquivo
# 🔢 Identifica o ano com base na palavra "LDO" seguida de um número de 4 dígitos
identificar_periodo <- function(nome_arquivo, periodos) {
  # Normaliza o nome (remove acentos e força maiúsculas para evitar variações como "ldo")
  nome_limpo <- stringi::stri_trans_general(nome_arquivo, "Latin-ASCII")
  nome_limpo <- toupper(nome_limpo)
  
  # Procura padrão "LDO" seguida de 4 dígitos (ex: LDO2023, LDO_2023, LDO-2023)
  match <- stringr::str_match(nome_limpo, "LDO[^0-9]*?(\\d{4})")
  
  if (!is.na(match[1, 2])) {
    ano <- match[1, 2]
    if (ano %in% periodos) return(ano)
  }
  
  return("Outros")
}


# 🔁 Realiza uma requisição com várias tentativas e backoff exponencial
tentar_requisicao <- function(req, max_tentativas = 5) {
  for (i in seq_len(max_tentativas)) {
    resp <- tryCatch(req_perform(req), error = function(e) NULL)
    
    if (!is.null(resp) && resp_status(resp) == 200) return(resp)
    
    espera <- runif(1, 1, 3) * 2^(i - 1)
    message(sprintf("⚠️  Tentativa %d falhou. Repetindo em %.1f segundos...", i, espera))
    Sys.sleep(espera)
  }
  return(NULL)
}

# 🧾 Extrai o nome original do arquivo do cabeçalho HTTP
obter_nome_arquivo_http <- function(url) {
  req <- request(url) |> req_timeout(10)
  resp <- tentar_requisicao(req, max_tentativas = 3)
  
  nome <- basename(url)
  if (is.null(resp)) return(nome)
  
  cabecalho <- resp_header(resp, "content-disposition")
  if (!is.null(cabecalho)) {
    nome_extraido <- str_match(cabecalho, 'filename="?([^";]+)"?')[, 2]
    if (!is.na(nome_extraido) && nome_extraido != "") nome <- nome_extraido
  }
  
  nome <- URLdecode(iconv(nome, from = "latin1", to = "UTF-8", sub = ""))
  nome <- str_replace_all(nome, "[\r\n\t]", "")
  nome <- sub("\\?.*$", "", nome)
  nome <- trimws(nome)
  
  if (is.na(nome) || nome == "" || nome %in% c("download", "file", "index")) {
    nome <- paste0("arquivo_", format(Sys.time(), "%Y%m%d%H%M%S"))
  }
  
  if (!str_detect(tolower(nome), "\\.[a-z0-9]+$")) nome <- paste0(nome, ".pdf")
  return(nome)
}

# 🧩 Limpa e padroniza nome de arquivo
tratar_nome_arquivo <- function(nome) {
  nome <- enc2utf8(nome)
  extensao <- tools::file_ext(nome)
  
  nome <- stri_trans_general(nome, "Latin-ASCII")
  nome <- str_replace_all(nome, "[^[:alnum:]_\\-\\. ]", "")
  nome <- str_squish(nome)
  nome <- str_replace_all(nome, " ", "_")
  nome <- substr(nome, 1, 100)
  
  if (extensao == "") {
    nome <- paste0(nome, ".pdf")
  } else if (!grepl(paste0("\\.", extensao, "$"), nome, ignore.case = TRUE)) {
    nome <- paste0(nome, ".", extensao)
  }
  
  return(nome)
}

# 💾 Faz o download de um único arquivo com feedbacks amigáveis
baixar_pdf <- function(url, nome_arquivo, pasta_base, periodos) {
  nome_arquivo <- tratar_nome_arquivo(nome_arquivo)
  periodo <- identificar_periodo(nome_arquivo, periodos)
  pasta_destino <- file.path(pasta_base, periodo)
  caminho_saida <- file.path(pasta_destino, nome_arquivo)
  
  if (!dir.exists(pasta_destino)) dir.create(pasta_destino, recursive = TRUE)
  
  message("⬇️  Baixando: ", nome_arquivo, " → ", periodo)
  
  req <- request(url) |> req_timeout(60)
  resp <- tentar_requisicao(req, max_tentativas = 5)
  
  if (is.null(resp)) {
    warning("❌ Falha após várias tentativas: ", url)
    return(NA)
  }
  
  tryCatch({
    writeBin(resp_body_raw(resp), caminho_saida)
    message("✅ Sucesso: ", caminho_saida, "\n")
  }, error = function(e) {
    warning("❌ Erro ao salvar arquivo: ", e$message)
  })
  
  return(caminho_saida)
}

# 🔗 Extrai todos os links <a> e o texto dentro do próprio link
extrair_links_e_nomes <- function(url_pagina, seletor) {
  message("🌐 Acessando página: ", url_pagina)
  
  pagina <- tryCatch(read_html(url_pagina), error = function(e) {
    stop("❌ Erro ao carregar a página: ", e$message)
  })
  
  tags_a <- pagina |> html_elements(seletor)
  links <- tags_a |> html_attr("href")
  textos <- tags_a |> html_text(trim = TRUE)
  
  df <- tibble::tibble(
    url = url_absolute(links, url_pagina),
    nome_link = textos
  ) |> filter(!is.na(url), url != "")
  
  if (nrow(df) == 0) stop("⚠️ Nenhum link encontrado com o seletor informado: ", seletor)
  
  message("🔗 ", nrow(df), " link(s) encontrado(s).\n")
  return(df)
}

# ==========================================================
# --- 3. Função orquestradora ---
# ==========================================================
baixar_pdfs_por_periodo <- function(url_pagina, seletor, pasta_base, periodos) {
  message("🚀 Iniciando processo de download...\n")
  
  criar_pastas_periodos(pasta_base, periodos)
  dados <- extrair_links_e_nomes(url_pagina, seletor)
  
  resultados <- mapply(
    function(url, nome) {
      nome_final <- if (!is.na(nome) && nome != "") nome else obter_nome_arquivo_http(url)
      baixar_pdf(url, nome_final, pasta_base, periodos)
    },
    dados$url,
    dados$nome_link,
    SIMPLIFY = FALSE
  )
  
  sucesso <- sum(!is.na(resultados))
  falhas <- length(resultados) - sucesso
  
  message("🏁 Processo concluído!")
  message("📦 Total: ", length(resultados), 
          " | ✅ Sucesso: ", sucesso, 
          " | ⚠️ Falhas: ", falhas, "\n")
  
  invisible(resultados)
}

# ==========================================================
# --- 4. Executando ---
# ==========================================================
periodos <- as.character(2001:2025)

url_pagina <- "http://adcon.rn.gov.br/ACERVO/seplan/Conteudo.asp?TRAN=PASTAC&TARG=2476&ACT=&PAGE=&PARM=&LBL="

seletor <- "#ACERVO a"

baixar_pdfs_por_periodo(
  url_pagina = url_pagina,
  seletor = seletor,
  pasta_base = "LDO-RN",
  periodos = periodos
)

# ==========================================================
# --- 5. Download adicional (LDO 2025) ---
# ==========================================================
url_extra <- "https://cdn.direcaoconcursos.com.br/uploads/2024/08/LDO-2025-RN.pdf"
nome_extra <- "LDO 2025 RN.pdf"

# Faz o download e salva na pasta correspondente automaticamente
baixar_pdf(
  url = url_extra,
  nome_arquivo = nome_extra,
  pasta_base = "LDO-RN",
  periodos = periodos
)
