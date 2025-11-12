# --- 1. Pacotes ---
message("Carregando pacotes necessários...")
suppressPackageStartupMessages({
  library(RSelenium)
  library(httr)
  library(stringr)
})

# --- 2. Configurações ---
DIRETORIO_BASE <- "PPA-TO"
PAUSA_MIN <- 0.5
PAUSA_MAX <- 2.0
TIMEOUT_PADRAO <- 120 # segundos

LISTA_PAGINAS <- list(
  list(url = "https://www.to.gov.br/seplan/ppa-2020-2023/5tsy3pqnxcmj",
       seletor = "body > section > div > div > div.col.s12.m10.l8.offset-m1.offset-l2.page_body > div > div:nth-child(8) > div > div > ul a",
       periodo = "2020-2023"),
  
  list(url = "https://www.to.gov.br/seplan/ppa-2016-2019/52w1i7089y5g",
       seletor = "body > section > div > div > div.col.s12.m10.l8.offset-m1.offset-l2.page_body > div > div:nth-child(3) > div > div > ul a",
       periodo = "2016-2019"),
  
  list(url = "https://www.to.gov.br/seplan/ppa-2012-2015/4llu5yd3jp5v",
       seletor = "body > section > div > div > div.col.s12.m10.l8.offset-m1.offset-l2.page_body > div > div:nth-child(1) > div > div > ul a",
       periodo = "2012-2015"),
  
  list(url = "https://www.to.gov.br/seplan/ppa-2008-2011/4gzq8zcq1f6z",
       seletor = "body > section > div > div > div.col.s12.m10.l8.offset-m1.offset-l2.page_body > div > div:nth-child(1) > div > div > ul a",
       periodo = "2008-2011")
)

# =====================================================================
# --- 3. Funções utilitárias ---
# =====================================================================

criar_pasta <- function(caminho) {
  if (!dir.exists(caminho)) {
    dir.create(caminho, recursive = TRUE)
    message("📁 Pasta criada: ", caminho)
  }
}

esperar <- function(min, max) {
  Sys.sleep(runif(1, min, max))
}

limpar_texto <- function(texto) {
  texto |>
    stringr::str_squish() |>
    stringr::str_replace_all("[<>:\"/\\\\|?*]", "_") |>
    stringr::str_replace_all("file_open|file_open ", "")
}

# =====================================================================
# --- 4. Funções do Selenium ---
# =====================================================================

iniciar_selenium <- function() {
  message("🚀 Iniciando Selenium...")
  
  if (.Platform$OS.type == "unix") system("pkill -f 'selenium-standalone'")
  else try(system("taskkill /F /IM java.exe /T"), silent = TRUE)
  
  driver <- RSelenium::rsDriver(
    chromever = NULL, 
    phantomver = NULL,
    browser = "firefox",
    check = FALSE,
    verbose = FALSE
  )
  
  cliente <- driver$client
  cliente$close()
  cliente$open()
  
  message("✅ Selenium pronto!")
  list(driver = driver, cliente = cliente)
}

encerrar_selenium <- function(sessao) {
  message("🛑 Encerrando Selenium...")
  try(sessao$cliente$close(), silent = TRUE)
  try(sessao$driver$server$stop(), silent = TRUE)
}

# =====================================================================
# --- 5. Funções de extração ---
# =====================================================================

extrair_elementos <- function(cliente, seletor) {
  elementos <- cliente$findElements(using = "css selector", value = seletor)
  if (length(elementos) == 0) return(NULL)
  elementos
}

extrair_dados_link <- function(elemento) {
  link <- try(unlist(elemento$getElementAttribute("href")), silent = TRUE)
  titulo <- try(unlist(elemento$getElementText()), silent = TRUE)
  
  if (is.null(titulo) || nchar(trimws(titulo)) == 0) titulo <- basename(link)
  titulo <- limpar_texto(titulo)
  
  data.frame(url = link, titulo = titulo, stringsAsFactors = FALSE)
}

extrair_links_pagina <- function(cliente, url, seletor) {
  message("🌐 Acessando página: ", url)
  try(cliente$navigate(url))
  Sys.sleep(5)
  
  elementos <- extrair_elementos(cliente, seletor)
  
  if (is.null(elementos)) {
    message("⚠️ Nenhum link encontrado com o seletor informado.")
    return(data.frame(url = character(), titulo = character()))
  }
  
  dados <- lapply(elementos, extrair_dados_link)
  resultado <- do.call(rbind, dados)
  resultado <- na.omit(resultado)
  
  message("✅ ", nrow(resultado), " links encontrados.")
  resultado
}

# =====================================================================
# --- 6. Funções de download ---
# =====================================================================

gerar_nome_arquivo <- function(resp, nome_base) {
  extensao <- stringr::str_extract(utils::URLdecode(basename(resp$url)), "\\.[^\\.]+$")
  if (is.na(extensao)) extensao <- ".pdf"
  nome <- paste0(nome_base, extensao)
  gsub("[<>:\"/\\|?*]", "_", nome)
}

baixar_arquivo <- function(url, pasta, nome_base = NULL, timeout = TIMEOUT_PADRAO) {
  tryCatch({
    resp <- httr::GET(
      url,
      httr::user_agent("Mozilla/5.0"),
      httr::timeout(timeout)
    )
    httr::stop_for_status(resp)
    
    nome_arquivo <- if (!is.null(nome_base) && nchar(nome_base) > 0)
      gerar_nome_arquivo(resp, nome_base)
    else
      basename(resp$url)
    
    caminho <- file.path(pasta, nome_arquivo)
    writeBin(httr::content(resp, "raw"), caminho)
    message("✅ Baixado: ", nome_arquivo)
    caminho
  },
  error = function(e) {
    message("❌ Erro ao baixar ", url, ": ", e$message)
    NA
  })
}

# =====================================================================
# --- 7. Execução por período ---
# =====================================================================

processar_periodo <- function(cliente, pagina) {
  pasta <- file.path(DIRETORIO_BASE, pagina$periodo)
  criar_pasta(pasta)
  
  message("\n--- 🔎 Coletando período ", pagina$periodo, " ---")
  links <- extrair_links_pagina(cliente, pagina$url, pagina$seletor)
  
  if (nrow(links) == 0) return()
  
  for (i in seq_len(nrow(links))) {
    link <- links$url[i]
    titulo <- links$titulo[i]
    
    message("[", i, "/", nrow(links), "] Baixando: ", titulo)
    baixar_arquivo(link, pasta, titulo, timeout = TIMEOUT_PADRAO)
    esperar(PAUSA_MIN, PAUSA_MAX)
  }
  
  message("✅ Downloads concluídos para ", pagina$periodo)
  esperar(2.0, 4.0)
}

# =====================================================================
# --- 8. Função principal ---
# =====================================================================

main <- function() {
  message("\n✨ Iniciando coleta de arquivos PPA-TO ✨")
  
  criar_pasta(DIRETORIO_BASE)
  sessao <- iniciar_selenium()
  cliente <- sessao$cliente
  
  for (pagina in LISTA_PAGINAS) {
    processar_periodo(cliente, pagina)
  }
  
  encerrar_selenium(sessao)
  message("\n🎉 Processo concluído com sucesso! 👋")
}

# --- 9. Rodar ---
main()


# =====================================================================
# --- 10. Baixar diretamente o PPA 2024-2027 ---
# =====================================================================

baixar_ppa_2024_2027 <- function() {
  url_ppa <- "https://central.to.gov.br/download/368857"
  periodo <- "2024-2027"
  pasta <- file.path(DIRETORIO_BASE, periodo)
  
  criar_pasta(pasta)
  
  message("\n--- 🔎 Baixando PPA ", periodo, " ---")
  nome_arquivo <- "Lei nº 4.275-2023 - Institui o PPA 2024-2027.pdf"
  caminho <- file.path(pasta, nome_arquivo)
  
  # Define método mais robusto conforme sistema operacional
  metodo <- if (.Platform$OS.type == "unix") "curl" else "wininet"
  
  tryCatch({
    download.file(
      url = url_ppa,
      destfile = caminho,
      mode = "wb",       # modo binário (essencial para PDF)
      method = metodo,
      quiet = FALSE
    )
    message("✅ PPA ", periodo, " baixado com sucesso!")
  },
  error = function(e) {
    message("❌ Falha ao baixar PPA ", periodo, ": ", e$message)
  })
}

# --- 11. Executar download do PPA 2024-2027 ---
baixar_ppa_2024_2027()