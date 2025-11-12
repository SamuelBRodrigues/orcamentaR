# =====================================================================
# 📘 COLETA DE ARQUIVOS PPA - ESTADO DE SERGIPE (PPA-SE)
# =====================================================================

# --- 1. Pacotes ---
message("Carregando pacotes necessários...")
suppressPackageStartupMessages({
  library(RSelenium)
  library(httr)
  library(stringr)
})

# --- 2. Configurações ---
DIRETORIO_BASE <- "PPA-SE"
PAUSA_MIN <- 0.5
PAUSA_MAX <- 2.0
TIMEOUT_PADRAO <- 120 # segundos

LISTA_PAGINAS <- list(
  list(
    url = "https://www.transparencia.se.gov.br/Planejamento/PPA.xhtml",
    seletor = "#frmPrincipal\\:j_idt168_data > tr:nth-child(2) a, tr:nth-child(3) a, tr:nth-child(4) a, tr:nth-child(5) a",
    periodo = "2024-2027"
  ),

  list(
    url = "https://www.transparencia.se.gov.br/Planejamento/PPA.xhtml",
    seletor = "#frmPrincipal\\:j_idt168_data > tr:nth-child(7) a",
    periodo = "2020-2023"
  ),
  
  list(
    url = "https://www.transparencia.se.gov.br/Planejamento/PPA.xhtml",
    seletor = "#frmPrincipal\\:j_idt168_data > tr:nth-child(9) a, tr:nth-child(10) a, tr:nth-child(11) a",
    periodo = "2016-2019"
  ),
  
  list(
    url = "https://www.transparencia.se.gov.br/Planejamento/PPA.xhtml",
    seletor = "#frmPrincipal\\:j_idt168_data > tr:nth-child(13) a, tr:nth-child(14) a, tr:nth-child(15) a, tr:nth-child(16) a",
    periodo = "2012-2015"
  ),
  
  list(
    url = "https://www.transparencia.se.gov.br/Planejamento/PPA.xhtml",
    seletor = "#frmPrincipal\\:j_idt168_data > tr:nth-child(18) a, tr:nth-child(19) a, tr:nth-child(20) a, tr:nth-child(21) a, tr:nth-child(22) a, tr:nth-child(23) a",
    periodo = "2008-2011"
  )
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
  
  # Encerra instâncias antigas
  if (.Platform$OS.type == "unix") system("pkill -f 'selenium-standalone'", ignore.stdout = TRUE, ignore.stderr = TRUE)
  else try(system("taskkill /F /IM java.exe /T"), silent = TRUE)
  
  # Inicia driver Firefox
  driver <- RSelenium::rsDriver(
    chromever = NULL,
    phantomver = NULL,
    browser = "firefox",
    check = FALSE,
    verbose = FALSE
  )
  
  cliente <- driver$client
  try(cliente$close(), silent = TRUE)
  Sys.sleep(2)
  try(cliente$open(), silent = TRUE)
  
  message("✅ Selenium pronto!")
  list(driver = driver, cliente = cliente)
}

encerrar_selenium <- function(sessao) {
  if (is.null(sessao)) return()
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
  
  # Espera inteligente até o seletor aparecer
  tentativas <- 0
  elementos <- NULL
  repeat {
    Sys.sleep(3)
    elementos <- extrair_elementos(cliente, seletor)
    tentativas <- tentativas + 1
    if (!is.null(elementos) || tentativas >= 5) break
  }
  
  if (is.null(elementos)) {
    message("⚠️ Nenhum link encontrado com o seletor informado após várias tentativas.")
    return(data.frame(url = character(), titulo = character()))
  }
  
  dados <- lapply(elementos, extrair_dados_link)
  resultado <- do.call(rbind, dados)
  resultado <- na.omit(resultado)
  resultado <- resultado[!duplicated(resultado$url), ]
  
  message("✅ ", nrow(resultado), " links únicos encontrados.")
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
  message("\n✨ Iniciando coleta de arquivos PPA-SE ✨")
  
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
