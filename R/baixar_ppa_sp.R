# --- 1. Pacotes ---
message("Carregando pacotes necessários...")
suppressPackageStartupMessages({
  library(RSelenium)
  library(httr)
  library(stringr)
})

# --- 2. Configurações ---
DIRETORIO_BASE <- "PPA-SP"
PAUSA_MIN <- 0.5
PAUSA_MAX <- 2.0
TIMEOUT_PADRAO <- 120 # segundos

LISTA_PAGINAS <- list(
  list(url = "http://planejamento.sp.gov.br/ppa/",
       seletor = "#lei-ppa-1 a, #lei-ppa-2 a, #anexo-ppa-3 a, #anexo-ppa-4 a",
       periodo = "2020-2023"),
  
  list(url = "http://planejamento.sp.gov.br/ppa/",
       seletor = "#accordion-ppa-2016-1 a, #accordion-ppa-2016-2 a, #accordion-ppa-2016-3 a, #accordion-ppa-2016-4 a, #accordion-ppa-2016-5 a, #accordion-ppa-2016-6 a",
       periodo = "2016-2019"),
  
  list(url = "http://planejamento.sp.gov.br/ppa/",
       seletor = "#accordion-ppa-2012 a",
       periodo = "2012-2015"),
  
  list(url = "http://planejamento.sp.gov.br/ppa/",
       seletor = "#accordion-ppa-2008 a",
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
  
  # 🔹 Remover links duplicados
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


# ------- Baixar 2024-2027 -------------

baixar_periodo_2024_2027 <- function(pasta_base = DIRETORIO_BASE) {
  periodo <- "2024-2027"
  pasta <- file.path(pasta_base, periodo)
  criar_pasta(pasta)
  
  url <- "https://portal.fazenda.sp.gov.br/servicos/planejamento/Documents/PPA2024-2027/Lei%20n.%C2%BA%2017.898-2024%20-%20Institui%20o%20PPA.pdf"
  nome_base <- "Lei_17.898_2024_PPA_2024-2027"
  
  message("\n--- 🔎 Baixando período ", periodo, " ---")
  caminho <- baixar_arquivo(url, pasta, nome_base, timeout = 600)
  if (!is.na(caminho)) {
    message("✅ Download concluído para o período ", periodo, ": ", caminho)
  } else {
    message("❌ Falha no download para o período ", periodo)
  }
}

baixar_periodo_2024_2027()
