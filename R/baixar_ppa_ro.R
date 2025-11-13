LISTA_PAGINAS <- list(  
  list(
    url = "https://sepog.ro.gov.br/Paginas/198/ppa-2024-2027-revisao-2025",
    seletor = '#collapse_34 a[title="Baixar arquivo"]',
    seletor_nome = "#collapse_34 > div > div > a > div > div.font-roboto.titulo.titulo-list-customizado",
    periodo = "2024-2027" # Revisão 2025
  ),

  list(
    url = "https://sepog.ro.gov.br/Paginas/129/plano-plurianual-2024-2027-elaboracao",
    seletor = '#collapse_34 a[title="Baixar arquivo"]',
    seletor_nome = "#collapse_34 > div > div > a > div > div.font-roboto.titulo.titulo-list-customizado",
    periodo = "2024-2027"
  ),

  list(
    url = "https://sepog.ro.gov.br/Paginas/12/plano-plurianual-2020-2023",
    seletor = '#collapse_0 > div a[title="Baixar arquivo"]',
    seletor_nome = "#collapse_0 > div > div > a > div > div.font-roboto.titulo.titulo-list-customizado",
    periodo = "2020-2023"
  ),
  
  list(
    url = "https://sepog.ro.gov.br/Paginas/111/plano-plurianual-2016-2019",
    seletor = '#collapse_0 > div a[title="Baixar arquivo"]',
    seletor_nome = "#collapse_0 > div > div > a > div > div.font-roboto.titulo.titulo-list-customizado",
    periodo = "2016-2019"
  ),
  
  list(
    url = "https://sepog.ro.gov.br/Paginas/107/plano-plurianual-2012-2015",
    seletor = '#collapse_0 > div a[title="Baixar arquivo"]',
    seletor_nome = "#collapse_0 > div > div > a > div > div.font-roboto.titulo.titulo-list-customizado",
    periodo = "2012-2015"
  ),
  
  list(
    url = "https://sepog.ro.gov.br/Paginas/110/plano-plurianual-2012-2015-revisao-2015",
    seletor = '#collapse_0 > div a[title="Baixar arquivo"]',
    seletor_nome = "#collapse_0 > div > div > a > div > div.font-roboto.titulo.titulo-list-customizado",
    periodo = "2012-2015"
  ),
  
  list(
    url = "https://sepog.ro.gov.br/Paginas/109/plano-plurianual-2012-2015-revisao-2014",
    seletor = '#collapse_0 > div a[title="Baixar arquivo"]',
    seletor_nome = "#collapse_0 > div > div > a > div > div.font-roboto.titulo.titulo-list-customizado",
    periodo = "2012-2015"
  ),
  
  list(
    url = "https://sepog.ro.gov.br/Paginas/108/plano-plurianual-2012-2015-revisao-2013",
    seletor = '#collapse_0 > div a[title="Baixar arquivo"]',
    seletor_nome = "#collapse_0 > div > div > a > div > div.font-roboto.titulo.titulo-list-customizado",
    periodo = "2012-2015"
  ),
  
  list(
    url = "https://sepog.ro.gov.br/Paginas/91/plano-plurianual-2008-2011",
    seletor = '#collapse_0 > div a[title="Baixar arquivo"]',
    seletor_nome = "#collapse_0 > div > div > a > div > div.font-roboto.titulo.titulo-list-customizado",
    periodo = "2008-2011"
  ),
  
  list(
    url = "https://sepog.ro.gov.br/Paginas/97/plano-plurianual-2008-2011-revisao-2011",
    seletor = '#collapse_0 > div a[title="Baixar arquivo"]',
    seletor_nome = "#collapse_0 > div > div > a > div > div.font-roboto.titulo.titulo-list-customizado",
    periodo = "2008-2011"
  ),
  
  list(
    url = "https://sepog.ro.gov.br/Paginas/96/plano-plurianual-2008-2011-revisao-2010",
    seletor = '#collapse_0 > div a[title="Baixar arquivo"]',
    seletor_nome = "#collapse_0 > div > div > a > div > div.font-roboto.titulo.titulo-list-customizado",
    periodo = "2008-2011"
  ),
  
  list(
    url = "https://sepog.ro.gov.br/Paginas/94/plano-plurianual-2008-2011-revisao-2009",
    seletor = '#collapse_0 > div a[title="Baixar arquivo"]',
    seletor_nome = "#collapse_0 > div > div > a > div > div.font-roboto.titulo.titulo-list-customizado",
    periodo = "2008-2011"
  ),
  
  list(
    url = "https://sepog.ro.gov.br/Conteudos/781/lei-1-699-de-01-de-janeiro-de-2007",
    seletor = 'body > div.main-container > main > section.download-area.mt-5 > a',
    seletor_nome = "#Titulo_Conteudo > span",
    periodo = "2004-2007"
  )
)

# --- 1. Pacotes ---
message("Carregando pacotes necessários...")
suppressPackageStartupMessages({
  library(RSelenium)
  library(httr)
  library(stringr)
})

# --- 2. Configurações ---
DIRETORIO_BASE <- "PPA-RO"
PAUSA_MIN <- 0.5
PAUSA_MAX <- 2.0
TIMEOUT_PADRAO <- 120 # segundos


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

iniciar_selenium <- function(porta = 4567L) {
  message("🚀 Iniciando Selenium...")

  # --- 1. Verifica se a porta está em uso ---
  porta_em_uso <- FALSE
  if (.Platform$OS.type == "unix") {
    # Linux / macOS
    saida <- system(sprintf("lsof -i :%d", porta), intern = TRUE, ignore.stderr = TRUE)
    porta_em_uso <- length(saida) > 0
  } else {
    # Windows
    saida <- system(sprintf("netstat -ano | findstr :%d", porta), intern = TRUE, ignore.stderr = TRUE)
    porta_em_uso <- length(saida) > 0
  }

  # --- 2. Encerra Selenium se a porta estiver em uso ---
  if (porta_em_uso) {
    message(sprintf("⚠️  Porta %d em uso. Encerrando processos antigos...", porta))
    if (.Platform$OS.type == "unix") {
      system("pkill -f 'selenium-standalone'", ignore.stdout = TRUE, ignore.stderr = TRUE)
      system("pkill -f 'geckodriver'", ignore.stdout = TRUE, ignore.stderr = TRUE)
    } else {
      system("taskkill /F /IM java.exe /T", ignore.stdout = TRUE, ignore.stderr = TRUE)
      system("taskkill /F /IM geckodriver.exe /T", ignore.stdout = TRUE, ignore.stderr = TRUE)
    }
    Sys.sleep(2) # pequena pausa para liberar recursos
  }

  # --- 3. Inicia o driver Firefox ---
  driver <- RSelenium::rsDriver(
    chromever = NULL, 
    phantomver = NULL,
    port = porta,
    browser = "firefox",
    check = FALSE,
    verbose = FALSE
  )

  cliente <- driver$client
  try(cliente$close(), silent = TRUE)
  Sys.sleep(2)
  try(cliente$open(), silent = TRUE)

  message("✅ Selenium pronto na porta ", porta, "!")
  list(driver = driver, cliente = cliente)
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
  
  # Espera mais curta
  Sys.sleep(1)
  
  # Tenta achar o seletor até 3 vezes, com pausas menores
  tentativas <- 0
  elementos <- NULL
  repeat {
    Sys.sleep(1) # diminui de 3 para 1 segundo
    elementos <- extrair_elementos(cliente, seletor)
    tentativas <- tentativas + 1
    if (!is.null(elementos) || tentativas >= 3) break
  }
  
  if (is.null(elementos)) {
    message("⚠️ Nenhum link encontrado.")
    return(data.frame(url = character(), titulo = character()))
  }
  
  dados <- lapply(elementos, extrair_dados_link)
  resultado <- do.call(rbind, dados)
  resultado <- na.omit(resultado)
  resultado <- resultado[!duplicated(resultado$url), ]
  
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
  message("\n✨ Iniciando coleta de arquivos PPA-RJ ✨")
  
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
