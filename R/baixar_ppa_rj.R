# --- 1. Pacotes ---
message("Carregando pacotes necessários...")
suppressPackageStartupMessages({
  library(RSelenium)
  library(httr)
  library(stringr)
})

# --- 2. Configurações ---
DIRETORIO_BASE <- "PPA-RJ"
PAUSA_MIN <- 0.5
PAUSA_MAX <- 2.0
TIMEOUT_PADRAO <- 120 # segundos

LISTA_PAGINAS <- list(
  list(
    url = "https://portal.fazenda.rj.gov.br/tesouro/ppa-ldo-loa/",
    seletor = '#collapse-parent-2367 > div > ul:nth-child(2) a, 
            #collapse-parent-2367 > div > ul:nth-child(4) a, 
            #collapse-parent-2367 > div > ul:nth-child(6) a, 
            #collapse-parent-2367 > div > ul:nth-child(8) a, 
            #collapse-parent-2367 > div > ul:nth-child(31) a',
    periodo = "2020-2023"
  ),
  
  list(
    url = "https://portal.fazenda.rj.gov.br/tesouro/ppa-ldo-loa/",
    seletor = '#collapse-parent-2367 > div > ul:nth-child(10) a, 
            #collapse-parent-2367 > div > ul:nth-child(12) a, 
            #collapse-parent-2367 > div > ul:nth-child(15) a, 
            #collapse-parent-2367 > div > ul:nth-child(18) a, 
            #collapse-parent-2367 > div > ul:nth-child(35) a, 
            #collapse-parent-2367 > div > ul:nth-child(39) a, 
            #collapse-parent-2367 > div > ul:nth-child(43) a, 
            #collapse-parent-2367 > div > ul:nth-child(47) a',
    periodo = "2016-2019"
  ),
  
  list(
    url = "https://portal.fazenda.rj.gov.br/tesouro/ppa-ldo-loa/",
    seletor = '#collapse-parent-2367 > div > ul:nth-child(22) a, 
            #collapse-parent-2367 > div > ul:nth-child(24) a, 
            #collapse-parent-2367 > div > ul:nth-child(26) a, 
            #collapse-parent-2367 > div > ul:nth-child(28) a,
            #collapse-parent-2367 > div > ul:nth-child(51) a,
            #collapse-parent-2367 > div > ul:nth-child(53) a,
            #collapse-parent-2367 > div > ul:nth-child(55) a,
            #collapse-parent-2367 > div > ul:nth-child(57) a',
    periodo = "2012-2015"
  ),
  
  list(
    url = "https://portal.fazenda.rj.gov.br/tesouro/ppa-ldo-loa/",
    seletor = '#collapse-parent-2367 > div > ul:nth-child(59) a',
    periodo = "2008-2011"
  ),
  
  list(
    url = "https://portal.fazenda.rj.gov.br/tesouro/ppa-ldo-loa/",
    seletor = '#collapse-parent-2367 > div > ul:nth-child(61) a',
    periodo = "2004-2007"
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
