library(RSelenium)
library(stringr)
library(purrr)

# =====================================================================
# --- 1. Utilidades ----------------------------------------------------
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
# --- 2. Encerrar Selenium --------------------------------------------
# =====================================================================

encerrar_selenium <- function(sessao) {
  message("🛑 Encerrando Selenium e restaurando o Firefox...")

  if (is.null(sessao)) {
    message("⚠️ Sessão Selenium inválida.")
    return(invisible(FALSE))
  }

  cliente <- sessao$cliente
  driver  <- sessao$driver
  profile_dir <- sessao$profile_dir

  # 1) Encerrar navegador
  try({ cliente$close() }, silent = TRUE)
  try({ cliente$quit() },  silent = TRUE)

  # 2) Encerrar servidor Selenium
  try({ driver$server$stop() }, silent = TRUE)

  # 3) Matar processos pendentes
  if (.Platform$OS.type == "unix") {
    system("pkill -f geckodriver", ignore.stdout = TRUE, ignore.stderr = TRUE)
    system("pkill -f firefox", ignore.stdout = TRUE, ignore.stderr = TRUE)
  } else {
    system("taskkill /F /IM geckodriver.exe /T", ignore.stdout = TRUE, ignore.stderr = TRUE)
    system("taskkill /F /IM firefox.exe /T",       ignore.stdout = TRUE, ignore.stderr = TRUE)
  }

  # 4) Remover perfil temporário
  if (!is.null(profile_dir) && dir.exists(profile_dir)) {
    unlink(profile_dir, recursive = TRUE, force = TRUE)
    message("🧹 Perfil temporário removido: ", profile_dir)
  }

  message("✅ Selenium encerrado e Firefox restaurado!")
  invisible(TRUE)
}

# =====================================================================
# --- 3. Aguardar downloads (Firefox) ---------------------------------
# =====================================================================

aguardar_downloads <- function(download_dir, intervalo = 1) {
  message("⬇️ Aguardando compactação e download do Google Drive...")
  readline("Aperte enter quando terminar de baixar: ")
  
}



# =====================================================================
# --- 4. Selenium ------------------------------------------------------
# =====================================================================

iniciar_selenium <- function(porta = 4567L, download_dir) {
  message("🚀 Iniciando Selenium...")

  # Criar diretório temporário de perfil
  profile_dir <- tempfile(pattern = "firefox_profile")
  dir.create(profile_dir)

  # Criar user.js com configurações
  user_js <- file.path(profile_dir, "user.js")
  writeLines(c(
    'user_pref("browser.download.folderList", 2);',
    paste0('user_pref("browser.download.dir", "', download_dir, '");'),
    'user_pref("browser.download.useDownloadDir", true);',
    'user_pref("browser.helperApps.neverAsk.saveToDisk", "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet, text/csv");',
    'user_pref("pdfjs.disabled", true);',
    'user_pref("browser.download.manager.showWhenStarting", false);',
    'user_pref("browser.download.manager.closeWhenDone", true);',
    'user_pref("browser.download.autoOpenValue", 0);'
  ), con = user_js)

  # Verificar porta
  porta_em_uso <- FALSE
  if (.Platform$OS.type == "unix") {
    saida <- system(sprintf("lsof -i :%d", porta), intern = TRUE, ignore.stderr = TRUE)
    porta_em_uso <- length(saida) > 0
  } else {
    saida <- system(sprintf("netstat -ano | findstr :%d", porta), intern = TRUE, ignore.stderr = TRUE)
    porta_em_uso <- length(saida) > 0
  }

  # Matar processos na porta
  if (porta_em_uso) {
    message(sprintf("⚠️ Porta %d em uso. Encerrando processos antigos...", porta))
    if (.Platform$OS.type == "unix") {
      system("pkill -f 'selenium-standalone'", ignore.stdout = TRUE, ignore.stderr = TRUE)
      system("pkill -f geckodriver", ignore.stdout = TRUE, ignore.stderr = TRUE)
    } else {
      system("taskkill /F /IM java.exe /T",       ignore.stdout = TRUE, ignore.stderr = TRUE)
      system("taskkill /F /IM geckodriver.exe /T", ignore.stdout = TRUE, ignore.stderr = TRUE)
    }
    Sys.sleep(2)
  }

  # Iniciar Firefox
  driver <- RSelenium::rsDriver(
    chromever = NULL,
    phantomver = NULL,
    port = porta,
    extraCapabilities = list(
      "moz:firefoxOptions" = list(
        args = list("-profile", profile_dir)
      )
    ),
    browser = "firefox",
    check = FALSE,
    verbose = FALSE
  )

  cliente <- driver$client
  try(cliente$close(), silent = TRUE)
  Sys.sleep(2)
  try(cliente$open(), silent = TRUE)

  message("✅ Selenium pronto na porta ", porta, "!")
  list(driver = driver, cliente = cliente, profile_dir = profile_dir)
}

# =====================================================================
# --- 5. Funções básicas: clicar e navegar ----------------------------
# =====================================================================

clicar_no_elemento <- function(cliente, seletor, by = "css selector", esperar = 2) {
  tryCatch({
    message("🖱️ Clique em: ", seletor)
    elemento <- cliente$findElement(using = by, value = seletor)
    elemento$clickElement()
    Sys.sleep(esperar)
    TRUE
  }, error = function(e) {
    message("❌ Erro ao clicar: ", seletor)
    FALSE
  })
}

ir_para_pagina <- function(cliente, url, esperar = 3) {
  message("🌐 Indo para: ", url)
  cliente$navigate(url)
  Sys.sleep(esperar)
}

# =====================================================================
# --- 6. Função Principal ---------------------------------------------
# =====================================================================

main <- function() {
  message("\n✨ Iniciando coleta de arquivos PPA-MG ✨")

  criar_pasta("PPA-MG")

  download_dir <- normalizePath("PPA-MG", mustWork = FALSE)
  
  sessao <- iniciar_selenium(download_dir = download_dir)
  cliente <- sessao$cliente

  ir_para_pagina(cliente, "https://drive.google.com/drive/u/0/folders/1JkrEK_CAQusDvQdjNN3khiL6EODoiZdk")

  botoes_baixar <- cliente$findElements("css selector", 'button[aria-label="Baixar"]')

  message("🔍 Encontrados ", length(botoes_baixar), " botões para baixar.")

  cliente$setWindowSize(1920, 1080)

  purrr::walk(botoes_baixar, ~{
    cliente$mouseMoveToLocation(webElement = .x)
    Sys.sleep(0.3)
    cliente$click()
    Sys.sleep(1)
  })


  aguardar_downloads(download_dir)

  encerrar_selenium(sessao)

  message("\n🎉 Processo concluído com sucesso! 👋")
}

# =====================================================================
# --- 7. Executar ------------------------------------------------------
# =====================================================================

main()

# ===============
# --- 8. Extração ----
# ===============

# Caminho da pasta onde estão os arquivos zip
base_path <- "PPA-MG"

# Lista todos os arquivos .zip
zip_files <- list.files(base_path, pattern = "\\.zip$", full.names = TRUE)

for (zip_file in zip_files) {
  # Nome da subpasta = nome do arquivo zip sem extensão
  folder_name <- tools::file_path_sans_ext(basename(zip_file))
  extract_path <- file.path(base_path, folder_name)
  
  # Cria subpasta se não existir
  if (!dir.exists(extract_path)) {
    dir.create(extract_path, recursive = TRUE)
  }
  
  # Extrai o zip
  unzip(zip_file, exdir = extract_path)
  
  message("Extraído: ", basename(zip_file), " → ", extract_path)
}

# ---- Remover os arquivos ZIP após a extração ----
file.remove(zip_files)
message("Arquivos ZIP removidos com sucesso!")