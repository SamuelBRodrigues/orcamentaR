# --- 1. Carregar bibliotecas ---
library(RSelenium)
library(httr) # Necessário para a função baixar_pdf
library(janitor) # Necessário para nomear os arquivos

message("Criando diretório temporário para perfil do Firefox...")
# Diretório de download:
# (Nota: O perfil do Firefox pode não funcionar como esperado para 'baixar_pdf' 
# que usa httr. Vamos definir o diretório de destino manualmente.)
diretorio_destino <- "LDO-TO"

# Verificar se o diretório existe, se não, criar
if (!dir.exists(diretorio_destino)) {
  dir.create(diretorio_destino, recursive = TRUE)
}

# --- 2. Configuração do RSelenium (Seu código) ---

# Criar um diretório temporário para o perfil do Firefox
profile_dir <- tempfile(pattern = "firefox_profile")
dir.create(profile_dir)

# Criar um arquivo user.js com as configurações de download
# (Nota: Estas configurações afetam o 'click' do navegador, 
# mas nossa abordagem de download com 'httr::GET' é mais direta)
user_js <- file.path(profile_dir, "user.js")
writeLines(c(
  'user_pref("browser.download.folderList", 2);',
  paste0('user_pref("browser.download.dir", "', normalizePath(diretorio_destino), '");'),
  'user_pref("browser.download.useDownloadDir", true);',
  'user_pref("browser.helperApps.neverAsk.saveToDisk", "application/pdf, application/vnd.openxmlformats-officedocument.spreadsheetml.sheet, text/csv");',
  'user_pref("pdfjs.disabled", true);', # Importante para forçar download
  'user_pref("browser.download.manager.showWhenStarting", false);',
  'user_pref("browser.download.manager.closeWhenDone", true);',
  'user_pref("browser.download.autoOpenValue", 0);'
), con = user_js)

# Encerrar qualquer processo Selenium ou servidor anterior
message("Verificando e encerrando processos Selenium antigos...")
if (.Platform$OS.type == "unix") {
  system("pkill -f 'selenium-standalone'")
} else {
  system("taskkill /F /IM java.exe /T")
}

# Iniciar o driver com o perfil do Firefox
rs_driver_object <- RSelenium::rsDriver(
  browser = "firefox",
  chromever = NULL,
  phantomver = NULL,
  extraCapabilities = list(
    "moz:firefoxOptions" = list(
      args = list("-profile", profile_dir)
    )
  ),
  verbose = FALSE
)

# Abrir o webdriver
remDr <- rs_driver_object$client

# (Opcional, mas seu código original tinha isso)
# Fechando a primeira janela (geralmente 'about:blank')
# remDr$close() 
# remDr$open()

Sys.sleep(5)

# --- 3. Navegação e Coleta (Seu código) ---
url_2010_2023 <- "https://www.to.gov.br/seplan/lei-de-diretrizes-orcamentarias-ldo-2010-2023/1bpah5uk1esw"
url_2024 <- "https://www.to.gov.br/seplan/lei-de-diretrizes-orcamentarias-ldo-2024/4lq2l5o9scup"
url_2025 <- "https://www.to.gov.br/seplan/ldo-2025-lei-de-diretrizes-orcamentarias-2025/3zukr3peuokh"

url <- url_2010_2023
message(paste("Navegando para:", url))
remDr$navigate(url)
Sys.sleep(5)

selector_css <- "body > section > div > div > div.col.s12.m10.l8.offset-m1.offset-l2.page_body > div > div:nth-child(2) > div > div > ul > li > a"
links_elementos <- remDr$findElements(using = "css selector", value = selector_css)

message(paste("Encontrados", length(links_elementos), "elementos <a>."))

# --- 4. Extração de Links e Textos (Sua lógica) ---
lista_de_dados <- lapply(links_elementos, function(el) {
  list(
    link = unlist(el$getElementAttribute("href")),
    texto = make_clean_names(
      gsub("file_open\\", "", unlist(el$getElementText()), fixed = TRUE)
    )
  )
})




# --- 8. [ADICIONADO] Encerrar o driver do Selenium ---
message("Encerrando o driver do Selenium...")
remDr$close()
rs_driver_object$server$stop()

# Limpar o perfil temporário
unlink(profile_dir, recursive = TRUE)
message("Processo concluído.")