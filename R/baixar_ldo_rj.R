# --- 1. Instalação (Caso ainda não tenha) ---
#install.packages("RSelenium")

# --- 2. Carregar a biblioteca ---
library(RSelenium)

message("Criando diretório temporário para perfil do Firefox...")
# Diretório de download:
download_dir <- normalizePath(file.path(getwd(), "LDO-RJ/"))

# Verificar se o diretório existe, se não, criar
if (!dir.exists(download_dir)) {
dir.create(download_dir, recursive = TRUE)
}

# Criar um diretório temporário para o perfil do Firefox
profile_dir <- tempfile(pattern = "firefox_profile")
dir.create(profile_dir)

# Criar um arquivo user.js com as configurações de download
user_js <- file.path(profile_dir, "user.js")
writeLines(c(
'user_pref("browser.download.folderList", 2);',  # Usar diretório específico
paste0('user_pref("browser.download.dir", "', download_dir, '");'),  # Diretório atual
'user_pref("browser.download.useDownloadDir", true);',  # Forçar o uso do diretório de download
'user_pref("browser.helperApps.neverAsk.saveToDisk", "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet, text/csv");',  # Tipos de arquivo
'user_pref("pdfjs.disabled", true);',  # Desabilitar visualização de PDF
'user_pref("browser.download.manager.showWhenStarting", false);',  # Evitar popup de download
'user_pref("browser.download.manager.closeWhenDone", true);',  # Fechar o gerenciador de download
'user_pref("browser.download.autoOpenValue", 0);'  # Forçar o download automático
), con = user_js)

# Encerrar qualquer processo Selenium ou servidor anterior
message("Verificando e encerrando processos Selenium antigos...")

# Se estiver rodando em um sistema Unix (Linux ou macOS), vamos matar o processo via comando de terminal
if (.Platform$OS.type == "unix") {
system("pkill -f 'selenium-standalone'")  # Matar todos os processos Selenium
} else {
# Em Windows, tentamos matar o processo pelo nome, pode ser necessário ajustar conforme o ambiente
system("taskkill /F /IM java.exe /T")  # Matar todos os processos java (o Selenium pode rodar como Java)
}

# Iniciar o driver com o perfil do Firefox
rs_driver_object <- RSelenium::rsDriver(
browser = "firefox",
chromever = NULL,  # Manter como NULL já que não usaremos o Chrome
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

# Fechando a primeira janela
remDr$close()

# Iniciar a Navegação Remota
remDr$open()
Sys.sleep(5)


message("Acessando portal da ANEEL...")
url <- "https://portal.fazenda.rj.gov.br/tesouro/ppa-ldo-loa/"
remDr$navigate(url)
Sys.sleep(5)

#message("Entrando no iframe do Power BI...")
#iframe <- remDr$findElement(using = "css selector", '#embedContainer > iframe:nth-child(1)')
#remDr$switchToFrame(iframe)
#Sys.sleep(3)

# Acessando a Base de Dados do Comparativo
remDr$findElement(
using = "css selector",
"#heading-parent-2383 > h5 > button"
)$clickElement()
Sys.sleep(5)


selector_css <- "#collapse-parent-2383 > div a"
links_elementos <- remDr$findElements(using = "css selector", value = selector_css)

todos_os_links <- unlist(lapply(links_elementos, function(el) {
  el$getElementAttribute("href")
}))

links_pdf <- todos_os_links[grepl("\\.pdf$", todos_os_links, ignore.case = TRUE)]




baixar_pdf <- function(url, destino, timeout_sec = 30) {
  # Cria o diretório se não existir
  dir.create(dirname(destino), showWarnings = FALSE, recursive = TRUE)
  
  # Tenta baixar o arquivo com timeout
  tryCatch({
    response <- httr::GET(url, httr::timeout(timeout_sec))
    
    if (httr::status_code(response) == 200) {
      writeBin(httr::content(response, "raw"), destino)
      message("✅ Download concluído: ", normalizePath(destino))
    } else {
      message("⚠️ Arquivo não encontrado (HTTP ", httr::status_code(response), "): ", url)
    }
    
  }, error = function(e) {
    message("❌ Erro ao baixar ", url, ": ", e$message)
  })
}


lapply(links_pdf, function(url_do_pdf) {
    # 5a. Extrair o nome original do arquivo da URL
    nome_arquivo <- basename(url_do_pdf)
    
    # 5b. Criar o caminho de destino completo
    # Ex: "meus_pdfs/nome_do_arquivo.pdf"
    caminho_completo <- file.path('LDO-RJ', nome_arquivo)
    
    # 5c. Chamar sua função para baixar o arquivo
    baixar_pdf(url = url_do_pdf, destino = caminho_completo)
})



# --- Encerrando a conexão e o servidor ---

message("Fechando o navegador (cliente)...")
# Verifica se o objeto remDr existe e fecha o cliente
if (exists("remDr")) {
  remDr$close()
}

message("Parando o servidor Selenium (liberando a porta)...")
# Verifica se o objeto rs_driver_object existe e para o servidor
if (exists("rs_driver_object")) {
  rs_driver_object$server$stop()
}

message("Sessão do RSelenium encerrada e porta liberada.")