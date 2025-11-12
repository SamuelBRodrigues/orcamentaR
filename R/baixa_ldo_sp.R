library(httr)  # Para timeout e verificação de status

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

baixar_ldo_sp <- function(anos = 2002:2026, destino_dir = "LDO-SP", timeout_sec = 30) {
  
  # Tabela de exceções: anos com URL diferente
  excecoes <- list(
    "2014" = "https://portal.fazenda.sp.gov.br/servicos/orcamento/Documents/LDO/LDO_2014_versao_final.pdf",
    "2026" = "https://portal.fazenda.sp.gov.br/servicos/orcamento/Documents/LDO/LEI%20DE%20DIRETRIZES%20OR%C3%87AMENT%C3%81RIAS%202026%20-%20ACESSO%20A%20LEI%20N%C2%B0%2018.178,%20DE%2016%20DE%20JULHO%20DE%202025%20-%20LDO%202026.pdf"
  )
  
  for (ano in anos) {
    destino <- file.path(destino_dir, paste0("LDO_SP_", ano, ".pdf"))
    
    # Verifica se o ano está nas exceções
    if (as.character(ano) %in% names(excecoes)) {
      url <- excecoes[[as.character(ano)]]
    } else {
      url <- paste0("https://portal.fazenda.sp.gov.br/servicos/orcamento/Documents/LDO/LDO_", ano, ".pdf")
    }
    
    message("📥 Tentando baixar ano ", ano, "...")
    baixar_pdf(url, destino, timeout_sec)
  }
}

# Executa
baixar_ldo_sp()
