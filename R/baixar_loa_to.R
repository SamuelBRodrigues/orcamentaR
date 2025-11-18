# ==========================================================
# --- 1. Pacotes necessários ---
# ==========================================================
library(rvest)
library(stringr)
library(dplyr)
library(stringi)
library(purrr)
library(curl)
library(httr2)

# ==========================================================
# --- 2. Funções de UTILIDADE (Responsabilidade única) ---
# ==========================================================

criar_pastas_periodos <- function(pasta_base, periodos) {
  if (!dir.exists(pasta_base)) {
    dir.create(pasta_base, recursive = TRUE)
    message("📁 Pasta base criada: ", pasta_base)
  }

  walk(c(periodos, "Outros"), ~ {
    dir.create(file.path(pasta_base, .x), recursive = TRUE, showWarnings = FALSE)
  })

  message("📚 Estrutura de pastas criada.\n")
}

identificar_periodo <- function(nome_arquivo, periodos) {
  nome_limpo <- stringi::stri_trans_general(nome_arquivo, "Latin-ASCII") |> toupper()
  match <- stringr::str_match(nome_limpo, "LOA[^0-9]*?(\\d{4})")

  if (!is.na(match[1, 2]) && match[1, 2] %in% periodos)
    return(match[1, 2])

  return("Outros")
}

obter_nome_arquivo_http <- function(url) {
  req <- request(url) |> req_timeout(10)

  resp <- tryCatch(req_perform(req), error = function(e) NULL)
  nome <- basename(url)

  if (!is.null(resp)) {
    cd <- resp_header(resp, "content-disposition")

    if (!is.null(cd)) {
      extraido <- str_match(cd, 'filename="?([^";]+)"?')[, 2]
      if (!is.na(extraido)) nome <- extraido
    }
  }

  nome <- URLdecode(iconv(nome, from = "latin1", to = "UTF-8", sub = ""))
  nome <- str_replace_all(nome, "[\r\n\t]", "")
  nome <- sub("\\?.*$", "", nome)
  nome <- trimws(nome)

  if (nome == "" | nome %in% c("download", "file", "index"))
    nome <- paste0("arquivo_", format(Sys.time(), "%Y%m%d%H%M%S"))

  if (!str_detect(tolower(nome), "\\.[a-z0-9]+$"))
    nome <- paste0(nome, ".pdf")

  nome
}

tratar_nome_arquivo <- function(nome) {
  nome <- enc2utf8(nome)

  # --- Normalização ---
  nome <- stringi::stri_trans_general(nome, "Latin-ASCII")
  nome <- str_replace_all(nome, "[^[:alnum:]_\\-\\. ]", "")
  nome <- str_squish(nome)
  nome <- str_replace_all(nome, " ", "_")
  nome <- str_replace_all(nome, "file_open_", "")
  nome <- substr(nome, 1, 100)

  # --- Verifica extensão novamente (DEPOIS da limpeza) ---
  ext <- tools::file_ext(nome)
  if (ext == "") nome <- paste0(nome, ".pdf")

  nome
}



# 🔗 Extrai links da página
extrair_links_e_nomes <- function(url_pagina, seletor) {
  message("🌐 Acessando página: ", url_pagina)

  pagina <- tryCatch(read_html(url_pagina), error = function(e) {
    stop("❌ Erro ao carregar página: ", e$message)
  })

  tags <- pagina |> html_elements(seletor)

  df <- tibble::tibble(
    url = url_absolute(html_attr(tags, "href"), url_pagina),
    nome_link = html_text(tags, trim = TRUE)
  ) |> filter(!is.na(url) & url != "")

  if (nrow(df) == 0)
    stop("⚠️ Nenhum link encontrado com o seletor: ", seletor)

  # >>> Melhorado: Mostrar lista dos links identificados
  message("🔗 ", nrow(df), " link(s) encontrado(s):")
  walk2(seq_len(nrow(df)), df$url, ~ message("   ", .x, ") ", .y))
  message("\n")

  df
}

# ==========================================================
# --- 3. Funções ESPECIALIZADAS DE DOWNLOAD ---
# ==========================================================

arquivo_e_pdf <- function(caminho, min_tamanho = 1000) {
  if (!file.exists(caminho)) return(FALSE)

  tamanho <- file.info(caminho)$size
  if (tamanho < min_tamanho) return(FALSE)

  con <- file(caminho, "rb")
  header <- readBin(con, what = "raw", n = 5)
  close(con)

  rawToChar(header) == "%PDF-"
}

fazer_download_curl <- function(url, caminho) {
  tryCatch({
    curl_download(
      url = url,
      destfile = caminho,
      mode = "wb",
      handle = new_handle(followlocation = 1)
    )
    TRUE
  }, error = function(e) {
    message("⚠️ Erro no download: ", e$message)
    FALSE
  })
}

baixar_com_retry <- function(url, caminho_saida, max_tentativas = 5) {

  for (i in seq_len(max_tentativas)) {

    if (i > 1) {
      espera <- runif(1, 1, 3) * 2^(i - 2)
      message(sprintf("   🔄 Tentativa %d após %.1f segundos...", i, espera))
      Sys.sleep(espera)
    }

    if (file.exists(caminho_saida)) file.remove(caminho_saida)

    if (fazer_download_curl(url, caminho_saida) && arquivo_e_pdf(caminho_saida)) {
      return(TRUE)
    }
  }

  FALSE
}

baixar_pdf <- function(url, nome_arquivo, pasta_base, periodos, indice, total) {

  nome_arquivo <- tratar_nome_arquivo(nome_arquivo)
  periodo <- identificar_periodo(nome_arquivo, periodos)
  pasta_destino <- file.path(pasta_base, periodo)
  dir.create(pasta_destino, recursive = TRUE, showWarnings = FALSE)

  caminho_saida <- file.path(pasta_destino, nome_arquivo)

  # >>> Melhorado: feedback mais claro
  message(sprintf("⬇️ (%d/%d) Baixando: %s → %s", indice, total, nome_arquivo, periodo))
  message("   🔗 URL: ", url)

  sucesso <- baixar_com_retry(url, caminho_saida)

  if (sucesso) {
    message("   ✅ OK: ", caminho_saida, "\n")
    return(caminho_saida)
  }

  warning("   ❌ Falhou após várias tentativas: ", url)
  return(NA)
}

# ==========================================================
# --- 4. Função ORQUESTRADORA ---
# ==========================================================
baixar_pdfs_por_periodo <- function(url_pagina, seletor, pasta_base, periodos) {

  message("\n🚀 Iniciando processo de download...")

  criar_pastas_periodos(pasta_base, periodos)
  dados <- extrair_links_e_nomes(url_pagina, seletor)

  total <- nrow(dados)

  message("📦 Total de arquivos a baixar: ", total, "\n")

  resultados <- pmap(
    list(dados$url, dados$nome_link, seq_len(total)),
    function(url, nome_link, indice) {

      nome_final <- if (!is.na(nome_link) && nome_link != "")
        nome_link
      else
        obter_nome_arquivo_http(url)

      baixar_pdf(url, nome_final, pasta_base, periodos, indice, total)
    }
  )

  sucesso <- sum(!is.na(resultados))
  falhas  <- length(resultados) - sucesso

  # >>> Melhorado: resumo completo
  message("\n🏁 Finalizado!")
  message("📊 Resumo:")
  message("   Total:   ", total)
  message("   Sucesso: ", sucesso)
  message("   Falhas:  ", falhas)

  if (falhas > 0) {
    message("\n⚠️ Links que falharam:")
    walk2(which(is.na(resultados)), dados$url[is.na(resultados)], ~ {
      message("   ", .x, ") ", .y)
    })
  }

  invisible(resultados)
}

# ==========================================================
# --- 5. EXECUTANDO ---
# ==========================================================

configs <- list(
  list(
    url_pagina = "https://www.to.gov.br/seplan/lei-orcamentaria-anual-loa-1989-a-2024/3xufjqbpkrhs",
    seletor    = ".media-embed ul li a",
    pasta_base = "LOA-TO",
    periodos   = as.character(1989:2022)
  ),
  list(
    url_pagina = "https://www.to.gov.br/seplan/lei-orcamentaria-anual-loa-2023/4pebh9vqlyyc",
    seletor    = ".media-embed ul li a",
    pasta_base = "LOA-TO",
    periodos   = "2023"
  ),
  list(
    url_pagina = "https://www.to.gov.br/seplan/lei-orcamentaria-anual-2024-loa-2024/1yvf308b9o8w",
    seletor    = ".media-embed ul li a",
    pasta_base = "LOA-TO",
    periodos   = "2024"
  )
)

walk(
  configs,
  ~ baixar_pdfs_por_periodo(
      url_pagina = .x$url_pagina,
      seletor    = .x$seletor,
      pasta_base = .x$pasta_base,
      periodos   = .x$periodos
    )
)
