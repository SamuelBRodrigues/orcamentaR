library(httr)
library(rvest)
library(stringr)

# URL da página principal
url <- "https://planejamento.rs.gov.br/lei-de-diretrizes-orcamentarias"

# Ler a página
message("🔍 Acessando a página principal...")
pagina <- read_html(url)

# Base URL
base_url <- "https://planejamento.rs.gov.br"

# Extrair nós <a> da área desejada
nodes <- pagina |>
  html_nodes("#bodyPrincipal > div.wrapper__corpo > div > div:nth-child(1) > article > div.artigo__texto > div a")

# Extrair texto e links
nomes <- nodes |> html_text(trim = TRUE)
links <- nodes |> html_attr("href") |> url_absolute(base = base_url)

# Garantir mesmo comprimento
stopifnot(length(nomes) == length(links))

total_links <- length(links)
message("✅ Foram encontrados ", total_links, " links para download.")

# Criar pasta principal
dir.create("LDO-RS", showWarnings = FALSE)

# Função para normalizar o nome do arquivo
limpar_nome <- function(nome) {
  nome |>
    str_replace_all("[\r\n\t]", " ") |>       # remove quebras de linha
    str_trim() |>
    stringi::stri_trans_general("Latin-ASCII") |> # remove acentos
    str_replace_all("[^a-zA-Z0-9_-]", "_") |> # substitui caracteres especiais por "_"
    str_squish()
}

# Função para baixar PDF
baixar_pdf <- function(link, nome_texto, indice) {
  # Extrai o ano do texto do link (<a>)
  ano <- str_extract(nome_texto, "(199[1-9]|20[0-2][0-9])")
  
  # Se ainda assim não houver ano, marca como desconhecido
  if (is.na(ano)) ano <- "Desconhecido"
  
  # Cria subpasta do ano
  pasta_ano <- file.path("LDO-RS", ano)
  dir.create(pasta_ano, showWarnings = FALSE, recursive = TRUE)
  
  # Define nome do arquivo com base no texto do link
  nome_limpo <- limpar_nome(nome_texto)
  destino <- file.path(pasta_ano, paste0(nome_limpo, ".pdf"))
  
  # Mensagem de progresso
  message("\n📥 [", indice, "/", total_links, "] Baixando: ", nome_texto, " (", ano, ")")
  
  # Faz o download (se ainda não existir)
  if (!file.exists(destino)) {
    try({
      download.file(link, destfile = destino, mode = "wb", quiet = TRUE)
      message("✅ Download concluído: ", destino)
    }, silent = TRUE)
  } else {
    message("⏩ Já existe: ", destino)
  }
  
  # Aguardar tempo aleatório entre downloads
  tempo_espera <- runif(1, min = 0, max = 3)
  message("⏳ Aguardando ", round(tempo_espera, 2), " segundos antes do próximo download...")
  Sys.sleep(tempo_espera)
}

# Aplicar a função com contagem
message("\n🚀 Iniciando downloads...\n")
for (i in seq_along(links)) {
  baixar_pdf(links[i], nomes[i], i)
}

message("\n🎉 Todos os downloads foram concluídos com sucesso!")
