# LDO -----
dir <- "data-raw/LDO/"
## AC -------
dir_ac <- stringr::str_glue("{dir}AC/")
html_ac <- rvest::read_html("https://seplan.ac.gov.br/lei-de-diretrizes-orcamentarias-ldo")
links_ac_a <- html_ac |> 
  rvest::html_elements(css = "h4") |> 
  rvest::html_element("a") |> 
  rvest::html_attr("href") 

nomes_ac_a <- html_ac |> 
  rvest::html_elements(css = "h4") |> 
  rvest::html_text()

links_ac_b <- html_ac |> 
  rvest::html_elements(".elementor-button-wrapper") |> 
  rvest::html_element("a") |> 
  rvest::html_attr("href")

nomes_ac_b <- html_ac |> 
  rvest::html_elements(".elementor-button-wrapper") |> 
  rvest::html_text() |> 
  stringr::str_remove_all("\t") |> 
  stringr::str_remove_all("\n")

links_ac <- c(links_ac_a, links_ac_b) |> stringr::str_trim()
nomes_ac <- c(stringr::str_glue("{dir_ac}{nomes_ac_a}.pdf"), stringr::str_glue("{dir_ac}{nomes_ac_b}.pdf"))

purrr::walk2(
  links_ac[-c(3,4)],
  nomes_ac[-c(3,4)],
  ~ download.file(
    .x,
    destfile = .y,
    mode = "wb",
    method = "libcurl"
  )
)
## AL ------
dir_al <- stringr::str_glue("{dir}AL/")
html_al <- rvest::read_html("https://dados.al.gov.br/catalogo/sv/dataset/lei-de-diretrizes-orcamentarias-ldo##:~:text=Tem%20como%20a%20principal%20finalidade,as%20empresas%20públicas%20e%20autarquias")
endpoint <- html_al |> 
  rvest::html_elements(".heading") |> 
  rvest::html_attr("href") |> 
  stringr::str_subset("/")

endpoint <- stringr::str_glue("https://dados.al.gov.br{endpoint}")

infos <- purrr::map_df(
  endpoint,
  ~{
    html_crawling <- rvest::read_html(.x)
    link <- html_crawling |> 
      rvest::html_element(".text-muted") |> 
      rvest::html_element("a") |> 
      rvest::html_text()
    nome <- link |> 
      stringr::str_extract("(?<=download/)\\w.*")
    table <- tibble::tibble(
      links = link,
      nomes = nome
    )
    return(table)
  }
)

purrr::pwalk(
  infos,
  function(links, nomes){
    dest <- stringr::str_glue("{dir_al}/{nomes}")
    download.file(
      url = links,
      destfile = dest,
      mode = "wb"
    )
  }
)

## AM -----------
dir_am <- "Downloads\\LDO\\AM"
url <- "https://www.sefaz.am.gov.br/submenu-externo/313"


## Diretório de download:
download_dir <- normalizePath(file.path(getwd(), dir_am))

## Verificar se o diretório existe, se não, criar
if (!dir.exists(download_dir)) {
  dir.create(download_dir, recursive = TRUE)
}

## Criar um diretório temporário para o perfil do Firefox
profile_dir <- tempfile(pattern = "firefox_profile")
dir.create(profile_dir)

## Criar um arquivo user.js com as configurações de download
user_js <- file.path(profile_dir, "user.js")
writeLines(c(
  'user_pref("browser.download.folderList", 2);',  ## Usar diretório específico
  paste0('user_pref("browser.download.dir", "', download_dir, '");'),  ## Diretório atual
  'user_pref("browser.download.useDownloadDir", true);',  ## Forçar o uso do diretório de download
  'user_pref("browser.helperApps.neverAsk.saveToDisk", "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet, text/csv");',  ## Tipos de arquivo
  'user_pref("pdfjs.disabled", true);',  ## Desabilitar visualização de PDF
  'user_pref("browser.download.manager.showWhenStarting", false);',  ## Evitar popup de download
  'user_pref("browser.download.manager.closeWhenDone", true);',  ## Fechar o gerenciador de download
  'user_pref("browser.download.autoOpenValue", 0);'  ## Forçar o download automático
), con = user_js)

## Iniciar o driver com o perfil do Firefox
rs_driver_object <- RSelenium::rsDriver(
  browser = "firefox",
  chromever = NULL,  ## Manter como NULL já que não usaremos o Chrome
  extraCapabilities = list(
    "moz:firefoxOptions" = list(
      args = list("-profile", profile_dir)
    )
  ),
  verbose = FALSE,
  phantomver = NULL
)


## Abrir o webdriver
remDr <- rs_driver_object$client

## Fechando a primeira janela
remDr$close()

## Iniciar a Navegação Remota
remDr$open()
## Navegando até o site
remDr$navigate(url)
buttons <- remDr$findElements(
  using = "css selector",
  ".flex.grow.items-center.gap-1"
)

purrr::walk(
  seq(1, length(buttons)),
  ~{
    css_selector <- stringr::str_glue(".space-y-6 > div:nth-child(1) > div:nth-child({.x}) > div:nth-child(1) > button:nth-child(1)")
    remDr$findElement(
      using = "css selector",
      value = css_selector
    )$clickElement()
    
    Sys.sleep(10)
  }
)

remDr$quit()
rs_driver_object$server$stop()

## AP --------------
dir_ap <- stringr::str_glue("{dir}AP/")

html_ap <- rvest::read_html("https://seplan.portal.ap.gov.br/pagina/orcamentos/lei-de-diretrizes-orcamentaria-ldo")

links <- html_ap |> rvest::html_elements(".tw-px-2") |> 
  rvest::html_attr("href")
links |> 
  stringr::str_extract("(?=LDO)\\w.*")

info <- tibble::tibble(
  links = links,
  nomes = stringr::str_extract(links, "(?=LDO)\\w.*")
)

purrr::pwalk(
  info[18:20,],
  function(links, nomes){
    dest <- stringr::str_glue("{dir_ap}{nomes}")
    valid_url <- utils::URLencode(links)
    download.file(
      url = valid_url,
      destfile = dest,
      mode = "wb"
    )
  }
)

## BA --------------
dir_ba <- stringr::str_glue("{dir}BA/")
html_ba_1 <- rvest::read_html("https://www.ba.gov.br/seplan/orcamento/ldo-lei-de-diretrizes-orcamentarias")
links_1 <- html_ba_1 |> 
  rvest::html_elements("li") |> 
  rvest::html_element("a") |> 
  rvest::html_attr("href") |> 
  stringr::str_subset("/seplan/sites")
links_1 |> stringr::str_extract("(?<=06/|uploads/)\\w*.+")

html_ba_2 <- rvest::read_html("https://www.ba.gov.br/seplan/orcamento/historico-de-ldo")
links_2 <- html_ba_2 |> 
  rvest::html_elements("li") |> 
  rvest::html_element("a") |> 
  rvest::html_attr("href") |> 
  stringr::str_subset("/seplan/sites")

table <- tibble::tibble(
  links = c(links_1, links_2)
) |> 
  dplyr::mutate(
    links = stringr::str_glue("https://www.ba.gov.br{links}"),
    nomes = stringr::str_extract(links, "(?<=06/|uploads/)\\w*.+")
  )

purrr::pwalk(
  table,
  function(links, nomes){
    dest <- stringr::str_glue("{dir_ba}{nomes}")

    download.file(
      url = links,
      destfile = dest,
      mode = "wb"
    )
  }
)


## CE -----------------
dir_ce <- stringr::str_glue("{dir}CE/")

purrr::walk(
  seq(2,26),
  ~{
    if(.x < 10){
      ano = stringr::str_glue("200{.x}")
    } else{
      ano = stringr::str_glue("20{.x}")
    }
    logger::log_info("Baixando os dados para {ano}")
    dir <- stringr::str_glue("{dir_ce}{ano}/")
    if(!dir.exists(dir)){
      dir.create(dir, recursive = T)
    }
    
    html_ce <- utils::URLencode(stringr::str_glue("https://www.seplag.ce.gov.br/planejamento/menu-lei-de-diretrizes-orcamentarias/lei-de-diretrizes-orcamentarias-{ano}"))
    logger::log_info("Acessando: {html_ce}")
    links <- rvest::read_html(html_ce) |> 
      rvest::html_element(".wrapper.container") |> 
      rvest::html_elements("a") |> 
      rvest::html_attr("href") |> 
      stringr::str_subset("https")

    info <- tibble::tibble(
      links = links,
      nomes = stringr::str_extract(links, "(?<=\\d{4}/\\d{2}/)\\w.*")
    )

    purrr::pwalk(
      info,
      function(links, nomes){
        dest <- stringr::str_glue("{dir}{nomes}")
        download.file(
          url = links,
          destfile = dest,
          mode = "wb",
          method = "libcurl",
          headers = c("User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/58.0.3029.110 Safari/537.36")
        )
      }
    )
  }
)
download.file(
  "https://www.seplag.ce.gov.br/wp-content/uploads/sites/14/2011/05/LDO-2002-Lei-13-138-de-23-de-julho-de-2001.pdf",
  destfile = "C:/Users/samba/Downloads/LDO/CE/LDO-2002-Lei-13-138-de-23-de-julho-de-2001.pdf",
  mode = "wb",
  method = "libcurl",
  headers = c("User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/58.0.3029.110 Safari/537.36")
)

## DF --------------
dir_df <- stringr::str_glue("{dir}DF/")
url_df <- "https://www.economia.df.gov.br/leis-de-diretrizes-orcamentarias-ldo"

html_df <- rvest::read_html(url_df)
links_df <- html_df |> 
  rvest::html_element(".gdf-web-content") |> 
  rvest::html_elements("p") |> 
  rvest::html_elements("a") |> 
  rvest::html_attr("href") |> 
  stringr::str_subset("##", negate = T) |> 
  stringr::str_unique()

purrr::walk(
  links_df,
  ~{
    if(stringr::str_detect(.x, "http")){
      logger::log_info("link direto - {.x}")
      html <- rvest::read_html(.x)

      links <- html |> 
        rvest::html_element(".materia") |> 
        rvest::html_elements("p") |> 
        rvest::html_elements("a") |> 
        rvest::html_attr("href")
      nomes <- html |> 
        rvest::html_element(".materia") |> 
        rvest::html_elements("p") |> 
        rvest::html_elements("a") |> 
        rvest::html_text() |> 
        stringr::str_remove_all("[:blank:]")

      infos <- tibble::tibble(
        links = links,
        nomes = nomes
      ) |> 
        dplyr::mutate(
          nomes= stringr::str_glue("{nomes}.pdf")
        )
      ano <- stringr::str_extract(.x, "\\d+")
      dir <- stringr::str_glue("{dir_df}{ano}/")
      if(!dir.exists(dir)){
        dir.create(dir, recursive = T)
      }
      
      purrr::pwalk(
        infos,
        function(links, nomes){
          dest <- stringr::str_glue("{dir}{nomes}")
          download.file(
            url = links,
            destfile = dest,
            mode = "wb"
          )
        }
      )

    } else if(stringr::str_detect(.x, "zip|doc|rar")){
      ano = stringr::str_extract(.x, "\\d+")
      dir <- stringr::str_glue("{dir_df}{ano}/")
      if(!dir.exists(dir)){
        dir.create(dir, recursive = T)
      }
      link <- stringr::str_glue("https://www.economia.df.gov.br{.x}")
      nome <- stringr::str_extract(.x, "(?<=seec/)\\w.*")
      file <- stringr::str_extract(.x, "\\w*$")
      nome_file <- stringr::str_glue("{dir}{nome}.{file}")
      download.file(
        url = link,
        destfile = nome_file,
        mode = "wb"
      )

    } else if(stringr::str_detect(.x, "2025")){
      url <- stringr::str_glue("http://www.economia.df.gov.br/pt{.x}")
      html <- rvest::read_html(url)

      links <- html |> 
        rvest::html_element("##conteudo") |> 
        rvest::html_elements("p") |> 
        rvest::html_elements("a") |> 
        rvest::html_attr("href") |> 
        stringr::str_subset("www|documents") |> 
        stringr::str_replace("^/documents/", "https://www.economia.df.gov.br/documents/")
      nomes <- html |> 
        rvest::html_element("##conteudo") |> 
        rvest::html_elements("p") |> 
        rvest::html_elements("a") |> 
        rvest::html_text() |> 
        stringr::str_remove_all("[:blank:]") |> 
        stringr::str_subset("\\w", negate = F) |> 
        stringr::str_subset("LDO2025-AcompanheaexecuçãodasDespesascomPessoalAutorizadasnoANEXOIV", negate = T) |> 
        stringr::str_c(".pdf")

      infos <- tibble::tibble(
        links = links,
        nomes = nomes
      ) 
      ano <- stringr::str_extract(.x, "\\d+")
      dir <- stringr::str_glue("{dir_df}{ano}/")
      if(!dir.exists(dir)){
        dir.create(dir, recursive = T)
      }
      
      purrr::pwalk(
        infos,
        function(links, nomes){
          dest <- stringr::str_glue("{dir}{nomes}")
          download.file(
            url = links,
            destfile = dest,
            mode = "wb"
          )
        }
      )
    } else{
      url <- stringr::str_glue("https://www.economia.df.gov.br{.x}")
      html <- rvest::read_html(url)

      links <- html |> 
        rvest::html_element("##conteudo") |> 
        rvest::html_elements("p") |> 
        rvest::html_elements("a") |> 
        rvest::html_attr("href") |> 
        stringr::str_subset("www|documents") |> 
        stringr::str_replace("^/documents/", "https://www.economia.df.gov.br/documents/")
      nomes <- html |> 
        rvest::html_element("##conteudo") |> 
        rvest::html_elements("p") |> 
        rvest::html_elements("a") |> 
        rvest::html_text() |> 
        stringr::str_remove_all("[:blank:]") |> 
        stringr::str_c(".pdf")

      infos <- tibble::tibble(
        links = links,
        nomes = nomes
      ) 
      ano <- stringr::str_extract(.x, "\\d+")
      dir <- stringr::str_glue("{dir_df}{ano}/")
      if(!dir.exists(dir)){
        dir.create(dir, recursive = T)
      }
      
      purrr::pwalk(
        infos,
        function(links, nomes){
          dest <- stringr::str_glue("{dir}{nomes}")
          download.file(
            url = links,
            destfile = dest,
            mode = "wb"
          )
        }
      )
    }
  }
)

## ES --------------
dir_es <- stringr::str_glue("{dir}ES/")
url_es <- "https://planejamento.es.gov.br/GrupodeArquivos/ldo"
html_es <- rvest::read_html(url_es)

links <- html_es |> 
  rvest::html_elements("th.coluna-1 > a") |> 
  rvest::html_attr("href") %>%
  stringr::str_c("https://planejamento.es.gov.br", .)
nomes <- links |> 
  utils::URLdecode() |> 
  stringr::str_remove_all("[:blank:]") |> 
  stringr::str_extract(
    "[^/]+(?<=\\.pdf)"
  )

infos <- tibble::tibble(
  links = links,
  nomes = nomes
)
options(timeout = max(300, getOption("timeout")))
purrr::pwalk(
  infos,
  function(links, nomes){
    dest <- stringr::str_glue("{dir_es}{nomes}")
    download.file(
      url = links,
      destfile = dest,
      mode = "wb"
    )
  }
)

## GO ----------------
dir_go <- stringr::str_glue("{dir}GO/")
url_go <- "https://transparencia.go.gov.br/orcamento-e-planejamento-pecas"
html_go <- rvest::read_html(url_go)
links <- html_go |> 
  rvest::html_element(".entry-content") |> 
  rvest::html_elements("p") |> 
  rvest::html_element("a") |>
  rvest::html_attr("href")

nomes <- html_go |> 
  rvest::html_element(".entry-content") |> 
  rvest::html_elements("p") |> 
  rvest::html_element("a") |>
  rvest::html_text2() |> 
  stringr::str_remove_all("[:blank:]")

info <- tibble::tibble(
  links = links,
  nomes = nomes
) |> 
  tidyr::drop_na() |> 
  dplyr::filter(
    stringr::str_detect(nomes, "LDO|LOA")
  ) |> 
  dplyr::mutate(
    nomes = stringr::str_remove_all(nomes, "\\W")
  )
purrr::pwalk(
  info,
  function(links, nomes){
    dest <- stringr::str_glue("{dir_go}{nomes}.pdf")
    
    resp <- httr2::request(links) |> httr2::req_perform()

    url <- resp$url

    if(stringr::str_detect(url, "pesquisa_legislacao")){
      code <- stringr::str_extract(url, "(?<=pesquisa_legislacao/)\\d+")
      url <- stringr::str_glue("https://legisla.casacivil.go.gov.br/api/v2/pesquisa/legislacoes/{code}/pdf")
      download.file(
        url = url,
        destfile = dest,
        mode = "wb"
      )
    } else{
      download.file(
        url = url,
        destfile = dest,
        mode = "wb"
      )
    }
    logger::log_info("Baixado em {dest}")
  }
)

## MA -------------------
dir_ma <- stringr::str_glue("{dir}MA/") ## Caminho da Pasta LDO na Pasta do Maranhão
url_ma <- "https://seplan.ma.gov.br/ldo" ## Url do site do Maranhão
resp <- httr2::request(url_ma) |>
  httr2::req_timeout(600) |> 
  httr2::req_headers(
    Accept = "text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.7",
    `Accept-Language` = "pt-BR,pt;q=0.9,en-US;q=0.8,en;q=0.7,es;q=0.6,it;q=0.5,th;q=0.4,id;q=0.3,pt-PT;q=0.2",
    `Cache-Control` = "max-age=0",
    `If-Modified-Since` = "Tue, 04 Nov 2025 12:19:23 GMT",
    `Upgrade-Insecure-Requests` = "1",
    `User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/138.0.0.0 Safari/537.36 OPR/122.0.0.0",
  ) |>
  httr2::req_perform()

## Links da LDO e PLDO
links <- resp |> 
  httr2::resp_body_html() |> 
  rvest::html_element(".h-entry__e-content.e-content") |> 
  rvest::html_elements("a") |> 
  rvest::html_attr("href") |> 
  stringr::str_replace(
    "^/uploads/", "https://seplan.ma.gov.br/uploads/"
  ) |> stringr::str_subset("LDO|dire")
## Nomes dos arquivos
nomes <- links |> 
  stringr::str_extract("(?<=docs/)\\w.*") |> ## regex para pegar todo texto depois de 'docs/'
  stringr::str_remove_all("\\W") |> 
  stringr::str_replace("pdf", ".pdf")
infos <- tibble::tibble(
  links = links,
  nomes = nomes,
  doc = stringr::str_extract(nomes, "LDO|PLDO"),
  ano = stringr::str_extract(nomes, "20\\d{2}")
) |> 
  dplyr::mutate(
    doc = ifelse(is.na(doc), "LDO", doc)
  )
options(timeout = 6000)
purrr::pwalk(
  infos[29:30,],
  function(links, nomes, doc, ano){
    logger::log_info("baixando {links}")
    dir <- stringr::str_glue("C:/Users/samba/Downloads/{doc}/MA/{ano}/")
    if(!dir.exists(dir)){
      dir.create(dir, recursive = T)
    }
    dest <- stringr::str_glue("{dir}{nomes}")
    resp <- httr2::request(links) |> 
      httr2::req_timeout(600) |> 
      httr2::req_retry(
        max_tries = 10, 
        max_seconds = 600,
        is_transient = ~ httr2::resp_status(.x) %in% c(429, 443, 500, 502, 503, 504),
        failure_timeout = 60
      ) |> 
      httr2::req_throttle(5) |> 
      httr2::req_user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/138.0.0.0 Safari/537.36 OPR/122.0.0.0") |> 
      httr2::req_perform(path = dest)
    Sys.sleep(runif(1,50,60))
  }
)
## MG -------------
dir_mg <- stringr::str_glue("{dir}MG/")
url_mg <- "https://www.almg.gov.br/atividade-parlamentar/orcamento-do-estado/ldo/leis/LDO-"

purrr::walk(
  seq(10,26),
  ~{
    url <- stringr::str_glue("{url_mg}20{.x}")

    html <- rvest::read_html(url)

    txt <- html |> 
      rvest::html_text2() |> 
      stringr::str_extract("A Lei \\d+.\\d+, de \\d+") |> 
      stringr::str_remove("\\.")

    n_lei <- stringr::str_extract(txt, "\\d+")
    n_ano <- stringr::str_extract(txt, "\\d+$")

    url_lei <- stringr::str_glue("https://www.almg.gov.br/legislacao-mineira/texto/LEI/{n_lei}/{n_ano}/?cons=1")

    html_lei <- rvest::read_html(url_lei)

    txt_lei <- html_lei |> 
      rvest::html_element(".container-fluid.container-lg.px-xl-15.my-4.my-lg-5") |> 
      rvest::html_text2() |> 
      stringr::str_extract("(?<=Email\\n)(?s).*")

    text_dest <- stringr::str_glue("{dir_mg}LDO_20{.x}.txt")

    writeLines(txt_lei, text_dest)
  }
)

## MS --------------------------
dir_ms <- stringr::str_glue("{dir}MS/")
url_ms <- "https://suorc.ms.gov.br/ldo"
html_ms <- rvest::read_html(url_ms)

links <- html_ms |> 
  rvest::html_element("#page") |> 
  rvest::html_element(".col-12") |> 
  rvest::html_elements("a") |> 
  rvest::html_attr("href") |> 
  stringr::str_subset(".pdf")

nomes <- html_ms |> 
  rvest::html_element("#page") |> 
  rvest::html_element(".col-12") |> 
  rvest::html_elements("a") |> 
  rvest::html_element("p") |> 
  rvest::html_text2() |> 
  stringr::str_extract("\\d+") |> 
  stringr::str_replace("2025", "2024") |> 
  stringr::str_replace("2026", "2025") |> 
  stringr::str_subset("\\d") %>%
  stringr::str_c("LDO_", ., ".pdf")
infos <- tibble::tibble(
  links = links, 
  nomes = nomes
)
purrr::pwalk(
  infos,
  function(links, nomes){
    dest <- stringr::str_glue("{dir_ms}{nomes}")
    download.file(
      url = links,
      destfile = dest,
      mode = "wb"
    )
  }
)

## MT -------------------
dir_mt <- stringr::str_glue("{dir}MT/")
url_mt <- "https://www.iomat.mt.gov.br/canal/ppa-ldo-loa"

html_mt <- rvest::read_html(url_mt)
codes <- html_mt |> 
  rvest::html_elements("tr") |> 
  purrr::pluck(2) |> 
  rvest::html_elements("td") |> 
  purrr::pluck(2) |> 
  rvest::html_elements("a") |> 
  rvest::html_attr("href") |> 
  stringr::str_replace("../../../", "http://iomat.mt.gov.br/") |> 
  stringr::str_replace("pdf", "html") |> 
  stringr::str_extract("\\d+$")

nomes <- html_mt |> 
  rvest::html_elements("tr") |> 
  purrr::pluck(2) |> 
  rvest::html_elements("td") |> 
  purrr::pluck(2) |> 
  rvest::html_elements("a") |> 
  rvest::html_text2() |> 
  stringr::str_remove("\\n")

infos <- tibble::tibble(
  links = stringr::str_glue("https://iomat.mt.gov.br/portal/edicoes/download/{codes}/download"),
  nomes = stringr::str_glue("Diario_Oficial_{nomes}.pdf")
)

purrr::pwalk(
  infos,
  function(links, nomes){
    dest <- stringr::str_glue("{dir_mt}{nomes}")

    download.file(
      url = links,
      destfile = dest,
      mode = "wb"
    )
  }
)

## PA -----------------
dir_pa <- stringr::str_glue("{dir}PA/")
url_pa <- "https://www.seplad.pa.gov.br/lei-de-diretrizes-orcamentarias-ldo-2"

html_pa <- rvest::read_html(url_pa)

links <- html_pa |> 
  rvest::html_element(".col-sm-12.wr-default-page") |> 
  rvest::html_elements("a") |> 
  rvest::html_attr("href")

ano <- html_pa |> 
  rvest::html_element(".col-sm-12.wr-default-page") |> 
  rvest::html_elements("a") |> 
  rvest::html_text2()

infos <- tibble::tibble(
  links = links,
  ano = ano
)
options(timeout = 120)
purrr::pwalk(
  infos,
  function(links, ano){
    dir_ano <- stringr::str_glue("{dir_pa}{ano}/")
    if(!dir.exists(dir_ano)){
      dir.create(dir_ano, recursive = T)
    }
    if(stringr::str_detect(links, "pdf")){
      dest <- stringr::str_glue("{dir_ano}{ano}.pdf")
      download.file(
        url = links,
        destfile = dest,
        mode = "wb"
      )
    } else{
      html <- rvest::read_html(links)

      links <- html |> 
        rvest::html_element(".col-sm-12.wr-default-page") |> 
        rvest::html_elements("a") |> 
        rvest::html_attr("href") |> 
        stringr::str_replace("http:", "https:")
      nomes <- html |> 
        rvest::html_element(".col-sm-12.wr-default-page") |> 
        rvest::html_elements("a") |> 
        rvest::html_text2() |> 
        stringr::str_remove_all("[:blank:]") |> 
        stringr::str_c(".pdf")
      infos <- tibble::tibble(
        link = links,
        nome = nomes
      )

      purrr::pwalk(
        infos,
        function(link, nome){
          dest <- stringr::str_glue("{dir_ano}{nome}")
          download.file(
            url = link,
            destfile = dest,
            mode = "wb"
          )
        }
      )
    }
  }
)

## PB ---------------------
dir_pb <- stringr::str_glue("{dir}PB/")
url_pb <- "https://www.tjpb.jus.br/transparencia/financas/outros-dados-da-execucao-orcamentaria-e-financeira/lei-de-diretrizes"

html_pb <- rvest::read_html(url_pb)

links <- html_pb |> 
  rvest::html_element(".por-categoria") |> 
  rvest::html_elements("a") |> 
  rvest::html_attr("href") |> 
  stringr::str_subset("^https")


nomes <- html_pb |> 
  rvest::html_element(".por-categoria") |> 
  rvest::html_elements("a") |> 
  rvest::html_attr("href") |> 
  stringr::str_subset("^https") |> 
  stringr::str_extract("[^/]+(?<=.pdf)")

anos <- html_pb |> 
  rvest::html_element(".por-categoria") |> 
  rvest::html_elements("a") |> 
  rvest::html_text2() |> 
  stringr::str_subset("^\\d")

infos <- tibble::tibble(
  link = links,
  nome = nomes, 
  ano = anos
)

options(timeout = 300)

purrr::pwalk(
  infos,
  function(link, nome, ano){
    dir_ano <- stringr::str_glue("{dir_pb}{ano}/")
    if(!dir.exists(dir_ano)){
      dir.create(dir_ano, recursive = T)
    }
    dest <- stringr::str_glue("{dir_ano}{nome}")
    download.file(
      url = link,
      destfile = dest,
      mode = "wb"
    )
  }
)

## PE --------------

url_pe <- "https://www.seplag.pe.gov.br/orcamento"

html_pe <- rvest::read_html(url_pe)

links <- html_pe |> 
  rvest::html_element(".page-content.post.item-page") |> 
  rvest::html_element("div.row") |> 
  rvest::html_elements("p") |> 
  rvest::html_element("a") |> 
  rvest::html_attr("href") |> 
  stringr::str_c("/download") |> 
  stringr::str_subset("\\w")

nomes <- html_pe |> 
  rvest::html_element(".page-content.post.item-page") |> 
  rvest::html_element("div.row") |> 
  rvest::html_elements("p") |> 
  rvest::html_element("a") |> 
  rvest::html_text2() |> 
  stringr::str_subset("\\w")

infos <- tibble::tibble(
  link = links,
  nome = nomes
) |> 
  dplyr::mutate(
    doc = stringr::str_extract(nome, "LDO|LOA|Plano") |> stringr::str_replace("Plano", "PPA"),
    ano = stringr::str_extract(nome, "\\d+")
  ) |> 
  dplyr::mutate(
    ano = dplyr::case_when(
      doc == "PPA" & ano >= 2008 & ano <= 2011 ~ "2008-2011",
      doc == "PPA" & ano >= 2012 & ano <= 2015 ~ "2012-2015",
      doc == "PPA" & ano >= 2016 & ano <= 2019 ~ "2016-2019",
      doc == "PPA" & ano >= 2020 & ano <= 2023 ~ "2020-2023",
      doc == "PPA" & ano >= 2024 & ano <= 2027 ~ "2024-2027",
      .default = ano
    ),
    nome = stringr::str_remove_all(nome, "[:blank:]"),
    nome = stringr::str_glue("{nome}.pdf"),
  )
options(timeout = 300)
purrr::pwalk(
  infos,
  function(link, nome, doc, ano){
    dir_doc <- stringr::str_glue("{dir}{doc}/PE/{ano}/")
    if(!dir.exists(dir_doc)){
      dir.create(dir_doc, recursive = T)
    }
    dest <- stringr::str_glue("{dir_doc}{nome}")
    download.file(
      url = link,
      destfile = dest,
      mode = "wb"
    )
  }
)

## PI -------------
dir_pi <- stringr::str_glue("{dir}PI/")

url_pi <- "https://www.seplan.pi.gov.br/ldo/#92-92-wpfd-top"

## Iniciar o driver com o perfil do Firefox
rs_driver_object <- RSelenium::rsDriver(
  browser = "firefox",
  chromever = NULL,  ## Manter como NULL já que não usaremos o Chrome
  verbose = FALSE,
  phantomver = NULL
)

## Abrir o webdriver
remDr <- rs_driver_object$client

## Fechando a primeira janela
remDr$close()

## Iniciar a Navegação Remota
remDr$open()
## Navegando até o site
remDr$navigate(url_pi)
Sys.sleep(10)

# listano as pastas com os dados dos anos presentes
pastas <- remDr$findElements(
  using = "css selector",
  value = "a.wpfdcategory.catlink"
)
n_pastas <- length(pastas)

if(n_pastas == 0){
  stop("Os dados de nenhum ano foi encontrado")
}


Sys.sleep(3)
# Pegando os links de download das pastas de cada ano
infos <- purrr::map_df(
  1:n_pastas,
  ~{
    # Pegando as pastas
    ## Essa estapa é necessário para que sempre seja as informações da página atualizada
    pastas <- remDr$findElements(
      using = "css selector",
      value = "a.wpfdcategory.catlink"
    )
    # Clicando na pasta do ano
    pastas[[.x]]$clickElement()

    Sys.sleep(7)

    # Pegando o link de download
    link <- remDr$findElement(
      using = "css selector",
      value = "a.default-download-category"
    )$getElementAttribute("href") |> 
      unlist()

    # Pegando o ano
    ano <- remDr$findElement(
      using = "css selector",
      value = "div.wpfd-categories"
    )$findChildElement(
      using = "css selector",
      value = "h2"
    )$getElementText() |> 
      unlist()

    Sys.sleep(2)
    remDr$findElement(
      using = "css selector",
      value = "i.zmdi.zmdi-chevron-left"
    )$clickElement()
    Sys.sleep(5)
    info <- tibble::tibble(
      link = link,
      nome = stringr::str_glue("{ano}.zip"),
      ano = ano
    )
  }
)
# Fechando a conexão do Selenium
remDr$close()
rs_driver_object$server$stop()

purrr::pwalk(
  infos,
  function(link, nome, ano){
    dir_ano <- stringr::str_glue("{dir_pi}{ano}/")
    if(!dir.exists(dir_ano)){
      dir.create(dir_ano, recursive = T)
    }
    dest <- stringr::str_glue("{dir_ano}{nome}")
    download.file(
      url = link,
      destfile = dest,
      mode = "wb"
    )

    zip::unzip(
      zipfile = dest,
      files = NULL,
      exdir = dir_ano
    )
    file.remove(dest)
  }
)

## PR ------------------------
dir_pr <- stringr::str_glue("{dir}PR/")

url_pr <- "https://www.fazenda.pr.gov.br/Pagina/Lei-de-Diretrizes-Orcamentarias-Historico-Sistema-Estadual-de-Legislacao"

html_pr <- rvest::read_html(url_pr)

links_redirecionamento <- html_pr |> 
  rvest::html_element("div#collapseCollapsibleMG5MO4M2YYVGC") |> 
  rvest::html_elements("a") |> 
  rvest::html_attr("href")


purrr::walk(
  links_redirecionamento,
  ~{
    link_redirecionado <- .x
    # Lendo o link redirecionado
    html <- rvest::read_html(link_redirecionado)
    # Pegando o corpo do texto
    txt <- html |> 
      rvest::html_element("form") |> 
      rvest::html_text2()

    # Extraindo o ano vigente da LDO
    ano <- html |> 
      rvest::html_element("p") |> 
      rvest::html_text2() |> 
      stringr::str_extract("\\d{4}")

    # Nome do arquivo
    nome <- stringr::str_glue("LDO_{ano}.txt")

    # Definindo o diretório do arquivo
    dir_ano <- stringr::str_glue("{dir_pr}{ano}/")
    if(!dir.exists(dir_ano)){
      dir.create(dir_ano, recursive = T)
    }
    dest <- stringr::str_glue("{dir_ano}{nome}")

    # Escrevendo a LDO em um arquivo txt
    writeLines(
      txt,
      dest
    )
    # Estabelecendo um intervalo aleatório entre as extrações 
    Sys.sleep(runif(1, 5, 10))
  }
)

## RN -------------------------
# Definindo o diretório para os dados de LDO do RN
dir_rn <- stringr::str_glue("{dir}RN/")
# Url onde os dados estão disponíveis
url_rn <- "http://adcon.rn.gov.br/ACERVO/seplan/Conteudo.asp?TRAN=PASTAC&TARG=2476&ACT=&PAGE=&PARM=&LBL="
# Lendo o html do url
html_rn <- rvest::read_html(url_rn)

links <- html_rn |> 
  rvest::html_element("div#ACERVO") |> 
  rvest::html_elements("a") |> 
  rvest::html_attr("href")

nomes <- html_rn |> 
  rvest::html_element("div#ACERVO") |> 
  rvest::html_elements("a") |> 
  rvest::html_text2() |> 
  stringr::str_remove_all("\\.pdf|\\.PDF") |> 
  stringr::str_remove_all("\\W") |> 
  stringr::str_c(".pdf")

anos <- html_rn |> 
  rvest::html_element("div#ACERVO") |> 
  rvest::html_elements("a") |> 
  rvest::html_text2() |> 
  stringr::str_extract("(?<=LDO\\s|PLDO\\s|LOA\\s)\\d{4}")

infos <- tibble::tibble(
  link = links,
  nome = nomes,
  ano = anos,
  doc = stringr::str_extract(nome, "PLDO|LDO|LOA")
)

purrr::pwalk(
  infos,
  function(link, nome, ano, doc){
    dir <- stringr::str_glue("data-raw/{doc}/")
    dir_ano <- stringr::str_glue("{dir}{ano}/")
    if(!dir.exists(dir_ano)){
      dir.create(dir_ano)
    }
    
  }
)

##
t = 1
# PPA -------------------------

dir <- "C:/Users/samba/Downloads/PPA/"
## AC ---------
dir_ac <- stringr::str_glue("{dir}AC/")
url_ac <- "https://seplan.ac.gov.br/plano-plurianual-ppa"


## Iniciar o driver com o perfil do Firefox
rs_driver_object <- RSelenium::rsDriver(
  browser = "firefox",
  chromever = NULL,  ## Manter como NULL já que não usaremos o Chrome
  verbose = FALSE,
  phantomver = NULL
)

## Abrir o webdriver
remDr <- rs_driver_object$client

## Fechando a primeira janela
remDr$close()

## Iniciar a Navegação Remota
remDr$open()
## Navegando até o site
remDr$navigate(url_ac)

Sys.sleep(5)

a <- remDr$findElements(
  using = "css selector",
  "a"
)

links <- purrr::map_chr(
  seq(1, length(a)),
  ~{
    a[[.x]]$getElementAttribute("href") |> unlist()
  }
) |> 
  stringr::str_subset("pdf") |> 
  stringr::str_unique()
rs_driver_object$server$stop()
purrr::walk(
  links,
  ~{
    nome <- stringr::str_extract(.x, "[^/]+(?<=.pdf)")
    
    dest <- stringr::str_glue("{dir_ac}{nome}")

    download.file(
      url = .x,
      destfile = dest,
      mode = "wb"
    )
    
  }
)

## AM -------------------------
dir_am <- stringr::str_glue("{dir}AM/")
url_ma <- "https://portaldoplanejamento.am.gov.br/ppa"
html_ma <- rvest::read_html(url_ma)

links_de_redirecionados <- html_ma |> 
  rvest::html_elements("a") |> 
  rvest::html_attr("href") |> 
  stringr::str_subset("ppa-")
options(timeout = 3000)
purrr::walk(
  links_de_redirecionados[4:6],
  ~{
    if(stringr::str_detect(.x, "2024")){
      ano <- stringr::str_extract(.x, "\\d+")

      dir_ano <- stringr::str_glue("{dir_am}{ano}/")
      if(!dir.exists(dir_ano)){
        dir.create(dir_ano, recursive = T)
      }
      
      html <- rvest::read_html(.x)

      links_caixa_selecao <- html |> 
        rvest::html_elements("option") |> 
        rvest::html_attr("value") |> 
        stringr::str_subset("\\w")

      link_pagina <- html |> 
        rvest::html_elements("a") |> 
        rvest::html_attr("href") |> 
        stringr::str_subset("pdf")

      links <- c(links_caixa_selecao, link_pagina) |> 
        stringr::str_subset("pdf|3d-flip-book")
      nomes <- stringr::str_extract(links, "[^/]+(?<=.pdf)")

      infos <- tibble::tibble(
        link = links,
        nome = nomes
      )

      purrr::pwalk(
        infos,
        function(link, nome){
          if(stringr::str_detect(link, "pdf")){
            dest <- stringr::str_glue("{dir_ano}{nome}")

            download.file(
              url = link,
              destfile = dest,
              mode = "wb"
            )
          } else{
            dest_dir <- stringr::str_glue("Downloads/PPA/AM/{ano}/")
            ## Diretório de download:
            download_dir <- normalizePath(file.path(getwd(), dest_dir))

            ## Verificar se o diretório existe, se não, criar
            if (!dir.exists(download_dir)) {
              dir.create(download_dir, recursive = TRUE)
            }

            ## Criar um diretório temporário para o perfil do Firefox
            profile_dir <- tempfile(pattern = "firefox_profile")
            dir.create(profile_dir)

            ## Criar um arquivo user.js com as configurações de download
            user_js <- file.path(profile_dir, "user.js")
            writeLines(c(
              'user_pref("browser.download.folderList", 2);',  ## Usar diretório específico
              paste0('user_pref("browser.download.dir", "', download_dir, '");'),  ## Diretório atual
              'user_pref("browser.download.useDownloadDir", true);',  ## Forçar o uso do diretório de download
              'user_pref("browser.helperApps.neverAsk.saveToDisk", "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet, text/csv");',  ## Tipos de arquivo
              'user_pref("pdfjs.disabled", true);',  ## Desabilitar visualização de PDF
              'user_pref("browser.download.manager.showWhenStarting", false);',  ## Evitar popup de download
              'user_pref("browser.download.manager.closeWhenDone", true);',  ## Fechar o gerenciador de download
              'user_pref("browser.download.autoOpenValue", 0);'  ## Forçar o download automático
            ), con = user_js)

            ## Iniciar o driver com o perfil do Firefox
            rs_driver_object <- RSelenium::rsDriver(
              browser = "firefox",
              chromever = NULL,  ## Manter como NULL já que não usaremos o Chrome
              extraCapabilities = list(
                "moz:firefoxOptions" = list(
                  args = list("-profile", profile_dir)
                )
              ),
              verbose = FALSE,
              phantomver = NULL
            )
            
            ## Abrir o webdriver
            remDr <- rs_driver_object$client

            ## Fechando a primeira janela
            remDr$close()

            ## Iniciar a Navegação Remota
            remDr$open()
            ## Navegando até o site
            remDr$navigate(link)

            Sys.sleep(45)

            iframe <- remDr$findElement(
              using = "css selector",
              "iframe"
            )
            # Entrando no iframe
            remDr$switchToFrame(iframe)

            remDr$findElement(
              using = "css selector",
              ".dropup.widSettings"
            )$clickElement()

            remDr$findElement(
              using = "css selector",
              ".cmd.cmdSave"
            )$clickElement()

            Sys.sleep(120)

            remDr$quit()
            rs_driver_object$server$stop()
          }
        }
      )

      
    } else{
      html <- rvest::read_html(.x)

      ano <- stringr::str_extract(.x, "\\d+")

      dir_ano <- stringr::str_glue("{dir_am}{ano}/")
      if(!dir.exists(dir_ano)){
        dir.create(dir_ano, recursive = T)
      }

      links_caixa_selecao <- html |> 
        #rvest::html_element(".content-btn") |> 
        #rvest::html_element("#PPA2024") |> 
        rvest::html_elements("option") |> 
        rvest::html_attr("value") |> 
        stringr::str_subset("\\w")

      link_pagina <- html |> 
        rvest::html_elements("a") |> 
        rvest::html_attr("href") |> 
        stringr::str_subset("pdf")

      links <- c(links_caixa_selecao, link_pagina) |> 
        stringr::str_subset("pdf|download")
      nomes <- stringr::str_extract(links, "[^/]+(?<=.pdf)")

      infos <- tibble::tibble(
        link = links,
        nome = nomes
      )

      purrr::pwalk(
        infos,
        function(link, nome){
          if(stringr::str_detect(link, "pdf")){
            dest <- stringr::str_glue("{dir_ano}{nome}")

            download.file(
              url = link,
              destfile = dest,
              mode = "wb"
            )
          } else{
            nome_aleatorio <- stringi::stri_rand_strings(1, length = 10)
            dest <- stringr::str_glue("{dir_ano}{nome_aleatorio}.pdf")

            download.file(
              url = link,
              destfile = dest,
              mode = "wb"
            )
          }
        }
      )
    }
    
  }
)

## BA -----------
dir_ba <- stringr::str_glue("{dir}BA/")

url_ba <- "https://www.ba.gov.br/seplan/planejamento/ppa-plano-plurianual"

html_ba <- rvest::read_html(url_ba)

links_1 <- html_ba |> 
  rvest::html_element(
    ".clearfix.text-formatted.field.field--name-body.field--type-text-with-summary.field--label-hidden.field__item"
  ) |> 
  rvest::html_elements("a") |> 
  rvest::html_attr("href")  %>%
  stringr::str_c("https://www.ba.gov.br",.)  |> 
  stringr::str_subset("pdf")


nomes_1 <- links_1 |> 
  stringr::str_subset("pdf") |> 
  stringr::str_extract("[^/]+(?<=.pdf)")

url_ba <- "https://www.ba.gov.br/seplan/planejamento/historico-do-ppa"

html_ba <- rvest::read_html(url_ba)

links_2 <- html_ba |> 
  rvest::html_elements("a") |> 
  rvest::html_attr("href") |> 
  stringr::str_subset("pdf") %>%
  stringr::str_c("https://www.ba.gov.br",.) 

nomes_2 <- links_2 |> 
  stringr::str_subset("pdf") |> 
  stringr::str_extract("[^/]+(?<=.pdf)")

infos <- tibble::tibble(
  link = c(links_1, links_2),
  nome = c(nomes_1, nomes_2)
) |> 
  dplyr::mutate(
    ano = dplyr::case_when(
      stringr::str_detect(nome, "2024|2025|2026|2027") ~ "2024-2027",
      stringr::str_starts(nome, "20120223") ~ "2012-2015",
      stringr::str_detect(nome, "2020|2021|2022|2023") ~ "2020-2023",
      stringr::str_detect(nome, "2016|2017|2018|2019") ~ "2016-2019",
      stringr::str_detect(nome, "2008|2009|2010|2011") ~ "2008-2011",
      stringr::str_detect(nome, "2004|2005|2006|2007") ~ "2004-2007",
      stringr::str_detect(nome, "Lei_14172_Lei_PPA_Alterada.pdf") ~ "2020-2023",
      stringr::str_starts(nome, "\\d_") ~ "2016-2019",
      stringr::str_detect(nome, "Programas_de_Governo_por_Diretriz_Estrategica_Poder_Executivo.pdf") ~ "2008-2011",
      stringr::str_starts(nome, "0\\d_") ~ "2004-2007"
    )
  )

purrr::pwalk(
  infos,
  function(link, nome, ano){
    dir_ano <- stringr::str_glue("{dir_ba}{ano}/")
    if(!dir.exists(dir_ano)){
      dir.create(dir_ano)
    }
    dest <- stringr::str_glue("{dir_ano}{nome}")

    download.file(
      url = link,
      destfile = dest,
      mode = "wb"
    )
  }
)

## CE -----------------

dir_ce <- stringr::str_glue("{dir}CE/")

url_ce <- "https://www.cearaprev.ce.gov.br/planejamento/ppa"

html_ce <- rvest::read_html(url_ce)

anos <- html_ce |> 
  rvest::html_element(
    "div.wrapper.container"
  ) |> 
  rvest::html_element(
    "ul.ListaEst.col2"
  ) |> 
  rvest::html_elements("a") |> 
  rvest::html_text2() |> 
  stringr::str_extract("\\d+-\\d+")

links_redirecionamento <- html_ce |> 
  rvest::html_element(
    "div.wrapper.container"
  ) |> 
  rvest::html_element(
    "ul.ListaEst.col2"
  ) |> 
  rvest::html_elements("a") |> 
  rvest::html_attr("href") |> 
  purrr::map_chr(
    ~{
      logger::log_info("Lendo o html de: {.x}")
      html <- rvest::read_html(.x)
      logger::log_info("Pegando o link de redirecionamento")
      links_formulacao_revisao <- html |> 
        rvest::html_elements("a") |> 
        rvest::html_attr("href") |> 
        stringr::str_subset("formulacao") |> 
        stringr::str_unique()
    }
  )
options(timeout = 300)
purrr::walk(
  links_redirecionamento,
  ~{
    link_redirecionado <- .x
    html_redirecionado <- rvest::read_html(link_redirecionado)

    links <- html_redirecionado |> 
      rvest::html_elements("a") |> 
      rvest::html_attr("href") |> 
      stringr::str_subset(".pdf$")

    nomes <- links |> 
      stringr::str_extract("[^/]+(?<=.pdf)")

    ano <- .x |> 
      stringr::str_extract("\\d+-\\d+")

    infos <- tibble::tibble(
      link = links,
      nome = nomes
    ) |> 
      dplyr::mutate(
        ano = ano
      )

    purrr::pwalk(
      infos,
      function(link, nome, ano){
        dir_ano <- stringr::str_glue("{dir_ce}{ano}/")
        if(!dir.exists(dir_ano)){
          dir.create(dir_ano, recursive = T)
        }
        dest <- stringr::str_glue("{dir_ano}{nome}")

        tryCatch(
          {
          download.file(
            url = link,
            destfile = dest,
            method = "libcurl",
            headers = c("User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/58.0.3029.110 Safari/537.36"),
            mode = "wb"
          )
          logger::log_info("Arquivo: {nome} do ano {ano} foi baixado com sucesso")
          }, error = function(e) {
            logger::log_info("Arquivo: {nome} do ano {ano} falhou")
          }
        )
      }
    )
  }
)

## DF -----------------------------
dir_df <- stringr::str_glue("{dir}DF/")
url_df_1 <- "https://www.economia.df.gov.br/ppa"
html_df_1 <- rvest::read_html(url_df_1)

links_1 <- html_df_1 |> 
  rvest::html_elements("a") |> 
  rvest::html_attr("href") |> 
  stringr::str_subset("pdf") |> 
  stringr::str_replace("^/documents/", "https://economia.df.gov.br/documents/")

nomes_1 <- links_1 |> 
  stringr::str_extract("(?<=seec/)\\w.*") |> 
  stringr::str_c(".pdf")

ano_1 <- "2024-2027"

infos_1 <- tibble::tibble(
  link = links_1,
  nome = nomes_1,
  ano = ano_1
)

url_df_2 <- "https://www.economia.df.gov.br/w/ppas-anteriores?p_l_back_url=%2Fbusca%3F_com_liferay_portal_search_web_search_bar_portlet_SearchBarPortlet_INSTANCE_templateSearch_formDate%3D1743170302492%26_com_liferay_portal_search_web_search_bar_portlet_SearchBarPortlet_INSTANCE_templateSearch_emptySearchEnabled%3Dfalse%26q%3DPPAs%2Banteriores%26_com_liferay_portal_search_web_search_bar_portlet_SearchBarPortlet_INSTANCE_templateSearch_scope%3D&p_l_back_url_title=busca"
html_df_2 <- rvest::read_html(url_df_2)


infos_2 <- html_df_2 |> 
  rvest::html_element("div.materia") |> 
  rvest::html_elements("a") |> 
  rvest::html_attr("href") |> 
  purrr::map_df(
    ~{
      link_redirecionado = .x

      html <- rvest::read_html(link_redirecionado)

      links <- html |> 
        rvest::html_elements("a") |> 
        rvest::html_attr("href") |> 
        stringr::str_subset("pdf") |> 
        stringr::str_unique() |> 
        stringr::str_replace("^/documents/", "https://www.economia.df.gov.br/documents/")
      
      nomes <- links |> 
        stringr::str_extract("(?<=seec/)\\w.*") |> 
        stringr::str_c(".pdf")

      ano <- stringr::str_extract(link_redirecionado, "\\d+-\\d+")

      infos <- tibble::tibble(
        link = links,
        nome = nomes
      ) |> 
        dplyr::mutate(
          ano = ano,
          nome = dplyr::case_when(
            is.na(nome) ~ stringr::str_glue("{stringi::stri_rand_strings(1, length = 10)}.pdf"),
            .default = nome
          )
        )

    }
  )

infos <- dplyr::bind_rows(
  infos_1, 
  infos_2
)
options(timeout = 300)
purrr::pwalk(
  infos,
  function(link, nome, ano){
    dir_ano <- stringr::str_glue("{dir_df}{ano}/")
    if(!dir.exists(dir_ano)){
      dir.create(dir_ano, recursive = T)
    }

    dest <- stringr::str_glue("{dir_ano}{nome}")
    tryCatch(
      {
        download.file(
          url = link,
          destfile = dest,
          #method = "libcurl",
          #headers = c("User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/58.0.3029.110 Safari/537.36"),
          mode = "wb"
        )
        logger::log_info("Arquivo: {nome} do ano {ano} foi baixado com sucesso")
        }, error = function(e) {
          logger::log_info("Arquivo: {nome} do ano {ano} falhou")
      }
    )
  }
)

## ES -------------
# Diretório da pasta PPA para estado ES
dir_es <- stringr::str_glue("{dir}ES/")
# URL que contém os dados PPA
url_es <- "https://planejamento.es.gov.br/plano-plurianual-ppa"
# Baixnado o html
html_es <- rvest::read_html(url_es)
# Pegando os links de redirecionamento para os dados de PPA de cada ano
links_redirecionamento <- html_es |> 
  rvest::html_element("#container-links") |> 
  rvest::html_elements(
    "a"
  ) |> 
  rvest::html_attr("href") %>%
  stringr::str_c("https://planejamento.es.gov.br", .)

infos <- purrr::map_df(
  links_redirecionamento,
  ~ {
    logger::log_info("Construindo tabela info para {.x}")
    link_redirecionado <- .x
    # Baixando o html do link redirecionado
    html <- rvest::read_html(link_redirecionado)

    # Pegando os links dos arquivos que serão baixados
    links <- html |> 
      rvest::html_elements("a") |> 
      rvest::html_attr("href") |> 
      stringr::str_subset(".pdf") |> 
      stringr::str_unique() |> 
      stringr::str_subset("^/Media/") |> 
      stringr::str_replace("^/Media/", "https://planejamento.es.gov.br/Media/")

    nomes <- links |> 
      stringr::str_extract("[^/]+(?<=.pdf)")

    ano <- link_redirecionado |> 
      stringr::str_extract("\\d+")

    if(is.na(ano)){
      ano = "anos_anteriores"
    }


    info <- tibble::tibble(
      link = links,
      nome = nomes
    ) |> 
      dplyr::mutate(
        ano = ano
      )

  }
)
options(timeout = 300)
purrr::pwalk(
  infos,
  function(link, nome, ano){
    dir_ano <- stringr::str_glue("{dir_es}{ano}/")
    if(!dir.exists(dir_ano)){
      dir.create(dir_ano, recursive = T)
    }

    dest <- stringr::str_glue("{dir_ano}{nome}")
    tryCatch(
      {
        download.file(
          url = link,
          destfile = dest,
          #method = "libcurl",
          #headers = c("User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/58.0.3029.110 Safari/537.36"),
          mode = "wb"
        )
        logger::log_info("Arquivo: {nome} do ano {ano} foi baixado com sucesso")
        }, error = function(e) {
          logger::log_info("Arquivo: {nome} do ano {ano} falhou")
      }
    )
  }
)

## GO -----------------------------

# Diretorio onde os dados de PPA do estado serão salvos
dir_go <- stringr::str_glue("{dir}GO/")

url_go <- "https://ppa.go.gov.br/ppas-anteriores"

html_go <- rvest::read_html(url_go)

links <- html_go |> 
  rvest::html_elements("a") |> 
  rvest::html_attr("href") |> 
  stringr::str_subset("arquivos|pdf|zip")

links <- c(links, "https://ppa.go.gov.br/wp-content/uploads/sites/9/2024/02/PL-PPA-V.12-02-2024.pdf")

nomes <- links |> 
  stringr::str_extract("[^/]+(?<=pdf|zip)")

anos <- nomes |> 
  stringr::str_extract("\\d{4}")

infos <- tibble::tibble(
  link = links,
  nome = nomes,
  ano = anos
) |> 
  dplyr::mutate(
    nome = dplyr::case_when(
      is.na(nome) ~ stringr::str_glue("{stringi::stri_rand_strings(1, 10)}.pdf"),
      stringr::str_detect(link, "103655") ~ stringr::str_glue("{stringi::stri_rand_strings(1, 10)}.pdf"),
      stringr::str_detect(link, "106715") ~ stringr::str_glue("{stringi::stri_rand_strings(1, 10)}.pdf"),
      .default = nome
    ),
    ano = ifelse(is.na(ano), "2020", ano)
  )

options(timeout = 300)
purrr::pwalk(
  infos,
  function(link, nome, ano){
    dir_ano <- stringr::str_glue("{dir_go}{ano}/")
    if(!dir.exists(dir_ano)){
      dir.create(dir_ano, recursive = T)
    }

    dest <- stringr::str_glue("{dir_ano}{nome}")
    tryCatch(
      {
        download.file(
          url = link,
          destfile = dest,
          #method = "libcurl",
          #headers = c("User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/58.0.3029.110 Safari/537.36"),
          mode = "wb"
        )
        logger::log_info("Arquivo: {nome} do ano {ano} foi baixado com sucesso")
        }, error = function(e) {
          logger::log_info("Arquivo: {nome} do ano {ano} falhou")
      }
    )
  }
)

## MA --------------

dir_ma <- stringr::str_glue("{dir}MA/")

url_ma <- "https://seplan.ma.gov.br/plurianual"

html_ma <- rvest::read_html(url_ma)

anos <- html_ma |> 
  rvest::html_element(".h-entry__e-content.e-content") |> 
  rvest::html_elements("h1") |> 
  rvest::html_text2()

infos <- purrr::map2_df(
  anos,
  seq(1, length(anos)),
  ~{
    n <- .y * 2
    print(n)
    print(.x)
    css_selector <- stringr::str_glue("#main > section > ul:nth-child({n})")

    links <- html_ma |> 
      rvest::html_element(css_selector) |> 
      rvest::html_elements("a") |> 
      rvest::html_attr("href") |> 
      stringr::str_subset("pdf") |> 
      stringr::str_replace("^/uploads/", "https://www.seplan.ma.gov.br/uploads/")

    nomes <- stringr::str_extract(links, "[^/]+(?<=.pdf)")

    info <- tibble::tibble(
      link = links,
      nome = nomes
    ) |> 
      dplyr::mutate(
        ano = .x
      )
  }
)

options(timeout = 300)
purrr::pwalk(
  infos,
  function(link, nome, ano){
    dir_ano <- stringr::str_glue("{dir_ma}{ano}/")
    if(!dir.exists(dir_ano)){
      dir.create(dir_ano, recursive = T)
    }

    dest <- stringr::str_glue("{dir_ano}{nome}")
    tryCatch(
      {
        download.file(
          url = link,
          destfile = dest,
          #method = "libcurl",
          #headers = c("User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/58.0.3029.110 Safari/537.36"),
          mode = "wb"
        )
        logger::log_info("Arquivo: {nome} do ano {ano} foi baixado com sucesso")
        }, error = function(e) {
          logger::log_info("Arquivo: {nome} do ano {ano} falhou")
      }
    )
  }
)

## MS -------------
dir_ms <- stringr::str_glue("{dir}MS/")
url_ms <- "https://suorc.ms.gov.br/ppa"
html_ms <- rvest::read_html(url_ms)

links <- html_ms |> 
  rvest::html_element("#page") |> 
  rvest::html_element(".col-12") |> 
  rvest::html_elements("a") |> 
  rvest::html_attr("href") |> 
  stringr::str_subset(".pdf")

nomes <- stringr::str_extract(links, "[^/]+(?<=.pdf)")

anos <- html_ms |> 
  rvest::html_element("#page") |> 
  rvest::html_element(".col-12") |> 
  rvest::html_elements("a") |> 
  rvest::html_element("p") |> 
  rvest::html_text2() |> 
  stringr::str_extract("\\d{4}/\\d{4}|\\d{4} a \\d{4}") |> 
  stringr::str_replace(" a ", "/") |> 
  stringr::str_replace("/", "-")
infos <- tibble::tibble(
  link = links, 
  nome = nomes,
  ano = anos
)
purrr::pwalk(
  infos,
  function(link, nome, ano){
    dir_ano <- stringr::str_glue("{dir_ms}{ano}/")
    if(!dir.exists(dir_ano)){
      dir.create(dir_ano, recursive = T)
    }
    dest <- stringr::str_glue("{dir_ano}{nome}")
    download.file(
      url = link,
      destfile = dest,
      mode = "wb"
    )
  }
)

## MT --------------------------
dir_mt <- stringr::str_glue("{dir}MT/")
url_mt <- "https://www.seplag.mt.gov.br/index.php?pg=ver&id=4566&c=111&sub=true"

html_mt <- rvest::read_html(url_mt)

links_redirecionamento <- html_mt |> 
  rvest::html_element("#bloco2") |> 
  rvest::html_elements("a") |> 
  rvest::html_attr("href") %>%
  stringr::str_c("https://www.seplag.mt.gov.br/", .)

infos <- purrr::map_df(
  links_redirecionamento,
  ~{
    link_redirecionado <- .x
    html <- rvest::read_html(link_redirecionado)
    links <- html |> 
      rvest::html_element("td.fontePadrao") |> 
      rvest::html_elements("a") |> 
      rvest::html_attr("href") |> 
      stringr::str_subset("pdf|xlsx") %>%
      stringr::str_c("https://www.seplag.mt.gov.br/", .)

    nomes <- links |> 
      stringr::str_extract("[^/]+(?<=.pdf|.xlsx)")

    ano <- html |> 
      rvest::html_element("h3.header-title") |> 
      rvest::html_text2() |> 
      stringr::str_extract("\\d{4} \\W \\d{4}|\\d{4}\\W\\d{4}")

    info <- tibble::tibble(
      link = links,
      nome = nomes,
      ano = ano
    )
  }
)

options(timeout = 300)
purrr::pwalk(
  infos,
  function(link, nome, ano){
    dir_ano <- stringr::str_glue("{dir_mt}{ano}/")
    if(!dir.exists(dir_ano)){
      dir.create(dir_ano, recursive = T)
    }

    dest <- stringr::str_glue("{dir_ano}{nome}")
    tryCatch(
      {
        download.file(
          url = link,
          destfile = dest,
          #method = "libcurl",
          #headers = c("User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/58.0.3029.110 Safari/537.36"),
          mode = "wb"
        )
        logger::log_info("Arquivo: {nome} do ano {ano} foi baixado com sucesso")
        }, error = function(e) {
          logger::log_info("Arquivo: {nome} do ano {ano} falhou")
      }
    )
  }
)

## PA ---------------------
dir_pa <- stringr::str_glue("{dir}PA/")

url_pa <- "https://www.seplad.pa.gov.br/plano-plurianual-ppa"

html_pa <- rvest::read_html(url_pa)

links_redirecionamento <- html_pa |>
  rvest::html_element("div.elementor-widget-container") |>
  rvest::html_elements("a") |>
  rvest::html_attr("href") |>
  stringr::str_subset("ppa") |> 
  stringr::str_replace("^p", "https://seplad.pa.gov.br/p")

infos <- purrr::map_df(
  links_redirecionamento,
  ~{
    logger::log_info("Extraindo as informações de {.x}")
    link_redirecionado = .x
    if(!stringr::str_detect(link_redirecionado, ".pdf")){
      html <- rvest::read_html(link_redirecionado)

      links <- html |> 
        rvest::html_element("section#action_2") |> 
        rvest::html_elements("a") |> 
        rvest::html_attr("href") |> 
        stringr::str_subset(".pdf") |> 
        stringr::str_unique()

      nomes <- stringr::str_extract(links, "[^/]+(?<=.pdf)")

      ano <- stringr::str_extract(link_redirecionado, "\\d{4}-\\d{4}")

      info <- tibble::tibble(
        link = links,
        nome = nomes,
        ano = ano
      )
    } else{
      nomes <- stringr::str_extract(link_redirecionado, "[^/]+(?<=.pdf)")

      ano <- stringr::str_extract(link_redirecionado, "\\d{4}-\\d{4}")

      info <- tibble::tibble(
        link = link_redirecionado,
        nome = nomes,
        ano = ano
      )
    }
  }
)

options(timeout = 300)
purrr::pwalk(
  infos,
  function(link, nome, ano){
    dir_ano <- stringr::str_glue("{dir_pa}{ano}/")
    if(!dir.exists(dir_ano)){
      dir.create(dir_ano, recursive = T)
    }

    dest <- stringr::str_glue("{dir_ano}{nome}")
    tryCatch(
      {
        download.file(
          url = link,
          destfile = dest,
          method = "libcurl",
          headers = c("User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/58.0.3029.110 Safari/537.36"),
          mode = "wb"
        )
        logger::log_info("Arquivo: {nome} do ano {ano} foi baixado com sucesso")
        }, error = function(e) {
          logger::log_info("Arquivo: {nome} do ano {ano} falhou")
      }
    )
  }
)

## PI ----------------------
dir_pi <- stringr::str_glue("{dir}PI/")

urls_pi <- c(
  "2024-2027" = "https://www.seplan.pi.gov.br/download/wpfdcat/93/ppa",
  "2020-2023" = "https://www.seplan.pi.gov.br/download/wpfdcat/242/2020-a-2023",
  "2016-2019" = "https://www.seplan.pi.gov.br/download/wpfdcat/241/2016-a-2019",
  "2012-2015" = "https://www.seplan.pi.gov.br/download/wpfdcat/240/2012-a-2015",
  "2008-2011" = "https://www.seplan.pi.gov.br/download/wpfdcat/238/2008-a-2011",
  "2004-2007" = "https://www.seplan.pi.gov.br/download/wpfdcat/237/2004-a-2007"
)

infos <- tibble::tibble(
  link = c("https://www.seplan.pi.gov.br/download/wpfdcat/93/ppa", "https://www.seplan.pi.gov.br/download/wpfdcat/242/2020-a-2023", "https://www.seplan.pi.gov.br/download/wpfdcat/241/2016-a-2019", "https://www.seplan.pi.gov.br/download/wpfdcat/240/2012-a-2015", "https://www.seplan.pi.gov.br/download/wpfdcat/238/2008-a-2011", "https://www.seplan.pi.gov.br/download/wpfdcat/237/2004-a-2007"),
  nome = c("2024-2027.zip", "2020-2023.zip", "2016-2019.zip", "2012-2015.zip", "2008-2011.zip", "2004-2007.zip"),
  ano = c("2024-2027", "2020-2023", "2016-2019", "2012-2015", "2008-2011", "2004-2007")
)

options(timeout = 300)
purrr::pwalk(
  infos,
  function(link, nome, ano){
    dir_ano <- stringr::str_glue("{dir_pi}{ano}/")
    if(!dir.exists(dir_ano)){
      dir.create(dir_ano, recursive = T)
    }

    dest <- stringr::str_glue("{dir_ano}{nome}")
    tryCatch(
      {
        download.file(
          url = link,
          destfile = dest,
          #method = "libcurl",
          #headers = c("User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/58.0.3029.110 Safari/537.36"),
          mode = "wb"
        )
        logger::log_info("Arquivo: {nome} do ano {ano} foi baixado com sucesso")
        }, error = function(e) {
          logger::log_info("Arquivo: {nome} do ano {ano} falhou")
      }
    )

    zip::unzip(
      zipfile = dest,
      files = NULL,
      exdir = dir_ano
    )
  }
)

## PR ----------
dir_pr <- stringr::str_glue("{dir}PR/")

url_pr <- "https://www.planejamento.pr.gov.br/PlanejaParana/Pagina/Transparencia-Planos-Plurianuais"

html_pr <- rvest::read_html(url_pr)

links <- html_pr |>
  rvest::html_element(".field.field--name-field-texto.field--type-text-long.field--label-hidden.field--item") |> 
  rvest::html_elements("a") |> 
  rvest::html_attr("href") |> 
  stringr::str_subset("\\.pdf") 

nomes <- html_pr |>
  rvest::html_element(".field.field--name-field-texto.field--type-text-long.field--label-hidden.field--item") |> 
  rvest::html_elements("a") |> 
  rvest::html_text2() |> 
  stringr::str_remove_all("[:blank:]") |> 
  stringr::str_remove_all("\\/") |> 
  stringr::str_c(".pdf")

infos <- tibble::tibble(
  link = links,
  nome = nomes
) |> 
  dplyr::mutate(
    ano = dplyr::case_when(
      stringr::str_detect(nome, "2024|2025|2026|2027") ~ "2024-2027",
      stringr::str_detect(nome, "2020|2021|2022|2023") ~ "2020-2023",
      stringr::str_detect(nome, "2016|2017|2018|2019") ~ "2016-2019",
      stringr::str_detect(nome, "2012|2013|2014|2015") ~ "2012-2015",
      stringr::str_detect(nome, "2008|2009|2010|2011") ~ "2008-2011",
      stringr::str_detect(nome, "2004|2005|2006|2007") ~ "2004-2007",
      stringr::str_detect(nome, "Subsídios") ~ "2008-2011"
    )
  ) |> 
  dplyr::distinct()

options(timeout = 300)
purrr::pwalk(
  infos,
  function(link, nome, ano){
    dir_ano <- stringr::str_glue("{dir_pr}{ano}/")
    if(!dir.exists(dir_ano)){
      dir.create(dir_ano, recursive = T)
    }

    dest <- stringr::str_glue("{dir_ano}{nome}")
    tryCatch(
      {
        download.file(
          url = link,
          destfile = dest,
          #method = "libcurl",
          #headers = c("User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/58.0.3029.110 Safari/537.36"),
          mode = "wb"
        )
        logger::log_info("Arquivo: {nome} do ano {ano} foi baixado com sucesso")
        }, error = function(e) {
          logger::log_info("Arquivo: {nome} do ano {ano} falhou")
      }
    )
  }
)

# LOA ------------------------

dir <- "data-raw/LOA/"

## AC -------------
dir_ac <- stringr::str_glue("{dir}AC/")
url_ac <- "https://seplan.ac.gov.br/lei-orcamentaria-anual-loa/"


## Iniciar o driver com o perfil do Firefox
rs_driver_object <- RSelenium::rsDriver(
  browser = "firefox",
  verbose = FALSE,
  phantomver = NULL
)
## Abrir o webdriver
remDr <- rs_driver_object$client

## Fechando a primeira janela
remDr$close()

## Iniciar a Navegação Remota
remDr$open()
## Navegando até o site
remDr$navigate(url_ac)

Sys.sleep(5)

a <- remDr$findElements(
  using = "css selector",
  "a"
)

links <- purrr::map_chr(
  seq(1, length(a)),
  ~{
    a[[.x]]$getElementAttribute("href") |> unlist()
  }
) |> 
  stringr::str_subset("pdf") |> 
  stringr::str_unique()
rs_driver_object$server$stop()
purrr::walk(
  links,
  ~{
    nome <- stringr::str_extract(.x, "[^/]+(?<=.pdf)")
    ano <- stringr::str_extract(nome, "\\d{4}")
    dir_ano <- stringr::str_glue("{dir_ac}{ano}/")
    if(!dir.exists(dir_ano)){
      dir.create(dir_ano, recursive = T)
    }
    
    dest <- stringr::str_glue("{dir_ano}{nome}")

    download.file(
      url = .x,
      destfile = dest,
      mode = "wb"
    )
    
  }
)

## AM ---------------
dir_am <- stringr::str_glue("{dir}AM/")

url_am <- "https://www.tjam.jus.br/index.php/transparencia/gestao-orcamentaria/lei-orcamentaria-anual-e-qdd?own=0"

html_am <- rvest::read_html(url_am)

links_redirecionamento <- html_am |> 
  rvest::html_elements("span.whitespace_preserver") |> 
  rvest::html_elements("a") |> 
  rvest::html_attr("href") %>%
  stringr::str_c("https://www.tjam.jus.br", .)

infos <- purrr::map_df(
  links_redirecionamento,
  ~{
    link_redirecionado = .x 

    html <- rvest::read_html(link_redirecionado)

    links <- html |> 
      rvest::html_elements("a") |> 
      rvest::html_attr("href") |> 
      stringr::str_subset(".pdf|.xlsx")

    nomes <- links |> 
      stringr::str_extract(
        "[^/]+(?<=.pdf|.xlsx)"
      )

    ano <- stringr::str_extract(link_redirecionado, "\\d{4}")

    info <- tibble::tibble(
      link = links,
      nome = nomes,
      ano = ano
    ) |> 
      dplyr::mutate(
        nome = stringr::str_remove_all(nome, "\\W"),
        nome = stringr::str_replace(nome, "pdf", ".pdf"),
        nome = stringr::str_replace(nome, "xlsx", ".xlsx"),
      )
  }
) |> 
  dplyr::distinct()

purrr::pwalk(
  infos,
  function(link, nome, ano){
    dir_ano <- stringr::str_glue("{dir_am}{ano}/")
    if(!dir.exists(dir_ano)){
      dir.create(dir_ano, recursive = T)
    }

    dest <- stringr::str_glue("{dir_ano}{nome}")

    download.file(
      url = link,
      destfile = dest,
      mode = "wb"
    )
  }
)


