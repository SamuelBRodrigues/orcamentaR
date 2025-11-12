# Instalar se ainda não tiver
# install.packages("pagedown")

library(pagedown)
library(stringr)

# Lista com as leis (sua lista)
ldo_rr <- list(
  list(url = "https://sapl.al.rr.leg.br/ta/2041/text?print", ano = 2021, nome = "Lei nº 1.449, de 08/01/2021"),
  list(url = "https://sapl.al.rr.leg.br/ta/1557/text?print", ano = 2020, nome = "Lei nº 1.327, de 31/07/2019"),
  list(url = "https://sapl.al.rr.leg.br/ta/1821/text?print", ano = 2019, nome = "Lei nº 1.280, de 07/08/2018"),
  list(url = "https://sapl.al.rr.leg.br/ta/1797/text?print", ano = 2018, nome = "Lei nº 1.198, de 24/07/2017"),
  list(url = "https://sapl.al.rr.leg.br/ta/2143/text?print", ano = 2017, nome = "Lei nº 1.095, de 11/08/2016"),
  list(url = "https://sapl.al.rr.leg.br/ta/1472/text?print", ano = 2016, nome = "Lei nº 1.005, de 27/07/2015"),
  list(url = "https://sapl.al.rr.leg.br/ta/1319/text?print", ano = 2015, nome = "Lei nº 978, de 08/08/2014"),
  list(url = "https://sapl.al.rr.leg.br/ta/1910/text?print", ano = 2014, nome = "Lei nº 920, de 30/07/2013"),
  list(url = "https://sapl.al.rr.leg.br/ta/2156/text?print", ano = 2013, nome = "Lei nº 865, de 10/08/2012"),
  list(url = "https://sapl.al.rr.leg.br/ta/2159/text?print", ano = 2012, nome = "Lei nº 817, de 01/08/2011"),
  list(url = "https://sapl.al.rr.leg.br/ta/2112/text?print", ano = 2011, nome = "Lei nº 785, de 04/08/2010"),
  list(url = "https://sapl.al.rr.leg.br/ta/1027/text?print", ano = 2010, nome = "Lei nº 735, de 23/07/2009"),
  list(url = "https://sapl.al.rr.leg.br/ta/1171/text?print", ano = 2009, nome = "Lei nº 678, de 05/08/2008"),
  list(url = "https://sapl.al.rr.leg.br/ta/1138/text?print", ano = 2008, nome = "Lei nº 607, de 17/07/2007"),
  list(url = "https://sapl.al.rr.leg.br/ta/1783/text?print", ano = 2007, nome = "Lei nº 557, de 26/07/2006"),
  list(url = "https://sapl.al.rr.leg.br/ta/1722/text?print", ano = 2006, nome = "Lei nº 503, de 04/08/2005"),
  list(url = "https://sapl.al.rr.leg.br/ta/954/text?print", ano = 2005, nome = "Lei nº 459, de 28/07/2004"),
  list(url = "https://sapl.al.rr.leg.br/ta/1625/text?print", ano = 2004, nome = "Lei nº 389, de 24/07/2003"),
  list(url = "https://sapl.al.rr.leg.br/ta/918/text?print", ano = 2003, nome = "Lei nº 339, de 17/07/2002"),
  list(url = "https://sapl.al.rr.leg.br/ta/804/text?print", ano = 2002, nome = "Lei nº 295, de 16/07/2001"),
  list(url = "https://sapl.al.rr.leg.br/ta/829/text?print", ano = 2001, nome = "Lei nº 267, de 02/08/2000"),
  list(url = "https://sapl.al.rr.leg.br/ta/670/text?print", ano = 2000, nome = "Lei nº 226, de 28/07/1999"),
  list(url = "https://sapl.al.rr.leg.br/ta/835/text?print", ano = 1999, nome = "Lei nº 210, de 22/07/1998"),
  list(url = "https://sapl.al.rr.leg.br/ta/527/text?print", ano = 1998, nome = "Lei nº 177, de 31/07/1997"),
  list(url = "https://sapl.al.rr.leg.br/ta/459/text?print", ano = 1997, nome = "Lei nº 19, de 26/07/1996"),
  list(url = "https://sapl.al.rr.leg.br/ta/460/text?print", ano = 1996, nome = "Lei nº 93, de 17/09/1995"),
  list(url = "https://sapl.al.rr.leg.br/ta/490/text?print", ano = 1995, nome = "Lei nº 80, de 12/09/1994"),
  list(url = "https://sapl.al.rr.leg.br/ta/396/text?print", ano = 1994, nome = "Lei nº 52, de 12/11/1993"),
  list(url = "https://sapl.al.rr.leg.br/ta/171/text?print", ano = 1993, nome = "Lei nº 18, de 05/08/1992")
)

# Diretório principal onde salvar tudo
pasta_base <- "LDO-RR"

# Cria o diretório principal, se não existir
if (!dir.exists(pasta_base)) {
  dir.create(pasta_base)
  message("📂 Pasta principal '", pasta_base, "' criada com sucesso!")
} else {
  message("📂 Pasta principal '", pasta_base, "' já existe.")
}

message("🚀 Iniciando o processo de download... Isso pode levar alguns minutos.")
message("---------------------------------------------------------------")

# Itera sobre a lista e baixa cada PDF
for (i in 1:length(ldo_rr)) {
  item <- ldo_rr[[i]]
  ano <- item$ano
  url <- item$url
  nome <- item$nome
  
  message("🔎 Processando item ", i, " de ", length(ldo_rr), ": ", nome, " (", ano, ")")
  
  # Pasta do ano
  pasta_ano <- file.path(pasta_base, ano)
  if (!dir.exists(pasta_ano)) {
    dir.create(pasta_ano)
    message("   -> Criando subpasta para o ano: ", ano)
  }
  
  # Nome do arquivo: remove caracteres especiais e substitui espaços
  nome_limpo <- nome |>
    stringr::str_replace_all("[^[:alnum:][:space:]]", "") |> # Remove não alfanuméricos
    stringr::str_replace_all("\\s+", "_")                   # Substitui espaços por _
  
  caminho_pdf <- file.path(pasta_ano, paste0(nome_limpo, ".pdf"))
  
  message("   📥 Baixando de: ", url)
  
  # Bloco tryCatch para capturar erros de forma mais elegante
  tryCatch({
    # A função chrome_print() pode ser demorada
    pagedown::chrome_print(
      input = url,
      output = caminho_pdf,
      timeout = 90 # Aumentando o tempo limite para 90 segundos
    )
    message("   ✅ Sucesso! Salvo em: ", normalizePath(caminho_pdf))
    
  }, error = function(e) {
    # Mensagem de erro amigável se algo falhar
    message("   ❌ Ops! Algo deu errado ao tentar baixar: ", nome)
    message("      Erro técnico: ", e$message)
  })
  
  # --- NOVIDADE: Pausa aleatória ---
  # Só pausa se não for o último item da lista
  if (i < length(ldo_rr)) {
    tempo_espera <- runif(1, min = 1, max = 3) # Sorteia um tempo entre 1 e 3 seg
    message("   💤 Fazendo uma pequena pausa de ", round(tempo_espera, 1), " segundos para não sobrecarregar o servidor...")
    Sys.sleep(tempo_espera) # Faz o R "dormir"
  }
  
  message("---------------------------------------------------------------")
}

message("🎉 Processo concluído! Todos os downloads foram tentados.")
message("Verifique a pasta '", normalizePath(pasta_base), "' para ver os resultados.")