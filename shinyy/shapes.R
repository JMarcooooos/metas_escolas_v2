library(tidyverse)
library(geobr)
library(sf)
library(readxl)

# 1. Carregar seus dados escolares já tratados (o RDS que criamos antes)
# Se não tiver o RDS, carregue o excel e faça o bind_rows como antes
dados_escolas <- readRDS("shinyy/dados_para_o_mapa.rds")

# 2. Criar uma tabela "De-Para": Município -> Regional
tabela_regionais <- dados_escolas %>%
  distinct(NM_MUNICIPIO, NM_REGIONAL) %>%
  mutate(
    # Criar chave de join sem acentos e maiúscula para evitar erros (ex: "Goiania" != "Goiânia")
    chave_join = stringi::stri_trans_general(str = toupper(NM_MUNICIPIO), id = "Latin-ASCII")
  )

# 3. Baixar malha de Municípios e Estado (IBGE)
message("Baixando mapas do IBGE...")
shape_estado <- read_state(code_state = "GO", year = 2020, showProgress = FALSE)
shape_munis  <- read_municipality(code_muni = "GO", year = 2020, showProgress = FALSE)

# 4. Cruzar Municípios com Regionais
shape_munis_enrich <- shape_munis %>%
  mutate(
    chave_join = stringi::stri_trans_general(str = toupper(name_muni), id = "Latin-ASCII")
  ) %>%
  left_join(tabela_regionais, by = "chave_join") %>%
  # Remover municípios que não tenham correspondência na sua base (opcional)
  filter(!is.na(NM_REGIONAL))

# 5. Salvar tudo num arquivo só (Lista com Estado e Municípios)
objetos_mapa <- list(
  estado = shape_estado,
  municipios = shape_munis_enrich
)

saveRDS(objetos_mapa, "shinyy/mapa_shapes.rds")

message("Arquivo 'mapa_shapes.rds' criado com sucesso!")