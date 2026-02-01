# processa_tudo.R
library(tidyverse)
library(readxl)
library(geobr)
library(sf)

message("--- 1. LENDO RESULTADOS DO MODELO (Descompactados) ---")
df_ef <- read_excel("resultado_previsao_escolas_ef.xlsx",skip = 5) %>% mutate(ETAPA = "Ensino Fundamental")
df_em <- read_excel("resultado_previsao_escolas_em.xlsx", skip=5) %>% mutate(ETAPA = "Ensino Médio")

dados_modelo <- bind_rows(df_ef, df_em)

message("--- 2. CRUZANDO COM LAT/LONG ---")
dados_latlong <- read_excel("ref_latlong.xlsx") %>%
  select(CD_ESCOLA = `CÓDIGO ESCOLA`, LATITUDE, LONGITUDE) %>%
  mutate(
    LATITUDE = as.numeric(gsub(",", ".", LATITUDE)),
    LONGITUDE = as.numeric(gsub(",", ".", LONGITUDE))
  )

dados_finais <- dados_modelo %>%
  inner_join(dados_latlong, by = "CD_ESCOLA") %>%
  filter(!is.na(LATITUDE), !is.na(LONGITUDE))

saveRDS(dados_finais, "app/dados_para_o_mapa.rds")
message("Arquivo 'dados_para_o_mapa.rds' gerado com sucesso!")

message("--- 3. GERANDO SHAPES (MAPA) ---")
tabela_regionais <- dados_finais %>%
  distinct(NM_MUNICIPIO, NM_REGIONAL) %>%
  mutate(chave_join = stringi::stri_trans_general(str = toupper(NM_MUNICIPIO), id = "Latin-ASCII"))

shape_estado <- read_state(code_state = "GO", year = 2020, showProgress = FALSE)
shape_munis  <- read_municipality(code_muni = "GO", year = 2020, showProgress = FALSE)

shape_munis_enrich <- shape_munis %>%
  mutate(chave_join = stringi::stri_trans_general(str = toupper(name_muni), id = "Latin-ASCII")) %>%
  inner_join(tabela_regionais, by = "chave_join")

objetos_mapa <- list(estado = shape_estado, municipios = shape_munis_enrich)
saveRDS(objetos_mapa, "app/mapa_shapes.rds")
message("Arquivo 'mapa_shapes.rds' gerado com sucesso!")