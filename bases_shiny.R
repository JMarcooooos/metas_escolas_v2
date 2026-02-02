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
  mutate(`CÓDIGO ESCOLA` = as.double(`CÓDIGO ESCOLA`)) %>%
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


message("--- 3. PROCESSANDO MAPAS (OFFLINE) ---")
caminho_shapes_brutos <- "shapes_goias_bruto.rds"

if (!file.exists(caminho_shapes_brutos)) {
  stop("ERRO CRÍTICO: O arquivo 'shapes_goias_bruto.rds' não foi encontrado na pasta.")
}

mapa_bruto <- readRDS(caminho_shapes_brutos)

shape_estado <- mapa_bruto$estado
shape_munis  <- mapa_bruto$municipios

tabela_regionais <- dados_finais %>%
  distinct(NM_MUNICIPIO, NM_REGIONAL) %>%
  mutate(chave = stringi::stri_trans_general(str = toupper(NM_MUNICIPIO), id = "Latin-ASCII"))

shape_munis_enrich <- shape_munis %>%
  mutate(chave = stringi::stri_trans_general(str = toupper(name_muni), id = "Latin-ASCII")) %>%
  inner_join(tabela_regionais, by = "chave")

objetos_mapa <- list(estado = shape_estado, municipios = shape_munis_enrich)

saveRDS(objetos_mapa, "app/mapa_shapes.rds")
message("Arquivo 'app/mapa_shapes.rds' gerado com sucesso!")