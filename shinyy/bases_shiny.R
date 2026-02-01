library(tidyverse)
library(readxl)

# 1. Carregar e Limpar as Bases
df_ef <- read_excel("Bases/resultado_previsao_escolas_ef.xlsx", skip=5) %>% 
  mutate(ETAPA = "Ensino Fundamental")
# Remove as linhas de rodapé (ajuste se necessário)
df_ef <- df_ef %>% filter(!is.na(CD_ESCOLA)) 

df_em <- read_excel("Bases/resultado_previsao_escolas_em.xlsx", skip=5) %>% 
  mutate(ETAPA = "Ensino Médio")
df_em <- df_em %>% filter(!is.na(CD_ESCOLA))

# 2. Juntar as duas bases
dados_completos <- bind_rows(df_ef, df_em)

# 3. Latitude e Longitude
dados_latlong <- read_excel("Bases/Detalhes Escolas Quantitativos.xlsx") %>%
  select(`CÓDIGO ESCOLA`, LATITUDE, LONGITUDE) %>%
  mutate(
    # Garante que é numérico (caso o Excel esteja com vírgula ou texto)
    LATITUDE = as.numeric(gsub(",", ".", LATITUDE)),
    LONGITUDE = as.numeric(gsub(",", ".", LONGITUDE))
  )

# 4. Juntando tudo
dados_finais_mapa <- dados_completos %>%
  left_join(dados_latlong, by = c("CD_ESCOLA" = "CÓDIGO ESCOLA")) %>%
  filter(!is.na(LATITUDE), !is.na(LONGITUDE)) # Remove escolas sem coordenada

# 5. SALVAR O ARQUIVO PRONTO
saveRDS(dados_finais_mapa, "shinyy/dados_para_o_mapa.rds")