library(geobr)
library(sf)

# 1. Baixa Estado e Municípios (Pode demorar um pouco, mas roda local)
shape_estado <- read_state(code_state = "GO", year = 2020)
shape_munis  <- read_municipality(code_muni = "GO", year = 2020)

# 2. Cria uma lista com tudo
mapa_bruto <- list(
  estado = shape_estado,
  municipios = shape_munis
)

# 3. Salva um arquivo .rds para subir no GitHub
saveRDS(mapa_bruto, "shapes_goias_bruto.rds")