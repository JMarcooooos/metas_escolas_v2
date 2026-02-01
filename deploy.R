# deploy.R
library(shiny)
library(leaflet)
library(bslib)
library(bsicons)
library(shinyWidgets)
library(leaflet.extras)
library(DT)
library(sf)

# deploy.R
options(repos = c(CRAN = "https://cloud.r-project.org"))
library(rsconnect)

# Remove cache antigo
unlink("app/rsconnect", recursive = TRUE)

# Força criação de novo manifest
writeManifest(
  appDir = "app",
  appFiles = c("app.R", "dados_para_o_mapa.rds", "mapa_shapes.rds")
)

# Configura conta
setAccountInfo(
  name   = Sys.getenv("SHINY_ACC_NAME"),
  token  = Sys.getenv("SHINY_TOKEN"),
  secret = Sys.getenv("SHINY_SECRET")
)

# Deploy
deployApp(
  appDir = "app",
  appName = "monitoramento-metas-goias",
  forceUpdate = TRUE,
  launch.browser = FALSE
)