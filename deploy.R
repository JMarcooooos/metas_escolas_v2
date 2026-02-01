# deploy.R
library(rsconnect)
library(shiny)
library(leaflet)
library(bslib)
library(bsicons)
library(shinyWidgets)
library(leaflet.extras)
library(DT)
library(sf)

setAccountInfo(
  name   = Sys.getenv("SHINY_ACC_NAME"),
  token  = Sys.getenv("SHINY_TOKEN"),
  secret = Sys.getenv("SHINY_SECRET")
)

deployApp(
  appDir = "app", 
  appName = "monitoramento-metas-goias", 
  appFiles = c("app.R", "dados_para_o_mapa.rds", "mapa_shapes.rds"),
  forceUpdate = TRUE,
  launch.browser = FALSE
)