# deploy.R

options(repos = NULL) 
options(repos = c(CRAN = "https://cloud.r-project.org"))

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

message("Iniciando deploy com repositório forçado: ", getOption("repos"))

deployApp(
  appDir = "app", 
  appName = "monitoramento-metas-goias", 
  forceUpdate = TRUE,
  launch.browser = FALSE
)