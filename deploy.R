# deploy.R

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

message("--- CORRIGINDO METADADOS DO BSICONS ---")
install.packages("bsicons", repos = "https://cloud.r-project.org")
install.packages("bslib", repos = "https://cloud.r-project.org")

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