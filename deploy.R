# deploy.R
library(rsconnect)

setAccountInfo(
  name   = Sys.getenv("SHINY_ACC_NAME"),
  token  = Sys.getenv("SHINY_TOKEN"),
  secret = Sys.getenv("SHINY_SECRET")
)

deployApp(
  appDir = "app",
  appName = "monitoramento-metas-goias", 
  forceUpdate = TRUE,
  launch.browser = FALSE
)