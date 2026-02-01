# deploy.R
options(repos = c(CRAN = "https://cloud.r-project.org"))
library(rsconnect)

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