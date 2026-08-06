# app.R - entry point. Run this file (or shiny::runApp(".")) to launch.

source("global.R")
for (f in list.files("R", pattern = "\\.R$", full.names = TRUE)) source(f)

shiny::shinyApp(ui = app_ui, server = app_server)
