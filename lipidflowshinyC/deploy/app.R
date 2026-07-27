# This file goes on the server at:
#   /srv/shiny-server/lipidflow-shiny/app.R
# following the exact same pattern as chico-shiny, mapa-shiny, etc.
# (see your earlier "Shiny应用部署域名配置" setup notes).

library(lipidflowshiny)

# Config-file uploads (IS table / annotation table) are small xlsx files.
# Raw mzXML data is NOT uploaded through the browser in this app - see
# README.md, "Why a server path instead of uploading raw data" - so the
# default request size limit only needs headroom for those small files.
options(shiny.maxRequestSize = 50 * 1024^2)

lipidflowshiny::run_lipidflow_shiny()
