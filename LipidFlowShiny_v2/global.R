# global.R - load libraries, set app-wide config. Sourced once when the app starts.

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(shinyjs)
  library(DT)
  library(readxl)
  library(readr)
  library(openxlsx)
  library(callr)
  library(zip)
})

# Raw files, MS2 files, and database .rda files can all be large.
options(shiny.maxRequestSize = 10000 * 1024^2)

# All UI code (kept unchanged from the original lipidflowshiny package)
# references static assets as "www/logo.png", "www/custom.css" etc. - that
# convention worked before because the package's run function called
# addResourcePath("www", ...). This app is no longer a package, so the
# same explicit mapping is set up here instead of touching every "www/xxx"
# reference across all the UI files.
shiny::addResourcePath("www", file.path(getwd(), "www"))

# Default path for the bundled MS-DIAL-derived database, if present.
LFS_DEFAULT_DB_DIR <- file.path(getwd(), "data")
