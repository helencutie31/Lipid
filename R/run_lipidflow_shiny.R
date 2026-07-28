#' Run the LipidFlow Shiny web application
#'
#' @description
#' Launches the LipidFlow web interface. This is the entry point analogous
#' to `chicoshiny::run_chico_shiny()` / `mapashiny::run_mapa_shiny()` in the
#' rest of the lab's Shiny app ecosystem, so the deployment steps you
#' already use (Shiny Server + Nginx + Cloudflare + Certbot) apply
#' unchanged - see deploy/app.R for the two-line wrapper that goes on the
#' server.
#'
#' @param ... Passed on to [shiny::shinyApp()], e.g. `options = list(port =
#'   1234)` for local testing.
#'
#' @export
run_lipidflow_shiny <- function(...) {

  # Static assets (custom.css) ship inside the installed package under
  # inst/app/www, exposed here at the "www/" URL path used in app_ui.R.
  www_dir <- system.file("app/www", package = "lipidflowshiny")
  if (nzchar(www_dir)) {
    shiny::addResourcePath("www", www_dir)
  }

  if (!requireNamespace("lipidflow", quietly = TRUE)) {
    message(
      "Note: the 'lipidflow' package isn't installed. The app will still ",
      "launch, but the Quantification tab will error when you try to run ",
      "it. Install with:\n  remotes::install_github('jaspershen/lipidflow')"
    )
  }
  if (!requireNamespace("massprocesser", quietly = TRUE)) {
    message(
      "Note: the 'massprocesser' package isn't installed. The app will ",
      "still launch, but the Peak Picking tab will error when you try to ",
      "run it. Install with:\n",
      "  remotes::install_github('tidymass/massprocesser')"
    )
  }
  if (!requireNamespace("massdataset", quietly = TRUE)) {
    message(
      "Note: the 'massdataset' package isn't installed. Peak Picking's ",
      "POS+NEG merge step needs it. Install with:\n",
      "  remotes::install_github('tidymass/massdataset')"
    )
  }

  shiny::shinyApp(ui = app_ui(), server = app_server, ...)
}
