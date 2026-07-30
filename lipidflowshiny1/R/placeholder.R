# Generic placeholder module for tabs that aren't built yet.
#
# Swap this out module-by-module as Peak Picking / Annotation get real
# implementations - see README.md for the suggested build order and where
# each future module would plug into app_ui.R / app_server.R.

placeholder_ui <- function(id, title, planned_approach) {
  ns <- shiny::NS(id)
  shiny::div(
    class = "lfs-placeholder",
    shiny::h2(title),
    shiny::span(class = "lfs-badge lfs-badge-muted", "In development"),
    shiny::p(class = "lfs-placeholder-text", planned_approach),
    shiny::p(shiny::tags$em("This tab is a stub - no inputs are wired up yet."))
  )
  
}
placeholder_ui <- function(id, title, description) {
  ns <- shiny::NS(id)
  shiny::div(
    style = "padding: 20px;",
    shiny::h3(title),
    shiny::p(description)
  )
}

placeholder_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    # nothing to do yet
  })
}
