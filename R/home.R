# Home page.
#
# Redesigned to match featureMSEA's actual homepage (dark navy navbar +
# teal gradient hero banner + hexagon logo on the right + two buttons).
# "Get Started" now routes into the lipidAnalysis tab (the 3-step
# accordion workflow), not directly into a specific step - matches the new
# single-tab, step-by-step architecture.

home_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::div(
    class = "lfs-home",

    # ---- hero banner, styled after featureMSEA's ----
    shiny::div(
      class = "lfs-hero-band",
      shiny::div(
        class = "lfs-hero-inner",
        shiny::div(
          class = "lfs-hero-text",
          shiny::h1("LipidFlow"),
          shiny::p(class = "lfs-hero-tagline", "Web-based untargeted lipidomics, end to end"),
          shiny::p(class = "lfs-hero-desc",
            "LipidFlow takes raw LC-MS files through peak picking, lipid class/species ",
            "annotation, and internal-standard-based quantification in one guided, ",
            "browser-based workflow - no LipidSearch export or command-line step required."),
          shiny::div(
            class = "lfs-hero-actions",
            shiny::actionButton(ns("get_started"), shiny::HTML("&#9654; Get Started"), class = "btn lfs-btn-hero-primary"),
            shiny::tags$a(href = "https://github.com/jaspershen/lipidflow", target = "_blank",
                          class = "btn lfs-btn-hero-outline", shiny::HTML("&#128214; Help documents"))
          )
        ),
        shiny::div(class = "lfs-hero-logo", shiny::tags$img(src = "www/logo.png", alt = "LipidFlow"))
      )
    ),

    # ---- overview / how it works ----
    shiny::div(
      class = "lfs-overview",
      shiny::h2("How it works"),
      shiny::div(
        class = "lfs-step-row",
        shiny::div(class = "lfs-mini-card",
                   shiny::span(class = "lfs-mini-num", "1"),
                   shiny::h4("Peak picking"),
                   shiny::p("Upload raw mzXML files once - LipidFlow detects features across all samples.")),
        shiny::div(class = "lfs-mini-card",
                   shiny::span(class = "lfs-mini-num", "2"),
                   shiny::h4("Lipid annotation"),
                   shiny::p("Features carry straight over - pick a spectral library and annotate.")),
        shiny::div(class = "lfs-mini-card",
                   shiny::span(class = "lfs-mini-num", "3"),
                   shiny::h4("Quantification"),
                   shiny::p("Annotated features carry over - pick internal standards and quantify."))
      ),
      shiny::div(
        class = "lfs-note",
        shiny::h4("Before you start"),
        shiny::tags$ul(
          shiny::tags$li("Raw files must already be mzXML (e.g. via ProteoWizard / msConvert)."),
          shiny::tags$li("All 3 steps live in one place - open ", shiny::tags$strong("lipidAnalysis"), " and work through them in order.")
        )
      )
    ),

    shiny::div(
      class = "lfs-footer",
      shiny::p(
        "Built on ", shiny::tags$a(href = "https://github.com/jaspershen/lipidflow", target = "_blank", "jaspershen/lipidflow"),
        " and ", shiny::tags$a(href = "https://github.com/tidymass/massprocesser", target = "_blank", "massprocesser"), "."
      )
    )
  )
}

home_server <- function(id, parent_session, parent_nav_id) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observeEvent(input$get_started, {
      bslib::nav_select(id = parent_nav_id, selected = "lipidAnalysis", session = parent_session)
    })
  })
}

