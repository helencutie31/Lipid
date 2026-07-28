# Top-level UI: dark navbar (logo + title left, ShenLab logo right) matching
# featureMSEA's actual navbar/hero style, 2 top-level tabs (Home,
# lipidAnalysis - the 3-step accordion lives entirely inside the second one).
# Peak Picking / Annotation / Quantification are no longer separate
# top-level tabs - see R/lipid_analysis.R.

app_ui <- function() {
  theme <- bslib::bs_theme(
    version = 5,
    bg = "#F5F6F7",
    fg = "#16202A",
    primary = "#2B6E63",
    secondary = "#C97A2B",
    base_font = bslib::font_google("Inter"),
    heading_font = bslib::font_google("Source Serif 4"),
    code_font = bslib::font_google("IBM Plex Mono")
  )

  bslib::page_navbar(
    id = "main_nav",
    title = shiny::tags$span(
      shiny::tags$img(src = "www/logo.png", height = "32px", style = "margin-right:8px; vertical-align:middle;"),
      "LipidFlow"
    ),
    theme = theme,
    bg = "#16232E",
    inverse = TRUE,
    header = shiny::tagList(
      shiny::tags$head(shiny::tags$link(rel = "stylesheet", href = "www/custom.css")),
      shinyjs::useShinyjs()
    ),

    bslib::nav_panel("Home", value = "home", home_ui("home")),
    bslib::nav_panel("lipidAnalysis", value = "lipidAnalysis", lipid_analysis_ui("analysis")),

    bslib::nav_spacer(),
    bslib::nav_item(shiny::tags$img(src = "www/lab_logo.png", height = "26px", style = "margin-top:11px;"))
  )
}
