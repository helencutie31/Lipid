# Top-level UI: a themed navbar wiring together the 4 tabs.
#
# Design tokens (deliberately not the default Shiny/Bootstrap look):
#   bg      #F5F6F7  cool off-white, closer to a lab notebook page than a
#                     marketing landing page
#   fg      #16202A  near-black ink
#   primary #2B6E63  deep teal - chromatography/glassware, not a generic
#                     "AI product" accent color
#   secondary #C97A2B ochre, used sparingly for the "Available" status badge
#   fonts   Source Serif 4 (headings, editorial/paper feel) + Inter (UI/body)
#           + IBM Plex Mono (numeric values - m/z, RT, ppm read better
#             tabular and monospaced)
# See inst/app/www/custom.css for the rest (cards, badges, checklist, log).

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
    title = "LipidFlow",
    theme = theme,
    header = shiny::tagList(
      shiny::tags$head(shiny::tags$link(rel = "stylesheet", href = "www/custom.css")),
      shinyjs::useShinyjs()
    ),

    bslib::nav_panel("Home", home_ui("home")),

    bslib::nav_panel("Peak Picking", peak_picking_ui("peak_picking")),

    bslib::nav_panel(
      "Lipid Annotation",
      placeholder_ui(
        "annotation", "Lipid Annotation",
        paste(
          "Decided approach: MS2 spectral matching via metid against",
          "MS-DIAL's public lipid MS/MS libraries (81 classes / ~550k spectra",
          "positive e, 94 classes / ~790k spectra negative e), consuming",
          "the mass_dataset object Peak Picking produces directly - no object-",
          "el conversion needed since metid and massprocesser share an",
          "author/ecosystem. Two open items before this is built: (1) features",
          "without triggered MS2 get no annotation this way, so a lightweight",
          "MS1-only fallback is probably still worth adding for full feature",
          "coverage; (2) confirm MS-DIAL's library terms cover reuse inside a",
          "separate redistributed tool, not just use within MS-DIAL itself."
        )
      )
    ),

    bslib::nav_panel("Quantification", quantification_ui("quant")),

    bslib::nav_spacer(),
    bslib::nav_item(
      shiny::tags$a("lipidflow on GitHub", href = "https://github.com/jaspershen/lipidflow",
                     target = "_blank", class = "lfs-nav-link")
    )
  )
}
