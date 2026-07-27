# Home page module.
#
# Purely presentational except for the "Open quantification" button, which
# needs to switch the *parent* navbar's selected tab - that's why
# mod_home_server() takes parent_session as an argument (see app_server.R).

# --- small inline line-icons, grounded in what each stage actually looks at:
# a chromatogram trace, a tagged fragment ion, a calibration ladder. Kept as
# plain stroke SVGs (currentColor) so they inherit each card's text color.

.lfs_icon_peak_picking <- function() {
  shiny::HTML('
    <svg viewBox="0 0 48 48" fill="none" stroke="currentColor" stroke-width="1.6"
         stroke-linecap="round" stroke-linejoin="round" aria-hidden="true">
      <path d="M4 36 L14 36 L18 14 L22 30 L26 22 L30 36 L44 36" />
      <line x1="4" y1="40" x2="44" y2="40" stroke-opacity="0.35" />
    </svg>')
}

.lfs_icon_annotation <- function() {
  shiny::HTML('
    <svg viewBox="0 0 48 48" fill="none" stroke="currentColor" stroke-width="1.6"
         stroke-linecap="round" stroke-linejoin="round" aria-hidden="true">
      <path d="M24 6 L36 13 V27 L24 34 L12 27 V13 Z" />
      <circle cx="24" cy="20" r="3.2" />
      <line x1="24" y1="23.2" x2="24" y2="34" stroke-opacity="0.5" />
      <line x1="36" y1="27" x2="43" y2="31" stroke-opacity="0.5" />
      <text x="34" y="43" font-size="9" stroke="none" fill="currentColor">PC 34:1</text>
    </svg>')
}

.lfs_icon_quantification <- function() {
  shiny::HTML('
    <svg viewBox="0 0 48 48" fill="none" stroke="currentColor" stroke-width="1.6"
         stroke-linecap="round" stroke-linejoin="round" aria-hidden="true">
      <line x1="6" y1="42" x2="6" y2="8" />
      <line x1="6" y1="42" x2="44" y2="42" />
      <circle cx="12" cy="34" r="2" fill="currentColor" stroke="none" />
      <circle cx="20" cy="26" r="2" fill="currentColor" stroke="none" />
      <circle cx="28" cy="19" r="2" fill="currentColor" stroke="none" />
      <circle cx="36" cy="12" r="2" fill="currentColor" stroke="none" />
      <path d="M12 34 L20 26 L28 19 L36 12" stroke-opacity="0.6" stroke-dasharray="2 3" />
    </svg>')
}

mod_home_ui <- function(id) {
  ns <- shiny::NS(id)

  status_card <- function(icon, title, tag_label, tag_class, description, button = NULL) {
    shiny::div(
      class = "lfs-card",
      shiny::div(class = "lfs-card-icon", icon),
      shiny::div(class = "lfs-card-title-row",
                 shiny::h3(title),
                 shiny::span(class = paste("lfs-badge", tag_class), tag_label)),
      shiny::p(class = "lfs-card-desc", description),
      button
    )
  }

  shiny::div(
    class = "lfs-home",

    shiny::div(
      class = "lfs-hero",
      shiny::h1("LipidFlow"),
      shiny::p(class = "lfs-hero-sub",
        "A browser-based workflow for untargeted lipidomics: from raw LC-MS ",
        "files to peak-picked features, class/species annotation, and ",
        "internal-standard-based absolute quantification.")
    ),

    shiny::div(
      class = "lfs-step-row",
      status_card(
        .lfs_icon_peak_picking(), "1. Peak picking", "In development", "lfs-badge-muted",
        "Detect and align chromatographic features across samples from raw mzXML files."
      ),
      status_card(
        .lfs_icon_annotation(), "2. Lipid annotation", "In development", "lfs-badge-muted",
        "Assign lipid class and species identities to detected features."
      ),
      status_card(
        .lfs_icon_quantification(), "3. Quantification", "Available", "lfs-badge-live",
        "Internal-standard-based absolute quantification, wrapping lipidflow::get_lipid_absolute_quantification().",
        shiny::actionButton(ns("go_quant"), "Open quantification \u2192", class = "btn lfs-btn-primary")
      )
    ),

    shiny::div(
      class = "lfs-note",
      shiny::h4("Before you start"),
      shiny::tags$ul(
        shiny::tags$li("Raw files must already be converted to mzXML (e.g. via ProteoWizard / msConvert) - this app does not do that conversion step."),
        shiny::tags$li("Quantification expects data already organised on the server as ", shiny::tags$code("path/POS/<group>/*.mzXML"), " and ", shiny::tags$code("path/NEG/<group>/*.mzXML"), ", the same layout the lipidflow R package itself expects."),
        shiny::tags$li("Small config files (internal standard table, lipid annotation table) can be uploaded from your browser on the Quantification tab.")
      )
    ),

    shiny::div(
      class = "lfs-footer",
      shiny::p(
        "Built on ", shiny::tags$a(href = "https://github.com/jaspershen/lipidflow", target = "_blank", "jaspershen/lipidflow"),
        ". If you use this tool, please cite: Shen X, et al. Metabolic Reaction Network-based Recursive ",
        "Metabolite Annotation for Untargeted Metabolomics. ", shiny::tags$em("Nature Communications"), " 2019, 10:1516."
      )
    )
  )
}

mod_home_server <- function(id, parent_session, parent_nav_id) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observeEvent(input$go_quant, {
      bslib::nav_select(id = parent_nav_id, selected = "Quantification", session = parent_session)
    })
  })
}
