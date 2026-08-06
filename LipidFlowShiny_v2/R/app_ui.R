# app_ui.R - outermost UI frame. 

# Home / "Introduction" panel - layout, section order and card language
# deliberately mirror featureMSEA's Introduction tab (see
# ref_featuremsea/index.html: hero gradient row with a light "Get Started"
# button + an outline "Help Documents" button, an "Overview" card below it,
# a row of 3 numbered step cards, then a callout/highlight bar) - only the
# palette, copy and step content are LipidFlow's own. The hero gradient
# (#0F4C44 -> #1C7C6C) is chosen specifically to set off the orange
# (#D3481A) LipidFlow logo mark sitting on top of it.
home_panel_ui <- function() {
  shiny::div(
    class = "lfs-home",

    # ---- hero banner: title/tagline/description + actions (left), logo (right) ----
    # Structure/classes match the "fancy", featureMSEA-grade hero spec:
    # hero-banner > (banner-content > banner-title/subtitle/desc/buttons, banner-logo-container).
    shiny::tags$div(
      class = "hero-banner",
      shiny::tags$div(
        class = "banner-content",
        shiny::tags$h1(class = "banner-title", "LipidFlow"),
        shiny::tags$h3(class = "banner-subtitle", "Web-based untargeted lipidomics, end to end"),
        shiny::tags$p(
          class = "banner-desc",
          "LipidFlow takes raw LC-MS files through internal-standard peak extraction, peak ",
          "picking, lipid class/species annotation, and internal-standard-based quantification ",
          "in one guided, browser-based workflow - no LipidSearch export or command-line step required."
        ),
        shiny::tags$div(
          class = "banner-buttons",
          shiny::actionButton("btn_get_started", " Get Started", icon = shiny::icon("play-circle"), class = "btn-hero-primary"),
          shiny::actionButton("btn_go_help", " Help Documents", icon = shiny::icon("book-open"), class = "btn-hero-secondary")
        )
      ),
      shiny::tags$div(
        class = "banner-logo-container",
        # "www/" prefix required by the explicit addResourcePath("www", ...)
        # set up in global.R (this app isn't a package, so the usual golem/
        # package resource mapping isn't available) - see global.R comment.
        shiny::tags$img(src = "www/logo.png", alt = "LipidFlow Logo")
      )
    ),

    # ---- "Overview of LipidFlow" card ----
    shiny::tags$div(
      class = "overview-card",
      shiny::tags$h3("Overview of LipidFlow"),
      shiny::tags$p(
        "LipidFlow scans a QC injection to lock in the optimal internal-standard adduct and ",
        "measured retention time, detects chromatographic features (peaks) across every raw ",
        "sample with massprocesser/xcms, annotates lipid species/classes against an MS-DIAL-",
        "derived spectral library via metID, and converts matched peak areas into absolute ",
        "concentrations using the internal-standard ratio method - all from one browser session, ",
        "with every intermediate table downloadable along the way."
      )
    ),

    # ---- 3-step overview cards ----
    shiny::div(
      class = "lfs-overview",
      shiny::h2("How it works"),
      shiny::div(
        class = "lfs-step-row",
        shiny::div(class = "lfs-mini-card",
                   shiny::span(class = "lfs-mini-num", style = "background:#2B6E63;", "1"),
                   shiny::h4("Peak picking"),
                   shiny::p("Upload raw mzXML files once - LipidFlow detects features across all samples.")),
        shiny::div(class = "lfs-mini-card",
                   shiny::span(class = "lfs-mini-num", style = "background:#3498DB;", "2"),
                   shiny::h4("Lipid annotation"),
                   shiny::p("Features carry straight over - pick a spectral library and annotate.")),
        shiny::div(class = "lfs-mini-card",
                   shiny::span(class = "lfs-mini-num", style = "background:#D3481A;", "3"),
                   shiny::h4("Quantification"),
                   shiny::p("Annotated features carry over - pick internal standards and quantify."))
      ),

      # ---- highlight/callout bar ----
      shiny::div(
        class = "lfs-note",
        shiny::div(
          class = "lfs-highlight-row",
          shiny::span(class = "lfs-highlight-icon", shiny::HTML("&#128161;")),
          shiny::div(
            shiny::tags$strong("Getting Started: "),
            "Run ", shiny::tags$strong("Small Tools → Peak Extraction"), " first to lock in your ",
            "internal standards, then open ", shiny::tags$strong("LipidAnalysis"),
            " and work through Steps 1-3 in order."
          )
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

small_tools_panel_ui <- function() {
  bslib::layout_sidebar(
    fillable = TRUE,
    sidebar = bslib::sidebar(width = 380, bg = "#F8F9FA", mod_small_tools_ui("smalltools")),
    bslib::card(
      class = "lfs-subcard",
      bslib::card_header("Internal Standard EIC & Optimal Adduct Selection"),
      bslib::card_body(
        shiny::p(class = "lfs-hint",
                 paste("Extract an EIC for each Internal Standard across candidate adducts, inspect peak",
                       "shape/intensity, then confirm the optimal adduct to use as the quantification",
                       "reference in Step 3.")),
        plotly::plotlyOutput(shiny::NS("smalltools", "eic_plot"), height = "420px"),
        shiny::tags$hr(),
        DT::DTOutput(shiny::NS("smalltools", "y_is_opt_table"))
      )
    )
  )
}

lipid_analysis_panel_ui <- function() {
  step_title <- function(icon_char, label) {
    shiny::tagList(shiny::span(class = "lfs-step-num", icon_char), label)
  }
  
  shiny::tagList(
    # --- Busy overlay remains unchanged ---
    shinyjs::hidden(shiny::div(
      id = "busy_overlay", class = "lfs-busy-overlay",
      shiny::div(
        class = "lfs-busy-card",
        shiny::h4(shiny::uiOutput("busy_title", inline = TRUE)),
        shiny::div(class = "lfs-spinner"),
        shiny::p(class = "lfs-busy-desc", shiny::uiOutput("busy_desc", inline = TRUE)),
        shiny::p(class = "lfs-busy-warn", "This may take several minutes. Please do not close the browser or navigate away."),
        shiny::p(class = "lfs-busy-status", shiny::uiOutput("busy_status", inline = TRUE))
      )
    )),
    
    # --- Horizontal Tabs Layout for Steps ---
    bslib::navset_tab(
      id = "lipid_steps_tabs",
      
      # 1. Data Import Tab
      bslib::nav_panel(
        title = step_title("\U0001F4E5", "Data Import"), 
        value = "data_import",
        bslib::layout_sidebar(
          fillable = TRUE,
          sidebar = bslib::sidebar(width = 340, bg = "#F8F9FA", mod_import_ui("import")),
          bslib::card(
            class = "lfs-subcard",
            bslib::card_header("Import Summary & Visualization"),
            bslib::card_body(
              shiny::p(class = "lfs-hint", "Data summaries and initial visualizations will appear here.")
            )
          )
        )
      ),
      
      # 2. Peak Picking Tab
      bslib::nav_panel(
        title = step_title("\u2460", "Step 1: Peak Picking"), 
        value = "step1",
        bslib::layout_sidebar(
          fillable = TRUE,
          sidebar = bslib::sidebar(width = 340, bg = "#F8F9FA", mod_peak_picking_ui("pp")),
          bslib::card(
            class = "lfs-subcard",
            bslib::card_header("Peak Picking Visualization"),
            bslib::card_body(
              shiny::p(class = "lfs-hint", "Chromatograms and peak feature plots will appear here.")
            )
          )
        )
      ),
      
      # 3. Lipid Annotation Tab
      bslib::nav_panel(
        title = step_title("\u2461", "Step 2: Lipid Annotation"), 
        value = "step2",
        bslib::layout_sidebar(
          fillable = TRUE,
          sidebar = bslib::sidebar(width = 340, bg = "#F8F9FA", mod_annotation_ui("annot")),
          bslib::card(
            class = "lfs-subcard",
            bslib::card_header("Annotation Visualization"),
            bslib::card_body(
              shiny::p(class = "lfs-hint", "Spectra matching and annotation confidence plots will appear here.")
            )
          )
        )
      ),
      
      # 4. Quantification Tab
      bslib::nav_panel(
        title = step_title("\u2462", "Step 3: Quantification"), 
        value = "step3",
        bslib::layout_sidebar(
          fillable = TRUE,
          sidebar = bslib::sidebar(width = 340, bg = "#F8F9FA", mod_quantification_ui("quant")),
          bslib::card(
            class = "lfs-subcard",
            bslib::card_header("Quantification Visualization"),
            bslib::card_body(
              shiny::p(class = "lfs-hint", "Calibration curves and concentration distributions will appear here.")
            )
          )
        )
      ),
      
      # 5. Save Results Tab
      bslib::nav_panel(
        title = step_title("\U0001F4BE", "Save Results"), 
        value = "save",
        bslib::layout_sidebar(
          fillable = TRUE,
          sidebar = bslib::sidebar(width = 340, bg = "#F8F9FA", mod_save_results_ui("save")),
          bslib::card(
            class = "lfs-results-card",
            bslib::card_header(
              shiny::div(class = "lfs-results-header",
                         shiny::h4("Final Analysis Results", class = "lfs-results-title"),
                         shiny::downloadButton("download_csv", "Download Table (CSV)", class = "btn lfs-btn-outline btn-sm"))
            ),
            bslib::card_body(
              shiny::uiOutput("summary_status"), 
              DT::DTOutput("summary_table")
            )
          )
        )
      )
    )
  )
}

app_ui <- function(request) {
  theme <- bslib::bs_theme(
    version = 5,
    bg = "#F5F6F7",
    fg = "#16202A",
    primary = "#2B6E63",
    secondary = "#C97A2B",
    base_font = bslib::font_google("Plus Jakarta Sans"),
    heading_font = bslib::font_google("Plus Jakarta Sans"),
    code_font = bslib::font_google("IBM Plex Mono")
  )
  
  bslib::page_navbar(
    id = "main_nav",
    # Logo + brand text sized/classed explicitly (app-logo-nav/app-title-nav)
    # so www/custom.css can size them reliably (55px logo, 1.6rem text) in a
    # sticky 85px-tall navbar - these were previously stuck at a barely-
    # visible 34px.
    title = shiny::span(
      shiny::tags$img(src = "www/logo.png", class = "app-logo-nav", alt = "LipidFlow"),
      shiny::tags$span(class = "app-title-nav", "LipidFlow")
    ),
    theme = theme,
    # Warm Cream navbar (#FFF8F5) + dark ink text - flipped from the
    # previous dark-charcoal navbar to a light one, so theme = "light" here
    # (not "dark") is what keeps bslib's own auto-styled elements, e.g. the
    # mobile hamburger toggle icon, dark-on-cream instead of an invisible
    # white-on-white. Accent is orange-red (#C23B22), see www/custom.css
    # "Top navbar v7".
    navbar_options = bslib::navbar_options(bg = "#FFF8F5", theme = "light"),
    header = shiny::tagList(
      shinyjs::useShinyjs(),
      shiny::tags$link(rel = "stylesheet", type = "text/css", href = "www/custom.css")
    ),
    bslib::nav_panel("Home", value = "home", home_panel_ui()),
    # Tab order follows the real usage flow: Small Tools -> Peak Extraction
    # (T0) has to be run before LipidAnalysis's Step 3 (Quantification) can
    # use its Y_IS_opt output, so it's placed right after Home instead of
    # after LipidAnalysis.
    bslib::nav_menu(
      "Small Tools",
      bslib::nav_panel("Peak Extraction", value = "small_tools_peak_extraction", small_tools_panel_ui())
    ),
    # Display label fixed to "LipidAnalysis" (was lowercase "lipidAnalysis" -
    # inconsistent capitalization next to Home/Small Tools/Help Documents).
    # value= is left as the original "lipidAnalysis" - it's an internal,
    # invisible navigation id referenced by app_server.R's nav_select() (the
    # Get Started button) and by the highlight-box copy above; renaming it
    # too would be a functional change for zero visible benefit.
    bslib::nav_panel("LipidAnalysis", value = "lipidAnalysis", lipid_analysis_panel_ui()),
    bslib::nav_panel("Help Documents", value = "help_docs", mod_help_docs_ui("help")),
    # right-aligned lab logo, sized up (55px) so it actually reads at
    # featureMSEA's proportions instead of disappearing into the navbar.
    bslib::nav_spacer(),
    bslib::nav_item(
      shiny::tags$div(
        class = "lab-logo-container",
        shiny::tags$img(src = "www/lab_logo.png", alt = "Shen Lab")
      )
    )
  )
}