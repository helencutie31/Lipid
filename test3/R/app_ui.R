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
      shiny::tags$img(src = "www/logo_lipidflow.png", height = "32px",
                      style = "margin-right:8px; vertical-align:middle;"),
      "LipidFlow"
    ),
    theme = theme,
    bg = "#2b3e50",
    inverse = TRUE,
    header = shiny::tagList(
      shiny::tags$head(shiny::tags$link(rel = "stylesheet", href = "www/custom.css")),
      shinyjs::useShinyjs()
    ),
    
    bslib::nav_panel("Home", value = "home", home_ui("home")),
    bslib::nav_panel("LipidFlow Analysis", value = "lipidAnalysis", lipid_analysis_ui("analysis")),
    bslib::nav_panel("Documentation", value = "documentation",
                     shiny::div(
                       class = "lfs-doc-page",
                       shiny::h2("Documentation"),
                       shiny::p("Full usage notes and package documentation live in the GitHub repository:"),
                       shiny::tags$a(href = "https://github.com/jaspershen-lab/lipidflow", target = "_blank",
                                     "github.com/jaspershen-lab/lipidflow")
                     )
    ),
    
    bslib::nav_spacer(),
    bslib::nav_item(
      shiny::tags$a(href = "https://www.shenlab.org/", target = "_blank",
                    shiny::tags$img(src = "www/logo_shenlab.png", height = "26px",
                                    style = "margin-top:11px;"))
    )
  )
}

# ---------------------------------------------------------------------------
# Home
# ---------------------------------------------------------------------------
home_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::div(
    class = "lfs-home",
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
        shiny::div(class = "lfs-hero-logo", shiny::tags$img(src = "www/logo_lipidflow.png", alt = "LipidFlow"))
      )
    ),
    shiny::div(
      class = "lfs-overview",
      shiny::h2("How it works"),
      shiny::div(
        class = "lfs-step-row",
        shiny::div(class = "lfs-mini-card",
                   shiny::span(class = "lfs-mini-num", "1"),
                   shiny::h4("Peak picking"),
                   shiny::p("Upload raw mzXML/MGF files once in Data Import - LipidFlow detects features across all samples.")),
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
          shiny::tags$li("All steps live in one place - open ", shiny::tags$strong("LipidFlow Analysis"), " and work through the sidebar in order.")
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

# ---------------------------------------------------------------------------
# Lipid analysis (sidebar shell)
# ---------------------------------------------------------------------------
lipid_analysis_ui <- function(id) {
  ns <- shiny::NS(id)
  
  step_title <- function(n, label) {
    shiny::tagList(shiny::span(class = "lfs-step-num", n), label)
  }
  
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 380, bg = "#f8f9fa",
      bslib::accordion(
        id = ns("steps"), multiple = FALSE, open = "data_import",
        bslib::accordion_panel(title = "Data Import", value = "data_import",
                               data_import_ui(ns("import"))),
        bslib::accordion_panel(title = step_title(1, "Peak picking"), value = "step1",
                               peak_picking_controls_ui(ns("pp"))),
        bslib::accordion_panel(title = step_title(2, "Lipid annotation"), value = "step2",
                               annotation_controls_ui(ns("annot"))),
        bslib::accordion_panel(title = step_title(3, "Quantification"), value = "step3",
                               quantification_controls_ui(ns("quant"))),
        bslib::accordion_panel(title = "Save Results", value = "save",
                               save_results_ui(ns("save")))
      )
    ),
    shiny::div(
      class = "lfs-analysis-main",
      shiny::h2("Lipid analysis"),
      shiny::p(class = "lfs-hint", "Work through the steps in the sidebar in order - each one carries its result into the next automatically."),
      peak_picking_results_ui(ns("pp")),
      annotation_results_ui(ns("annot")),
      quantification_results_ui(ns("quant"))
    )
  )
}

# ---------------------------------------------------------------------------
# Data Import
# ---------------------------------------------------------------------------
data_import_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::div(
    class = "lfs-step-body",
    shiny::tabsetPanel(
      id = ns("mode"), type = "tabs",
      shiny::tabPanel(
        "Option A: Raw / feature table", value = "optionA",
        shiny::br(),
        shiny::fluidRow(
          shiny::column(6, shiny::fileInput(ns("mzxml_pos"), "Raw MS1 files - positive mode",
                                            multiple = TRUE, accept = c(".mzXML", ".mzML"))),
          shiny::column(6, shiny::fileInput(ns("mzxml_neg"), "Raw MS1 files - negative mode",
                                            multiple = TRUE, accept = c(".mzXML", ".mzML")))
        ),
        shiny::fluidRow(
          shiny::column(6, shiny::fileInput(ns("mgf_pos"), "MS2 files (MGF) - positive mode",
                                            multiple = TRUE, accept = ".mgf")),
          shiny::column(6, shiny::fileInput(ns("mgf_neg"), "MS2 files (MGF) - negative mode",
                                            multiple = TRUE, accept = ".mgf"))
        ),
        shiny::selectInput(ns("ms2_database"), "MS2 database",
                           choices = c("MS-DIAL (upload .msp below)" = "msdial")),
        shiny::fluidRow(
          shiny::column(6, shiny::fileInput(ns("msp_pos"), "MS-DIAL MS2 library - positive mode (.msp)", accept = ".msp")),
          shiny::column(6, shiny::fileInput(ns("msp_neg"), "MS-DIAL MS2 library - negative mode (.msp)", accept = ".msp"))
        ),
        shiny::fileInput(ns("is_table"), "Internal standard table (.xlsx)", accept = ".xlsx"),
        shiny::actionButton(ns("load_demo_data"), "Load Demo Data", class = "btn lfs-btn-outline"),
        shiny::div(class = "lfs-status-row", shiny::uiOutput(ns("stage_status_a")))
      ),
      shiny::tabPanel(
        "Option B: Existing dataset", value = "optionB",
        shiny::br(),
        shiny::p(class = "lfs-hint", "Upload a .rds produced by this app's Save Results step (or your own, matching format list(mass_dataset_pos=, mass_dataset_neg=, work_dir=))."),
        shiny::fileInput(ns("existing_rds"), "Existing mass_dataset (.rds)", accept = ".rds"),
        shiny::actionButton(ns("load_demo_result"), "Load Demo Result", class = "btn lfs-btn-outline"),
        shiny::div(class = "lfs-status-row", shiny::uiOutput(ns("stage_status_b")))
      )
    )
  )
}

# ---------------------------------------------------------------------------
# Step 1: Peak picking
# ---------------------------------------------------------------------------
peak_picking_controls_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::div(
    class = "lfs-step-body",
    shiny::p(class = "lfs-hint", "Raw files are uploaded in Data Import. Set parameters below and click Run."),
    
    shiny::actionLink(ns("toggle_advanced"), shiny::HTML("&#9662; Advanced settings")),
    shinyjs::hidden(shiny::div(
      id = ns("advanced_panel"),
      shiny::fluidRow(
        shiny::column(6, shiny::numericInput(ns("ppm"), "ppm", value = 15, min = 1)),
        shiny::column(6, shiny::selectInput(ns("detect_peak_algorithm"), "Algorithm",
                                            choices = c("xcms", "massprocesser"), selected = "xcms"))
      ),
      shiny::fluidRow(
        shiny::column(6, shiny::numericInput(ns("peakwidth_min"), "Peak width min (s)", value = 5, min = 0)),
        shiny::column(6, shiny::numericInput(ns("peakwidth_max"), "Peak width max (s)", value = 30, min = 0))
      ),
      shiny::fluidRow(
        shiny::column(6, shiny::numericInput(ns("snthresh"), "S/N threshold", value = 10, min = 0)),
        shiny::column(6, shiny::numericInput(ns("noise"), "Noise cutoff", value = 500, min = 0))
      ),
      shiny::fluidRow(
        shiny::column(6, shiny::numericInput(ns("threads"), "Threads", value = 6, min = 1)),
        shiny::column(6, shiny::numericInput(ns("min_fraction"), "Min. fraction of samples", value = 0.5, min = 0, max = 1, step = 0.05))
      ),
      shiny::fluidRow(
        shiny::column(6, shiny::numericInput(ns("ms2_mz_tol"), "MS1-MS2 match ppm", value = 10, min = 1)),
        shiny::column(6, shiny::numericInput(ns("ms2_rt_tol"), "MS1-MS2 match RT tol. (s)", value = 15, min = 0))
      )
    )),
    
    shiny::div(class = "lfs-run-row", shiny::actionButton(ns("run_btn"), "Run", class = "btn lfs-btn-primary")),
    shiny::div(class = "lfs-status-row", shiny::uiOutput(ns("status_badge")))
  )
}

peak_picking_results_ui <- function(id) {
  ns <- shiny::NS(id)
  
  bslib::card(
    id = ns("card"),
    bslib::card_header(
      class = "d-flex justify-content-between align-items-center",
      "Peak picking",
      shiny::downloadButton(ns("download_csv"), "Download Table (CSV)", class = "btn-sm lfs-btn-outline")
    ),
    bslib::card_body(
      shiny::tags$pre(class = "lfs-log", shiny::textOutput(ns("log"))),
      shiny::conditionalPanel(
        condition = "output.has_result == true", ns = ns,
        shiny::selectInput(ns("table_mode"), NULL, choices = c("POS", "NEG"), selected = "POS", width = "150px"),
        DT::DTOutput(ns("result_table"))
      )
    )
  )
}

# ---------------------------------------------------------------------------
# Step 2: Lipid annotation
# ---------------------------------------------------------------------------
annotation_controls_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::div(
    class = "lfs-step-body",
    shiny::uiOutput(ns("inherited_summary")),
    shiny::p(class = "lfs-hint", "MS2 library files are uploaded in Data Import."),
    
    shiny::actionLink(ns("toggle_advanced"), shiny::HTML("&#9662; Advanced settings")),
    shinyjs::hidden(shiny::div(
      id = ns("advanced_panel"),
      shiny::fluidRow(
        shiny::column(6, shiny::numericInput(ns("ms1_match_ppm"), "MS1 match ppm", value = 25, min = 1)),
        shiny::column(6, shiny::numericInput(ns("ms2_match_tol"), "MS2 match tolerance (Da)", value = 0.5, min = 0.01, step = 0.01))
      ),
      shiny::fluidRow(
        shiny::column(6, shiny::numericInput(ns("candidate_num"), "Candidates per feature", value = 3, min = 1)),
        shiny::column(6, shiny::numericInput(ns("threads"), "Threads", value = 3, min = 1))
      )
    )),
    
    shiny::div(class = "lfs-run-row", shiny::actionButton(ns("annotate_btn"), "Annotate", class = "btn lfs-btn-primary")),
    shiny::div(class = "lfs-status-row", shiny::uiOutput(ns("status_badge")))
  )
}

annotation_results_ui <- function(id) {
  ns <- shiny::NS(id)
  
  bslib::card(
    id = ns("card"),
    bslib::card_header(
      class = "d-flex justify-content-between align-items-center",
      "Lipid annotation",
      shiny::downloadButton(ns("download_csv"), "Download Table (CSV)", class = "btn-sm lfs-btn-outline")
    ),
    bslib::card_body(
      shiny::tags$pre(class = "lfs-log", shiny::textOutput(ns("log"))),
      DT::DTOutput(ns("result_table"))
    )
  )
}

# ---------------------------------------------------------------------------
# Step 3: Quantification
# ---------------------------------------------------------------------------
quantification_controls_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::div(
    class = "lfs-step-body",
    shiny::uiOutput(ns("inherited_summary")),
    shiny::p(class = "lfs-hint", "Internal standard table is uploaded in Data Import."),
    
    shiny::actionLink(ns("toggle_advanced"), shiny::HTML("&#9662; Advanced settings")),
    shinyjs::hidden(shiny::div(
      id = ns("advanced_panel"),
      shiny::fluidRow(
        shiny::column(4, shiny::numericInput(ns("ppm"), "ppm", value = 40, min = 1)),
        shiny::column(4, shiny::numericInput(ns("rt_tolerance"), "RT tolerance (s)", value = 180, min = 0)),
        shiny::column(4, shiny::numericInput(ns("chol_rt"), "Cholesterol RT (s)", value = 1169, min = 0))
      ),
      shiny::fluidRow(
        shiny::column(4, shiny::textInput(ns("rt_confirm_group"), "RT confirm group", value = "QC")),
        shiny::column(4, shiny::numericInput(ns("threads"), "Threads", value = 3, min = 1))
      )
    )),
    
    shiny::div(class = "lfs-run-row", shiny::actionButton(ns("quantify_btn"), "Quantify", class = "btn lfs-btn-primary")),
    shiny::div(class = "lfs-status-row", shiny::uiOutput(ns("status_badge")))
  )
}

quantification_results_ui <- function(id) {
  ns <- shiny::NS(id)
  
  bslib::card(
    id = ns("card"),
    bslib::card_header(
      class = "d-flex justify-content-between align-items-center",
      "Quantification",
      shiny::downloadButton(ns("download_csv"), "Download Table (CSV)", class = "btn-sm lfs-btn-outline")
    ),
    bslib::card_body(
      shiny::tags$pre(class = "lfs-log", shiny::textOutput(ns("log"))),
      DT::DTOutput(ns("result_table"))
    )
  )
}

# ---------------------------------------------------------------------------
# Save Results
# ---------------------------------------------------------------------------
save_results_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::div(
    class = "lfs-step-body",
    shiny::uiOutput(ns("summary")),
    shiny::downloadButton(ns("download_bundle"), "Save Results (.rds)", class = "btn lfs-btn-primary"),
    shiny::downloadButton(ns("download_csv"), "Download Final Table (CSV)", class = "btn lfs-btn-outline")
  )
}