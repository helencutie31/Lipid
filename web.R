install.packages(c("usethis", "renv", "BiocManager"))
BiocManager::install(c("xcms", "CAMERA", "MSnbase", "Spectra", "mzR"))
BiocManager::install("tidymass/massdataset")   # shared object class
BiocManager::install("tidymass/masstools")     # low-level MS helpers
BiocManager::install("tidymass/metid")         # reuse for MS1/MS2 matching engine
BiocManager::install("tidymass/metpath")       # reuse for generic enrichment engine
BiocManager::install("tidymass/featuremsea")   # reuse for rank-based enrichment engine
remotes::install_github("lifs-tools/rgoslin")  # lipid shorthand-nomenclature normalizer

usethis::create_package("lipidpick")
renv::init()   # per-package renv, or one renv at the monorepo root

pick_peaks <- function(raw_files,
                       sample_info,
                       polarity = c("positive", "negative"),
                       peakwidth = c(5, 30),
                       ppm = 15,
                       noise = 1000,
                       snthresh = 6) {
  polarity <- match.arg(polarity)
  
  raw_data <- MSnbase::readMSData(raw_files, mode = "onDisk")
  
  cwp <- xcms::CentWaveParam(
    peakwidth = peakwidth, ppm = ppm,
    noise = noise, snthresh = snthresh,
    prefilter = c(3, 500)
  )
  xdata <- xcms::findChromPeaks(raw_data, param = cwp)
  
  xdata <- xcms::adjustRtime(xdata, param = xcms::ObiwarpParam(binSize = 0.6))
  
  pdp <- xcms::PeakDensityParam(
    sampleGroups = sample_info$group,
    minFraction = 0.5, bw = 5
  )
  xdata <- xcms::groupChromPeaks(xdata, param = pdp)
  xdata <- xcms::fillChromPeaks(xdata)
  
  masstools::xcms_object2mass_dataset(
    xdata,
    sample_info = sample_info,
    polarity = polarity
  )
}

build_lipid_database <- function(lipidmaps_export, lipidblast_msp) {
  metid::construct_database(
    path = lipidmaps_export,
    metabolite.info.name = "lipidmaps_ms1.csv",
    source = "LIPID MAPS",
    link = "https://www.lipidmaps.org/resources/rest",
    creater = "your_lab",
    ms1.info = lipidmaps_export,
    ms2.info = lipidblast_msp,
    rt = FALSE
  )
}

annotate_lipids <- function(object, lipid_db, polarity = "positive",
                            ms1.match.ppm = 15, ms2.match.tol = 0.3) {
  annotated <- metid::identify_metabolites(
    object = object,
    ms1.match.ppm = ms1.match.ppm,
    ms2.match.tol = ms2.match.tol,
    candidate.num = 5,
    database = lipid_db,
    threads = 4
  )
  
  ids <- massdataset::extract_annotation_table(annotated)
  ids$lipid_name_canonical <- vapply(
    ids$Compound.name,
    function(x) tryCatch(rgoslin::parseLipidNames(x)$Normalized.Name,
                         error = function(e) NA_character_),
    character(1)
  )
  massdataset::mutate_annotation_table(annotated, ids)
}

quantify_lipids <- function(object, is_map, qc_group = "QC") {
  # is_map: named vector, lipid_class -> internal_standard_feature_id
  expr <- massdataset::extract_expression_data(object)
  ann  <- massdataset::extract_annotation_table(object)
  
  normalized <- expr
  for (cls in names(is_map)) {
    is_row <- expr[is_map[[cls]], ]
    cls_features <- ann$variable_id[ann$lipid_class == cls]
    normalized[cls_features, ] <- sweep(expr[cls_features, , drop = FALSE],
                                        2, as.numeric(is_row), "/")
  }
  
  qc_cols <- massdataset::extract_sample_info(object)$sample_id[
    massdataset::extract_sample_info(object)$group == qc_group
  ]
  order <- seq_along(colnames(normalized))
  corrected <- t(apply(normalized, 1, function(row) {
    fit <- loess(row[qc_cols] ~ order[colnames(normalized) %in% qc_cols])
    row / predict(fit, newdata = order)
  }))
  
  massdataset::mutate_expression_data(object, as.data.frame(corrected))
}
enrich_lipid_sets <- function(ranked_features, lipid_set_db, min_size = 5) {
  # ranked_features: named numeric vector, feature_id -> rank statistic
  # lipid_set_db: list, set_name -> character vector of feature_ids
  #   (build once from lipid_class, LION terms, or LIPID MAPS pathways)
  
  featuremsea::feature_set_enrichment(
    feature_rank = ranked_features,
    pathway_database = lipid_set_db,
    min.pathway.size = min_size,
    nperm = 1000
  )
}

build_lion_set_db <- function(lion_annotation_table) {
  split(lion_annotation_table$feature_id, lion_annotation_table$lion_term)
}

mod_peak_picking_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::card(
    bslib::card_header("Peak picking"),
    shiny::fileInput(ns("raw_files"), "Raw files (.mzML)", multiple = TRUE),
    shiny::numericInput(ns("ppm"), "ppm tolerance", value = 15),
    shiny::actionButton(ns("run"), "Pick peaks", class = "btn-primary"),
    shiny::uiOutput(ns("status"))
  )
}

mod_peak_picking_server <- function(id, mass_dataset_rv) {
  shiny::moduleServer(id, function(input, output, session) {
    pick_task <- shiny::ExtendedTask$new(function(files, ppm) {
      promises::future_promise({
        lipidpick::pick_peaks(files, ppm = ppm)
      })
    }) |> shiny::bind_task_button("run")
    
    shiny::observeEvent(input$run, {
      pick_task$invoke(input$raw_files$datapath, input$ppm)
    })
    
    shiny::observeEvent(pick_task$result(), {
      mass_dataset_rv(pick_task$result())
    })
    
    output$status <- shiny::renderUI({
      if (pick_task$status() == "success") {
        shiny::tags$span(class = "text-success", "Done — object ready for annotation.")
      }
    })
  })
}

ui <- bslib::page_navbar(
  title = "Open Lipidomics",
  theme = bslib::bs_theme(version = 5, primary = "#1f7a6c"),
  bslib::nav_panel("1 · Peak picking", mod_peak_picking_ui("pick")),
  bslib::nav_panel("2 · Annotation",   mod_annotation_ui("annotate")),
  bslib::nav_panel("3 · Quantification", mod_quant_ui("quant")),
  bslib::nav_panel("4 · Functional analysis", mod_enrichment_ui("enrich"))
)

server <- function(input, output, session) {
  mass_dataset_rv <- shiny::reactiveVal(NULL)
  mod_peak_picking_server("pick", mass_dataset_rv)
  mod_annotation_server("annotate", mass_dataset_rv)
  mod_quant_server("quant", mass_dataset_rv)
  mod_enrichment_server("enrich", mass_dataset_rv)
}

shiny::shinyApp(ui, server)

