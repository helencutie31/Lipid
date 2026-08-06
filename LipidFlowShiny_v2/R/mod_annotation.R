# Step 2: Lipid Annotation - parameters + Annotate only.

mod_annotation_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::div(
    class = "lfs-step-body",
    shiny::uiOutput(ns("inherited_summary")),
    
    shiny::div(
      class = "lfs-note",
      shiny::strong("Before using this: "),
      "the MS-DIAL library (.msp) needs to be built into a ", shiny::tags$code("metid"),
      " database (.rda) beforehand, offline, and uploaded in Data Import."
    ),
    
    shiny::selectInput(ns("column_type"), "Chromatography column type", choices = c("rp", "hilic"), selected = "rp"),
    
    shiny::actionLink(ns("toggle_advanced"), shiny::HTML("&#9662; Advanced settings")),
    shinyjs::hidden(shiny::div(
      id = ns("advanced_panel"),
      shiny::fluidRow(
        shiny::column(6, shiny::numericInput(ns("ms1_ppm"), "ms1.match.ppm", value = 10, min = 1)),
        shiny::column(6, shiny::numericInput(ns("ms2_ppm"), "ms2.match.ppm", value = 20, min = 1))
      ),
      shiny::fluidRow(
        shiny::column(6, shiny::numericInput(ns("ms2_tol"), "ms2.match.tol", value = 0.02, min = 0, step = 0.01)),
        shiny::column(6, shiny::numericInput(ns("candidate_num"), "candidate.num", value = 3, min = 1))
      ),
      shiny::fluidRow(
        shiny::column(6, shiny::numericInput(ns("ms1_ms2_ppm"), "MS1-MS2 match ppm", value = 10, min = 1)),
        shiny::column(6, shiny::numericInput(ns("ms1_ms2_rt_seconds"), "MS1-MS2 match RT (s)", value = 20, min = 0))
      ),
      shiny::checkboxInput(ns("disable_rt_match"), "Disable RT matching (rt.match.tol = NA)", value = TRUE),
      shiny::conditionalPanel(
        condition = "!input.disable_rt_match", ns = ns,
        shiny::numericInput(ns("rt_tol"), "rt.match.tol (s)", value = 30, min = 0)
      ),
      shiny::numericInput(ns("threads"), "CPU Threads (Parallel Processing)", value = 4, min = 1),
      shiny::p(class = "lfs-hint", style = "color:#2B6E63;",
               "Increase threads (e.g., 4 or 8) to significantly speed up annotation using multi-core processing.")
    )),
    
    shiny::div(class = "lfs-run-row",
               shiny::actionButton(ns("annotate_btn"), "Annotate", class = "btn lfs-btn-primary")),
    
    shiny::div(class = "lfs-status-row", shiny::uiOutput(ns("status_badge"))),
    shiny::tags$pre(class = "lfs-log", shiny::textOutput(ns("log"))),
    
    shiny::uiOutput(ns("result_area"))
  )
}

mod_annotation_server <- function(id, pipeline_state) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    rv <- shiny::reactiveValues(status = "idle", process = NULL, log_path = NULL, log_text = "")
    
    shiny::observeEvent(input$toggle_advanced, { shinyjs::toggle("advanced_panel") })
    
    output$inherited_summary <- shiny::renderUI({
      if (is.null(pipeline_state$mass_dataset_pos) || is.null(pipeline_state$mass_dataset_neg)) {
        return(shiny::p(class = "lfs-hint", "No data yet - complete Peak Picking first."))
      }
      n_pos <- tryCatch(nrow(pipeline_state$mass_dataset_pos@variable_info), error = function(e) NA)
      n_neg <- tryCatch(nrow(pipeline_state$mass_dataset_neg@variable_info), error = function(e) NA)
      shiny::p(class = "lfs-hint",
               sprintf("Received %s POS features, %s NEG features from Peak Picking.", n_pos, n_neg))
    })
    
    shiny::observeEvent(input$annotate_btn, {
      missing <- character()
      if (is.null(pipeline_state$mass_dataset_pos) || is.null(pipeline_state$mass_dataset_neg)) missing <- c(missing, "Peak Picking result")
      if (is.null(pipeline_state$annot_ms2_pos) || is.null(pipeline_state$annot_ms2_neg)) missing <- c(missing, "MS2 files (Data Import)")
      if (is.null(pipeline_state$annot_db_pos_path) || is.null(pipeline_state$annot_db_neg_path)) missing <- c(missing, "spectral database (Data Import)")
      
      if (length(missing) > 0) {
        rv$status <- "error"
        rv$log_text <- paste0("Cannot start - missing: ", paste(missing, collapse = "; "), "\n")
        return()
      }
      
      work_dir <- tempfile("lfs_annot_")
      dir.create(work_dir)
      ms2_pos_dir <- file.path(work_dir, "ms2_pos")
      ms2_neg_dir <- file.path(work_dir, "ms2_neg")
      .lfs_organize_uploads_flat(pipeline_state$annot_ms2_pos, ms2_pos_dir)
      .lfs_organize_uploads_flat(pipeline_state$annot_ms2_neg, ms2_neg_dir)
      
      log_path <- tempfile(fileext = ".log")
      file.create(log_path)
      
      run_annotation <- function(md_pos, md_neg, ms2_pos_dir, ms2_neg_dir,
                                 db_pos_path, db_neg_path, column_type,
                                 ms1_ppm, ms2_ppm, ms2_tol, ms1_ms2_ppm, ms1_ms2_rt_seconds,
                                 rt_tol, candidate_num, threads) {
        
        cat("=== Environment & Parallel Setup ===\n")
        cat("R version:", R.version.string, "\n")
        cat("Threads requested:", threads, "\n")
        
        suppressPackageStartupMessages({
          library(metid)
          library(massdataset)
          library(BiocParallel)
        })
        
        # Safe Multi-threading setup for Windows and Unix
        if (threads > 1) {
          if (.Platform$OS.type == "windows") {
            BiocParallel::register(BiocParallel::SnowParam(workers = threads, progressbar = FALSE))
            cat("Registered Windows SnowParam with", threads, "workers.\n")
          } else {
            BiocParallel::register(BiocParallel::MulticoreParam(workers = threads, progressbar = FALSE))
            cat("Registered MulticoreParam with", threads, "workers.\n")
          }
        }
        
        load_rda_object <- function(path) {
          e <- new.env(); load(path, envir = e); get(ls(e)[1], envir = e)
        }
        
        annotate_one_mode <- function(md, ms2_dir, db_path, polarity) {
          cat("=== ", toupper(polarity), ": attaching MS2 (mutate_ms2) ===\n")
          md2 <- massdataset::mutate_ms2(
            md, column = column_type, polarity = polarity,
            ms1.ms2.match.mz.tol = ms1_ms2_ppm,
            ms1.ms2.match.rt.tol = ms1_ms2_rt_seconds,
            path = ms2_dir
          )
          
          cat("=== ", toupper(polarity), ": loading + validating database ===\n")
          database <- load_rda_object(db_path)
          metid::check_database(database)
          
          cat("=== ", toupper(polarity), ": annotate_metabolites_mass_dataset ===\n")
          metid::annotate_metabolites_mass_dataset(
            object = md2, database = database, polarity = polarity, column = column_type,
            ms1.match.ppm = ms1_ppm, ms2.match.ppm = ms2_ppm, ms2.match.tol = ms2_tol,
            rt.match.tol = rt_tol, candidate.num = candidate_num, threads = threads
          )
        }
        
        md_pos3 <- annotate_one_mode(md_pos, ms2_pos_dir, db_pos_path, "positive")
        md_neg3 <- annotate_one_mode(md_neg, ms2_neg_dir, db_neg_path, "negative")
        
        list(pos = md_pos3, neg = md_neg3)
      }
      
      rv$process <- .lfs_run_bg_job(
        run_annotation,
        list(
          md_pos = pipeline_state$mass_dataset_pos, md_neg = pipeline_state$mass_dataset_neg,
          ms2_pos_dir = ms2_pos_dir, ms2_neg_dir = ms2_neg_dir,
          db_pos_path = pipeline_state$annot_db_pos_path, db_neg_path = pipeline_state$annot_db_neg_path,
          column_type = input$column_type,
          ms1_ppm = input$ms1_ppm, ms2_ppm = input$ms2_ppm, ms2_tol = input$ms2_tol,
          ms1_ms2_ppm = input$ms1_ms2_ppm, ms1_ms2_rt_seconds = input$ms1_ms2_rt_seconds,
          rt_tol = if (isTRUE(input$disable_rt_match)) NA else input$rt_tol,
          candidate_num = input$candidate_num, threads = input$threads
        ),
        log_path
      )
      rv$log_path <- log_path
      rv$log_text <- "Starting parallel annotation job...\n"
      rv$status <- "running"
      pipeline_state$busy <- TRUE
      pipeline_state$busy_title <- "Running Step 2: Lipid Annotation"
      pipeline_state$busy_desc <- "Processing parallel annotation..."
    })
    
    poll <- shiny::reactiveTimer(1500)
    shiny::observe({
      poll()
      shiny::req(identical(rv$status, "running"))
      if (!is.null(rv$log_path) && file.exists(rv$log_path)) {
        rv$log_text <- paste(readLines(rv$log_path, warn = FALSE), collapse = "\n")
        log_lines <- readLines(rv$log_path, warn = FALSE)
        pipeline_state$busy_status_text <- if (length(log_lines) > 0) utils::tail(log_lines, 1) else "Initializing..."
      }
      if (!is.null(rv$process) && !rv$process$is_alive()) {
        result <- tryCatch(rv$process$get_result(), error = function(e) NULL)
        if (!is.null(result)) {
          pipeline_state$annotation_result <- result
          pipeline_state$annotation_done <- TRUE
          rv$status <- "done"
        } else {
          rv$status <- "error"
        }
        pipeline_state$busy <- FALSE
      }
    })
    
    output$status_badge <- shiny::renderUI({
      label <- switch(rv$status, idle = "Idle", running = "Running...", done = "Done", error = "Error", rv$status)
      cls <- switch(rv$status, idle = "lfs-badge-muted", running = "lfs-badge-running",
                    done = "lfs-badge-live", error = "lfs-badge-error", "lfs-badge-muted")
      shiny::span(class = paste("lfs-badge", cls), label)
    })
    output$log <- shiny::renderText({ rv$log_text })
    
    output$result_area <- shiny::renderUI({
      shiny::req(pipeline_state$annotation_done)
      DT::DTOutput(ns("result_table"))
    })
    output$result_table <- DT::renderDT({
      shiny::req(pipeline_state$annotation_result)
      tbl <- .lfs_combined_annotation_table(pipeline_state$annotation_result)
      DT::datatable(tbl, options = list(scrollX = TRUE, pageLength = 10))
    })
  })
}