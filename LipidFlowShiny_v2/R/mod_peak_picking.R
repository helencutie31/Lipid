# Step 1: Peak Picking - parameters + Run only.
#
# All file/path inputs now live in Data Import (see data_import.R) and
# arrive here via pipeline_state (pp_mode, pp_data_source, pp_files_pos/neg,
# pp_server_path, pp_existing_pos/neg). One Run button handles both cases:
# if Data Import's Option B was chosen, Run loads the existing result;
# otherwise it launches a fresh massprocesser job.
#
# Real backend: massprocesser::process_data(), run via a callr background
# job. See utils_peak_picking.R for .lfs_finalize_peak_picking().

mod_peak_picking_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::div(
    class = "lfs-step-body",
    shiny::uiOutput(ns("mode_summary")),
    
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
        shiny::column(6, shiny::numericInput(ns("threads"), "Threads", value = 1, min = 1)),
        shiny::column(6, shiny::numericInput(ns("min_fraction"), "Min. fraction of samples", value = 0.5, min = 0, max = 1, step = 0.05))
      )
    )),
    
    shiny::div(class = "lfs-run-row",
               shiny::actionButton(ns("run_btn"), "Run", class = "btn lfs-btn-primary")),
    
    shiny::div(class = "lfs-status-row", shiny::uiOutput(ns("status_badge"))),
    shiny::tags$pre(class = "lfs-log", shiny::textOutput(ns("log"))),
    
    shiny::conditionalPanel(
      condition = "output.has_result == true", ns = ns,
      shiny::selectInput(ns("table_mode"), NULL, choices = c("POS", "NEG"), selected = "POS", width = "150px"),
      bslib::navset_tab(
        bslib::nav_panel("Peak table",
                         shiny::downloadButton(ns("download_table"), "Download CSV"),
                         DT::DTOutput(ns("result_table"))),
        bslib::nav_panel("QC plots", shiny::uiOutput(ns("qc_plot_links")))
      )
    )
  )
}

mod_peak_picking_server <- function(id, pipeline_state) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    rv <- shiny::reactiveValues(status = "idle", process = NULL, log_path = NULL, log_text = "")
    
    shiny::observeEvent(input$toggle_advanced, { shinyjs::toggle("advanced_panel") })
    
    output$mode_summary <- shiny::renderUI({
      if (identical(pipeline_state$pp_mode, "existing")) {
        return(shiny::p(class = "lfs-hint",
                        "Data Import is set to Option B - Run will load the existing result files, no parameters needed."))
      }
      shiny::p(class = "lfs-hint",
               "Data Import is set to Option A - Run will process raw files with the parameters below.")
    })
    
    shiny::observeEvent(input$run_btn, {
      
      if (identical(pipeline_state$pp_mode, "existing")) {
        shiny::req(pipeline_state$pp_existing_pos, pipeline_state$pp_existing_neg)
        
        load_and_merge <- function(pos_path, neg_path) {
          load_obj <- function(path) { e <- new.env(); load(path, envir = e); get(ls(e)[1], envir = e) }
          cat("Loading existing result...\n")
          md_pos <- load_obj(pos_path)
          md_neg <- load_obj(neg_path)
          list(md_pos = md_pos, md_neg = md_neg)
        }
        
        log_path <- tempfile(fileext = ".log")
        file.create(log_path)
        rv$process <- .lfs_run_bg_job(
          load_and_merge,
          list(pos_path = pipeline_state$pp_existing_pos$datapath, neg_path = pipeline_state$pp_existing_neg$datapath),
          log_path
        )
        rv$log_path <- log_path
        rv$log_text <- "Loading existing result...\n"
        rv$work_dir <- NULL
        rv$status <- "running"
        pipeline_state$busy <- TRUE
        pipeline_state$busy_title <- "Running Step 1: Peak Picking"
        pipeline_state$busy_desc <- "Loading existing result..."
        return()
      }
      
      if (identical(pipeline_state$pp_data_source, "upload")) {
        shiny::req(pipeline_state$pp_files_pos, pipeline_state$pp_files_neg)
        work_dir <- tempfile("lfs_pp_")
        dir.create(work_dir)
        .lfs_organize_uploads(pipeline_state$pp_files_pos, work_dir, "POS")
        .lfs_organize_uploads(pipeline_state$pp_files_neg, work_dir, "NEG")
      } else {
        shiny::req(pipeline_state$pp_server_path, nzchar(pipeline_state$pp_server_path))
        shiny::req(dir.exists(pipeline_state$pp_server_path))
        work_dir <- pipeline_state$pp_server_path
      }
      
      common_args <- list(
        ppm = input$ppm,
        peakwidth = c(input$peakwidth_min, input$peakwidth_max),
        snthresh = input$snthresh,
        prefilter = c(3, 500),
        fitgauss = FALSE,
        integrate = 2,
        mzdiff = 0.01,
        noise = input$noise,
        threads = input$threads,
        binSize = 0.025,
        bw = 5,
        output_tic = TRUE,
        output_bpc = TRUE,
        output_rt_correction_plot = TRUE,
        min_fraction = input$min_fraction,
        fill_peaks = FALSE,
        group_for_figure = "QC",
        detect_peak_algorithm = input$detect_peak_algorithm
      )
      pos_args <- c(list(path = file.path(work_dir, "POS"), polarity = "positive"), common_args)
      neg_args <- c(list(path = file.path(work_dir, "NEG"), polarity = "negative"), common_args)
      
      log_path <- tempfile(fileext = ".log")
      file.create(log_path)
      
      run_both <- function(pos_args, neg_args) {
        cat("=== POS ===\n"); do.call(massprocesser::process_data, pos_args)
        cat("\n=== NEG ===\n"); do.call(massprocesser::process_data, neg_args)
        
        cat("\n=== Loading POS/NEG results ===\n")
        load_obj <- function(path) { e <- new.env(); load(path, envir = e); get(ls(e)[1], envir = e) }
        md_pos <- load_obj(file.path(pos_args$path, "Result", "object"))
        md_neg <- load_obj(file.path(neg_args$path, "Result", "object"))
        list(md_pos = md_pos, md_neg = md_neg)
      }
      
      rv$process <- .lfs_run_bg_job(run_both, list(pos_args = pos_args, neg_args = neg_args), log_path)
      rv$log_path <- log_path
      rv$log_text <- "Starting...\n"
      rv$work_dir <- work_dir
      rv$status <- "running"
      pipeline_state$busy <- TRUE
      pipeline_state$busy_title <- "Running Step 1: Peak Picking"
      pipeline_state$busy_desc <- "Processing raw files..."
    })
    
    poll <- shiny::reactiveTimer(1500)
    shiny::observe({
      poll()
      shiny::req(identical(rv$status, "running"), !is.null(rv$log_path))
      if (file.exists(rv$log_path)) {
        rv$log_text <- paste(readLines(rv$log_path, warn = FALSE), collapse = "\n")
        log_lines <- readLines(rv$log_path, warn = FALSE)
        pipeline_state$busy_status_text <- if (length(log_lines) > 0) utils::tail(log_lines, 1) else "Initializing..."
      }
      if (!is.null(rv$process) && !rv$process$is_alive()) {
        result <- tryCatch(rv$process$get_result(), error = function(e) NULL)
        if (!is.null(result)) {
          pipeline_state$mass_dataset_pos <- result$md_pos
          pipeline_state$mass_dataset_neg <- result$md_neg
          pipeline_state$pp_summary <- data.frame(
            mode = c("POS", "NEG"),
            n_features = c(tryCatch(nrow(result$md_pos@variable_info), error = function(e) NA),
                           tryCatch(nrow(result$md_neg@variable_info), error = function(e) NA)),
            n_samples = c(tryCatch(nrow(result$md_pos@sample_info), error = function(e) NA),
                          tryCatch(nrow(result$md_neg@sample_info), error = function(e) NA))
          )
          pipeline_state$pp_done <- TRUE
          rv$status <- "done"
          pipeline_state$busy <- FALSE
        } else {
          rv$status <- "error"
          pipeline_state$busy <- FALSE
        }
      }
    })
    
    output$status_badge <- shiny::renderUI({
      label <- switch(rv$status, idle = "Idle", running = "Running...", done = "Done", error = "Error", rv$status)
      cls <- switch(rv$status, idle = "lfs-badge-muted", running = "lfs-badge-running",
                    done = "lfs-badge-live", error = "lfs-badge-error", "lfs-badge-muted")
      shiny::span(class = paste("lfs-badge", cls), label)
    })
    output$log <- shiny::renderText({ rv$log_text })
    
    output$has_result <- shiny::reactive({ identical(rv$status, "done") })
    shiny::outputOptions(output, "has_result", suspendWhenHidden = FALSE)
    
    peak_table_current <- shiny::reactive({
      shiny::req(identical(rv$status, "done"), input$table_mode)
      md <- if (identical(input$table_mode, "POS")) pipeline_state$mass_dataset_pos else pipeline_state$mass_dataset_neg
      shiny::req(md)
      .lfs_peak_table(md)
    })
    
    output$result_table <- DT::renderDT({
      DT::datatable(peak_table_current(), options = list(scrollX = TRUE, pageLength = 10))
    })
    
    output$download_table <- shiny::downloadHandler(
      filename = function() paste0("peak_table_", input$table_mode, ".csv"),
      content = function(file) utils::write.csv(peak_table_current(), file, row.names = FALSE)
    )
    
    output$qc_plot_links <- shiny::renderUI({
      shiny::req(identical(rv$status, "done"))
      if (is.null(rv$work_dir)) {
        return(shiny::p(class = "lfs-hint",
                        "QC plots aren't available - this result was loaded via Option B (no linked run folder)."))
      }
      resource_id <- paste0("lfs_pp_", session$token)
      shiny::addResourcePath(resource_id, rv$work_dir)
      
      link_list <- function(mode) {
        result_dir <- file.path(rv$work_dir, mode, "Result")
        pdfs <- list.files(result_dir, pattern = "\\.pdf$", full.names = FALSE)
        if (length(pdfs) == 0) return(shiny::p(class = "lfs-hint", paste(mode, ": no plots were generated.")))
        shiny::tags$ul(lapply(pdfs, function(f) {
          shiny::tags$li(shiny::tags$a(f, href = file.path(resource_id, mode, "Result", f), target = "_blank"))
        }))
      }
      shiny::tagList(shiny::h6("POS"), link_list("POS"), shiny::h6("NEG"), link_list("NEG"))
    })
  })
}