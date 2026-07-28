# Step 1: Peak picking.
#
# Simplified from the earlier standalone-tab version: no server path, no
# validation checklist screen - just "upload raw files, click run". Group
# is inferred from filename (see .lfs_infer_group in utils_peak_picking.R).
# Detailed xcms parameters are still here (massprocesser needs them) but
# tucked behind an "Advanced settings" toggle, off by default.
#
# Real backend: massprocesser::process_data(), same as before, run via a
# callr background job so the UI stays responsive.

peak_picking_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::div(
    class = "lfs-step-body",
    shiny::fluidRow(
      shiny::column(6, shiny::fileInput(ns("files_pos"), "Raw files - positive mode",
                                         multiple = TRUE, accept = c(".mzXML", ".mzML"))),
      shiny::column(6, shiny::fileInput(ns("files_neg"), "Raw files - negative mode",
                                         multiple = TRUE, accept = c(".mzXML", ".mzML")))
    ),
    shiny::p(class = "lfs-hint",
             "Sample group is read from the filename (drops a trailing replicate number) - ",
             shiny::tags$code("D25_1.mzXML"), " and ", shiny::tags$code("D25_2.mzXML"),
             " both become group ", shiny::tags$code("D25"), "."),

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
      )
    )),

    shiny::div(class = "lfs-run-row",
               shiny::actionButton(ns("run_btn"), "Run", class = "btn lfs-btn-primary")),

    shiny::div(class = "lfs-status-row", shiny::uiOutput(ns("status_badge"))),
    shiny::tags$pre(class = "lfs-log", shiny::textOutput(ns("log"))),

    shiny::conditionalPanel(
      condition = "output.has_result == true", ns = ns,
      shiny::selectInput(ns("table_mode"), NULL, choices = c("POS", "NEG"), selected = "POS", width = "150px"),
      DT::DTOutput(ns("result_table"))
    )
  )
}

peak_picking_server <- function(id, pipeline_state) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    rv <- shiny::reactiveValues(
      status = "idle", process = NULL, log_path = NULL, log_text = "", work_dir = NULL
    )

    shiny::observeEvent(input$toggle_advanced, {
      shinyjs::toggle("advanced_panel")
    })

    shiny::observeEvent(input$run_btn, {
      shiny::req(input$files_pos, input$files_neg)

      work_dir <- tempfile("lfs_pp_")
      dir.create(work_dir)
      .lfs_organize_uploads(input$files_pos, work_dir, "POS")
      .lfs_organize_uploads(input$files_neg, work_dir, "NEG")

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
      }

      rv$process <- .lfs_run_bg_job(run_both, list(pos_args = pos_args, neg_args = neg_args), log_path)
      rv$log_path <- log_path
      rv$log_text <- "Starting...\n"
      rv$work_dir <- work_dir
      rv$status <- "running"
    })

    poll <- shiny::reactiveTimer(1500)
    shiny::observe({
      poll()
      shiny::req(identical(rv$status, "running"))
      if (!is.null(rv$log_path) && file.exists(rv$log_path)) {
        rv$log_text <- paste(readLines(rv$log_path, warn = FALSE), collapse = "\n")
      }
      if (!is.null(rv$process) && !rv$process$is_alive()) {
        pos_obj <- file.path(rv$work_dir, "POS", "Result", "object")
        neg_obj <- file.path(rv$work_dir, "NEG", "Result", "object")
        if (file.exists(pos_obj) && file.exists(neg_obj)) {
          # Each object file save()s a mass_dataset under its own variable
          # name inside massprocesser - load into an isolated environment
          # and grab whatever landed there, rather than assuming a fixed name.
          load_one <- function(path) {
            e <- new.env()
            load(path, envir = e)
            get(ls(e)[1], envir = e)
          }
          md_pos <- load_one(pos_obj)
          md_neg <- load_one(neg_obj)

          # POS and NEG features can't false-collide here: merge_mass_dataset's
          # default join key is variable_id + mz + rt together, and positive
          # vs negative ions never share both mz and rt by coincidence -
          # confirmed by reading massdataset's own merge_mass_dataset() source
          # rather than assumed. sample_direction="full" keeps every sample
          # even if e.g. it only ran in one mode; variable_direction="full"
          # keeps every feature from both modes (a union, since real matches
          # across modes are essentially impossible on that join key).
          merged <- massdataset::merge_mass_dataset(
            x = md_pos, y = md_neg,
            sample_direction = "full", variable_direction = "full"
          )

          pipeline_state$mass_dataset <- merged
          pipeline_state$pp_summary <- data.frame(
            mode = c("POS", "NEG", "Merged"),
            n_features = c(tryCatch(nrow(md_pos@variable_info), error = function(e) NA),
                            tryCatch(nrow(md_neg@variable_info), error = function(e) NA),
                            tryCatch(nrow(merged@variable_info), error = function(e) NA)),
            n_samples = c(tryCatch(nrow(md_pos@sample_info), error = function(e) NA),
                           tryCatch(nrow(md_neg@sample_info), error = function(e) NA),
                           tryCatch(nrow(merged@sample_info), error = function(e) NA))
          )
          rv$status <- "done"
          pipeline_state$pp_done <- TRUE
        } else {
          rv$status <- "error"
        }
      }
    })

    output$status_badge <- shiny::renderUI({
      label <- switch(rv$status, idle = "Idle", running = "Running\u2026", done = "Done", error = "Error", rv$status)
      cls <- switch(rv$status, idle = "lfs-badge-muted", running = "lfs-badge-running",
                    done = "lfs-badge-live", error = "lfs-badge-error", "lfs-badge-muted")
      shiny::span(class = paste("lfs-badge", cls), label)
    })
    output$log <- shiny::renderText({ rv$log_text })

    output$has_result <- shiny::reactive({ identical(rv$status, "done") })
    shiny::outputOptions(output, "has_result", suspendWhenHidden = FALSE)

    output$result_table <- DT::renderDT({
      shiny::req(identical(rv$status, "done"), input$table_mode)
      f <- file.path(rv$work_dir, input$table_mode, "Result", "Peak_table_for_cleaning.csv")
      shiny::req(file.exists(f))
      DT::datatable(readr::read_csv(f, show_col_types = FALSE), options = list(scrollX = TRUE, pageLength = 10))
    })
  })
}
