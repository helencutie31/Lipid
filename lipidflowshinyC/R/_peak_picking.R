# Peak Picking module.
#
# Wraps massprocesser::process_data(), called once for POS and once for NEG
# in a single background job (see .lfs_run_bg_job in utils_peak_picking.R).
# Reuses the exact path/POS/<group>/*.mzXML convention Quantification
# already uses - confirmed against massprocesser's actual vignette, not
# assumed - so the two tabs can point at the same data folder.

mod_peak_picking_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 380,
      title = "1. Point at your data",

      shiny::textInput(ns("data_path"), "Data folder on the server",
                        placeholder = "/srv/shiny-server/lipidflow-shiny/jobs/my_run"),
      shiny::helpText("Same folder Quantification uses: ", shiny::tags$code("path/POS/<group>/*.mzXML"),
                       " and ", shiny::tags$code("path/NEG/<group>/*.mzXML"),
                       ". Point both tabs at the same folder and this field stays in sync between them."),
      shiny::actionButton(ns("scan_btn"), "Scan folder", class = "btn lfs-btn-secondary"),
      shiny::uiOutput(ns("scan_summary")),
      shiny::selectInput(ns("group_for_figure"), "Group to use for QC plots (TIC/BPC)",
                          choices = c("Scan folder first" = "")),

      shiny::tags$hr(),
      shiny::h6("2. Peak detection (xcms CentWave)"),
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
        shiny::column(6, shiny::numericInput(ns("prefilter_count"), "Prefilter: peaks", value = 3, min = 1)),
        shiny::column(6, shiny::numericInput(ns("prefilter_intensity"), "Prefilter: intensity", value = 500, min = 0))
      ),
      shiny::numericInput(ns("mzdiff"), "mzdiff", value = 0.01, min = 0, step = 0.001),
      shiny::checkboxInput(ns("fitgauss"), "Fit Gaussian to peak shape", value = FALSE),
      shiny::selectInput(ns("integrate"), "Integration method", choices = c("1", "2"), selected = "2"),

      shiny::tags$hr(),
      shiny::h6("3. Alignment / grouping"),
      shiny::fluidRow(
        shiny::column(6, shiny::numericInput(ns("binSize"), "binSize", value = 0.025, min = 0.001, step = 0.001)),
        shiny::column(6, shiny::numericInput(ns("bw"), "bw", value = 5, min = 0))
      ),
      shiny::numericInput(ns("min_fraction"), "Minimum fraction of samples", value = 0.5, min = 0, max = 1, step = 0.05),
      shiny::checkboxInput(ns("fill_peaks"), "Fill missing peaks (NA imputation)", value = FALSE),

      shiny::tags$hr(),
      shiny::h6("4. Output & run"),
      shiny::fluidRow(
        shiny::column(4, shiny::checkboxInput(ns("output_tic"), "TIC", value = TRUE)),
        shiny::column(4, shiny::checkboxInput(ns("output_bpc"), "BPC", value = TRUE)),
        shiny::column(4, shiny::checkboxInput(ns("output_rt_plot"), "RT plot", value = TRUE))
      ),
      shiny::numericInput(ns("threads"), "Threads", value = 6, min = 1),
      shiny::checkboxInput(ns("clear_cache"), "Clear cached intermediate data (force full re-run)", value = FALSE),

      shiny::actionButton(ns("validate_btn"), "Validate structure", class = "btn lfs-btn-secondary"),
      shiny::uiOutput(ns("validate_output")),
      shinyjs::disabled(
        shiny::actionButton(ns("run_btn"), "Run peak picking", class = "btn lfs-btn-primary")
      )
    ),

    shiny::div(class = "lfs-quant-main",
      shiny::div(class = "lfs-status-row", shiny::h4("Status"), shiny::uiOutput(ns("status_badge"))),
      shiny::tags$pre(class = "lfs-log", shiny::textOutput(ns("log"))),

      bslib::navset_tab(
        bslib::nav_panel(
          "Feature table",
          shiny::selectInput(ns("table_mode"), NULL, choices = c("POS", "NEG"), selected = "POS", width = "150px"),
          shiny::downloadButton(ns("download_table"), "Download CSV"),
          DT::DTOutput(ns("result_table"))
        ),
        bslib::nav_panel("QC plots", shiny::uiOutput(ns("qc_plot_links"))),
        bslib::nav_panel(
          "Everything",
          shiny::p("Full Result/ folders (POS and NEG) as produced by massprocesser, including the mass_dataset object the Annotation tab will consume."),
          shiny::downloadButton(ns("download_zip"), "Download both Result/ folders as .zip")
        )
      )
    )
  )
}

mod_peak_picking_server <- function(id, shared_data_path) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    rv <- shiny::reactiveValues(
      groups = character(),
      validation = NULL,
      status = "idle",
      process = NULL,
      log_path = NULL,
      log_text = "",
      data_path = NULL
    )

    # ---- keep data_path in sync with the shared value other tabs use ------
    shiny::observeEvent(input$data_path, {
      if (!identical(input$data_path, shared_data_path())) shared_data_path(input$data_path)
    }, ignoreInit = TRUE)

    shiny::observeEvent(shared_data_path(), {
      if (!identical(input$data_path, shared_data_path())) {
        shiny::updateTextInput(session, "data_path", value = shared_data_path())
      }
    }, ignoreInit = TRUE)

    # ---- scan --------------------------------------------------------------
    shiny::observeEvent(input$scan_btn, {
      shiny::req(input$data_path)
      pos_groups <- .lfs_list_groups(input$data_path, "POS")
      neg_groups <- .lfs_list_groups(input$data_path, "NEG")
      groups <- sort(unique(c(pos_groups, neg_groups)))
      rv$groups <- groups
      shiny::updateSelectInput(session, "group_for_figure", choices = groups,
                                selected = if ("QC" %in% groups) "QC" else if (length(groups) > 0) groups[1] else NULL)
    })

    output$scan_summary <- shiny::renderUI({
      if (length(rv$groups) == 0) {
        shiny::tags$p(class = "lfs-hint", "Click Scan folder to look for POS/ and NEG/ group subfolders.")
      } else {
        shiny::tags$p(class = "lfs-hint", paste0("Found groups: ", paste(rv$groups, collapse = ", ")))
      }
    })

    # ---- validate ------------------------------------------------------------
    shiny::observeEvent(input$validate_btn, {
      shiny::req(input$data_path)
      rv$validation <- .lfs_validate_peak_picking_structure(input$data_path)
      all_ok <- nrow(rv$validation) > 0 && all(rv$validation$status == "ok")
      shinyjs::toggleState("run_btn", condition = all_ok)
    })

    output$validate_output <- shiny::renderUI({
      shiny::req(rv$validation)
      rows <- lapply(seq_len(nrow(rv$validation)), function(i) {
        r <- rv$validation[i, ]
        ok <- identical(r$status, "ok")
        shiny::div(class = paste("lfs-check-row", if (ok) "lfs-check-ok" else "lfs-check-fail"),
                   shiny::span(class = "lfs-check-icon", if (ok) "\u2713" else "\u2717"),
                   shiny::span(class = "lfs-check-label", r$check),
                   shiny::span(class = "lfs-check-msg", r$message))
      })
      shiny::div(class = "lfs-checklist", rows)
    })

    # ---- run ---------------------------------------------------------------
    shiny::observeEvent(input$run_btn, {
      shiny::req(input$data_path, rv$validation)
      shiny::req(all(rv$validation$status == "ok"))

      path <- input$data_path
      if (isTRUE(input$clear_cache)) .lfs_clear_peak_picking_cache(path)

      common_args <- list(
        ppm = input$ppm,
        peakwidth = c(input$peakwidth_min, input$peakwidth_max),
        snthresh = input$snthresh,
        prefilter = c(input$prefilter_count, input$prefilter_intensity),
        fitgauss = isTRUE(input$fitgauss),
        integrate = as.numeric(input$integrate),
        mzdiff = input$mzdiff,
        noise = input$noise,
        threads = input$threads,
        binSize = input$binSize,
        bw = input$bw,
        output_tic = isTRUE(input$output_tic),
        output_bpc = isTRUE(input$output_bpc),
        output_rt_correction_plot = isTRUE(input$output_rt_plot),
        min_fraction = input$min_fraction,
        fill_peaks = isTRUE(input$fill_peaks),
        group_for_figure = if (nzchar(input$group_for_figure)) input$group_for_figure else "QC",
        detect_peak_algorithm = input$detect_peak_algorithm
      )
      pos_args <- c(list(path = file.path(path, "POS"), polarity = "positive"), common_args)
      neg_args <- c(list(path = file.path(path, "NEG"), polarity = "negative"), common_args)

      log_path <- tempfile(fileext = ".log")
      file.create(log_path)

      run_both <- function(pos_args, neg_args) {
        cat("=== POS ===\n")
        do.call(massprocesser::process_data, pos_args)
        cat("\n=== NEG ===\n")
        do.call(massprocesser::process_data, neg_args)
      }

      rv$process <- .lfs_run_bg_job(run_both, list(pos_args = pos_args, neg_args = neg_args), log_path)
      rv$log_path <- log_path
      rv$log_text <- "Starting...\n"
      rv$data_path <- path
      rv$status <- "running"
    })

    # ---- poll ----------------------------------------------------------------
    poll <- shiny::reactiveTimer(1500)
    shiny::observe({
      poll()
      shiny::req(identical(rv$status, "running"))
      if (!is.null(rv$log_path) && file.exists(rv$log_path)) {
        rv$log_text <- paste(readLines(rv$log_path, warn = FALSE), collapse = "\n")
      }
      if (!is.null(rv$process) && !rv$process$is_alive()) {
        done <- file.exists(file.path(rv$data_path, "POS", "Result", "object")) &&
          file.exists(file.path(rv$data_path, "NEG", "Result", "object"))
        rv$status <- if (done) "done" else "error"
      }
    })

    output$status_badge <- shiny::renderUI({
      label <- switch(rv$status, idle = "Idle", running = "Running\u2026", done = "Done", error = "Error", rv$status)
      cls <- switch(rv$status, idle = "lfs-badge-muted", running = "lfs-badge-running",
                    done = "lfs-badge-live", error = "lfs-badge-error", "lfs-badge-muted")
      shiny::span(class = paste("lfs-badge", cls), label)
    })
    output$log <- shiny::renderText({ rv$log_text })

    # ---- results --------------------------------------------------------
    result_table <- shiny::reactive({
      shiny::req(identical(rv$status, "done"), input$table_mode)
      f <- file.path(rv$data_path, input$table_mode, "Result", "Peak_table_for_cleaning.csv")
      shiny::req(file.exists(f))
      readr::read_csv(f, show_col_types = FALSE)
    })

    output$result_table <- DT::renderDT({
      DT::datatable(result_table(), options = list(scrollX = TRUE, pageLength = 15))
    })

    output$download_table <- shiny::downloadHandler(
      filename = function() paste0("peak_table_", input$table_mode, ".csv"),
      content = function(file) utils::write.csv(result_table(), file, row.names = FALSE)
    )

    output$download_zip <- shiny::downloadHandler(
      filename = function() "massprocesser_result.zip",
      content = function(file) {
        shiny::req(identical(rv$status, "done"))
        zip::zip(file, files = c("POS/Result", "NEG/Result"), root = rv$data_path)
      }
    )

    output$qc_plot_links <- shiny::renderUI({
      shiny::req(identical(rv$status, "done"))
      resource_id <- paste0("lfs_pp_", session$token)
      shiny::addResourcePath(resource_id, rv$data_path)

      link_list <- function(mode) {
        result_dir <- file.path(rv$data_path, mode, "Result")
        pdfs <- list.files(result_dir, pattern = "\\.pdf$", full.names = FALSE)
        if (length(pdfs) == 0) return(shiny::p(class = "lfs-hint", paste(mode, ": no plots were generated for this run.")))
        shiny::tags$ul(lapply(pdfs, function(f) {
          shiny::tags$li(shiny::tags$a(f, href = file.path(resource_id, mode, "Result", f), target = "_blank"))
        }))
      }
      shiny::tagList(shiny::h6("POS"), link_list("POS"), shiny::h6("NEG"), link_list("NEG"))
    })
  })
}
