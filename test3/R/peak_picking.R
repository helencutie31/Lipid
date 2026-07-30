# Step 1: Peak picking.
#
# Ngoai peak picking MS1 (massprocesser::process_data(), khong doi), buoc nay
# gio con gan MS2 vao tung mass_dataset qua massdataset::mutate_ms2() - can
# thiet cho buoc Annotation (metid) o Step 2. Vi mutate_ms2()/annotate deu
# can polarity rieng biet, POS/NEG KHONG merge o day nua (merge doi sang cuoi
# Step 2, xem R/annotation.R). work_dir (chua raw mzXML) duoc giu nguyen,
# khong xoa, vi Step 3 can doc lai raw file de trich EIC targeted.

peak_picking_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::div(
    class = "lfs-step-body",
    shiny::fluidRow(
      shiny::column(6, shiny::fileInput(ns("files_pos"), "Raw MS1 files - positive mode",
                                        multiple = TRUE, accept = c(".mzXML", ".mzML"))),
      shiny::column(6, shiny::fileInput(ns("files_neg"), "Raw MS1 files - negative mode",
                                        multiple = TRUE, accept = c(".mzXML", ".mzML")))
    ),
    shiny::fluidRow(
      shiny::column(6, shiny::fileInput(ns("mgf_pos"), "MS2 files (MGF) - positive mode",
                                        multiple = TRUE, accept = ".mgf")),
      shiny::column(6, shiny::fileInput(ns("mgf_neg"), "MS2 files (MGF) - negative mode",
                                        multiple = TRUE, accept = ".mgf"))
    ),
    shiny::p(class = "lfs-hint",
             "Sample group is read from the filename (drops a trailing replicate number) - ",
             shiny::tags$code("D25_1.mzXML"), " and ", shiny::tags$code("D25_2.mzXML"),
             " both become group ", shiny::tags$code("D25"), ". MGF file names should match ",
             "the mzXML sample names so MS2 spectra get matched to the right sample."),
    
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
      status = "idle", process = NULL, log_path = NULL, log_text = "",
      work_dir = NULL, out_path = NULL
    )
    
    shiny::observeEvent(input$toggle_advanced, {
      shinyjs::toggle("advanced_panel")
    })
    
    shiny::observeEvent(input$run_btn, {
      shiny::req(input$files_pos, input$files_neg, input$mgf_pos, input$mgf_neg)
      
      work_dir <- tempfile("lfs_pp_")
      dir.create(work_dir)
      .lfs_organize_uploads(input$files_pos, work_dir, "POS")
      .lfs_organize_uploads(input$files_neg, work_dir, "NEG")
      .lfs_organize_flat_uploads(input$mgf_pos, file.path(work_dir, "POS_mgf"))
      .lfs_organize_flat_uploads(input$mgf_neg, file.path(work_dir, "NEG_mgf"))
      
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
      out_path <- tempfile("lfs_pp_out_", fileext = ".rds")
      
      run_both <- function(pos_args, neg_args, work_dir, mgf_pos_dir, mgf_neg_dir,
                           ms2_mz_tol, ms2_rt_tol, out_path) {
        cat("=== POS ===\n"); do.call(massprocesser::process_data, pos_args)
        cat("\n=== NEG ===\n"); do.call(massprocesser::process_data, neg_args)
        
        load_one <- function(path) {
          e <- new.env()
          load(path, envir = e)
          get(ls(e)[1], envir = e)
        }
        md_pos <- load_one(file.path(work_dir, "POS", "Result", "object"))
        md_neg <- load_one(file.path(work_dir, "NEG", "Result", "object"))
        
        cat("\n=== Attaching MS2 (POS) ===\n")
        md_pos <- massdataset::mutate_ms2(
          object = md_pos, column = "rp", polarity = "positive",
          ms1.ms2.match.mz.tol = ms2_mz_tol, ms1.ms2.match.rt.tol = ms2_rt_tol,
          path = mgf_pos_dir
        )
        cat("\n=== Attaching MS2 (NEG) ===\n")
        md_neg <- massdataset::mutate_ms2(
          object = md_neg, column = "rp", polarity = "negative",
          ms1.ms2.match.mz.tol = ms2_mz_tol, ms1.ms2.match.rt.tol = ms2_rt_tol,
          path = mgf_neg_dir
        )
        
        saveRDS(list(md_pos = md_pos, md_neg = md_neg), out_path)
        cat("\nDone.\n")
      }
      
      rv$process <- .lfs_run_bg_job(
        run_both,
        list(pos_args = pos_args, neg_args = neg_args, work_dir = work_dir,
             mgf_pos_dir = file.path(work_dir, "POS_mgf"),
             mgf_neg_dir = file.path(work_dir, "NEG_mgf"),
             ms2_mz_tol = input$ms2_mz_tol, ms2_rt_tol = input$ms2_rt_tol,
             out_path = out_path),
        log_path
      )
      rv$log_path <- log_path
      rv$log_text <- "Starting...\n"
      rv$work_dir <- work_dir
      rv$out_path <- out_path
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
        if (file.exists(rv$out_path)) {
          out <- readRDS(rv$out_path)
          pipeline_state$mass_dataset_pos <- out$md_pos
          pipeline_state$mass_dataset_neg <- out$md_neg
          pipeline_state$work_dir <- rv$work_dir
          pipeline_state$pp_summary <- data.frame(
            mode = c("POS", "NEG"),
            n_features = c(tryCatch(nrow(out$md_pos@variable_info), error = function(e) NA),
                           tryCatch(nrow(out$md_neg@variable_info), error = function(e) NA)),
            n_samples = c(tryCatch(nrow(out$md_pos@sample_info), error = function(e) NA),
                          tryCatch(nrow(out$md_neg@sample_info), error = function(e) NA))
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


  # ---------------------------------------------------------------------------
# Copy 1 batch MGF upload (shiny fileInput() data.frame) phang vao
# <dest_dir>/<ten goc> - khac .lfs_organize_uploads(), MS2 khong can nhom
# theo "group" vi massdataset::mutate_ms2() tu khop MGF voi sample, khong can
# cau truc POS/<group>/ nhu massprocesser doi hoi voi mzXML.
# ---------------------------------------------------------------------------
.lfs_organize_flat_uploads <- function(files_df, dest_dir) {
  dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
  for (i in seq_len(nrow(files_df))) {
    file.copy(files_df$datapath[i], file.path(dest_dir, files_df$name[i]), overwrite = TRUE)
  }
  invisible(dest_dir)
}
