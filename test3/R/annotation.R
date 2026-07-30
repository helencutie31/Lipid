# Step 2: Lipid annotation.
#
# Nhan mass_dataset_pos/mass_dataset_neg (da co MS2) tu Step 1 tu dong, khong
# can upload lai. Annotate rieng tung polarity bang metid + thu vien MS-DIAL
# (MSP), sau do MOI merge POS+NEG thanh 1 mass_dataset duy nhat - xem ly do
# o R/lipid_analysis.R va R/peak_picking.R. Gan cot `polarity` vao
# variable_info truoc khi merge de Step 3 con tach lai duoc POS/NEG khi can
# trich EIC targeted rieng tung mode.

annotation_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::div(
    class = "lfs-step-body",
    shiny::uiOutput(ns("inherited_summary")),
    
    shiny::fluidRow(
      shiny::column(6, shiny::fileInput(ns("msp_pos"), "MS-DIAL MS2 library - positive mode (.msp)", accept = ".msp")),
      shiny::column(6, shiny::fileInput(ns("msp_neg"), "MS-DIAL MS2 library - negative mode (.msp)", accept = ".msp"))
    ),
    
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
    
    shiny::div(class = "lfs-run-row",
               shiny::actionButton(ns("annotate_btn"), "Annotate", class = "btn lfs-btn-primary")),
    
    shiny::div(class = "lfs-status-row", shiny::uiOutput(ns("status_badge"))),
    shiny::tags$pre(class = "lfs-log", shiny::textOutput(ns("log"))),
    
    shiny::uiOutput(ns("re
}

annotation_server <- function(id, pipeline_state) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    rv <- shiny::reactiveValues(status = "idle", process = NULL, log_path = NULL, log_text = "", out_path = NULL)

    shiny::observeEvent(input$toggle_advanced, { shinyjs::toggle("advanced_panel") })

    output$inherited_summary <- shiny::renderUI({
      if (is.null(pipeline_state$mass_dataset_pos) || is.null(pipeline_state$mass_dataset_neg)) {
        return(shiny::p(class = "lfs-hint", "No data yet - finish Peak picking first."))
      }
      n_pos <- tryCatch(nrow(pipeline_state$mass_dataset_pos@variable_info), error = function(e) NA)
      n_neg <- tryCatch(nrow(pipeline_state$mass_dataset_neg@variable_info), error = function(e) NA)
      shiny::p(class = "lfs-hint",
               paste0("Received ", n_pos, " positive-mode and ", n_neg,
                      " negative-mode features from Peak picking (automatic, no re-upload needed)."))
    })

    shiny::observeEvent(input$annotate_btn, {
      shiny::req(pipeline_state$mass_dataset_pos, pipeline_state$mass_dataset_neg,
                 input$msp_pos, input$msp_neg)

      out_path <- tempfile("lfs_annot_", fileext = ".rds")
      log_path <- tempfile(fileext = ".log")
      file.create(log_path)

      run_annotation_job <- function(md_pos, md_neg, msp_pos_path, msp_neg_path,
                                      ms1.match.ppm, ms2.match.tol, candidate.num, threads,
                                      out_path) {
        cat("Building POS database...\n")
        db_pos <- .lfs_build_msdial_database(msp_pos_path, polarity = "positive")
        cat("Building NEG database...\n")
        db_neg <- .lfs_build_msdial_database(msp_neg_path, polarity = "negative")

        cat("Annotating POS features...\n")
        md_pos <- .lfs_run_annotation(md_pos, db_pos, polarity = "positive",
                                       ms1.match.ppm = ms1.match.ppm, ms2.match.tol = ms2.match.tol,
                                       candidate.num = candidate.num, threads = threads)
        cat("Annotating NEG features...\n")
        md_neg <- .lfs_run_annotation(md_neg, db_neg, polarity = "negative",
                                       ms1.match.ppm = ms1.match.ppm, ms2.match.tol = ms2.match.tol,
                                       candidate.num = candidate.num, threads = threads)

        md_pos@variable_info$polarity <- "positive"
        md_neg@variable_info$polarity <- "negative"

        cat("Merging POS + NEG...\n")
        merged <- massdataset::merge_mass_dataset(
          x = md_pos, y = md_neg,
          sample_direction = "full", variable_direction = "full"
        )
        saveRDS(merged, out_path)
        cat("Done.\n")
      }

      rv$process <- .lfs_run_bg_job(
        run_annotation_job,
        list(
          md_pos = pipeline_state$mass_dataset_pos,
          md_neg = pipeline_state$mass_dataset_neg,
          msp_pos_path = input$msp_pos$datapath,
          msp_neg_path = input$msp_neg$datapath,
          ms1.match.ppm = input$ms1_match_ppm,
          ms2.match.tol = input$ms2_match_tol,
          candidate.num = input$candidate_num,
          threads = input$threads,
          out_path = out_path
        ),
        log_path
      )
      rv$log_path <- log_path
      rv$log_text <- "Starting...\n"
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
          merged <- readRDS(rv$out_path)
          pipeline_state$mass_dataset <- merged
          pipeline_state$annotation_result <- massdataset::extract_variable_info(merged)
          rv$status <- "done"
          pipeline_state$annotation_done <- TRUE
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

    output$result_area <- shiny::renderUI({
      shiny::req(pipeline_state$annotation_done)
      DT::DTOutput(ns("result_table"))
    })
    output$result_table <- DT::renderDT({
      shiny::req(pipeline_state$annotation_result)
      DT::datatable(pipeline_state$annotation_result, options = list(scrollX = TRUE, pageLength = 10))
    })
  })
}