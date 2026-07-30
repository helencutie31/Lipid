# Step 3: Quantification.
#
# Nhan annotation_result + work_dir tu Step 2/1 tu dong. Chi con 1 input moi
# thuc su can o buoc nay: bang Internal Standard (dung chung cho POS/NEG).
# Chay .lfs_run_lipid_absolute_quantification() (utils_quant_bridge.R) nen
# vector match_item mac dinh cua lab (.lfs_default_match_item_pos/neg trong
# utils_quant.R) - chua co UI chinh sua match_item trong lan nay, de danh gia
# rieng neu can sau.

quantification_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::div(
    class = "lfs-step-body",
    shiny::uiOutput(ns("inherited_summary")),
    
    shiny::fileInput(ns("is_table"), "Internal standard table (.xlsx)", accept = ".xlsx"),
    
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
    
    shiny::div(class = "lfs-run-row",
               shiny::actionButton(ns("quantify_btn"), "Quantify", class = "btn lfs-btn-primary")),
    
    shiny::div(class = "lfs-status-row", shiny::uiOutput(ns("status_badge"))),
    shiny::tags$pre(class = "lfs-log", shiny::textOutput(ns("log"))),
    
    shiny::uiOutput(ns("result_area"))
  )
}

quantification_server <- function(id, pipeline_state) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    rv <- shiny::reactiveValues(status = "idle", process = NULL, log_path = NULL, log_text = "", work_dir = NULL)
    
    shiny::observeEvent(input$toggle_advanced, { shinyjs::toggle("advanced_panel") })
    
    output$inherited_summary <- shiny::renderUI({
      if (is.null(pipeline_state$annotation_result)) {
        return(shiny::p(class = "lfs-hint", "No data yet - finish Lipid annotation first."))
      }
      shiny::p(class = "lfs-hint",
               paste0("Received ", nrow(pipeline_state$annotation_result),
                      " annotated features from step 2 (automatic, no re-upload needed)."))
    })
    
    shiny::observeEvent(input$quantify_btn, {
      shiny::req(pipeline_state$annotation_result, pipeline_state$work_dir, input$is_table)
      
      work_dir <- pipeline_state$work_dir
      for (mode in c("POS", "NEG")) {
        file.copy(input$is_table$datapath, file.path(work_dir, mode, "IS_information.xlsx"), overwrite = TRUE)
      }
      
      log_path <- tempfile(fileext = ".log")
      file.create(log_path)
      
      run_quant <- function(path, annotation_result, is_info_name, chol_rt, ppm, rt.tolerance,
                            threads, which_group_for_rt_confirm) {
        .lfs_run_lipid_absolute_quantification(
          path = path,
          annotation_result = annotation_result,
          is_info_name = is_info_name,
          chol_rt = chol_rt,
          ppm = ppm,
          rt.tolerance = rt.tolerance,
          threads = threads,
          which_group_for_rt_confirm = which_group_for_rt_confirm
        )
      }
      
      rv$process <- .lfs_run_bg_job(
        run_quant,
        list(
          path = work_dir,
          annotation_result = pipeline_state$annotation_result,
          is_info_name = "IS_information.xlsx",
          chol_rt = input$chol_rt,
          ppm = input$ppm,
          rt.tolerance = input$rt_tolerance,
          threads = input$threads,
          which_group_for_rt_confirm = input$rt_confirm_group
        ),
        log_path
      )
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
        result_file <- file.path(rv$work_dir, "Result", "lipid_data_um.xlsx")
        if (file.exists(result_file)) {
          pipeline_state$quant_result <- readxl::read_xlsx(result_file)
          rv$status <- "done"
          pipeline_state$quant_done <- TRUE
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
      shiny::req(pipeline_state$quant_done)
      DT::DTOutput(ns("result_table"))
    })
    output$result_table <- DT::renderDT({
      shiny::req(pipeline_state$quant_result)
      DT::datatable(pipeline_state$quant_result, options = list(scrollX = TRUE, pageLength = 10))
    })
  })
}