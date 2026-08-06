# "Small Tools -> Peak Extraction" - a standalone tab (own top-level nav
# item, not part of the lipidAnalysis step flow), matching the project doc's
# Giai doan 0: extract QC peaks for each Internal Standard across candidate
# adducts, so the user can eyeball which adduct gives the cleanest peak and
# lock in a real measured RT + Peak_Area. Its output (Y_IS_opt) is staged
# into pipeline_state so Step 3 (Quantification) can use it as the
# normalization reference - see .lfs_quantify_lipids() in utils_quantification.R.
#
# Kept as inputs+run only in its own sidebar (same shape as mod_peak_picking.R
# / mod_annotation.R); the EIC plot + Y_IS_opt table are rendered in the main
# card next to it (see small_tools_panel_ui() in app_ui.R) since an EIC chart
# needs more width than a 340px sidebar.
#
# Runs synchronously (no callr background job) - unlike Step 1/Step 2, this
# operates on a single QC file, not a whole per-sample study, so a blocking
# call behind the existing busy overlay is simple and fast enough; it also
# avoids having to re-serialize this file's helper functions into a
# separate callr worker process (Step 1/2's background jobs call massprocesser/
# metid functions directly by package::name for exactly this reason).

mod_small_tools_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::div(
    class = "lfs-step-body",

    shiny::div(
      class = "lfs-note",
      shiny::strong("What this does: "),
      "Scans a QC raw file for each Internal Standard's theoretical m/z (per candidate adduct), ",
      "so you can confirm which adduct gives the most symmetric, highest-intensity peak before quantifying."
    ),

    shiny::h6("1. QC raw file"),
    shiny::fileInput(ns("qc_file"), shiny::HTML("<b>QC raw data</b> (single file)"),
                      accept = c(".mzXML", ".mzML")),

    shiny::h6("2. Internal Standard list"),
    shiny::fileInput(ns("is_csv"), shiny::HTML("<b>IS table</b> (.csv - columns: ID, Name, Formula, Accurate_Mass)"),
                      accept = ".csv"),
    shiny::uiOutput(ns("is_csv_status")),

    shiny::tags$hr(),
    shiny::h6("3. Adduct + search parameters"),
    shiny::radioButtons(ns("mode"), "Ion mode", choices = c("Positive" = "positive", "Negative" = "negative"),
                        selected = "positive", inline = TRUE),
    shiny::uiOutput(ns("adduct_picker")),

    shiny::fluidRow(
      shiny::column(6, shiny::numericInput(ns("ppm"), "m/z tolerance (ppm)", value = 15, min = 1)),
      shiny::column(6, shiny::numericInput(ns("rt_max"), "RT window max (s, blank = full run)",
                                            value = NA, min = 0))
    ),

    shiny::div(class = "lfs-run-row",
               shiny::actionButton(ns("extract_btn"), "Extract Peaks", class = "btn lfs-btn-primary")),

    shiny::div(class = "lfs-status-row", shiny::uiOutput(ns("status_badge"))),
    shiny::uiOutput(ns("run_message")),

    shiny::conditionalPanel(
      condition = "output.has_result == true", ns = ns,
      shiny::tags$hr(),
      shiny::downloadButton(ns("download_y_is_opt"), "Download Y_IS_opt (CSV)", class = "btn lfs-btn-outline btn-sm"),
      shiny::div(class = "lfs-run-row",
                 shiny::actionButton(ns("confirm_btn"), "Use for Quantification (Step 3)", class = "btn lfs-btn-secondary")),
      shiny::uiOutput(ns("confirm_status"))
    )
  )
}

mod_small_tools_server <- function(id, pipeline_state) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    rv <- shiny::reactiveValues(status = "idle", eic_data = NULL, y_is_opt = NULL, message = NULL)

    output$adduct_picker <- shiny::renderUI({
      choices <- .lfs_is_adduct_table(input$mode)$adduct
      shiny::checkboxGroupInput(ns("adducts"), "Candidate adducts", choices = choices,
                                selected = choices, inline = TRUE)
    })

    is_table_rv <- shiny::reactiveVal(NULL)

    shiny::observeEvent(input$is_csv, {
      check <- .lfs_read_is_csv(input$is_csv$datapath)
      if (isTRUE(check$ok)) {
        is_table_rv(check$data)
        shiny::showNotification("Internal Standard CSV loaded successfully!", type = "message", duration = 4)
      } else {
        is_table_rv(NULL)
        shiny::showNotification(check$message, type = "error", duration = 6)
      }
    })

    output$is_csv_status <- shiny::renderUI({
      if (is.null(input$is_csv)) return(NULL)
      if (is.null(is_table_rv())) {
        return(shiny::p(class = "lfs-hint", style = "color:#B84C3C;", "Invalid IS table - see notification."))
      }
      shiny::p(class = "lfs-hint", style = "color:#2B6E63;",
               paste0("✓ Valid - ", nrow(is_table_rv()), " internal standard(s) found."))
    })

    shiny::observeEvent(input$extract_btn, {
      rv$message <- NULL

      missing <- character()
      if (is.null(input$qc_file)) missing <- c(missing, "QC raw file")
      if (is.null(is_table_rv())) missing <- c(missing, "a valid IS CSV")
      if (length(input$adducts) == 0) missing <- c(missing, "at least one adduct")

      if (length(missing) > 0) {
        rv$status <- "error"
        rv$message <- paste0("Cannot start - missing: ", paste(missing, collapse = "; "), ".")
        return()
      }

      rt_window <- if (is.na(input$rt_max) || !is.numeric(input$rt_max)) NULL else c(0, input$rt_max)

      rv$status <- "running"
      pipeline_state$busy <- TRUE
      pipeline_state$busy_title <- "Small Tools: Peak Extraction"
      pipeline_state$busy_desc <- "Scanning QC file for internal standard peaks..."

      result <- .lfs_run_peak_extraction(
        qc_file_path = input$qc_file$datapath,
        is_table = is_table_rv(),
        adducts = input$adducts,
        mode = input$mode,
        ppm = input$ppm,
        rt_window = rt_window
      )

      pipeline_state$busy <- FALSE

      if (isTRUE(result$ok)) {
        rv$eic_data <- result$eic_data
        rv$y_is_opt <- result$y_is_opt
        rv$status <- "done"
      } else {
        rv$status <- "error"
        rv$message <- result$message
      }
    })

    output$status_badge <- shiny::renderUI({
      label <- switch(rv$status, idle = "Idle", running = "Running...", done = "Done", error = "Error", rv$status)
      cls <- switch(rv$status, idle = "lfs-badge-muted", running = "lfs-badge-running",
                    done = "lfs-badge-live", error = "lfs-badge-error", "lfs-badge-muted")
      shiny::span(class = paste("lfs-badge", cls), label)
    })

    output$run_message <- shiny::renderUI({
      shiny::req(rv$message)
      shiny::p(class = "lfs-hint", style = "color:#B84C3C;", rv$message)
    })

    output$has_result <- shiny::reactive({ identical(rv$status, "done") })
    shiny::outputOptions(output, "has_result", suspendWhenHidden = FALSE)

    output$eic_plot <- plotly::renderPlotly({
      shiny::req(identical(rv$status, "done"), rv$eic_data)
      long_df <- .lfs_unnest_eic(rv$eic_data)
      shiny::validate(shiny::need(nrow(long_df) > 0, "No signal found in the requested m/z / RT window."))

      p <- ggplot2::ggplot(long_df, ggplot2::aes(x = rt, y = intensity, color = Adduct)) +
        ggplot2::geom_line() +
        ggplot2::facet_wrap(~IS_Name, scales = "free", ncol = 2) +
        ggplot2::labs(x = "Retention time (s)", y = "Intensity") +
        ggplot2::theme_minimal()
      plotly::ggplotly(p)
    })

    output$y_is_opt_table <- DT::renderDT({
      shiny::req(identical(rv$status, "done"), rv$y_is_opt)
      DT::datatable(rv$y_is_opt, options = list(scrollX = TRUE, pageLength = 10), rownames = FALSE)
    })

    output$download_y_is_opt <- shiny::downloadHandler(
      filename = function() "Y_IS_opt.csv",
      content = function(file) {
        shiny::req(rv$y_is_opt)
        utils::write.csv(rv$y_is_opt, file, row.names = FALSE)
      }
    )

    shiny::observeEvent(input$confirm_btn, {
      shiny::req(rv$y_is_opt)
      pipeline_state$small_tools_is_opt <- rv$y_is_opt
      shiny::showNotification("Y_IS_opt saved - Step 3 (Quantification) will use this as the IS reference.",
                              type = "message", duration = 5)
    })

    output$confirm_status <- shiny::renderUI({
      shiny::req(!is.null(pipeline_state$small_tools_is_opt))
      shiny::p(class = "lfs-hint", style = "color:#2B6E63;",
               paste0("✓ In use for Quantification - ", nrow(pipeline_state$small_tools_is_opt), " IS(s)."))
    })
  })
}
