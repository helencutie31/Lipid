# Quantification module.
#
# Wraps lipidflow::get_lipid_absolute_quantification() end to end. See
# utils_quant.R for the helper functions this file relies on, and README.md
# for the reasoning behind the design choices below (server-path input for
# raw data, adaptive class-column detection, callr background execution).

mod_quantification_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 380,
      title = "1. Point at your data",

      shiny::textInput(ns("data_path"), "Data folder on the server",
                        placeholder = "/srv/shiny-server/lipidflow-shiny/jobs/my_run"),
      shiny::helpText("Expected layout: ", shiny::tags$code("path/POS/<group>/*.mzXML"),
                       " and ", shiny::tags$code("path/NEG/<group>/*.mzXML"),
                       " - the same structure lipidflow and massprocesser both expect. Place raw ",
                       "data here the way you already do (rsync/scp). This field stays in sync ",
                       "with the Peak Picking tab."),
      shiny::actionButton(ns("scan_btn"), "Scan folder", class = "btn lfs-btn-secondary"),
      shiny::uiOutput(ns("scan_summary")),

      shiny::selectInput(ns("group_rt"), "Group used to confirm IS retention time",
                          choices = c("Scan folder first" = "")),

      shiny::tags$hr(),
      shiny::h6("2. Config files"),
      shiny::fileInput(ns("is_info_pos"), "Internal standard table - POS (.xlsx)", accept = ".xlsx"),
      shiny::fileInput(ns("is_info_neg"), "Internal standard table - NEG (.xlsx)", accept = ".xlsx"),
      shiny::fileInput(ns("annot_pos"), "Lipid annotation table - POS (.xlsx)", accept = ".xlsx"),
      shiny::fileInput(ns("annot_neg"), "Lipid annotation table - NEG (.xlsx)", accept = ".xlsx"),

      shiny::fluidRow(
        shiny::column(6, shiny::selectInput(ns("class_col_pos"), "Class column (POS)",
                                             choices = c("Upload POS annotation file first" = ""))),
        shiny::column(6, shiny::selectInput(ns("class_col_neg"), "Class column (NEG)",
                                             choices = c("Upload NEG annotation file first" = "")))
      ),
      shiny::actionButton(ns("detect_classes_btn"), "Detect lipid classes", class = "btn lfs-btn-secondary"),
      shiny::uiOutput(ns("match_item_ui")),

      shiny::tags$hr(),
      shiny::h6("3. Parameters"),
      shiny::fluidRow(
        shiny::column(6, shiny::numericInput(ns("ppm"), "m/z tolerance (ppm)", value = 40, min = 1)),
        shiny::column(6, shiny::numericInput(ns("rt_tolerance"), "RT tolerance (sec)", value = 180, min = 1))
      ),
      shiny::fluidRow(
        shiny::column(6, shiny::numericInput(ns("chol_rt"), "Fallback cholesterol RT (sec)", value = 1169, min = 1)),
        shiny::column(6, shiny::numericInput(ns("threads"), "Threads", value = 3, min = 1))
      ),
      shiny::checkboxInput(ns("output_eic"), "Output EIC peak-shape plots", value = TRUE),
      shiny::checkboxInput(ns("rerun"), "Force re-run peak picking (ignore cached results)", value = FALSE),
      shiny::checkboxInput(ns("use_manual_is_info"), "Use my IS table's RT/adduct as-is (skip auto RT detection)", value = FALSE),
      shiny::textInput(ns("forced_table"), "Manual peak-correction table name (advanced, optional)", value = ""),

      shiny::tags$hr(),
      shiny::h6("4. Run"),
      shiny::actionButton(ns("validate_btn"), "Validate structure", class = "btn lfs-btn-secondary"),
      shiny::uiOutput(ns("validate_output")),
      shinyjs::disabled(
        shiny::actionButton(ns("run_btn"), "Run quantification", class = "btn lfs-btn-primary")
      )
    ),

    # ---- main panel ------------------------------------------------------
    shiny::div(class = "lfs-quant-main",
      shiny::div(class = "lfs-status-row",
                 shiny::h4("Status"), shiny::uiOutput(ns("status_badge"))),
      shiny::tags$pre(class = "lfs-log", shiny::textOutput(ns("log"))),

      bslib::navset_tab(
        bslib::nav_panel("Results table",
                          shiny::downloadButton(ns("download_table"), "Download CSV"),
                          DT::DTOutput(ns("result_table"))),
        bslib::nav_panel("Class-level plots",
                          shiny::uiOutput(ns("class_plot_gallery"))),
        bslib::nav_panel("Everything",
                          shiny::p("Full Result/ folder as produced by lipidflow, including per-lipid EIC HTML widgets."),
                          shiny::downloadButton(ns("download_zip"), "Download Result/ as .zip"))
      )
    )
  )
}

mod_quantification_server <- function(id, shared_data_path) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    rv <- shiny::reactiveValues(
      groups = character(),
      classes_pos = character(),
      classes_neg = character(),
      validation = NULL,
      status = "idle",   # idle | running | done | error
      process = NULL,
      log_path = NULL,
      log_text = "",
      data_path = NULL
    )

    # ---- keep data_path in sync with the shared value Peak Picking uses ----
    shiny::observeEvent(input$data_path, {
      if (!identical(input$data_path, shared_data_path())) shared_data_path(input$data_path)
    }, ignoreInit = TRUE)

    shiny::observeEvent(shared_data_path(), {
      if (!identical(input$data_path, shared_data_path())) {
        shiny::updateTextInput(session, "data_path", value = shared_data_path())
      }
    }, ignoreInit = TRUE)

    # ---- auto-detect columns as soon as an annotation file is uploaded ----
    shiny::observeEvent(input$annot_pos, {
      cols <- .lfs_xlsx_colnames(input$annot_pos$datapath)
      shiny::updateSelectInput(session, "class_col_pos", choices = cols)
    })
    shiny::observeEvent(input$annot_neg, {
      cols <- .lfs_xlsx_colnames(input$annot_neg$datapath)
      shiny::updateSelectInput(session, "class_col_neg", choices = cols)
    })

    # ---- scan the server-side data folder ----------------------------------
    shiny::observeEvent(input$scan_btn, {
      shiny::req(input$data_path)
      pos_groups <- .lfs_list_groups(input$data_path, "POS")
      neg_groups <- .lfs_list_groups(input$data_path, "NEG")
      groups <- sort(unique(c(pos_groups, neg_groups)))
      rv$groups <- groups
      shiny::updateSelectInput(session, "group_rt", choices = groups,
                                selected = if ("QC" %in% groups) "QC" else if (length(groups) > 0) groups[1] else NULL)
    })

    output$scan_summary <- shiny::renderUI({
      if (length(rv$groups) == 0) {
        shiny::tags$p(class = "lfs-hint", "Click Scan folder to look for POS/ and NEG/ group subfolders.")
      } else {
        shiny::tags$p(class = "lfs-hint", paste0("Found groups: ", paste(rv$groups, collapse = ", ")))
      }
    })

    # ---- detect lipid classes and build the match_item UI ------------------
    shiny::observeEvent(input$detect_classes_btn, {
      shiny::req(input$annot_pos, input$annot_neg)
      shiny::req(nzchar(input$class_col_pos), nzchar(input$class_col_neg))
      rv$classes_pos <- .lfs_unique_column_values(input$annot_pos$datapath, input$class_col_pos)
      rv$classes_neg <- .lfs_unique_column_values(input$annot_neg$datapath, input$class_col_neg)
    })

    output$match_item_ui <- shiny::renderUI({
      shiny::req(length(rv$classes_pos) > 0 || length(rv$classes_neg) > 0)

      is_names_pos <- .lfs_is_names(if (!is.null(input$is_info_pos)) input$is_info_pos$datapath else NULL)
      is_names_neg <- .lfs_is_names(if (!is.null(input$is_info_neg)) input$is_info_neg$datapath else NULL)
      defaults_pos <- .lfs_default_match_item_pos()
      defaults_neg <- .lfs_default_match_item_neg()

      build_row <- function(cls, prefix, is_names, defaults) {
        # compare normalized (handles the NBSP issue - see .lfs_normalize_ws),
        # but select the real un-normalized value from is_names so the
        # exact string lipidflow expects is what actually gets submitted.
        default_sel <- if (cls %in% names(defaults)) {
          is_names[.lfs_normalize_ws(is_names) %in% .lfs_normalize_ws(defaults[[cls]])]
        } else {
          character()
        }
        shiny::selectizeInput(ns(paste0(prefix, make.names(cls))),
                               label = cls, choices = is_names, selected = default_sel,
                               multiple = TRUE, width = "100%")
      }

      shiny::tagList(
        shiny::h6("Match each lipid class to its internal standard(s)"),
        shiny::p(class = "lfs-hint",
                 "Pre-filled where a class name matches the lab's usual internal standard mix - check before running."),
        if (length(rv$classes_pos) > 0) shiny::div(
          shiny::strong("POS"),
          lapply(rv$classes_pos, build_row, prefix = "match_pos_", is_names = is_names_pos, defaults = defaults_pos)
        ),
        if (length(rv$classes_neg) > 0) shiny::div(
          shiny::strong("NEG"),
          lapply(rv$classes_neg, build_row, prefix = "match_neg_", is_names = is_names_neg, defaults = defaults_neg)
        )
      )
    })

    build_match_item <- function(classes, prefix) {
      out <- lapply(classes, function(cls) input[[paste0(prefix, make.names(cls))]])
      names(out) <- classes
      out[vapply(out, function(x) length(x) > 0, logical(1))]
    }

    # ---- validate ------------------------------------------------------------
    shiny::observeEvent(input$validate_btn, {
      shiny::req(input$data_path)
      group_rt <- if (is.null(input$group_rt) || !nzchar(input$group_rt)) "QC" else input$group_rt
      rv$validation <- .lfs_validate_path_structure(
        path = input$data_path,
        is_info_name_pos = "IS_information.xlsx",
        is_info_name_neg = "IS_information.xlsx",
        lipid_annotation_table_pos = "lipid_annotation_table_pos.xlsx",
        lipid_annotation_table_neg = "lipid_annotation_table_neg.xlsx",
        which_group_for_rt_confirm = group_rt
      )
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

    # ---- run -------------------------------------------------------------
    shiny::observeEvent(input$run_btn, {
      shiny::req(input$data_path, rv$validation)
      shiny::req(all(rv$validation$status == "ok"))

      path <- input$data_path

      copy_if_present <- function(file_input, dest_dir, dest_name) {
        if (!is.null(file_input)) {
          file.copy(file_input$datapath, file.path(dest_dir, dest_name), overwrite = TRUE)
        }
      }
      copy_if_present(input$is_info_pos, file.path(path, "POS"), "IS_information.xlsx")
      copy_if_present(input$is_info_neg, file.path(path, "NEG"), "IS_information.xlsx")
      copy_if_present(input$annot_pos, file.path(path, "POS"), "lipid_annotation_table_pos.xlsx")
      copy_if_present(input$annot_neg, file.path(path, "NEG"), "lipid_annotation_table_neg.xlsx")

      match_item_pos <- build_match_item(rv$classes_pos, "match_pos_")
      match_item_neg <- build_match_item(rv$classes_neg, "match_neg_")
      if (length(match_item_pos) == 0) match_item_pos <- .lfs_default_match_item_pos()
      if (length(match_item_neg) == 0) match_item_neg <- .lfs_default_match_item_neg()

      args_list <- list(
        path = path,
        is_info_name_pos = "IS_information.xlsx",
        is_info_name_neg = "IS_information.xlsx",
        use_manual_is_info = isTRUE(input$use_manual_is_info),
        lipid_annotation_table_pos = "lipid_annotation_table_pos.xlsx",
        lipid_annotation_table_neg = "lipid_annotation_table_neg.xlsx",
        chol_rt = input$chol_rt,
        output_eic = isTRUE(input$output_eic),
        forced_targeted_peak_table_name = if (nzchar(input$forced_table)) input$forced_table else NULL,
        ppm = input$ppm,
        rt.tolerance = input$rt_tolerance,
        threads = input$threads,
        rerun = isTRUE(input$rerun),
        which_group_for_rt_confirm = input$group_rt,
        match_item_pos = match_item_pos,
        match_item_neg = match_item_neg
      )

      log_path <- tempfile(fileext = ".log")
      file.create(log_path)

      rv$process <- .lfs_run_quant_job(args_list, log_path)
      rv$log_path <- log_path
      rv$log_text <- "Starting...\n"
      rv$data_path <- path
      rv$status <- "running"
    })

    # ---- poll while running -------------------------------------------------
    poll <- shiny::reactiveTimer(1500)

    shiny::observe({
      poll()
      shiny::req(identical(rv$status, "running"))

      if (!is.null(rv$log_path) && file.exists(rv$log_path)) {
        rv$log_text <- paste(readLines(rv$log_path, warn = FALSE), collapse = "\n")
      }

      if (!is.null(rv$process) && !rv$process$is_alive()) {
        result_file <- file.path(rv$data_path, "Result", "lipid_data_um.xlsx")
        rv$status <- if (file.exists(result_file)) "done" else "error"
      }
    })

    output$status_badge <- shiny::renderUI({
      label <- switch(rv$status, idle = "Idle", running = "Running\u2026",
                       done = "Done", error = "Error", rv$status)
      cls <- switch(rv$status, idle = "lfs-badge-muted", running = "lfs-badge-running",
                    done = "lfs-badge-live", error = "lfs-badge-error", "lfs-badge-muted")
      shiny::span(class = paste("lfs-badge", cls), label)
    })

    output$log <- shiny::renderText({ rv$log_text })

    # ---- results --------------------------------------------------------
    result_table <- shiny::reactive({
      shiny::req(identical(rv$status, "done"))
      readxl::read_xlsx(file.path(rv$data_path, "Result", "lipid_data_um.xlsx"))
    })

    output$result_table <- DT::renderDT({
      DT::datatable(result_table(), options = list(scrollX = TRUE, pageLength = 15))
    })

    output$download_table <- shiny::downloadHandler(
      filename = function() "lipidflow_quantification.csv",
      content = function(file) utils::write.csv(result_table(), file, row.names = FALSE)
    )

    output$download_zip <- shiny::downloadHandler(
      filename = function() "lipidflow_result.zip",
      content = function(file) {
        shiny::req(identical(rv$status, "done"))
        zip::zip(file, files = "Result", root = rv$data_path)
      }
    )

    output$class_plot_gallery <- shiny::renderUI({
      shiny::req(identical(rv$status, "done"))
      plot_dir <- file.path(rv$data_path, "Result", "class_plot")
      if (!dir.exists(plot_dir)) {
        return(shiny::p(class = "lfs-hint", "No class_plot folder was produced for this run."))
      }
      imgs <- list.files(plot_dir, pattern = "\\.(png|jpg|jpeg)$", full.names = FALSE, ignore.case = TRUE)
      if (length(imgs) == 0) return(shiny::p(class = "lfs-hint", "No image files found in class_plot."))

      # addResourcePath is app-wide, not per-session - key it on session$token
      # so concurrent users on the shared server never resolve each other's
      # plot files under the same URL prefix.
      resource_id <- paste0("lfs_job_", session$token)
      shiny::addResourcePath(resource_id, plot_dir)

      shiny::div(class = "lfs-gallery",
                 lapply(imgs, function(f) {
                   shiny::div(class = "lfs-gallery-item",
                              shiny::tags$img(src = file.path(resource_id, f), alt = f),
                              shiny::tags$p(f))
                 }))
    })
  })
}
