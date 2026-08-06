# app_server.R - top-level server. Body below is the original
# home_server() + lipid_analysis_server() content, preserved verbatim -
# only mechanical changes: module calls renamed to mod_*, and the
# moduleServer("lipidAnalysis", ...) wrapping is removed since this is now
# the top-level app_server directly, not a nested module (no separate
# lipid_analysis.R in the new file layout to hold it as one).

app_server <- function(input, output, session) {

  shiny::observeEvent(input$btn_get_started, {
    bslib::nav_select(id = "main_nav", selected = "lipidAnalysis", session = session)
  })
  shiny::observeEvent(input$btn_go_help, {
    bslib::nav_select(id = "main_nav", selected = "help_docs", session = session)
  })

  shared_data_path <- shiny::reactiveVal("")

  pipeline_state <- shiny::reactiveValues(
    # staged inputs from Data Import
    pp_mode = "new", pp_data_source = "upload",
    pp_files_pos = NULL, pp_files_neg = NULL, pp_server_path = "",
    pp_existing_pos = NULL, pp_existing_neg = NULL,
    annot_ms2_pos = NULL, annot_ms2_neg = NULL,
    annot_db_pos_path = NULL, annot_db_neg_path = NULL,
    annot_db_pos_label = NULL, annot_db_neg_label = NULL,
    db_pos_error = NULL, db_neg_error = NULL,
    quant_is_table = NULL, quant_is_names = character(), is_table_error = NULL,
    busy = FALSE, busy_title = "Processing", busy_desc = "Working...", busy_status_text = "",

    # Small Tools -> Peak Extraction output (Y_IS_opt), consumed by Step 3
    small_tools_is_opt = NULL,

    # pipeline results
    mass_dataset = NULL, mass_dataset_pos = NULL, mass_dataset_neg = NULL,
    pp_done = FALSE, pp_summary = NULL,
    annotation_result = NULL, annotation_done = FALSE,
    quant_result = NULL, quant_done = FALSE
  )

  mod_import_server("import", pipeline_state, shared_data_path)
  mod_peak_picking_server("pp", pipeline_state)
  mod_annotation_server("annot", pipeline_state)
  mod_quantification_server("quant", pipeline_state)
  mod_save_results_server("save", pipeline_state)
  mod_small_tools_server("smalltools", pipeline_state)
  mod_help_docs_server("help")

  # ---- busy overlay, driven by pipeline_state$busy* fields which
  # mod_peak_picking.R and mod_annotation.R set around their long jobs ----
  shiny::observeEvent(pipeline_state$busy, {
    if (isTRUE(pipeline_state$busy)) {
      shinyjs::show("busy_overlay")
    } else {
      shinyjs::hide("busy_overlay")
    }
  }, ignoreNULL = FALSE)

  output$busy_title <- shiny::renderUI({ pipeline_state$busy_title })
  output$busy_desc <- shiny::renderUI({ pipeline_state$busy_desc })
  output$busy_status <- shiny::renderUI({ pipeline_state$busy_status_text })

  # corner toast, same idea as featureMSEA's "Step Progress" notification
  shiny::observeEvent(pipeline_state$busy, {
    shiny::req(isTRUE(pipeline_state$busy))
    shiny::showNotification(
      paste0(pipeline_state$busy_title, " - ", pipeline_state$busy_desc),
      id = "lfs_progress_toast", duration = NULL, type = "message"
    )
  })
  shiny::observeEvent(pipeline_state$busy, {
    shiny::req(isFALSE(pipeline_state$busy))
    shiny::removeNotification(id = "lfs_progress_toast")
  })

  shiny::observeEvent(pipeline_state$pp_done, {
    shiny::req(pipeline_state$pp_done)
    bslib::accordion_panel_set(id = "steps", values = "step2", session = session)
  })
  shiny::observeEvent(pipeline_state$annotation_done, {
    shiny::req(pipeline_state$annotation_done)
    bslib::accordion_panel_set(id = "steps", values = "step3", session = session)
  })

  # ---- main-panel "Analysis Results" - most-advanced table available ----
  current_table <- shiny::reactive({
    if (!is.null(pipeline_state$quant_result)) return(pipeline_state$quant_result)
    if (!is.null(pipeline_state$annotation_result)) {
      return(.lfs_combined_annotation_table(pipeline_state$annotation_result))
    }
    if (!is.null(pipeline_state$mass_dataset_pos)) return(.lfs_peak_table(pipeline_state$mass_dataset_pos))
    NULL
  })

  output$summary_status <- shiny::renderUI({
    if (is.null(current_table())) {
      return(shiny::p(class = "lfs-hint", "No results yet - run Step 1 in the sidebar to get started."))
    }
    stage <- if (!is.null(pipeline_state$quant_result)) "Quantification"
    else if (!is.null(pipeline_state$annotation_result)) "Lipid Annotation"
    else "Peak Picking"
    shiny::p(class = "lfs-hint", paste0("Showing latest result from: ", stage, "."))
  })

  output$summary_table <- DT::renderDT({
    shiny::req(current_table())
    DT::datatable(current_table(), options = list(scrollX = TRUE, pageLength = 10))
  })

  output$download_csv <- shiny::downloadHandler(
    filename = function() "lipidflow_analysis_result.csv",
    content = function(file) {
      shiny::req(current_table())
      utils::write.csv(current_table(), file, row.names = FALSE)
    }
  )
}
