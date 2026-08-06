# Step 3: Quantification - IS selection + Quantify only.
#
# The IS table upload lives in Data Import (see mod_import.R);
# pipeline_state$quant_is_names/quant_is_table are already staged there.
# This step's own inputs are just the IS subset to quantify with, and the
# Quantify button.
#
# Real math (Giai doan 3 of the project doc), implemented in
# .lfs_quantify_lipids() (utils_quantification.R):
#   Final_Calculated = (Peak_Area_Sample / Peak_Area_IS_opt) * Known_Conc_IS
# - Peak_Area_Sample comes from the annotated feature table (Step 2).
# - Peak_Area_IS_opt + its class match come from Small Tools -> Peak
#   Extraction's Y_IS_opt (pipeline_state$small_tools_is_opt) - measured
#   once from a QC run, not per-sample.
# - Known_Conc_IS comes from the uploaded IS table (Data Import).
# This step is therefore blocked until BOTH Step 2 (annotation) AND
# Small Tools -> Peak Extraction have been run.

mod_quantification_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::div(
    class = "lfs-step-body",
    shiny::uiOutput(ns("inherited_summary")),

    shiny::div(
      class = "lfs-note",
      shiny::strong("Formula: "),
      "Final_Calculated = (Peak_Area_Sample / Peak_Area_IS_opt) × Known_Conc_IS, ",
      "where Peak_Area_IS_opt is the QC-measured internal standard peak area from ",
      shiny::strong("Small Tools → Peak Extraction"), "."
    ),

    shiny::selectizeInput(ns("is_selected"), "Select internal standards to quantify with",
                           choices = NULL, multiple = TRUE, width = "100%"),

    shiny::div(class = "lfs-run-row",
               shiny::actionButton(ns("quantify_btn"), "Quantify", class = "btn lfs-btn-primary")),

    shiny::uiOutput(ns("run_message")),
    shiny::uiOutput(ns("result_area"))
  )
}

mod_quantification_server <- function(id, pipeline_state) {
  shiny::moduleServer(id, function(input, output, session) {

    rv <- shiny::reactiveValues(message = NULL)

    output$inherited_summary <- shiny::renderUI({
      missing <- character()
      if (is.null(pipeline_state$annotation_result)) missing <- c(missing, "Step 2: Lipid Annotation")
      if (is.null(pipeline_state$small_tools_is_opt)) missing <- c(missing, "Small Tools → Peak Extraction (Y_IS_opt)")
      if (is.null(pipeline_state$quant_is_table)) missing <- c(missing, "Internal Standard table (Data Import)")

      if (length(missing) > 0) {
        return(shiny::p(class = "lfs-hint",
                        paste0("Not ready yet - still need: ", paste(missing, collapse = "; "), ".")))
      }
      annot_tbl <- .lfs_combined_annotation_table(pipeline_state$annotation_result)
      shiny::p(class = "lfs-hint",
               sprintf("Ready - %d annotated feature(s) from Step 2, %d IS(s) available from Peak Extraction.",
                       nrow(annot_tbl), nrow(pipeline_state$small_tools_is_opt)))
    })

    shiny::observeEvent(pipeline_state$quant_is_names, {
      shiny::updateSelectizeInput(session, "is_selected", choices = pipeline_state$quant_is_names)
    })

    shiny::observeEvent(input$quantify_btn, {
      rv$message <- NULL

      missing <- character()
      if (is.null(pipeline_state$annotation_result)) missing <- c(missing, "Step 2: Lipid Annotation result")
      if (is.null(pipeline_state$small_tools_is_opt)) missing <- c(missing, "Small Tools → Peak Extraction (Y_IS_opt)")
      if (is.null(pipeline_state$quant_is_table)) missing <- c(missing, "Internal Standard table (Data Import)")
      if (length(missing) > 0) {
        rv$message <- paste0("Cannot quantify - missing: ", paste(missing, collapse = "; "), ".")
        return()
      }

      result <- tryCatch({
        annot_tbl <- .lfs_combined_annotation_table(pipeline_state$annotation_result)
        lipid_name_col <- .lfs_detect_lipid_name_col(annot_tbl)
        if (is.na(lipid_name_col)) stop("could not find a lipid-name column in the annotation table.")

        is_table <- readxl::read_xlsx(pipeline_state$quant_is_table$datapath)
        is_table <- as.data.frame(is_table, stringsAsFactors = FALSE)

        y_is_opt <- pipeline_state$small_tools_is_opt
        if (length(input$is_selected) > 0) {
          y_is_opt <- y_is_opt[y_is_opt$IS_Name %in% input$is_selected, , drop = FALSE]
          is_table <- is_table[is_table$name %in% input$is_selected, , drop = FALSE]
          if (nrow(y_is_opt) == 0) stop("none of the selected internal standards were found in Y_IS_opt.")
        }

        match_item <- utils::modifyList(.lfs_default_match_item_neg(), .lfs_default_match_item_pos())

        .lfs_quantify_lipids(
          annotation_table = annot_tbl, y_is_opt = y_is_opt, is_table = is_table,
          match_item = match_item, lipid_name_col = lipid_name_col
        )
      }, error = function(e) {
        message("[mod_quantification] Quantify failed: ", conditionMessage(e))
        e
      })

      if (inherits(result, "error")) {
        rv$message <- paste0("Quantification failed: ", conditionMessage(result))
        return()
      }

      pipeline_state$quant_result <- result
      pipeline_state$quant_done <- TRUE
    })

    output$run_message <- shiny::renderUI({
      shiny::req(rv$message)
      shiny::p(class = "lfs-hint", style = "color:#B84C3C;", rv$message)
    })

    output$result_area <- shiny::renderUI({
      shiny::req(pipeline_state$quant_done)
      DT::DTOutput(session$ns("result_table"))
    })
    output$result_table <- DT::renderDT({
      shiny::req(pipeline_state$quant_result)
      DT::datatable(pipeline_state$quant_result, options = list(scrollX = TRUE, pageLength = 10))
    })
  })
}
