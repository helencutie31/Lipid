# Save Results panel - last item in the sidebar accordion. Matches
# featureMSEA's "Save Result" / "Save All Data" pattern: one button for the
# current-stage table, one for everything produced so far.

mod_save_results_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(
    class = "lfs-step-body",
    shiny::uiOutput(ns("status")),
    shiny::downloadButton(ns("save_result"), "Save Result", class = "btn lfs-btn-primary"),
    shiny::downloadButton(ns("save_all"), "Save All Data", class = "btn lfs-btn-secondary")
  )
}

mod_save_results_server <- function(id, pipeline_state) {
  shiny::moduleServer(id, function(input, output, session) {

    latest_table <- shiny::reactive({
      if (!is.null(pipeline_state$quant_result)) return(pipeline_state$quant_result)
      if (!is.null(pipeline_state$annotation_result)) {
        return(.lfs_combined_annotation_table(pipeline_state$annotation_result))
      }
      if (!is.null(pipeline_state$mass_dataset_pos)) return(.lfs_peak_table(pipeline_state$mass_dataset_pos))
      NULL
    })

    output$status <- shiny::renderUI({
      if (is.null(latest_table())) {
        return(shiny::p(class = "lfs-hint", "No analysis results available yet."))
      }
      shiny::p(class = "lfs-hint", "Latest result ready to save.")
    })

    output$save_result <- shiny::downloadHandler(
      filename = function() "lipidflow_result.csv",
      content = function(file) {
        shiny::req(latest_table())
        utils::write.csv(latest_table(), file, row.names = FALSE)
      }
    )

    output$save_all <- shiny::downloadHandler(
      filename = function() "lipidflow_all_data.zip",
      content = function(file) {
        shiny::req(latest_table())
        tmp_dir <- tempfile(); dir.create(tmp_dir)
        utils::write.csv(latest_table(), file.path(tmp_dir, "latest_result.csv"), row.names = FALSE)
        if (!is.null(pipeline_state$pp_summary)) {
          utils::write.csv(pipeline_state$pp_summary, file.path(tmp_dir, "peak_picking_summary.csv"), row.names = FALSE)
        }
        zip::zip(file, files = list.files(tmp_dir, full.names = FALSE), root = tmp_dir)
      }
    )
  })
}
