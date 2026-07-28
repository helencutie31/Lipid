# lipidAnalysis tab - the 3-step, state-chained workflow.
#
# One accordion, one open panel at a time. Each step's module writes its
# result into a shared `pipeline_state` (a reactiveValues object, passed by
# reference to all 3 child modules - standard Shiny cross-module state
# pattern). This module watches pipeline_state and auto-opens the next
# panel once a step finishes, so switching to step 2 or 3 never requires a
# re-upload - the previous step's output is already sitting in
# pipeline_state when that panel's server code runs.

lipid_analysis_ui <- function(id) {
  ns <- shiny::NS(id)

  step_title <- function(n, label) {
    shiny::tagList(shiny::span(class = "lfs-step-num", n), label)
  }

  shiny::div(
    class = "lfs-analysis",
    shiny::h2("Lipid analysis"),
    shiny::p(class = "lfs-hint", "Work through the 3 steps in order - each one carries its result into the next automatically."),
    bslib::accordion(
      id = ns("steps"), multiple = FALSE, open = "step1",
      bslib::accordion_panel(title = step_title(1, "Peak picking"), value = "step1",
                              peak_picking_ui(ns("pp"))),
      bslib::accordion_panel(title = step_title(2, "Lipid annotation"), value = "step2",
                              placeholder_ui(ns("annot"))),
      bslib::accordion_panel(title = step_title(3, "Quantification"), value = "step3",
                              quantification_ui(ns("quant")))
    )
  )
}

lipid_analysis_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {

    pipeline_state <- shiny::reactiveValues(
      mass_dataset = NULL,       # list(pos = <mass_dataset>, neg = <mass_dataset>) from step 1
      pp_done = FALSE,
      pp_summary = NULL,         # small data.frame: mode, n_features, n_samples

      annotation_result = NULL,  # feature table + annotation column, from step 2
      annotation_done = FALSE,

      quant_result = NULL,       # from step 3
      quant_done = FALSE
    )

    peak_picking_server("pp", pipeline_state)
    placeholder_server("annot", pipeline_state)
    quantification_server("quant", pipeline_state)

    # auto-advance the accordion as each step completes
    shiny::observeEvent(pipeline_state$pp_done, {
      shiny::req(pipeline_state$pp_done)
      bslib::accordion_panel_set(id = "steps", values = "step2", session = session)
    })
    shiny::observeEvent(pipeline_state$annotation_done, {
      shiny::req(pipeline_state$annotation_done)
      bslib::accordion_panel_set(id = "steps", values = "step3", session = session)
    })
  })
}
