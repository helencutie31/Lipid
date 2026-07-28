# Step 2: Lipid annotation.
#
# HONESTY NOTE: the real algorithm here (metid matched against MS-DIAL's
# MS/MS lipid libraries) hasn't been researched/built yet - that's the
# still-pending Giai đoạn 4 work. What IS real in this file: receiving
# pipeline_state$mass_dataset automatically (no re-upload, no re-navigation
# needed - it's just sitting in shared state from step 1), and the UI shape
# for picking a library and running. The "Annotate" button produces a
# clearly-labeled placeholder result so the 3-step flow is fully click-
# throughable for UI testing today; swap out .lfs_run_annotation_stub()
# for a real metid call once that research is done, and the rest of this
# file (state wiring, UI) shouldn't need to change.

placeholder_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::div(
    class = "lfs-step-body",
    shiny::uiOutput(ns("inherited_summary")),

    shiny::div(
      class = "lfs-note",
      shiny::strong("Chưa có thuật toán thật ở bước này."),
      " metid + thư viện MS-DIAL chưa được tích hợp (Giai đoạn 4, chưa làm). ",
      "Nút Annotate bên dưới hiện chỉ trả về kết quả placeholder để test luồng giao diện."
    ),

    shiny::selectInput(ns("library"), "Thư viện phổ MS2",
                        choices = c("MS-DIAL (positive)", "MS-DIAL (negative)", "Cả hai")),

    shiny::actionButton(ns("annotate_btn"), "Annotate", class = "btn lfs-btn-primary"),

    shiny::uiOutput(ns("result_area"))
  )
}

placeholder_server <- function(id, pipeline_state) {
  shiny::moduleServer(id, function(input, output, session) {

    output$inherited_summary <- shiny::renderUI({
      if (is.null(pipeline_state$mass_dataset)) {
        return(shiny::p(class = "lfs-hint", "Chưa có dữ liệu - hoàn thành bước Peak picking trước."))
      }
      n_features <- tryCatch(nrow(pipeline_state$mass_dataset@variable_info), error = function(e) NA)
      shiny::p(class = "lfs-hint",
               paste0("Đã nhận ", n_features, " feature từ Peak Picking (tự động, không cần upload lại)."))
    })

    shiny::observeEvent(input$annotate_btn, {
      shiny::req(pipeline_state$mass_dataset)

      var_info <- pipeline_state$mass_dataset@variable_info
      var_info$annotation <- "Chưa annotate (đang chờ tích hợp metid + MS-DIAL)"
      var_info$library_selected <- input$library

      pipeline_state$annotation_result <- var_info
      pipeline_state$annotation_done <- TRUE
    })

    output$result_area <- shiny::renderUI({
      shiny::req(pipeline_state$annotation_done)
      DT::DTOutput(session$ns("result_table"))
    })
    output$result_table <- DT::renderDT({
      shiny::req(pipeline_state$annotation_result)
      DT::datatable(pipeline_state$annotation_result, options = list(scrollX = TRUE, pageLength = 10))
    })
  })
}
