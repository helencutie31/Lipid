# Step 3: Quantification.
#
# HONESTY NOTE, same as placeholder.R: lipidflow::get_lipid_absolute_quantification()
# needs a properly-structured LipidSearch-style annotation table (Class
# column etc.) as input - step 2's placeholder doesn't produce that yet, so
# this step can't honestly call the real function until Giai đoạn 4 exists.
# What's real here: automatic inheritance of step 2's result (no
# re-navigation, no re-upload of features), and the UI shape. The internal
# standard table is a genuinely new input at this step (not something
# earlier steps produce), so it's the one small upload that's still here.

quantification_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::div(
    class = "lfs-step-body",
    shiny::uiOutput(ns("inherited_summary")),

    shiny::div(
      class = "lfs-note",
      shiny::strong("Chưa có thuật toán thật ở bước này."),
      " lipidflow::get_lipid_absolute_quantification() cần bảng annotation kiểu LipidSearch thật ",
      "(có cột Class) làm input - bước 2 hiện chỉ trả placeholder nên bước này cũng vậy. ",
      "Nút Quantify bên dưới trả kết quả placeholder để test luồng giao diện."
    ),

    shiny::fileInput(ns("is_table"), "Bảng Internal Standard (.xlsx)", accept = ".xlsx"),
    shiny::selectizeInput(ns("is_selected"), "Chọn Internal Standard dùng để định lượng",
                           choices = NULL, multiple = TRUE, width = "100%"),

    shiny::actionButton(ns("quantify_btn"), "Quantify", class = "btn lfs-btn-primary"),

    shiny::uiOutput(ns("result_area"))
  )
}

quantification_server <- function(id, pipeline_state) {
  shiny::moduleServer(id, function(input, output, session) {

    output$inherited_summary <- shiny::renderUI({
      if (is.null(pipeline_state$annotation_result)) {
        return(shiny::p(class = "lfs-hint", "Chưa có dữ liệu - hoàn thành bước Lipid annotation trước."))
      }
      shiny::p(class = "lfs-hint",
               paste0("Đã nhận ", nrow(pipeline_state$annotation_result),
                      " feature đã annotate từ bước 2 (tự động, không cần upload lại)."))
    })

    shiny::observeEvent(input$is_table, {
      names <- .lfs_is_names(input$is_table$datapath)
      shiny::updateSelectizeInput(session, "is_selected", choices = names)
    })

    shiny::observeEvent(input$quantify_btn, {
      shiny::req(pipeline_state$annotation_result)

      result <- pipeline_state$annotation_result
      result$is_used <- if (length(input$is_selected)) paste(input$is_selected, collapse = "; ") else "(chưa chọn IS)"
      result$quant_value <- NA_real_
      result$note <- "Placeholder - cần Annotation thật (Giai đoạn 4) để tính đúng"

      pipeline_state$quant_result <- result
      pipeline_state$quant_done <- TRUE
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
