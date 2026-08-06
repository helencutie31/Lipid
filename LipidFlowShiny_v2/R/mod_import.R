# Data Import panel - the ONE place in the app where you provide files or
# paths. Steps 1-3 only show run parameters and an action button; every
# file/path input for all 3 steps lives here instead, staged into shared
# pipeline_state for each step's server to read when its button is clicked.
#
# Trade-off worth stating plainly: moving these fields out of
# peak_picking.R/placeholder.R/quantification.R means their input IDs now
# live under this module's namespace instead of each step's own - the
# *local* id strings (files_pos, ms2_pos, is_table, etc.) are unchanged,
# but their full reactive path necessarily changes, since they're now
# physically defined in a different module. Unavoidable consequence of
# relocating the fields, not something silently ignored.

mod_import_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::div(
    class = "lfs-step-body",
    
    shiny::h6("1. Peak Picking data"),
    shiny::radioButtons(ns("mode"), NULL,
                        choices = c("Option A: New Analysis" = "new", "Option B: Load Existing Result" = "existing"),
                        selected = "new", inline = TRUE),
    
    shiny::conditionalPanel(
      condition = "input.mode == 'new'", ns = ns,
      shiny::fluidRow(
        shiny::column(6, shiny::fileInput(ns("files_pos"), shiny::HTML("<b>Mass spectrum raw data</b> - positive mode"),
                                          multiple = TRUE, accept = c(".mzXML", ".mgf"))),
        shiny::column(6, shiny::fileInput(ns("files_neg"), shiny::HTML("<b>Mass spectrum raw data</b> - negative mode"),
                                          multiple = TRUE, accept = c(".mzXML", ".mgf")))
      ),
      shiny::p(class = "lfs-hint", shiny::strong("Files must be .mzXML or .mgf format.")),
      
      shiny::uiOutput(ns("group_preview")),
      shiny::div(class = "lfs-hint-row",
                 shiny::span("Or use demo data: "),
                 shiny::actionLink(ns("load_demo"), shiny::HTML("&#128190; Load demo data")))
    ),
    shiny::conditionalPanel(
      condition = "input.mode == 'existing'", ns = ns,
      shiny::fluidRow(
        shiny::column(6, shiny::fileInput(ns("existing_pos"), shiny::HTML("<b>Saved result</b> - POS (object file)"))),
        shiny::column(6, shiny::fileInput(ns("existing_neg"), shiny::HTML("<b>Saved result</b> - NEG (object file)")))
      )
    ),
    
    shiny::tags$hr(),
    shiny::h6("2. Lipid Annotation data"),
    shiny::fluidRow(
      shiny::column(6, shiny::fileInput(ns("ms2_pos"), shiny::HTML("<b>MS2 spectra</b> - positive mode"), multiple = TRUE,
                                        accept = c(".mzXML", ".mzML", ".mgf", ".msp"))),
      shiny::column(6, shiny::fileInput(ns("ms2_neg"), shiny::HTML("<b>MS2 spectra</b> - negative mode"), multiple = TRUE,
                                        accept = c(".mzXML", ".mzML", ".mgf", ".msp")))
    ),
    
    # ---- NEW UI: Database Selection Toggle ----
    shiny::radioButtons(ns("db_source"), "Database Source",
                        choices = c("1. MS-DIAL database" = "builtin", 
                                    "2. Upload custom database (.rda)" = "custom"),
                        selected = "builtin"),
    
    shiny::conditionalPanel(
      condition = "input.db_source == 'builtin'", ns = ns,
      shiny::fluidRow(
        shiny::column(6, 
                      shiny::p(shiny::HTML("<b>Spectral database</b> - positive mode (.rda)")),
                      shiny::p(style="color:#2B6E63; font-weight:bold; font-size: 1.2em;", "\u2713")
        ),
        shiny::column(6, 
                      shiny::p(shiny::HTML("<b>Spectral database</b> - negative mode (.rda)")),
                      shiny::p(style="color:#2B6E63; font-weight:bold; font-size: 1.2em;", "\u2713")
        )
      )
    ),
    
    shiny::conditionalPanel(
      condition = "input.db_source == 'custom'", ns = ns,
      shiny::fluidRow(
        shiny::column(6, shiny::fileInput(ns("db_pos"), shiny::HTML("<b>Spectral database</b> - positive mode (.rda)"), accept = ".rda")),
        shiny::column(6, shiny::fileInput(ns("db_neg"), shiny::HTML("<b>Spectral database</b> - negative mode (.rda)"), accept = ".rda"))
      )
    ),
    
    shiny::uiOutput(ns("db_status")),
    
    shiny::tags$hr(),
    shiny::h6("3. Quantification data"),
    shiny::fileInput(ns("is_table"), shiny::HTML("<b>Internal Standard table</b> (.xlsx)"), accept = ".xlsx"),
    shiny::uiOutput(ns("is_table_status"))
  )
}

mod_import_server <- function(id, pipeline_state, shared_data_path) {
  shiny::moduleServer(id, function(input, output, session) {
    
    pipeline_state$pp_data_source <- "upload"
    
    # ---- State variables to accumulate uploaded files ----
    acc_files_pos <- shiny::reactiveVal(data.frame())
    acc_files_neg <- shiny::reactiveVal(data.frame())
    
    shiny::observeEvent(input$load_demo, {
      shiny::req(requireNamespace("lipidflow", quietly = TRUE))
      demo_path <- file.path(tempdir(), "lfs_demo_data")
      if (!dir.exists(demo_path)) {
        dir.create(demo_path)
        file.copy(system.file("POS", package = "lipidflow"), demo_path, recursive = TRUE)
        file.copy(system.file("NEG", package = "lipidflow"), demo_path, recursive = TRUE)
      }
      
      # Clear any accumulated user files if demo is loaded
      acc_files_pos(data.frame())
      acc_files_neg(data.frame())
      pipeline_state$pp_files_pos <- NULL
      pipeline_state$pp_files_neg <- NULL
      
      pipeline_state$pp_data_source <- "path"
      pipeline_state$pp_server_path <- demo_path
      shared_data_path(demo_path)
      shiny::showNotification("Demo data loaded successfully!", type = "message", duration = 4)
    })
    
    # ---- mirror state ----
    shiny::observe({ pipeline_state$pp_mode <- input$mode })
    
    # Accumulate POS files over multiple uploads
    shiny::observeEvent(input$files_pos, {
      shiny::req(input$files_pos)
      current <- acc_files_pos()
      new_f <- input$files_pos
      combined <- rbind(current, new_f)
      combined <- combined[!duplicated(combined$name), ] # Prevent exact duplicate names
      acc_files_pos(combined)
      
      pipeline_state$pp_files_pos <- combined
      pipeline_state$pp_data_source <- "upload"
      shiny::showNotification("Added POS raw data files!", type = "message", duration = 3)
    })
    
    # Accumulate NEG files over multiple uploads
    shiny::observeEvent(input$files_neg, {
      shiny::req(input$files_neg)
      current <- acc_files_neg()
      new_f <- input$files_neg
      combined <- rbind(current, new_f)
      combined <- combined[!duplicated(combined$name), ] # Prevent exact duplicate names
      acc_files_neg(combined)
      
      pipeline_state$pp_files_neg <- combined
      pipeline_state$pp_data_source <- "upload"
      shiny::showNotification("Added NEG raw data files!", type = "message", duration = 3)
    })
    
    # Button handler to clear all accumulated files
    shiny::observeEvent(input$clear_files, {
      acc_files_pos(data.frame())
      acc_files_neg(data.frame())
      pipeline_state$pp_files_pos <- NULL
      pipeline_state$pp_files_neg <- NULL
      shiny::showNotification("Cleared all uploaded raw data files.", type = "warning", duration = 3)
    })
    
    shiny::observeEvent(input$existing_pos, {
      pipeline_state$pp_existing_pos <- input$existing_pos
      shiny::showNotification("Saved result (POS) loaded successfully!", type = "message", duration = 4)
    })
    
    shiny::observeEvent(input$existing_neg, {
      pipeline_state$pp_existing_neg <- input$existing_neg
      shiny::showNotification("Saved result (NEG) loaded successfully!", type = "message", duration = 4)
    })
    
    # ---- Chunk: sample-group preview, updating with accumulated files ----
    output$group_preview <- shiny::renderUI({
      pos_names <- if (nrow(acc_files_pos()) > 0) acc_files_pos()$name else character()
      neg_names <- if (nrow(acc_files_neg()) > 0) acc_files_neg()$name else character()
      all_names <- c(pos_names, neg_names)
      
      if (length(all_names) == 0) return(NULL)
      
      preview <- .lfs_build_group_preview(all_names)
      shiny::tagList(
        shiny::div(style = "display: flex; justify-content: space-between; align-items: baseline; margin-bottom: 5px;",
                   shiny::p(class = "lfs-hint", shiny::strong(sprintf("Samples will be grouped as (%d files):", length(all_names)))),
                   shiny::actionLink(session$ns("clear_files"), "Clear all files", style = "color:#B84C3C; font-size: 0.9em; text-decoration: underline;")
        ),
        DT::renderDataTable(DT::datatable(preview, options = list(dom = "t", pageLength = 20), rownames = FALSE))
      )
    })
    
    shiny::observe({ pipeline_state$annot_ms2_pos <- input$ms2_pos })
    shiny::observe({ pipeline_state$annot_ms2_neg <- input$ms2_neg })
    
    shiny::observeEvent(list(input$ms2_pos, input$ms2_neg), {
      shiny::req(input$ms2_pos, input$ms2_neg)
      shiny::showNotification("MS2 spectra loaded successfully!", type = "message", duration = 4)
    })
    
    # ---- NEW SERVER: Handle Database Toggle Logic ----
    shiny::observeEvent(input$db_source, {
      if (input$db_source == "builtin") {
        # Direct relative path to the data folder
        pos_path <- file.path("data", "msdial_lipid_pos_db.rda")
        neg_path <- file.path("data", "msdial_lipid_neg_db.rda")
        
        # Pass path silently if file exists
        pipeline_state$annot_db_pos_path <- if (file.exists(pos_path)) pos_path else NULL
        pipeline_state$annot_db_neg_path <- if (file.exists(neg_path)) neg_path else NULL
        
        # Clean up old text variables to hide them from UI
        pipeline_state$annot_db_pos_label <- NULL
        pipeline_state$annot_db_neg_label <- NULL
        pipeline_state$db_pos_error <- NULL
        pipeline_state$db_neg_error <- NULL
        
      } else {
        # Reset state if switched to custom upload without a file
        if (is.null(input$db_pos)) {
          pipeline_state$annot_db_pos_path <- NULL
          pipeline_state$annot_db_pos_label <- NULL
        }
        if (is.null(input$db_neg)) {
          pipeline_state$annot_db_neg_path <- NULL
          pipeline_state$annot_db_neg_label <- NULL
        }
      }
    })
    
    # Handle custom uploads
    shiny::observeEvent(input$db_pos, {
      shiny::req(input$db_source == "custom")
      pipeline_state$annot_db_pos_path <- input$db_pos$datapath
      pipeline_state$annot_db_pos_label <- input$db_pos$name
      shiny::showNotification("Custom POS database loaded successfully!", type = "message", duration = 4)
    })
    
    shiny::observeEvent(input$db_neg, {
      shiny::req(input$db_source == "custom")
      pipeline_state$annot_db_neg_path <- input$db_neg$datapath
      pipeline_state$annot_db_neg_label <- input$db_neg$name
      shiny::showNotification("Custom NEG database loaded successfully!", type = "message", duration = 4)
    })
    
    # Render UI status (Only shown when custom upload is used)
    output$db_status <- shiny::renderUI({
      # Hide this output completely if using built-in database for a clean UI
      if (input$db_source == "builtin") return(NULL)
      
      rows <- list()
      if (!is.null(pipeline_state$annot_db_pos_label)) rows[[length(rows) + 1]] <- shiny::p(class = "lfs-hint", style = "color:#2B6E63;", paste("\u2713 Uploaded POS:", pipeline_state$annot_db_pos_label))
      if (!is.null(pipeline_state$annot_db_neg_label)) rows[[length(rows) + 1]] <- shiny::p(class = "lfs-hint", style = "color:#2B6E63;", paste("\u2713 Uploaded NEG:", pipeline_state$annot_db_neg_label))
      shiny::tagList(rows)
    })
    
    # ---- Chunk 3: validate the IS table ----
    shiny::observeEvent(input$is_table, {
      check <- .lfs_validate_is_table(input$is_table$datapath)
      if (isTRUE(check$ok)) {
        pipeline_state$quant_is_table <- input$is_table
        pipeline_state$quant_is_names <- .lfs_is_names(input$is_table$datapath)
        pipeline_state$is_table_error <- NULL
        shiny::showNotification("Internal Standard table loaded successfully!", type = "message", duration = 4)
      } else {
        pipeline_state$quant_is_table <- NULL
        pipeline_state$quant_is_names <- character()
        pipeline_state$is_table_error <- check$message
      }
    })
    
    output$is_table_status <- shiny::renderUI({
      if (!is.null(pipeline_state$is_table_error)) {
        return(shiny::p(class = "lfs-hint", style = "color:#B84C3C;", pipeline_state$is_table_error))
      }
      if (!is.null(pipeline_state$quant_is_table)) {
        return(shiny::p(class = "lfs-hint", style = "color:#2B6E63;",
                        paste0("\u2713 Valid - ", length(pipeline_state$quant_is_names), " internal standards found.")))
      }
      NULL
    })
  })
}