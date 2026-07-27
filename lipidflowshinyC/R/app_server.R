# Top-level server. Each nav_panel's content is a module - this just starts
# each module's server logic against the matching id used in app_ui.R.
#
# mod_home_server needs the *top-level* session and the navbar's id so its
# "Open quantification ->" button can switch tabs via bslib::nav_select().
#
# shared_data_path is a single reactiveVal passed into both Peak Picking and
# Quantification, since both operate on the same path/POS,NEG/... folder -
# typing it in one tab fills it in on the other (see the sync observers at
# the top of each module's server function).

app_server <- function(input, output, session) {
  shared_data_path <- shiny::reactiveVal("")

  mod_home_server("home", parent_session = session, parent_nav_id = "main_nav")
  mod_peak_picking_server("peak_picking", shared_data_path = shared_data_path)
  mod_placeholder_server("annotation")
  mod_quantification_server("quant", shared_data_path = shared_data_path)
}
