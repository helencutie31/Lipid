# Top-level server. Each nav_panel's content is a module - this just starts
# each module's server logic against the matching id used in app_ui.R.
#
# home_server needs the *top-level* session and the navbar's id so its
# "Open quantification ->" button can switch tabs via bslib::nav_select().
#
# shared_data_path is a single reactiveVal passed into both Peak Picking and
# Quantification, since both operate on the same path/POS,NEG/... folder -
# typing it in one tab fills it in on the other (see the sync observers at
# the top of each module's server function).

app_server <- function(input, output, session) {
  shared_data_path <- shiny::reactiveVal("")

  home_server("home", parent_session = session, parent_nav_id = "main_nav")
  peak_picking_server("peak_picking", shared_data_path = shared_data_path)
  placeholder_server("annotation")
  quantification_server("quant", shared_data_path = shared_data_path)
}
