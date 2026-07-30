# Top-level server. Home needs the top-level session + navbar id so its
# "Get Started" button can switch to the lipidAnalysis tab. lipidAnalysis
# owns everything else (its own accordion + shared pipeline state) - see
# R/lipid_analysis.R.

app_server <- function(input, output, session) {
  home_server("home", parent_session = session, parent_nav_id = "main_nav")
  lipid_analysis_server("analysis")
}
