library(shiny)
library(bslib)

# --- CUSTOM CSS STYLING (SHENLAB / FEATUREMSEA STYLE) ---
custom_css <- tags$head(
  tags$style(HTML("
    /* Import Modern Font */
    @import url('https://fonts.googleapis.com/css2?family=Inter:wght@300;400;500;600;700&display=swap');
    
    body {
      font-family: 'Inter', sans-serif;
      background-color: #f8fafc;
    }
    
    /* Top Navbar Customization */
    .navbar {
      background-color: #1e293b !important;
      border-bottom: 1px solid #334155;
      padding: 0.8rem 1.5rem;
    }
    .navbar-brand {
      font-weight: 700;
      font-size: 1.4rem;
      letter-spacing: -0.5px;
      color: #ffffff !important;
    }
    
    /* Hero Banner Gradient Box */
    .hero-banner {
      background: linear-gradient(135deg, #0f172a 0%, #0d9488 60%, #10b981 100%);
      color: white;
      border-radius: 16px;
      padding: 45px 40px;
      margin-bottom: 35px;
      box-shadow: 0 10px 25px -5px rgba(13, 148, 136, 0.3);
      position: relative;
      overflow: hidden;
    }
    
    .hero-title {
      font-size: 2.8rem;
      font-weight: 800;
      margin-bottom: 12px;
      letter-spacing: -1px;
    }
    
    .hero-subtitle {
      font-size: 1.25rem;
      font-weight: 500;
      opacity: 0.95;
      margin-bottom: 16px;
      color: #e2e8f0;
    }
    
    .hero-description {
      font-size: 0.98rem;
      line-height: 1.6;
      max-width: 850px;
      opacity: 0.88;
      margin-bottom: 28px;
    }
    
    /* Custom Call-to-Action Buttons */
    .btn-hero-primary {
      background-color: #ffffff !important;
      color: #0f172a !important;
      font-weight: 600 !important;
      border: none !important;
      padding: 10px 26px !important;
      border-radius: 8px !important;
      margin-right: 12px;
      transition: all 0.2s ease;
    }
    .btn-hero-primary:hover {
      background-color: #f1f5f9 !important;
      transform: translateY(-2px);
      box-shadow: 0 4px 12px rgba(0,0,0,0.15);
    }
    
    .btn-hero-outline {
      background-color: transparent !important;
      color: #ffffff !important;
      font-weight: 600 !important;
      border: 1.5px solid rgba(255, 255, 255, 0.6) !important;
      padding: 10px 26px !important;
      border-radius: 8px !important;
      transition: all 0.2s ease;
    }
    .btn-hero-outline:hover {
      border-color: #ffffff !important;
      background-color: rgba(255, 255, 255, 0.1) !important;
    }
    
    /* Workflow Cards */
    .workflow-card {
      background: #ffffff;
      border: 1px solid #e2e8f0;
      border-radius: 12px;
      padding: 24px;
      transition: all 0.25s ease;
      height: 100%;
    }
    .workflow-card:hover {
      transform: translateY(-5px);
      box-shadow: 0 12px 20px -5px rgba(0, 0, 0, 0.08);
      border-color: #0d9488;
    }
    .workflow-number {
      display: inline-block;
      width: 32px;
      height: 32px;
      background-color: #ccfbf1;
      color: #0d9488;
      font-weight: 700;
      border-radius: 8px;
      text-align: center;
      line-height: 32px;
      margin-bottom: 12px;
    }
    .workflow-title {
      font-size: 1.1rem;
      font-weight: 700;
      color: #1e293b;
      margin-bottom: 8px;
    }
    .workflow-text {
      font-size: 0.88rem;
      color: #64748b;
      line-height: 1.5;
    }
  "))
)

# --- USER INTERFACE (UI) ---
ui <- page_navbar(
  title = "LipidFlow",
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  header = custom_css,
  
  # 1. HOME TAB
  nav_panel(
    title = "Home",
    icon = icon("house"),
    
    div(
      style = "max-width: 1300px; margin: 0 auto; padding: 20px 10px;",
      
      # Hero Banner Section (ShenLab featureMSEA style)
      div(
        class = "hero-banner",
        div(
          class = "row align-items-center",
          div(
            class = "col-lg-9",
            div(class = "hero-title", "LipidFlow"),
            div(class = "hero-subtitle", "Open-source Untargeted Lipidomics & Functional Enrichment Analysis"),
            div(
              class = "hero-description",
              "LipidFlow provides an end-to-end open-source pipeline designed to replace proprietary vendor software. ",
              "It seamlessly integrates LC-MS peak picking, Goslin-standardized structural annotation, ",
              "robust quantification, and rank-based Lipid Set Enrichment Analysis (LipidMSEA) into an interactive platform."
            ),
            div(
              actionButton("btn_get_started", "Get Started", icon = icon("play"), class = "btn-hero-primary"),
              actionButton("btn_docs", "Documentation", icon = icon("book"), class = "btn-hero-outline")
            )
          ),
          div(
            class = "col-lg-3 d-none d-lg-block text-end",
            # Decorative Hexagon / Logo Placeholder
            div(
              style = "font-size: 7rem; opacity: 0.25; color: white;",
              icon("dna")
            )
          )
        )
      ),
      
      # Overview Title
      div(
        style = "margin-bottom: 20px;",
        h3(style = "font-weight: 700; color: #0f172a;", "Overview of LipidFlow Pipeline"),
        p(style = "color: #64748b;", "Four modular steps delivering rigorous lipidomic profiling from raw LC-MS files to biological insights.")
      ),
      
      # 4 Workflow Step Cards
      layout_columns(
        fill = FALSE,
        gap = "20px",
        
        div(
          class = "workflow-card",
          div(class = "workflow-number", "1"),
          div(class = "workflow-title", "Peak Picking"),
          div(class = "workflow-text", "CentWave chromatographic peak detection optimized for RPLC lipidomics in both positive and negative polarities.")
        ),
        
        div(
          class = "workflow-card",
          div(class = "workflow-number", "2"),
          div(class = "workflow-title", "Annotation"),
          div(class = "workflow-text", "MS1/MS2 accurate mass matching with adduct rules, standardized strictly according to Goslin lipid shorthand nomenclature.")
        ),
        
        div(
          class = "workflow-card",
          div(class = "workflow-number", "3"),
          div(class = "workflow-title", "Quantification"),
          div(class = "workflow-text", "Integrated peak area extraction, missing value imputation, and LOESS quality control batch normalization.")
        ),
        
        div(
          class = "workflow-card",
          div(class = "workflow-number", "4"),
          div(class = "workflow-title", "Lipid MSEA"),
          div(class = "workflow-text", "Rank-based Feature Lipid Set Enrichment Analysis enabling mechanistic interpretation without significance cutoffs.")
        )
      )
    )
  ),
  
  # 2. MODULE TAB PLACEHOLDERS
  nav_panel("1. Peak Picking", icon = icon("chart-line"), h4("Module 1: Peak Picking Interface")),
  nav_panel("2. Annotation", icon = icon("tags"), h4("Module 2: Annotation Interface")),
  nav_panel("3. Quantification", icon = icon("calculator"), h4("Module 3: Quantification Interface")),
  nav_panel("4. Lipid MSEA", icon = icon("network-wired"), h4("Module 4: Functional Enrichment Interface"))
)

# --- SERVER LOGIC ---
server <- function(input, output, session) {
  # Direct 'Get Started' button to Tab 1
  observeEvent(input$btn_get_started, {
    nav_select(id = "navbar", selected = "1. Peak Picking")
  })
}

# --- RUN APPLICATION ---
shinyApp(ui = ui, server = server)