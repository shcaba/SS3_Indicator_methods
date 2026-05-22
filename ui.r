library(shiny)
library(bslib)
library(ggplot2)
library(r4ss)
library(reshape2)

ui <- page_sidebar(
  title = "Indicator Analysis",
  sidebar = sidebar(
    card(
      card_header("Data Input"),
      fileInput(
        "rdata_file",
        "Upload r4ss Output (.RData)",
        accept = c(".RData", ".rda", "rds")
      ),
      helpText("Upload an RData file containing r4ss model output")
    ),
    card(
      card_header("Index options"),
      selectInput(
        "fleet_num",
        "Fleet Number",
        choice = NULL
      ),
      numericInput(
        "index_rp",
        "Depletion Reference Point",
        value = 0.25,
        min = 0,
        max = 1,
        step = 0.05
      ),
      actionButton(
        "run_analysis_index",
        "Run Index Analysis",
        class = "btn-primary",
        width = "100%"
      )
    ),
    card(
      card_header("Mean length options"),
      selectInput(
        "fleet_num_mtl",
        "Fleet Name",
        choice = NULL
      ),
      selectInput(
        "sex_num",
        "Sex option",
        choice = NULL
      ),
      numericInput(
        "mlt_rp",
        "Depletion Reference Point",
        value = 0.25,
        min = 0,
        max = 1,
        step = 0.05
      ),
      actionButton(
        "run_analysis_mtl",
        "Run Length Analysis",
        class = "btn-primary",
        width = "100%"
      )
    )
  ),

  navset_card_tab(
    nav_panel(
      "Depletion vs Index",
      plotOutput("lm_plot", height = "500px")
    ),
    nav_panel(
      "Index Time Series",
      plotOutput("index_plot", height = "500px")
    ),
    nav_panel(
      "Data Table",
      card(
        card_header("Analysis Results"),
        tableOutput("data_table")
      )
    ),
    nav_panel(
      "Model Summary",
      card(
        card_header("Linear Model Summary"),
        verbatimTextOutput("model_summary")
      )
    )
  )
)
