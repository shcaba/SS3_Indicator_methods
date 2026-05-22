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
      card_header("Parameters"),
      numericInput(
        "fleet_num",
        "Fleet Number",
        value = 1,
        min = 1,
        step = 1
      ),
      numericInput(
        "index_rp",
        "Index Reference Point",
        value = 0.25,
        min = 0,
        max = 1,
        step = 0.05
      ),
      actionButton(
        "run_analysis",
        "Run Analysis",
        class = "btn-primary",
        width = "100%"
      )
    ),
    card(
      card_header("Parameters"),
      numericInput(
        "fleet_num",
        "Fleet Number",
        value = 1,
        min = 1,
        step = 1
      ),
      numericInput(
        "index_rp",
        "Index Reference Point",
        value = 0.25,
        min = 0,
        max = 1,
        step = 0.05
      ),
      actionButton(
        "run_analysis",
        "Run Analysis",
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
