library(shiny)
library(bslib)
library(ggplot2)
library(r4ss)
library(reshape2)
library(plotly)

ui <- page_sidebar(
  title = "Indicator Analysis",
  sidebar = sidebar(
    card(
      card_header("Data Input"),
      fileInput(
        "rdata_file",
        "Upload R data object saved from using the SS_output() for a SS3 model",
        buttonLabel = "Browse model output file",
        accept = c(".RData", ".rda", "rds")
      ),
      #      helpText("Upload an RData file containing r4ss model output")
    ),
    conditionalPanel(
      condition = "input.tabs == 'tab1'",
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
          width = "100%",
          style = "color: #fff; background-color: #183de3; border-color: #91caf6"
        )
      )
    ),
    conditionalPanel(
      condition = "input.tabs == 'tab2'",
      card(
        card_header("Mean length options"),
        selectInput(
          "fleet_num_mlt",
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
          "run_analysis_mlt",
          "Run Length Analysis",
          class = "btn-primary",
          width = "100%",
          style = "color: #fff; background-color: #17532a; border-color: #4bf18b"
        )
      )
    )
  ),

  navset_card_tab(
    id = "tabs",
    nav_panel(
      title = "Index Indicator Method",
      value = "tab1",
      layout_columns(
        col_widths = c(6, 6, 6, 6),
        card(
          full_screen = TRUE,
          card_header("Depletion vs Index"),
          plotlyOutput("lm_plot", height = "500px")
        ),
        card(
          full_screen = TRUE,
          card_header("Index Time Series"),
          plotlyOutput("index_plot")
        ),
        card(
          full_screen = TRUE,
          card_header("Data table"),
          tableOutput("data_table")
        ),
        card(
          full_screen = TRUE,
          card_header("Linear Model Summary"),
          verbatimTextOutput("model_summary")
        )
      )
    ),
    nav_panel(
      title = "Mean Length Indicator",
      value = "tab2",
      layout_columns(
        col_widths = c(6, 6, 6, 6),
        card(
          full_screen = TRUE,
          card_header("Depletion vs Indicator"),
          plotlyOutput("lm_plot_mlt", height = "500px")
        ),
        card(
          full_screen = TRUE,
          card_header("Mean Length Time Series"),
          plotlyOutput("mlt_plot", height = "500px")
        ),
        card(
          full_screen = TRUE,
          card_header("Data Table"),
          tableOutput("data_table_mlt")
        ),
        card(
          full_screen = TRUE,
          card_header("Linear Model Summary"),
          verbatimTextOutput("model_summary_mlt")
        )
      )
    )
  )
)
