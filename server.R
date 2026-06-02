library(shiny)
library(bslib)
library(ggplot2)
library(r4ss)
library(reshape2)
library(plotly)

# Define the Index.method function
Index.method <- function(spp.out, fleet.in, Index.RP = 0.25) {
  spp.out.cpue.survey <- subset(spp.out$cpue, Fleet == fleet.in)
  spp.out.cpue.survey <- spp.out.cpue.survey[, c('Yr', 'Obs', 'SE_input')]

  spp.out.dep <- spp.out$sprseries[
    spp.out$sprseries$Yr %in% unique(spp.out.cpue.survey$Yr),
    c('Yr', 'Deplete')
  ]

  RP.test <- spp.out.dep$Deplete <= Index.RP
  RP.test[RP.test == "TRUE"] <- "BELOW RP"
  RP.test[RP.test == "FALSE"] <- "ABOVE RP"

  spp.out.cpue.dep <- cbind(
    spp.out.dep,
    spp.out.cpue.survey$Obs,
    spp.out.cpue.survey$SE_input,
    RP.test
  )
  colnames(spp.out.cpue.dep)[3:5] <- c("Index", "CV", "RP.test")

  # Create plots
  lm.plot <- ggplot(spp.out.cpue.dep, aes(Index, Deplete)) +
    geom_point(size = 3, color = "#2C3E50") +
    geom_smooth(
      formula = y ~ x + 0,
      method = "lm",
      weight = "CV",
      color = "#3498DB",
      fill = "#3498DB",
      alpha = 0.2
    ) +
    labs(
      title = "Index vs Depletion",
      x = "Index",
      y = "Depletion"
    ) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"))

  spp.lm.out <- lm(
    Deplete ~ Index + 0,
    data = spp.out.cpue.dep,
    weights = spp.out.cpue.dep$CV
  )

  Index.plot <- ggplot(data = spp.out.cpue.dep) +
    geom_errorbar(
      aes(
        Yr,
        ymin = Index - (Index * CV * 1.96),
        ymax = Index + (Index * CV * 1.96),
        color = RP.test
      ),
      width = 0.2
    ) +
    geom_point(
      #data = spp.out.cpue.dep,
      aes(Yr, Index, color = RP.test),
      size = 3
    ) +
    scale_color_manual(
      name = "RP.test",
      values = c(
        "BELOW RP" = "darkred",
        "ABOVE RP" = "green"
      )
    ) +
    geom_hline(
      yintercept = Index.RP / spp.lm.out$coefficients[1],
      col = "#E74C3C",
      linetype = "dashed",
      linewidth = 1
    ) +
    ylim(
      0,
      max(
        spp.out.cpue.dep$Index +
          (spp.out.cpue.dep$Index * spp.out.cpue.dep$CV * 1.96),
        Index.RP / spp.lm.out$coefficients[1]
      )
    ) +
    labs(
      title = "Index Over Time with Reference Point",
      x = "Year",
      y = "Index"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      legend.position = "none"
    )

  return(list(
    data = spp.out.cpue.dep,
    lm_plot = lm.plot,
    index_plot = Index.plot,
    lm_model = spp.lm.out,
    RP.out = Index.RP / spp.lm.out$coefficients[1]
  ))
}
################################

############################
### Mean length function ###
############################
MeanLt.method <- function(spp.out, fleet.in, Sex = 1, Ltm.RP = 0.25) {
  spp.out.Ltm <- subset(
    spp.out$len_comp_fit_table,
    Fleet_Name == fleet.in & Sexes == Sex
  )
  spp.out.Ltm <- spp.out.Ltm[, c(
    'Yr',
    'All_obs_mean',
    'All_exp_5%',
    'All_exp_95%',
    'Sexes'
  )]

  spp.out.dep <- spp.out$sprseries[
    spp.out$sprseries$Yr %in% spp.out.Ltm$Yr,
    c('Yr', 'Deplete')
  ]

  RP.test <- spp.out.dep$Deplete <= Ltm.RP
  RP.test[RP.test == "TRUE"] <- "BELOW RP"
  RP.test[RP.test == "FALSE"] <- "ABOVE RP"

  spp.out.Ltm.dep <- cbind(
    spp.out.dep,
    spp.out.Ltm$All_obs_mean,
    spp.out.Ltm$'All_exp_5%',
    spp.out.Ltm$'All_exp_95%',
    ((spp.out.Ltm$'All_exp_95%' - spp.out.Ltm$'All_exp_5%') / (2 * 1.96)) /
      spp.out.Ltm$All_obs_mean,
    RP.test
  )
  colnames(spp.out.Ltm.dep)[3:7] <- c("Mean_Lt", "Lt5", "Lt95", "CV", "RP.test")

  lm.meanlt.plot <- ggplot(spp.out.Ltm.dep, aes(Mean_Lt, Deplete)) +
    geom_point(size = 3, color = "#06880c") +
    geom_smooth(
      formula = y ~ x + 0,
      method = "lm",
      weight = "CV",
      color = "#1ef368",
      fill = "#1ef368",
      alpha = 0.2
    ) +
    theme_minimal()
  #print(lm.meanlt.plot)

  spp.mlt.lm.out <- lm(
    Deplete ~ Mean_Lt + 0,
    data = spp.out.Ltm.dep,
    weights = spp.out.Ltm.dep$CV
  )

  MeanLt.plot <- ggplot(data = spp.out.Ltm.dep) +
    geom_errorbar(
      aes(Yr, ymin = Lt5, ymax = Lt95, color = RP.test),
      width = 0.2
    ) +
    geom_point(aes(Yr, Mean_Lt, color = RP.test), size = 2) +
    scale_color_manual(
      name = "RP.test",
      values = c(
        "BELOW RP" = "darkred",
        "ABOVE RP" = "green"
      )
    ) +
    geom_hline(
      yintercept = Ltm.RP / spp.mlt.lm.out$coefficients[1],
      col = "#E74C3C",
      linetype = "dashed",
      linewidth = 1
    ) +
    ylim(
      0,
      max(spp.out.Ltm.dep$Lt95, Ltm.RP / spp.mlt.lm.out$coefficients[1])
    ) +
    theme_minimal() +
    theme(legend.position = "none")

  return(list(
    data = spp.out.Ltm.dep,
    lm_plot = lm.meanlt.plot,
    mlt_plot = MeanLt.plot,
    lm_model = spp.mlt.lm.out,
    RP.out = Ltm.RP / spp.mlt.lm.out$coefficients[1]
  ))
}
##########################################
##########################################

server <- function(input, output, session) {
  options(shiny.maxRequestSize = 30 * 1024^2) #increase max size of upload file
  # Reactive value to store loaded data
  spp_data <- reactiveVal(NULL)

  # Load RData file
  observeEvent(input$rdata_file, {
    req(input$rdata_file)

    tryCatch(
      {
        # Load the RData file
        env <- new.env()
        load(input$rdata_file$datapath, envir = env)

        # Get the first object (assuming it's the r4ss output)
        obj_name <- ls(env)[1]
        spp_data(env[[obj_name]])

        showNotification("Data loaded successfully!", type = "message")
      },
      error = function(e) {
        showNotification(
          paste("Error loading file:", e$message),
          type = "error"
        )
      }
    )
  })

  #Fleet names and number for indices
  fleet.name1 <- reactive({
    data.frame(
      Fleet.name = unique(spp_data()$cpue$Fleet_name),
      Fleet.number = unique(spp_data()$cpue$Fleet)
    )
  })

  observe({
    updateSelectInput(
      session,
      "fleet_num",
      label = "Fleet Name",
      choices = fleet.name1()$Fleet.name
    )
  })

  # Run INDEX analysis when button is clicked
  withProgress(message = 'Calculating indicators', value = 0, {
    analysis_results <- eventReactive(input$run_analysis_index, {
      req(spp_data())

      tryCatch(
        {
          Index.method(
            spp.out = spp_data(),
            fleet.in = fleet.name1()$Fleet.number[
              fleet.name1()$Fleet.name == input$fleet_num
            ],
            Index.RP = input$index_rp
          )
        },
        error = function(e) {
          showNotification(
            paste("Error running analysis:", e$message),
            type = "error"
          )
          NULL
        }
      )
    })
  })

  # Output: LM Plot
  output$lm_plot <- renderPlotly({
    req(analysis_results())
    ggplotly(analysis_results()$lm_plot)
  })

  # Output: Index Plot
  output$index_plot <- renderPlotly({
    req(analysis_results())
    ggplotly(analysis_results()$index_plot)
  })

  # Output: Data Table
  output$data_table <- renderTable(
    {
      req(analysis_results())
      analysis_results()$data
    },
    striped = TRUE,
    hover = TRUE,
    bordered = TRUE
  )

  # Output: Model Summary
  output$model_summary <- renderPrint({
    req(analysis_results())
    summary(analysis_results()$lm_model)
  })

  #############################
  ### Mean Length Indicator ###
  #############################
  #Fleet names and sex for mean length

  fleet.name2 <- reactive({
    unique(spp_data()$len_comp_fit_table$Fleet_Name)
  })

  observe({
    updateSelectInput(
      session,
      "fleet_num_mlt",
      label = "Fleet Name",
      choices = fleet.name2(),
      selected = fleet.name2()[1]
    )
  })

  sex.mlt <- reactive({
    #browser()
    if (!is.null(spp_data())) {
      Lts.fleet <- subset(
        spp_data()$len_comp_fit_table,
        Fleet_Name == input$fleet_num_mlt
      )
      return(unique(Lts.fleet$Sexes))
    }
    #if (exists("Lts.fleet")) {
    #if (is.null(input$fleet_num_mlt)) {
    #   Lts.fleet = 0
    #  }
    #}
  })

  observe({
    updateSelectInput(
      session,
      "sex_num",
      label = "Sex option",
      choices = sex.mlt(),
      selected = sex.mlt()[1]
    )
  })

  # Run Mean Length analysis when button is clicked
  withProgress(message = 'Calculating length indicators', value = 0, {
    analysis_results_mlt <- eventReactive(input$run_analysis_mlt, {
      req(spp_data())

      tryCatch(
        {
          MeanLt.method(
            spp_data(),
            fleet.in = input$fleet_num_mlt,
            Sex = as.numeric(input$sex_num),
            Ltm.RP = input$mlt_rp
          )
        },
        error = function(e) {
          showNotification(
            paste("Error running analysis:", e$message),
            type = "error"
          )
          NULL
        }
      )
    })
  })

  # Output: LM Plot
  output$lm_plot_mlt <- renderPlotly({
    req(analysis_results_mlt())
    ggplotly(analysis_results_mlt()$lm_plot)
  })

  # Output: Mean Length Plot
  output$mlt_plot <- renderPlotly({
    req(analysis_results_mlt())
    ggplotly(analysis_results_mlt()$mlt_plot)
  })

  # Output: Data Table
  output$data_table_mlt <- renderTable(
    {
      req(analysis_results_mlt())
      analysis_results_mlt()$data
    },
    striped = TRUE,
    hover = TRUE,
    bordered = TRUE
  )

  # Output: Model Summary
  output$model_summary_mlt <- renderPrint({
    req(analysis_results_mlt())
    summary(analysis_results_mlt()$lm_model)
  })
}
