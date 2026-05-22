library(shiny)
library(bslib)
library(ggplot2)
library(r4ss)
library(reshape2)

# Define the Index.method function
Index.method <- function(spp.out, fleet.in, Index.RP = 0.25) {
  spp.out.cpue.survey <- subset(spp.out$cpue, Fleet == fleet.in)
  spp.out.cpue.survey <- spp.out.cpue.survey[, c('Yr', 'Obs', 'SE_input')]

  spp.out.dep <- spp.out$sprseries[
    spp.out$sprseries$Yr %in% unique(spp.out.cpue.survey$Yr),
    c('Yr', 'Deplete')
  ]

  spp.out.cpue.dep <- cbind(
    spp.out.dep,
    spp.out.cpue.survey$Obs,
    spp.out.cpue.survey$SE_input
  )
  colnames(spp.out.cpue.dep)[3:4] <- c("Index", "CV")

  # Create plots
  lm.plot <- ggplot(spp.out.cpue.dep, aes(Index, Deplete)) +
    geom_point(size = 3, color = "#2C3E50") +
    geom_smooth(
      formula = y ~ x + 0,
      method = "lm",
      color = "#3498DB",
      fill = "#3498DB",
      alpha = 0.2
    ) +
    labs(
      title = "Depletion vs Index",
      x = "Index",
      y = "Depletion"
    ) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"))

  spp.lm.out <- lm(Deplete ~ Index + 0, data = spp.out.cpue.dep)

  Index.plot <- ggplot(spp.out.cpue.dep, aes(Yr, Index)) +
    geom_point(size = 3, color = "#2C3E50") +
    geom_errorbar(
      aes(
        ymin = Index - (Index * CV * 1.96),
        ymax = Index + (Index * CV * 1.96)
      ),
      width = 0.2,
      color = "#34495E"
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
          (spp.out.cpue.dep$Index * spp.out.cpue.dep$CV * 1.96)
      )
    ) +
    labs(
      title = "Index Over Time with Reference Point",
      x = "Year",
      y = "Index"
    ) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"))

  return(list(
    data = spp.out.cpue.dep,
    lm_plot = lm.plot,
    index_plot = Index.plot,
    lm_model = spp.lm.out
  ))
}
################################

############################
### Mean length function ###
############################
MeanLt.method <- function(spp.out, fleet.in, Sex = 1, Ltm.RP = 0.25) {
  spp.out.Ltm <- subset(
    spp.out$len_comp_fit_table,
    Fleet == fleet.in & Sexes == Sex
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

  spp.out.Ltm.dep <- cbind(
    spp.out.dep,
    spp.out.Ltm$All_obs_mean,
    spp.out.Ltm$'All_exp_5%',
    spp.out.Ltm$'All_exp_95%'
  )
  colnames(spp.out.Ltm.dep)[3:5] <- c("Mean_Lt", "Lt5", "Lt95")

  lm.meanlt.plot <- ggplot(spp.out.Ltm.dep, aes(Mean_Lt, Deplete)) +
    geom_point() +
    geom_smooth(formula = y ~ x + 0, method = "lm")
  print(lm.meanlt.plot)

  spp.mlt.lm.out <- lm(Deplete ~ Mean_Lt + 0, data = spp.out.Ltm.dep)

  MeanLt.plot <- ggplot(spp.out.Ltm.dep, aes(Yr, Mean_Lt)) +
    geom_point() +
    geom_errorbar(
      ymin = spp.out.Ltm.dep$Lt5,
      ymax = spp.out.Ltm.dep$Lt95,
      width = 0.2
    ) +
    geom_hline(
      yintercept = Ltm.RP / spp.mlt.lm.out$coefficients[1],
      col = "red"
    ) +
    ylim(
      0,
      max(spp.out.Ltm.dep$Lt95, 0.9 / spp.mlt.lm.out$coefficients[1])
    )
  print(MeanLt.plot)
  return(spp.out.Ltm.dep)
}
##########################################
##########################################

server <- function(input, output, session) {
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

  #Fleet names and sex for mean length

  fleet.name2 <- reactive({
    unique(spp_data()$len_comp_fit_table$Fleet_name)
  })

  observe({
    updateSelectInput(
      session,
      "fleet_num_mtl",
      label = "Fleet Name",
      choices = fleet.name2()
    )
  })

  sex.mtl <- reactive({
    if (!is.null(spp_data())) {
      Lts.fleet <- subset(
        spp_data()$len_comp_fit_table,
        Fleet_Name == input$fleet_num_mtl
      )
      unique(Lts.fleet$Sexes)
    }
  })

  observe({
    updateSelectInput(
      session,
      "sex_num",
      label = "Sex option",
      choices = sex.mtl()
    )
  })

  # Run analysis when button is clicked
  analysis_results <- eventReactive(input$run_analysis_index, {
    req(spp_data())

    tryCatch(
      {
        Index.method(
          spp.out = spp_data(),
          fleet.in = fleet.name1$Fleet.number[
            fleet.name1$Fleet.name == input$fleet_num
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

  # Output: LM Plot
  output$lm_plot <- renderPlot({
    req(analysis_results())
    analysis_results()$lm_plot
  })

  # Output: Index Plot
  output$index_plot <- renderPlot({
    req(analysis_results())
    analysis_results()$index_plot
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
}
