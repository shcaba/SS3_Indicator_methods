library(shiny)
library(bslib)
library(ggplot2)
library(r4ss)
library(reshape2)
library(plotly)
library(stringr)
library(vctrs)

# Define the Index.method function
Index.method <- function(spp.out, fleet.in, Index.RP = 0.25) {
  spp.out.cpue.survey <- subset(spp.out$cpue, Fleet == fleet.in)
  spp.out.cpue.survey <- spp.out.cpue.survey[, c('Yr', 'Obs', 'SE')]

  #Extract depletion
  bratio <- spp.out$derived_quants[
    grep("Bratio_", spp.out$derived_quants$Label),
  ]
  spp.out.dep <- data.frame(
    Yr = as.numeric(str_split_i(bratio$Label, "_", 2)),
    Deplete = bratio$Value
  )

  spp.out.dep <- spp.out.dep[
    spp.out.dep$Yr %in% unique(spp.out.cpue.survey$Yr),
  ]
  #spp.out.dep <- spp.out$sprseries[
  #  spp.out$sprseries$Yr %in% unique(spp.out.cpue.survey$Yr),
  #  c('Yr', 'Deplete')
  #]

  RP.test <- spp.out.dep$Deplete <= Index.RP
  RP.test[RP.test == "TRUE"] <- "BELOW RP"
  RP.test[RP.test == "FALSE"] <- "ABOVE RP"

  spp.out.cpue.dep <- cbind(
    spp.out.dep,
    spp.out.cpue.survey$Obs,
    spp.out.cpue.survey$SE,
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
MeanLt.method <- function(
  spp.out,
  data.in,
  fleet.in,
  Sex = 1,
  Part.in = 0,
  Ltm.RP = 0.25,
  Lt.metric = "Mean"
) {
  data.out.Ltm <- subset(
    data.in$lencomp,
    fleet == fleet.in & sex == Sex & part == Part.in
  )

  Lt.vec <- data.in$lbin_vector
  Lt.dat <- data.out.Ltm[, -c(1:6)]
  Lt.dat.F <- Lt.dat[, 1:data.in$N_lbins]
  Lt.dat.M <- Lt.dat[, (data.in$N_lbins + 1):ncol(Lt.dat)]

  if (Sex == 0 | Sex == 1) {
    cdf <- mapply(
      function(x) {
        cumsum(as.numeric(Lt.dat.F[x, ]) / sum(as.numeric(Lt.dat.F[x, ])))
      },
      x = 1:nrow(Lt.dat.F),
      SIMPLIFY = FALSE
    )
    Lt.qtls.05 <- mapply(
      function(x) Lt.vec[min(which(cdf[[x]] >= 0.05))],
      x = 1:length(cdf),
      SIMPLIFY = TRUE
    )
    Lt.qtls.25 <- mapply(
      function(x) Lt.vec[min(which(cdf[[x]] >= 0.25))],
      x = 1:length(cdf),
      SIMPLIFY = TRUE
    )
    Lt.qtls.5 <- mapply(
      function(x) Lt.vec[min(which(cdf[[x]] >= 0.5))],
      x = 1:length(cdf),
      SIMPLIFY = TRUE
    )
    Lt.qtls.75 <- mapply(
      function(x) Lt.vec[min(which(cdf[[x]] >= 0.75))],
      x = 1:length(cdf),
      SIMPLIFY = TRUE
    )
    Lt.qtls.95 <- mapply(
      function(x) Lt.vec[min(which(cdf[[x]] >= 0.95))],
      x = 1:length(cdf),
      SIMPLIFY = TRUE
    )

    Lt.mean <- mapply(
      function(x) sum(Lt.dat.F[x, ] * Lt.vec) / sum(Lt.dat.F[x, ]),
      x = 1:nrow(Lt.dat.F),
      SIMPLIFY = TRUE
    )
  }
  if (Sex == 2) {
    cdf <- mapply(
      function(x) {
        cumsum(as.numeric(Lt.dat.M[x, ]) / sum(as.numeric(Lt.dat.M[x, ])))
      },
      x = 1:nrow(Lt.dat.M),
      SIMPLIFY = FALSE
    )
    Lt.qtls.05 <- mapply(
      function(x) Lt.vec[min(which(cdf[[x]] >= 0.05))],
      x = 1:length(cdf),
      SIMPLIFY = TRUE
    )
    Lt.qtls.25 <- mapply(
      function(x) Lt.vec[min(which(cdf[[x]] >= 0.25))],
      x = 1:length(cdf),
      SIMPLIFY = TRUE
    )
    Lt.qtls.5 <- mapply(
      function(x) Lt.vec[min(which(cdf[[x]] >= 0.5))],
      x = 1:length(cdf),
      SIMPLIFY = TRUE
    )
    Lt.qtls.75 <- mapply(
      function(x) Lt.vec[min(which(cdf[[x]] >= 0.75))],
      x = 1:length(cdf),
      SIMPLIFY = TRUE
    )
    Lt.qtls.95 <- mapply(
      function(x) Lt.vec[min(which(cdf[[x]] >= 0.95))],
      x = 1:length(cdf),
      SIMPLIFY = TRUE
    )

    Lt.mean <- mapply(
      function(x) sum(Lt.dat.M[x, ] * Lt.vec) / sum(Lt.dat.M[x, ]),
      x = 1:nrow(Lt.dat.M),
      SIMPLIFY = TRUE
    )
  }
  if (Sex == 3) {
    Lt.dat.prop <- mapply(
      function(x) Lt.dat[x, ] / sum(Lt.dat[x, ]),
      x = 1:nrow(Lt.dat),
      SIMPLIFY = FALSE
    )
    Lt.dat.prop <- do.call(rbind, Lt.dat.prop)
    Lt.dat1 <- Lt.dat.prop[, 1:data.in$N_lbins]
    Lt.dat2 <- Lt.dat.prop[, (data.in$N_lbins + 1):ncol(Lt.dat.prop)]
    Lt.dat3 <- Lt.dat1 + Lt.dat2

    cdf <- mapply(
      function(x) {
        cumsum(as.numeric(Lt.dat3[x, ]) / sum(as.numeric(Lt.dat3[x, ])))
      },
      x = 1:nrow(Lt.dat3),
      SIMPLIFY = FALSE
    )
    Lt.qtls.05 <- mapply(
      function(x) Lt.vec[min(which(cdf[[x]] >= 0.05))],
      x = 1:length(cdf),
      SIMPLIFY = TRUE
    )
    Lt.qtls.25 <- mapply(
      function(x) Lt.vec[min(which(cdf[[x]] >= 0.25))],
      x = 1:length(cdf),
      SIMPLIFY = TRUE
    )
    Lt.qtls.5 <- mapply(
      function(x) Lt.vec[min(which(cdf[[x]] >= 0.5))],
      x = 1:length(cdf),
      SIMPLIFY = TRUE
    )
    Lt.qtls.75 <- mapply(
      function(x) Lt.vec[min(which(cdf[[x]] >= 0.75))],
      x = 1:length(cdf),
      SIMPLIFY = TRUE
    )
    Lt.qtls.95 <- mapply(
      function(x) Lt.vec[min(which(cdf[[x]] >= 0.95))],
      x = 1:length(cdf),
      SIMPLIFY = TRUE
    )

    Lt.mean <- mapply(
      function(x) sum(Lt.dat[x, ] * Lt.vec) / sum(Lt.dat[x, ]),
      x = 1:nrow(Lt.dat),
      SIMPLIFY = TRUE
    )
  }

  # spp.out.Ltm <- spp.out.Ltm[, c(
  #   'Yr',
  #   'All_obs_mean',
  #   'All_exp_5%',
  #   'All_exp_95%',
  #   'Sexes'
  # )]

  spp.out.Ltm <- data.frame(
    Year = data.out.Ltm$year,
    Neff = data.out.Ltm$Nsamp,
    Q5 = Lt.qtls.05,
    Q25 = Lt.qtls.25,
    Q50 = Lt.qtls.5,
    Mean = Lt.mean,
    Q75 = Lt.qtls.75,
    Q95 = Lt.qtls.95
  )

  #Extract depletion
  bratio <- spp.out$derived_quants[
    grep("Bratio_", spp.out$derived_quants$Label),
  ]
  spp.out.dep <- data.frame(
    Year = as.numeric(str_split_i(bratio$Label, "_", 2)),
    Deplete = bratio$Value
  )

  spp.out.dep <- spp.out.dep[
    spp.out.dep$Year %in% unique(spp.out.Ltm$Year),
  ]

  RP.test <- spp.out.dep$Deplete <= Ltm.RP
  RP.test[RP.test == "TRUE"] <- "BELOW RP"
  RP.test[RP.test == "FALSE"] <- "ABOVE RP"

  # spp.out.Ltm.dep <- cbind(
  #   spp.out.dep,
  #   spp.out.Ltm$All_obs_mean,
  #   spp.out.Ltm$'All_exp_5%',
  #   spp.out.Ltm$'All_exp_95%',
  #   ((spp.out.Ltm$'All_exp_95%' - spp.out.Ltm$'All_exp_5%') / (2 * 1.96)) /
  #     spp.out.Ltm$All_obs_mean,
  #   RP.test
  # )
  # colnames(spp.out.Ltm.dep)[3:7] <- c("Mean_Lt", "Lt5", "Lt95", "CV", "RP.test")

  spp.out.Ltm.dep <- cbind(
    spp.out.dep,
    spp.out.Ltm[, -1],
    RP.test
  )
  colnames(spp.out.Ltm.dep)[ncol(spp.out.Ltm.dep)] <- "RP.test"

  lm.meanlt.plot <- ggplot(spp.out.Ltm.dep, aes(Mean, Deplete)) +
    geom_point(size = 3, color = "#5D9741") +
    geom_smooth(
      formula = y ~ x,
      method = "lm",
      weight = "Neff",
      color = "#1ef368",
      fill = "#1ef368",
      alpha = 0.2
    ) +
    theme_minimal()

  spp.mlt.lm.out <- lm(
    Deplete ~ spp.out.Ltm.dep[, 6],
    data = spp.out.Ltm.dep,
    weights = spp.out.Ltm.dep$Neff
  )

  Metric_lt = spp.out.Ltm.dep[, names(spp.out.Ltm.dep) == Lt.metric]

  MeanLt.plot <- ggplot(data = spp.out.Ltm.dep) +
    geom_errorbar(
      aes(Year, ymin = Q5, ymax = Q95, color = RP.test),
      width = 0.2
    ) +
    geom_point(
      aes(
        Year,
        Metric_lt,
        color = RP.test
      ),
      size = 2
    ) +
    scale_color_manual(
      name = "RP.test",
      values = c(
        "BELOW RP" = "darkred",
        "ABOVE RP" = "green"
      )
    ) +
    geom_hline(
      yintercept = (Ltm.RP - spp.mlt.lm.out$coefficients[1]) /
        spp.mlt.lm.out$coefficients[2],
      col = "#E74C3C",
      linetype = "dashed",
      linewidth = 1
    ) +
    ylim(
      0,
      max(
        spp.out.Ltm.dep$Q95,
        (Ltm.RP - spp.mlt.lm.out$coefficients[1]) /
          spp.mlt.lm.out$coefficients[2]
      )
    ) +
    ylab(Lt.metric) +
    theme_minimal() +
    theme(legend.position = "none")

  return(list(
    data = spp.out.Ltm.dep,
    lm_plot = lm.meanlt.plot,
    mlt_plot = MeanLt.plot,
    lm_model = spp.mlt.lm.out,
    RP.out = (Ltm.RP - spp.mlt.lm.out$coefficients[1]) /
      spp.mlt.lm.out$coefficients[2]
  ))
}
##########################################
##########################################

server <- function(input, output, session) {
  options(shiny.maxRequestSize = 30 * 1024^2) #increase max size of upload file
  # Reactive value to store loaded data
  spp_data <- reactiveVal(NULL)
  datafile <- reactiveVal(NULL)
  #envrep <- new.env()
  #envdat <- new.env()

  # Load RData file
  observeEvent(input$rmodel_file, {
    req(input$rmodel_file)

    tryCatch(
      {
        # Load the RData file
        #env.rep <- new.env()
        spp_data(get(load(input$rmodel_file$datapath)))
        # Get the first object (assuming it's the r4ss output)
        # obj_name <- ls(env.rep)[1]
        #         spp_data<-(env.rep[[obj_name]])

        showNotification(
          "Model output loaded successfully!",
          type = "message",
          duration = 1.5
        )
      },
      error = function(e) {
        showNotification(
          paste("Error loading file:", e$message),
          type = "error"
        )
      }
    )
  })

  observeEvent(input$rdata_file, {
    req(input$rdata_file)

    tryCatch(
      {
        # Load the RData file
        #env.data <- new.env()
        datafile(get(load(input$rdata_file$datapath)))
        #return()
        # Get the first object (assuming it's the r4ss output)
        #obj_name <- ls(env.data)[1]
        # datafile(env.data[[obj_name]])

        showNotification(
          "Data loaded successfully!",
          type = "message",
          duration = 1.5
        )
      },
      error = function(ee) {
        showNotification(
          paste("Error loading file:", ee$message),
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
  analysis_results <- eventReactive(input$run_analysis_index, {
    req(spp_data())
    withProgress(message = 'Calculating indicators', value = 0, {
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
    list(
      Model_summary = summary(analysis_results()$lm_model),
      Index_RP = input$index_rp / analysis_results()$lm_model$coefficients[1]
    )
  })

  #############################
  ### Mean Length Indicator ###
  #############################
  #Fleet names and sex for mean length

  fleet.name2 <- reactive({
    req(spp_data(), datafile()$lencomp)
    #    if (!is.null(spp_data()) & !is.null(datafile())) {
    datafile <- datafile()
    names(datafile$lencomp)[1:6] <- c(
      "year",
      "month",
      "fleet",
      "sex",
      "part",
      "Nsamp"
    )

    datafile$fleetnames[unique(datafile$lencomp$fleet)]
    #unique(spp_data()$len_comp_fit_table$Fleet_Name)
    #   }
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

  # observe({
  #   updateSelectInput(
  #     session,
  #     "fleet_num",
  #     label = "Fleet Name",
  #     choices = fleet.name1()$Fleet.name
  #   )
  # })

  sex.mlt <- reactive({
    req(spp_data(), datafile()$lencomp)
    #    if (!is.null(spp_data()) & !is.null(datafile())) {
    datafile <- datafile()
    names(datafile$lencomp)[1:6] <- c(
      "year",
      "month",
      "fleet",
      "sex",
      "part",
      "Nsamp"
    )

    fleet.name.num <- data.frame(
      fleetnames = datafile$fleetnames[unique(datafile$lencomp$fleet)],
      fleetnumber = unique(datafile$lencomp$fleet)
    )

    Lts.fleet <- subset(
      datafile$lencomp,
      fleet ==
        fleet.name.num[fleet.name.num$fleetnames == input$fleet_num_mlt, 2]
    )
    return(unique(Lts.fleet$sex))
    #   }
  })

  part.mlt <- reactive({
    req(spp_data(), datafile()$lencomp)
    #if (!is.null(spp_data()) & !is.null(datafile()$lencomp)) {
    datafile <- datafile()
    names(datafile$lencomp)[1:6] <- c(
      "year",
      "month",
      "fleet",
      "sex",
      "part",
      "Nsamp"
    )

    fleet.name.num <- data.frame(
      fleetnames = datafile$fleetnames[unique(datafile$lencomp$fleet)],
      fleetnumber = unique(datafile$lencomp$fleet)
    )

    Lts.fleet <- subset(
      datafile$lencomp,
      fleet ==
        fleet.name.num[fleet.name.num$fleetnames == input$fleet_num_mlt, 2]
    )
    return(unique(Lts.fleet$part))
    #}
  })
  #if (exists("Lts.fleet")) {
  #if (is.null(input$fleet_num_mlt)) {
  #   Lts.fleet = 0
  #  }
  #}

  observe({
    updateSelectInput(
      session,
      "sex_num",
      label = "Sex",
      choices = sex.mlt(),
      selected = sex.mlt()[1]
    )
  })

  observe({
    updateSelectInput(
      session,
      "part_num",
      label = "Partition",
      choices = part.mlt(),
      selected = part.mlt()[1]
    )
  })

  # Run Mean Length analysis when button is clicked
  withProgress(message = 'Calculating length indicators', value = 0, {
    analysis_results_mlt <- eventReactive(input$run_analysis_mlt, {
      req(spp_data(), datafile()$lencomp)
      datafile <- datafile()
      names(datafile$lencomp)[1:6] <- c(
        "year",
        "month",
        "fleet",
        "sex",
        "part",
        "Nsamp"
      )

      fleet.name.num <- data.frame(
        fleetnames = datafile$fleetnames[unique(datafile$lencomp$fleet)],
        fleetnumber = unique(datafile$lencomp$fleet)
      )
      fleet.num <- fleet.name.num[
        fleet.name.num$fleetnames == input$fleet_num_mlt,
        2
      ]
      withProgress(message = 'Calculating indicators', value = 0, {
        tryCatch(
          {
            MeanLt.method(
              spp.out = spp_data(),
              data.in = datafile,
              fleet.in = fleet.num,
              Sex = as.numeric(input$sex_num),
              Part.in = as.numeric(input$part_num),
              Ltm.RP = input$mlt_rp,
              Lt.metric = input$lt_compare
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
    list(
      Model_summary = summary(analysis_results_mlt()$lm_model),
      Index_RP = (input$mlt_rp -
        analysis_results_mlt()$lm_model$coefficients[1]) /
        analysis_results_mlt()$lm_model$coefficients[2]
    )
  })
}
