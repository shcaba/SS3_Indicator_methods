MeanLt.method <- function(
  spp.out,
  fleet.in,
  Sex = 1,
  Part.in = 0,
  Ltm.RP = 0.25
) {
  spp.out.Ltm <- subset(
    spp.out$len_comp_fit_table,
    Fleet_Name == fleet.in & Sexes == Sex & Part == Part.in
  )
  spp.out.Ltm <- spp.out.Ltm[, c(
    'Yr',
    'All_obs_mean',
    'All_exp_5%',
    'All_exp_95%',
    'Sexes'
  )]

  #Extract depletion
  bratio <- spp.out$derived_quants[
    grep("Bratio_", spp.out$derived_quants$Label),
  ]
  spp.out.dep <- data.frame(
    Yr = as.numeric(str_split_i(bratio$Label, "_", 2)),
    Deplete = bratio$Value
  )

  spp.out.dep <- spp.out.dep[
    spp.out.dep$Yr %in% unique(spp.out.Ltm$Yr),
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
      formula = y ~ x,
      method = "lm",
      weight = "CV",
      color = "#1ef368",
      fill = "#1ef368",
      alpha = 0.2
    ) +
    theme_minimal()
  #print(lm.meanlt.plot)

  spp.mlt.lm.out <- lm(
    Deplete ~ Mean_Lt,
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
      yintercept = (Ltm.RP - spp.mlt.lm.out$coefficients[1]) /
        spp.mlt.lm.out$coefficients[2],
      col = "#E74C3C",
      linetype = "dashed",
      linewidth = 1
    ) +
    ylim(
      0,
      max(
        spp.out.Ltm.dep$Lt95,
        (Ltm.RP - spp.mlt.lm.out$coefficients[1]) /
          spp.mlt.lm.out$coefficients[2]
      )
    ) +
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
