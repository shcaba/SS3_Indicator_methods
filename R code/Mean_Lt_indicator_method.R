library(r4ss)
library(reshape2)
library(ggplot2)

###########################
### Depletion Indicator ###
###########################
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
