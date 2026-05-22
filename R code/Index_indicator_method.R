library(r4ss)
library(reshape2)
library(ggplot2)

###########################
### Depletion Indicator ###
###########################
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

  lm.plot <- ggplot(spp.out.cpue.dep, aes(Index, Deplete)) +
    geom_point() +
    geom_smooth(formula = y ~ x + 0, method = "lm")
  print(lm.plot)

  spp.lm.out <- lm(Deplete ~ Index + 0, data = spp.out.cpue.dep)

  # index.pred <- data.frame(
  #   Index = seq(
  #     round(min(spp.out.cpue.dep$Index) / 1000),
  #     min(spp.out.cpue.dep$Index),
  #     round(
  #       round(
  #         min(spp.out.cpue.dep$Index) /
  #           round(min(spp.out.cpue.dep$Index) / 1000)
  #       ) /
  #         10
  #     )
  #   )
  # )

  # spp.pred.index <- predict(
  #   lm(Deplete ~ Index, data = spp.out.cpue.dep),
  #   index.pred
  # )

  Index.plot <- ggplot(spp.out.cpue.dep, aes(Yr, Index)) +
    geom_point() +
    geom_errorbar(
      ymin = spp.out.cpue.dep$Index -
        (spp.out.cpue.dep$Index * spp.out.cpue.dep$CV * 1.96),
      ymax = spp.out.cpue.dep$Index +
        (spp.out.cpue.dep$Index * spp.out.cpue.dep$CV * 1.96),
      width = 0.2
    ) +
    geom_hline(
      yintercept = Index.RP / spp.lm.out$coefficients[1],
      #      yintercept = max(index.pred$Index[spp.pred.index <= Index.RP]),
      col = "red"
    ) +
    ylim(
      0,
      max(
        spp.out.cpue.dep$Index +
          (spp.out.cpue.dep$Index * spp.out.cpue.dep$CV * 1.96)
      )
    )
  print(Index.plot)
  return(spp.out.cpue.dep)
}
