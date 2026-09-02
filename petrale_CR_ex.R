library(ggplot2)

Petrale_Ct <- read.csv(
    "C:/Users/Jason.Cope/Documents/Github/SS3_Indicator_methods/ss3 outputs/Petrale_catch example.csv"
)

ggplot(Petrale_Ct, aes(Year, Catch, color = Source)) +
    geom_point() +
    theme_bw()
