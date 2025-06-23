install.packages("openair")
install.packages("dplyr")
library(openair)
library(dplyr)

myron_beaco2n_pred <- read.csv("./model_predictions/myron/myron_beaco2n_pred.csv")
myron_beaco2n_pred$timestamp <- as.POSIXct(myron_beaco2n_pred$timestamp, tz="UTC") # nolint
myron_beaco2n_pred = myron_beaco2n_pred %>%
  rename(date = timestamp)

png(
  filename = "./model_predictions/myron/pred/summary_plot.png",
  width    = 10  * 300,
  height   = 10  * 300,
  res      = 300
)
summaryPlot(myron_beaco2n_pred, pollutant = "X0")
dev.off()

png(
  filename = "./model_predictions/myron/pred/calendar_plot.png",
  width    = 10  * 300,
  height   = 10  * 300,
  res      = 300
)
calendarPlot(myron_beaco2n_pred, pollutant = "X0")
dev.off()

png(
  filename = "./model_predictions/myron/pred/time_var_plot.png",
  width    = 10  * 300,
  height   = 10  * 300,
  res      = 300
)
timeVariation(myron_beaco2n_pred, pollutant = "X0")
dev.off()