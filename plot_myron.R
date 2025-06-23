install.packages("openair")
install.packages("dplyr")
library(openair)
library(dplyr)

myron_beaco2n_pred = read.csv("./model_predictions/myron/pred/myron_beaco2n_pred.csv") # nolint
myron_aqs = read.csv("./model_predictions/myron/true/myron_aqs.csv")
myron_beaco2n_pred$timestamp <- as.POSIXct(myron_beaco2n_pred$timestamp, tz="UTC") # nolint
myron_aqs$timestamp <- as.POSIXct(myron_aqs$timestamp, tz="UTC") # nolint
myron_beaco2n_pred = myron_beaco2n_pred %>% rename(date = timestamp) %>% rename(co = X0)
myron_aqs = myron_aqs %>% rename(date = timestamp) %>% rename(co = co_aqs)

myron_raw = read.csv("./model_predictions/myron/raw/myron_beaco2n_raw.csv")
myron_raw$timestamp = as.POSIXct(myron_raw$timestamp, tz="UTC")
myron_raw = myron_raw %>% rename(date = timestamp) %>% rename(co=co_meas)

png(
  filename = "./model_predictions/myron/pred/summary_plot.png",
  width    = 10  * 300,
  height   = 10  * 300,
  res      = 300
)
summaryPlot(myron_beaco2n_pred, pollutant = "co")
dev.off()

png(
  filename = "./model_predictions/myron/true/summary_plot.png",
  width    = 10  * 300,
  height   = 10  * 300,
  res      = 300
)
summaryPlot(myron_aqs, pollutant = "co")
dev.off()

png(
  filename = "./model_predictions/myron/raw/summary_plot.png",
  width    = 10  * 300,
  height   = 10  * 300,
  res      = 300
)
summaryPlot(myron_raw, pollutant = "co")
dev.off()

png(
  filename = "./model_predictions/myron/pred/calendar_plot.png",
  width    = 10  * 300,
  height   = 10  * 300,
  res      = 300
)
calendarPlot(myron_beaco2n_pred, pollutant = "co")
dev.off()

png(
  filename = "./model_predictions/myron/true/calendar_plot.png",
  width    = 10  * 300,
  height   = 10  * 300,
  res      = 300
)
calendarPlot(myron_aqs, pollutant = "co")
dev.off()

png(
  filename = "./model_predictions/myron/raw/calendar_plot.png",
  width    = 10  * 300,
  height   = 10  * 300,
  res      = 300
)
calendarPlot(myron_raw, pollutant = "co")
dev.off()

png(
  filename = "./model_predictions/myron/pred/time_var_plot.png",
  width    = 10  * 300,
  height   = 10  * 300,
  res      = 300
)
timeVariation(myron_beaco2n_pred, pollutant = "co")
dev.off()

png(
  filename = "./model_predictions/myron/true/time_var_plot.png",
  width    = 10  * 300,
  height   = 10  * 300,
  res      = 300
)
timeVariation(myron_aqs, pollutant = "co")
dev.off()

png(
  filename = "./model_predictions/myron/raw/time_var_plot.png",
  width    = 10  * 300,
  height   = 10  * 300,
  res      = 300
)
timeVariation(myron_raw, pollutant = "co")
dev.off()