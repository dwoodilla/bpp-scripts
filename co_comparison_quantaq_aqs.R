# install.packages("openair")
# install.packages("dplyr")
# install.packages("ggplot2")
library(openair)
library(dplyr)
library(clock)

merged_df = read.csv("./intermediary_datasets/merged_quantaq_aqs.csv")
merged_df$date = as.POSIXct(merged_df$date, tz="UTC")

png(
    filename="./test.png", 
    width = 15 * 300,
    height = 15*300,
    res = 300
)
timeVariation(merged_df, pollutant=c("co_aqs_cranston", "co_quantaq_pha", "co_quantaq_dpw", "co_quantaq_pema"), plot=TRUE)
dev.off()