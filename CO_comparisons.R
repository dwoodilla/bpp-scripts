install.packages("openair")
install.packages("dplyr")
library(openair)
library(dplyr)

merged_df = read.csv("./combined_dataset.csv")
merged_df$date = as.POSIXct(merged_df$date, tz="UTC")


timeVariation(merged_df, pollutant=c("co_aqs_myron", "co_myron"))
