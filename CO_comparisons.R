install.packages("openair")
install.packages("dplyr")
library(openair)
library(dplyr)

merged_df = read.csv("./combined_dataset.csv")
merged_df$date = as.POSIXct(merged_df$date, tz="UTC")

# make_plots = function(col1, col2) {

# }

png(
    filename = "./model_graphs/myron/seasonal.png",
    width = 15*300, height=15*300,
    res = 300
)
timeVariation(merged_df, pollutant=c("co_aqs_myron", "co_beaco2n_myron"), type="season", difference=TRUE, cols=c("red", "blue", "purple")) # nolint
dev.off()

png(
    filename = "./model_graphs/myron/reg.png",
    width = 15*300, height=15*300, res=300
)
timePlot(selectByDate(merged_df, start="1/7/2022", end="1/3/2024"), pollutant=c("co_aqs_myron", "co_beaco2n_myron"), auto.text=FALSE, name.pol=c("AQS CO (ppm)", "BEACO2N CO (ppm)"), smooth=TRUE, ci=TRUE, cols=c("red", "blue"), stack=TRUE)
dev.off()