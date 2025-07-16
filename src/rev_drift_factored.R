library(tidyverse)
library(sgolay)
library(ggpmisc)
library(zoo)
library(checkmate)
library(latex2exp)
source("./import/import_cleaned.R")
source("./src/plot_helpers.R")

AVG_WINDOW = 24
SAVGOL_FILTER_LEN = 24+1

co = import_co()

dates_of_deployment = dates_of_deployment(df=co, parameter_arg="co", sensors=c("beaco2n","aqs"))

co_long = tibble()
if (file.exists("./clean_data/beaco2n_drift_long.csv")) {
    co_long = read_csv("./clean_data/beaco2n_drift_long.csv")
} else {
    co_long = elongate_df(df=co, parameter_arg="co", sensors=c("beaco2n","aqs"), dates_of_deployment=dates_of_deployment)
    write_csv(x=co_long, file="./clean_data/beaco2n_drift_long.csv", col_names=TRUE)
}

season_data = arrange_season_data(
    dataset=co_long,
    noise_filter="original",
    meas_sensor="beaco2n",
    meas_location="myron",
    self_ref=TRUE,
    ref_sensor="aqs",
    ref_location="myron"
)

deployment_density(
    season_data=season_data,
    filepath="./plots/hist_test.png",
    title="Distributions by Operating Month",
    subtitle="BEACO2N vs AQS at Myron (filter=original)", 
    x="[CO] (ppm)",
    y=TeX("$\\frac{d (Cumulative\\_density)}{d[CO]}=pdf$")
)
dens = deployment_density_stats(
    season_data=season_data
)
print(dens, width=Inf, n=Inf)

