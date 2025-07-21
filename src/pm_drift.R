library(tidyverse)
library(sgolay)
library(ggpmisc)
library(zoo)
library(checkmate)
library(latex2exp)
library(gridExtra)

source("./clean/import_cleaned.R")
source("./src/plot_helpers.R")

AVG_WINDOW = 24
SAVGOL_FILTER_LEN = 24+1

pm=import_pm()
pm_dod = dates_of_deployment(df=pm, parameter_arg="pm", sensors=c("beaco2n","quantaq"))
pm_filtered = pm %>%
    pivot_wider(
        names_from="parameter",
        values_from="value"
    ) %>%
    filter(is.na(temp) | temp < 30, is.na(rh) | rh < 75) %>%
    pivot_longer(
        cols=c("pm25","temp","rh"),
        names_to = "parameter",
        values_to="value"
    )
# pm_long = pm_elongate_wrapper(TRUE)
pm_long = elongate_wrapper(
    df=pm,
    parameter="pm25",
    sensors=c("beaco2n","quantaq"),
    dates_of_deployment=pm_dod,
    filepath="./cache/pm_drift_long.csv"
)


for (site in c("dpw","pema","pha")) {
    pm_plottable = arrange_season_data(
        dataset=pm_long,
        noise_filter="original",
        meas_sensor="beaco2n",
        meas_location=site,
        self_ref=FALSE,
        ref_sensor="quantaq",
        ref_location=site
    )
    violin_year_season(
        season_data = pm_plottable,
        filepath=paste0("./plots/pm_analysis/", site,"/violin_year_season.png")
    )
    violin_season(
        season_data = pm_plottable,
        filepath=paste0("./plots/pm_analysis/", site,"/violin_season.png")
    )
}


# deployment_density(
#     season_data=pm_plottable,
#     filepath="./test.png",
#     Title="pm test"
# )

violin_year_season(
    season_data = pm_plottable,
    filepath="./test.png"
)
violin_season(
    season_data = pm_plottable,
    filepath="./test_sn.png"
)