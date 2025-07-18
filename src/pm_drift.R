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

pm_elongate_wrapper = function(attempt_read = TRUE) {
    if (attempt_read & file.exists("./clean_data/pm_drift_long.csv")) {
        pm_long = read_csv("./clean_data/pm_drift_long.csv")
    } else {
        pm_long = elongate_df(df=pm, parameter_arg="pm25", sensors=c("beaco2n","quantaq"), dates_of_deployment=pm_dod)
        write_csv(x=pm_long, file="./clean_data/pm_drift_long.csv", col_names=TRUE)
    }
}

pm=import_pm()
pm_dod = dates_of_deployment(df=pm, parameter_arg="pm", sensors=c("beaco2n","quantaq"))
pm = pm %>%
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
pm_long = pm_elongate_wrapper(TRUE)


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