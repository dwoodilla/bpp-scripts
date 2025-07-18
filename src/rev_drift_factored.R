library(tidyverse)
library(sgolay)
library(ggpmisc)
library(zoo)
library(checkmate)
library(latex2exp)
library(gridExtra)

source("./import/import_cleaned.R")
source("./src/plot_helpers.R")

AVG_WINDOW = 24
SAVGOL_FILTER_LEN = 24+1

elongate_wrapper = function(attempt_read = TRUE) {
    if (attempt_read & file.exists("./clean_data/beaco2n_drift_long.csv")) {
        co_long = read_csv("./clean_data/beaco2n_drift_long.csv")
    } else {
        co_long = elongate_df(df=co, parameter_arg="co", sensors=c("beaco2n","aqs"), dates_of_deployment=dates_of_deployment)
        write_csv(x=co_long, file="./clean_data/beaco2n_drift_long.csv", col_names=TRUE)
    }
}

co = import_co()
dates_of_deployment = dates_of_deployment(df=co, parameter_arg="co", sensors=c("beaco2n","aqs"))
co = co %>% 
    pivot_wider(
        names_from="parameter",
        values_from="value"
    ) %>%
    filter(is.na(temp) | temp < 30) %>%
    pivot_longer(
        cols=c("co","temp","rh"),
        names_to = "parameter",
        values_to="value"
    )

co_long = tibble()
co_long = elongate_wrapper(TRUE)

self_ref=FALSE
season_data = arrange_season_data(
    dataset=co_long,
    noise_filter="original",
    meas_sensor="beaco2n",
    meas_location="myron",
    self_ref=self_ref,
    ref_sensor="aqs",
    ref_location="myron"
)
deployment_density(
    season_data=season_data,
    filepath="./plots/myron_pdfs_ext.png",
    title="Distributions by Operating Month",
    subtitle="BEACO2N vs AQS at Myron (filter=original)", 
    x="[CO] (ppm)",
    y=TeX("$\\frac{d (Cumulative\\_density)}{d[CO]}=pdf$")
)
dens = deployment_density_stats(season_data=season_data)
divergence_line_plot(
    dens[[1]], 
    filepath="./plots/myron_divs_ext.png",
    self_ref=self_ref
)
png(filename="./plots/myron_divtab_ext.png", width=8.5, height=11, units="in", res=300)
grid.table(dens[[1]] %>% 
    filter(statistic %in% c("KL", "hellinger", "euclidean")) %>%
    pivot_wider(
        names_from="statistic",
        values_from="value"
    ) %>%
    select(-c(pdf_resolution, plottype)) %>%
    drop_na(c("KL","hellinger","euclidean"))
)
dev.off()

self_ref=TRUE
season_data = arrange_season_data(
    dataset=co_long,
    noise_filter="original",
    meas_sensor="beaco2n",
    meas_location="myron",
    self_ref=self_ref
)
deployment_density(
    season_data=season_data,
    filepath="./plots/myron_pdfs_self.png",
    title="Distributions by Operating Month",
    subtitle="BEACO2N vs self (filter=original)", 
    x="[CO] (ppm)",
    y=TeX("$\\frac{d (Cumulative\\_density)}{d[CO]}=pdf$")
)
dens = deployment_density_stats(season_data=season_data)
divergence_line_plot(
    dens[[1]], 
    filepath="./plots/myron_divs_self.png",
    self_ref=self_ref
)

png(filename="./plots/myron_divtab_self.png", width=8.5, height=11, units="in", res=300)
grid.table(dens[[1]] %>% 
    filter(statistic %in% c("KL", "hellinger", "euclidean")) %>%
    pivot_wider(
        names_from="statistic",
        values_from="value"
    ) %>%
    select(-c(pdf_resolution, plottype))
)
dev.off()