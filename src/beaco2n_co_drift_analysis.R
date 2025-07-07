library(tidyverse)
library(openair)
library(checkmate)
library(zoo)
library(pracma)

source("./src/import_cleaned.R")

AVG_WINDOW = 24*7
SAVGOL_FILTER_LEN = 24*7-1

unlink(c("./plots/*.png", "./plots/*.png"), expand=TRUE) # Hit Ctrl-Enter on this line to clear temporary files.

# combined_df = read.csv("./clean_data/all_co_temp_rh_2022.csv")
# combined_df$date = as.POSIXct(combined_df$date, tz="UTC")
# tidy_combined_df = combined_df %>% pivot_longer( # Convert combined_df to tidy format
#     cols = -date,
#     names_to = c("parameter","sensor","location"),
#     values_to = "value",
#     names_pattern = "([^_]+)_([^_]+)_(.+)"
# )

tidy_combined_df = import_cleaned()

# BEACO2N DRIFT:
# 1) Plot BEACO2N vs AQS residual over time (Myron): residual, rolling avg residual, stat tests for residual.
# 2) Plot BEACO2N DPW vs AQS Cranston: residual, rolling avg residual, stat tests
myron_df_wide = tidy_combined_df %>% 
    filter(location=="myron", parameter=="co", sensor %in% c("aqs","beaco2n")) %>%
    pivot_wider(
        id_cols = "date",
        names_from = "sensor",
        values_from = "value"
    ) %>%
    filter(if_all(c(aqs, beaco2n), ~ !is.na(.))) # Choose rows s.t. no measurement entry is NA

myron_rolling_wide = myron_df_wide %>%
    mutate(aqs = rollmean(aqs, k=AVG_WINDOW, align="center", fill=NA), 
           beaco2n = rollmean(beaco2n, k=AVG_WINDOW, align="center", fill=NA)) %>%
    mutate(res = beaco2n - aqs)

myron_savgol_wide = myron_df_wide %>%
    mutate(aqs = savgol(aqs, fl=SAVGOL_FILTER_LEN), beaco2n = savgol(beaco2n, fl=SAVGOL_FILTER_LEN)) %>%
    mutate(res = beaco2n - aqs)

myron_df_long = myron_df_wide %>% 
    mutate(res = beaco2n - aqs) %>%
    pivot_longer(
        cols = -date,
        names_to = "sensor",
        values_to = "value"
    )
myron_rolling_long = myron_rolling_wide %>%
    pivot_longer(
        cols=-date,
        names_to = "sensor",
        values_to = "value"
    )
myron_savgol_long = myron_savgol_wide %>%
    pivot_longer(
        cols=-date,
        names_to = "sensor",
        values_to = "value"
    )

season = function(vec) {
    m = month(as.Date(vec))
    return(
        case_when(
            m %in% c(12,1,2) ~ "Winter",
            m %in% c(3,4,5) ~ "Spring",
            m %in% c(6,7,8) ~ "Summer",
            m %in% c(9,10,11) ~ "Fall"
        )
    )
}

count_from_season_start = function(vec) {
    d = as.POSIXct(vec)
    sn = season(vec)
    sn_start = 
        case_when(
            sn=="Winter" ~ make_datetime(year=if_else(month(d)==12, year(d), year(d)-1), month=12, day=1),
            sn=="Spring" ~ make_datetime(year=year(d), month=3, day=1),
            sn=="Summer" ~ make_datetime(year=year(d), month=6, day=1),
            sn=="Fall" ~ make_datetime(year=year(d), month=9, day=1)
        )
    return(int_length(interval(start=sn_start, end=d, tz="UTC")))
}

plot_graphs = function(dataset, dataset_name) {
    year_by_season_ts = 
        ggplot(
            data=dataset, 
            mapping=aes(
                x=from_sn_start, 
                y=value, 
                color=sensor
            )
        ) + 
        facet_grid(rows=vars(year), cols=vars(season)) +
        geom_line() + theme_bw() +
        theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1)) +
        geom_hline(yintercept = 0, color="red") +
        labs(
            title="Myron: AQS vs BEACO2N Time Series",
            subtitle=paste0("2022-2024 Faceted by Year and Season. Data: ", dataset_name),
            x = "Seconds from first day of season",
            y = "CO (ppm)",
            caption=dataset_name
        )
    ggsave(plot=year_by_season_ts, filename=paste0("./plots/beaco2n_co_drift_analysis/", dataset_name, "/year_by_sn_timeseries.png"))

    by_season_ts = 
        ggplot(
            data=dataset, 
            mapping=aes(
                x=from_sn_start,
                y=value,
                color=sensor,
                linetype=factor(year)
            )
        ) + 
        scale_linetype_manual(values=c("dotdash","solid","longdash")) +
        facet_wrap(~ season) +
        geom_line() + theme_bw() +
        theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1)) +
        geom_hline(yintercept = 0, color="red") +
        labs(
            title="Myron: AQS vs BEACO2N Time Series",
            subtitle=paste0("2022-2024 Faceted by Season. Data: ", dataset_name),
            x = "Seconds from first day of season",
            y = "CO (ppm)",
            color="Sensor",
            linetype="Year",
            caption=dataset_name
        )
    ggsave(plot=by_season_ts, filename=paste0("./plots/beaco2n_co_drift_analysis/", dataset_name, "/by_sn_timeseries.png"))

    year_by_season_box_nores = 
        ggplot(
            data=dataset %>% filter(sensor %in% c("aqs","beaco2n")), # Residual scaled poorly on plot
            mapping=aes(
                x=sensor, 
                y=value,
                fill=sensor
            )
        ) + 
        facet_grid(rows=vars(year), cols=vars(season), scales="fixed") +
        geom_boxplot() + theme_bw() +
        theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1)) +
        labs(
            title="Myron: AQS vs BEACO2N Summary Boxplots",
            subtitle=paste0("2022-2024 Faceted by Year and Season. Data: ", dataset_name),
            x = "Sensor",
            y = "CO (ppm)",
            caption=dataset_name
        )
    ggsave(plot=year_by_season_box_nores, filename=paste0("./plots/beaco2n_co_drift_analysis/",dataset_name,"/yr_by_sn_box_nores.png"))

    year_by_season_box_onlyres = 
        ggplot(
            data=dataset %>% filter(sensor == "res"), # Residual scaled poorly on plot
            mapping=aes(
                x=sensor, 
                y=value
            )
        ) + 
        facet_grid(rows=vars(year), cols=vars(season), scales="fixed") +
        geom_boxplot() + theme_bw() +
        theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1)) +
        labs(
            title="Myron: Residual Summary Boxplots",
            subtitle=paste0("2022-2024 Faceted by Year and Season. Data: ", dataset_name),
            x = "Sensor",
            y = "CO (ppm)",
            caption=dataset_name
        )
    ggsave(plot=year_by_season_box_onlyres, filename=paste0("./plots/beaco2n_co_drift_analysis/",dataset_name,"/yr_by_sn_box_onlyres.png"))

    by_season_box_nores = 
        ggplot(
            data=dataset %>% filter(sensor %in% c("aqs","beaco2n")), # Residual scaled poorly on plot
            mapping=aes(
                x=year, 
                y=value,
                fill=sensor,
            )
        ) + 
        facet_wrap(~ season) +
        geom_boxplot() + theme_bw() +
        theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1)) +
        labs(
            title="Myron: AQS vs BEACO2N Summary Boxplots",
            subtitle=paste0("2022-2024 Faceted by Season. Data: ", dataset_name),
            x = "Sensor",
            y = "CO (ppm)",
            caption=dataset_name
        )
    ggsave(plot=by_season_box_nores, filename=paste0("./plots/beaco2n_co_drift_analysis/",dataset_name,"/by_sn_box_nores.png"))

    by_season_box_onlyres = 
        ggplot(
            data=dataset %>% filter(sensor == "res"), # Residual scaled poorly on plot
            mapping=aes(
                x=year, 
                y=value
            )
        ) + 
        facet_wrap(~ season) +
        geom_boxplot() + theme_bw() +
        theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1)) +
        labs(
            title="Myron: Residual Summary Boxplots",
            subtitle=paste0("2022-2024 Faceted by Season. Data: ", dataset_name),
            x = "Sensor",
            y = "CO (ppm)",
            caption=dataset_name
        )
    ggsave(plot=by_season_box_onlyres, filename=paste0("./plots/beaco2n_co_drift_analysis/",dataset_name,"/by_sn_box_onlyres.png"))

    by_year_box_nores = 
        ggplot(
            data=dataset %>% filter(sensor %in% c("aqs","beaco2n")), # Residual scaled poorly on plot
            mapping=aes(
                x=sensor, 
                y=value,
                fill=sensor
            )
        ) + 
        facet_wrap(~ year) +
        geom_boxplot() + theme_bw() +
        theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1)) +
        labs(
            title="Myron: AQS vs BEACO2N Summary Boxplots",
            subtitle=paste0("2022-2024 Faceted by Year. Data: ", dataset_name),
            x = "Sensor",
            y = "CO (ppm)",
            fill="Sensor",
            caption=dataset_name
        )
    ggsave(plot=by_year_box_nores, filename=paste0("./plots/beaco2n_co_drift_analysis/",dataset_name,"/by_year_box_nores.png"))

    by_year_box_onlyres = 
        ggplot(
            data=dataset %>% filter(sensor=="res"),
            mapping=aes(
                x=sensor, 
                y=value
            )
        ) + 
        facet_wrap(~ year) +
        geom_boxplot() + theme_bw() +
        labs(
            title="Myron: Residual Summary Boxplots",
            subtitle=paste0("2022-2024 Faceted by Year. Data: ", dataset_name),
            x = "Sensor",
            y = "CO (ppm)",
            caption=dataset_name
        )
    ggsave(plot=by_year_box_onlyres, filename=paste0("./plots/beaco2n_co_drift_analysis/",dataset_name,"/by_year_box_onlyres.png"))

}

myron_df_long_seasonal =  myron_df_long %>%
    mutate(
        year=if_else(month(date)==12, year(date)+1, year(date)), 
        year=factor(year, levels=c(2022,2023,2024)),
        season=season(date), 
        season=factor(season, levels = c("Winter", "Spring", "Summer", "Fall")),
        from_sn_start=count_from_season_start(date)
    )
myron_rolling_long_seasonal = myron_rolling_long %>%
    mutate(
        year=if_else(month(date)==12, year(date)+1, year(date)), 
        year=factor(year, levels=c(2022,2023,2024)),
        season=season(date), 
        season=factor(season, levels = c("Winter", "Spring", "Summer", "Fall")),
        from_sn_start=count_from_season_start(date)
    ) 
myron_savgol_long_seasonal = myron_savgol_long %>%
    mutate(
        year=if_else(month(date)==12, year(date)+1, year(date)), 
        year=factor(year, levels=c(2022,2023,2024)),
        season=season(date), 
        season=factor(season, levels = c("Winter", "Spring", "Summer", "Fall")),
        from_sn_start=count_from_season_start(date)
    ) 

plot_graphs(myron_df_long_seasonal, "original")
plot_graphs(myron_rolling_long_seasonal, "rolling")
plot_graphs(myron_savgol_long_seasonal, "savgol")