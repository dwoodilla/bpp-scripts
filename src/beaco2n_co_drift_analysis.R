library(tidyverse)
library(openair)
library(checkmate)
library(zoo)
library(pracma)
library(ggpmisc)

source("./import/import_cleaned.R")

AVG_WINDOW = 24*7
SAVGOL_FILTER_LEN = 24*7+1

import = import_cleaned()

unlink(c("./plots/*.png", "./plots/*.png"), expand=TRUE) # Hit Ctrl-Enter on this line to clear temporary files.

# combined_df = read.csv("./clean_data/all_co_temp_rh_2022.csv")
# combined_df$date = as.POSIXct(combined_df$date, tz="UTC")
# tidy_combined_df = combined_df %>% pivot_longer( # Convert combined_df to tidy format
#     cols = -date,
#     names_to = c("parameter","sensor","location"),
#     values_to = "value",
#     names_pattern = "([^_]+)_([^_]+)_(.+)"
# )

tidy_combined_df = import_co()

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
    seconds_from_sn_start = int_length(interval(start=sn_start, end=d, tz="UTC"))
    days_from_sn_start = seconds_from_sn_start/86400 # divide by seconds per day
    return(days_from_sn_start)
}

mos_from_deployment_start_fn = function(vec) {
    date = ymd_hms(format(vec, "%F %T"), tz="UTC")
    dp_start <- make_datetime(year = 2022, month = 7, day = 1, tz = "UTC")
    return(interval(dp_start, date) %/% months(1))
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



plot_by_op_year = function(dataset, smoothing_name) {
    by_year_ts = 
        ggplot(
            data=dataset, 
            mapping=aes(
                x=date, 
                y=value, 
                color=sensor
            )
        ) + 
        facet_wrap(~ operating_month) +
        geom_line() + theme_bw() +
        theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1)) +
        geom_hline(yintercept = 0, color="red") +
        labs(
            title="Myron: AQS vs BEACO2N Time Series",
            subtitle=paste0("2022-2024 Faceted by Year and Season. Data: ", smoothing_name),
            x = "Seconds from first day of season",
            y = "CO (ppm)",
            caption=smoothing_name
        )
    ggsave(plot=by_year_ts, filename=paste0("./plots/beaco2n_co_drift_analysis/", smoothing_name, "/opyear_test.png"))
    # print(by_year_ts)
}



# BEACO2N DRIFT:
# 1) Plot BEACO2N vs AQS residual over time (Myron): residual, rolling avg residual, stat tests for residual.
# 2) Plot BEACO2N DPW vs AQS Cranston: residual, rolling avg residual, stat tests
combined_co_wide = import %>% 
    filter(location=="myron",parameter=="co", sensor %in% c("aqs","beaco2n")) %>%
    pivot_wider(
        id_cols = c(date, location),
        names_from = "sensor",
        values_from = "value"
    ) %>%
    filter(if_any(c(aqs, beaco2n), ~ !is.na(.))) # Choose rows s.t. not both measurement entries are NA

# print(head(combined_co_wide))
# write.csv(combined_co_wide, "./test.csv")
# stop("CALLED STOP")

# combined_co_long = combined_co_wide %>%
#   transmute(
#     date,
#     aqs_raw = aqs,
#     beaco2n_raw = beaco2n,
#     res_raw = beaco2n - aqs,
#     aqs_rolling = rollmean(aqs, k = AVG_WINDOW, align = "center", fill = NA),
#     beaco2n_rolling = rollmean(beaco2n, k = AVG_WINDOW, align = "center", fill = NA),
#     res_rolling = rollmean(beaco2n, k = AVG_WINDOW, align = "center", fill = NA) - 
#                   rollmean(aqs, k = AVG_WINDOW, align = "center", fill = NA),
#     aqs_savgol = savgol(aqs, fl = SAVGOL_FILTER_LEN),
#     beaco2n_savgol = savgol(beaco2n, fl = SAVGOL_FILTER_LEN),
#     res_savgol = savgol(beaco2n, fl = SAVGOL_FILTER_LEN) - 
#                  savgol(aqs, fl = SAVGOL_FILTER_LEN)
#   ) %>%  pivot_longer(
#     cols=-date,
#     names_to = c("sensor", "smoothing"),
#     names_sep = "_",
#     values_to = "value"
#   ) %>%
#   mutate(
#     season_year = factor(if_else(month(date) == 12, year(date) + 1, year(date)),
#                          levels = c(2022, 2023, 2024)),
#     season = factor(season(date), levels = c("Winter", "Spring", "Summer", "Fall")),
#     days_into_season = count_from_season_start(date)
#   )



combined_co_long_ref = combined_co_wide %>%
    rename(
        aqs_raw=aqs, 
        beaco2n_raw=beaco2n
    # ) %>% mutate(
    #     aqs_rolling = rollmean(aqs_raw, k = AVG_WINDOW, align = "center", fill = NA),
    #     beaco2n_rolling = rollmean(beaco2n_raw, k=AVG_WINDOW, align="center", fill=NA), 
    #     aqs_savgol = savgol(aqs_raw, fl=SAVGOL_FILTER_LEN),
    #     beaco2n_savgol = savgol(beaco2n_raw, fl=SAVGOL_FILTER_LEN)
    ) %>% mutate(
        res_raw = beaco2n_raw-aqs_raw
        # res_rolling = beaco2n_rolling-aqs_rolling,
        # res_savgol = beaco2n_savgol - aqs_savgol
    ) %>% pivot_longer(
        cols=-c("date","location"),
        names_to=c("sensor", "smoothing"),
        names_sep="_",
        values_to="value"
    )
combined_co_long = combined_co_long_ref %>% mutate(
        season_year = factor(if_else(month(date)==12, year(date)+1, year(date)), levels=c(2022,2023,2024)),
        season = factor(season(date), levels=c("Winter", "Spring", "Summer", "Fall")),
        days_into_season = count_from_season_start(date),
        operating_month = months_into_deployment(date, location, sensor, combined_co_long_ref)
    ) %>% mutate(
        operating_year = floor(operating_month/12)
    )
print(tail(combined_co_long %>% select(date,location,sensor,value,operating_year,operating_month)))
plot_by_op_year(combined_co_long, smoothing_name="original")

# print(tail(combined_co_long %>% select(date, operating_year, operating_month)))
# myron_rolling_wide = myron_df_wide %>%
#     mutate(aqs = rollmean(aqs, k=AVG_WINDOW, align="center", fill=NA), 
#            beaco2n = rollmean(beaco2n, k=AVG_WINDOW, align="center", fill=NA)) %>%
#     mutate(res = beaco2n - aqs)
#
# myron_savgol_wide = myron_df_wide %>%
#     mutate(aqs = savgol(aqs, fl=SAVGOL_FILTER_LEN), beaco2n = savgol(beaco2n, fl=SAVGOL_FILTER_LEN)) %>%
#     mutate(res = beaco2n - aqs)
#
# myron_df_long = myron_df_wide %>% 
#     mutate(res = beaco2n - aqs) %>%
#     pivot_longer(
#         cols = -date,
#         names_to = "sensor",
#         values_to = "value"
#     )
# myron_rolling_long = myron_rolling_wide %>%
#     pivot_longer(
#         cols=-date,
#         names_to = "sensor",
#         values_to = "value"
#     )
# myron_savgol_long = myron_savgol_wide %>%
#     pivot_longer(
#         cols=-date,
#         names_to = "sensor",
#         values_to = "value"
#     )
# myron_all_long = bind_rows(
#     myron_df_long %>% mutate(smoothing="none"),
#     myron_rolling_long %>% mutate(smoothing="rolling"),
#     myron_savgol_long %>% mutate(smoothing="savgol")
# )
# print(head(combined_co_long))
# write.csv(combined_co_long, "./combined.csv")
stop("CALLED STOP")


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

# plot_graphs(myron_df_long_seasonal, "original")
# plot_graphs(myron_rolling_long_seasonal, "rolling")
# plot_graphs(myron_savgol_long_seasonal, "savgol")

myron_df_wide_season_deployment = myron_df_wide %>%
    mutate(
        year=if_else(month(date)==12, year(date)+1, year(date)),
        year=factor(year, levels=c(2022,2023,2024)),
        season=season(date), 
        season=factor(season, levels = c("Winter", "Spring", "Summer", "Fall")),
        from_sn_start=count_from_season_start(date),
        mos_from_deployment_start = mos_from_deployment_start_fn(date)
    )

deployment_plot = ggplot(
    data=myron_df_wide_season_deployment,
    mapping=aes(
        x=aqs,
        y=beaco2n
    )
) + 
facet_wrap(
    ~ mos_from_deployment_start, 
    ncol=3, 
) +
stat_poly_line() + stat_poly_eq() +
geom_abline(slope=1, intercept=0, color="red") +
geom_point() 
print(deployment_plot)

