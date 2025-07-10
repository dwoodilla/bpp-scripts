library(tidyverse)
library(openair)
library(checkmate)
library(ggpmisc)
library(gridExtra)
library(sgolay)

source("./import/import_cleaned.R")

SAVGOL_FILTER_LEN = 301

colocated_pm = import_pm() %>% 
    filter(location %in% c("dpw","pha","pema"), sensor %in% c("quantaq","beaco2n"), parameter=="pm25") %>%
    filter(value < 3*sd(value, na.rm=TRUE))
# print(head(colocated_pm))

# print(head(colocated_pm_wide))
# write.csv(colocated_pm_wide, "./savgol_test.csv")
# stop("CALLED STOP")

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
    days_from_sn_start = trunc(seconds_from_sn_start/86400) # divide by seconds per day
    return(days_from_sn_start)
}

pm_boxplot = function(df, filter="raw", ...) {
    df = df %>% filter(noise_filter==filter)
    y_partitions = seq(0,150,by=10)
    plt = 
        ggplot(
            data=df,
            mapping=aes(
                x=interaction(sensor, location, sep=" @ ", lex.order=FALSE),
                y=value,
                fill=sensor
            )
        ) + 
        geom_boxplot() + 
        theme_bw() + theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1)) +
        scale_y_continuous(
            breaks = y_partitions,
            labels = y_partitions
        ) +
        labs(
            ...,
            x = "Sensor type @ Location",
            y = "PM2.5 (ppm)"
        )
    return(plt)
}

# Assumes one colocated site, caller must filter
plot_graphs = function(dataset, noise_filter) {
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
            subtitle=paste0("2022-2024 Faceted by Year and Season. Data: ", noise_filter),
            x = "Seconds from first day of season",
            y = "PM2.5 (ppm)",
            caption=noise_filter
        )
    ggsave(plot=year_by_season_ts, filename=paste0("./plots/pm_prelim_analysis/", noise_filter, "/year_by_sn_timeseries.png"))

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
            subtitle=paste0("2022-2024 Faceted by Season. Data: ", noise_filter),
            x = "Seconds from first day of season",
            y = "PM2.5 (ppm)",
            color="Sensor",
            linetype="Year",
            caption=noise_filter
        )
    ggsave(plot=by_season_ts, filename=paste0("./plots/pm_prelim_analysis/", noise_filter, "/by_sn_timeseries.png"))

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
            subtitle=paste0("2022-2024 Faceted by Year and Season. Data: ", noise_filter),
            x = "Sensor",
            y = "PM2.5 (ppm)",
            caption=noise_filter
        )
    ggsave(plot=year_by_season_box_nores, filename=paste0("./plots/pm_prelim_analysis/",noise_filter,"/yr_by_sn_box_nores.png"))

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
            subtitle=paste0("2022-2024 Faceted by Year and Season. Data: ", noise_filter),
            x = "Sensor",
            y = "PM2.5 (ppm)",
            caption=noise_filter
        )
    ggsave(plot=year_by_season_box_onlyres, filename=paste0("./plots/pm_prelim_analysis/",noise_filter,"/yr_by_sn_box_onlyres.png"))

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
            subtitle=paste0("2022-2024 Faceted by Season. Data: ", noise_filter),
            x = "Sensor",
            y = "PM2.5 (ppm)",
            caption=noise_filter
        )
    ggsave(plot=by_season_box_nores, filename=paste0("./plots/pm_prelim_analysis/",noise_filter,"/by_sn_box_nores.png"))

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
            subtitle=paste0("2022-2024 Faceted by Season. Data: ", noise_filter),
            x = "Sensor",
            y = "PM2.5 (ppm)",
            caption=noise_filter
        )
    ggsave(plot=by_season_box_onlyres, filename=paste0("./plots/pm_prelim_analysis/",noise_filter,"/by_sn_box_onlyres.png"))

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
            subtitle=paste0("2022-2024 Faceted by Year. Data: ", noise_filter),
            x = "Sensor",
            y = "PM2.5 (ppm)",
            fill="Sensor",
            caption=noise_filter
        )
    ggsave(plot=by_year_box_nores, filename=paste0("./plots/pm_prelim_analysis/",noise_filter,"/by_year_box_nores.png"))

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
            subtitle=paste0("2022-2024 Faceted by Year. Data: ", noise_filter),
            x = "Sensor",
            y = "PM2.5 (ppm)",
            caption=noise_filter
        )
    ggsave(plot=by_year_box_onlyres, filename=paste0("./plots/pm_prelim_analysis/",noise_filter,"/by_year_box_onlyres.png"))

}

colocated_pm = colocated_pm %>% 
    pivot_wider(
        id_cols=c("date","location"),
        names_from=sensor,
        names_prefix="raw_",
        values_from="value"
    ) %>% mutate(
        savgol_beaco2n = sgolayfilt(raw_beaco2n, n=SAVGOL_FILTER_LEN, p=4),
        savgol_quantaq = sgolayfilt(raw_quantaq, n=SAVGOL_FILTER_LEN, p=4),
        year=if_else(month(date)==12, year(date)+1, year(date)), 
        year=factor(year, levels=c(2022,2023,2024)),
        season=season(date), 
        season=factor(season, levels = c("Winter", "Spring", "Summer", "Fall")),
        from_sn_start=count_from_season_start(date)
    ) %>% pivot_longer(
        cols=-c(date, location, year, season, from_sn_start),
        names_to=c("noise_filter","sensor"),
        names_pattern="^(raw|savgol)_(beaco2n|quantaq)$",
        values_to="value"
    )
write.csv(colocated_pm %>% filter(sensor=="quantaq", noise_filter=="savgol"), file="./pm_test.csv")
stop("CALLED STOP")

plot_graphs(colocated_pm %>% filter(noise_filter=="savgol", location=="dpw"), noise_filter="savgol")


# filter = "savgol" # Is this doing anything?
# test_box = pm_boxplot(colocated_pm, noise_filter=filter, title="Test")
# ggsave(plot=test_box, filename=paste0("./test_box_",filter,".png"))
# stop("CALLED STOP")
