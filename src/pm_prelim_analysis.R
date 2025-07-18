library(tidyverse)
library(openair)
library(checkmate)
library(ggpmisc)
library(gridExtra)
library(sgolay)
library(patchwork)

source("./import/import_cleaned.R")

SAVGOL_FILTER_LEN = 25

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
    days_from_sn_start = seconds_from_sn_start/86400 # divide by seconds per day
    return(days_from_sn_start)
}

pm_histogram = function(df, ...) {
    x_partitions = seq(0,50,by=5)
    yrange = c(0, 1)
    xrange = c(0, 50)
    y_partitions = seq(yrange[1],yrange[2],by=15)
    plt = 
        ggplot(
            data=df,
            mapping=aes(x=value, y=after_stat(count/sum(count))) #, y=after_stat(count/sum(count))
        ) +
        geom_histogram(
            color="black",
            binwidth=5,
            boundary=0,
            position="identity",
            alpha=0.6
        ) +
        # scale_x_continuous(
        #     breaks = x_partitions,
        #     labels = x_partitions
        # ) + 
        # scale_y_continuous(
        #     breaks = y_partitions,
        #     labels = y_partitions
        # ) +
        # coord_cartesian(ylim=yrange, xlim=xrange) +
        theme_bw() + theme(axis.text.x = element_text(angle=60, hjust=1, vjust=1)) +
        labs(
            ...,
            x="PM2.5 (ppm)",
            y="Relative Frequency"
        )

    return(plt)
}

pm_boxplot = function(df, filter="raw", ...) {
    df = df %>% filter(noise_filter==filter)
    # y_partitions = seq(0,150,by=10)
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
        # scale_y_continuous(
        #     breaks = y_partitions,
        #     labels = y_partitions
        # ) +
        labs(
            ...,
            x = "Sensor type @ Location",
            y = "PM2.5 (ppm)"
        )
    return(plt)
}


# Assumes one colocated site, caller must filter
plot_graphs = function(dataset, noise_filter, site) {
    year_by_season_ts = 
        ggplot(
            data=dataset %>% filter(sensor != "res"), # residual confuses plot.
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
            title=paste("BEACO2N vs QuantAQ PM2.5 at",site),
            subtitle=paste0("2022-2025 Faceted by Year and Season. Data: ", noise_filter),
            x = "Days since first day of season",
            y = "PM2.5 (ppm)",
            caption=site
        )
    ggsave(
        plot=year_by_season_ts, 
        filename=paste0("./plots/pm_prelim_analysis/",noise_filter,"/",site,"/year_by_sn_timeseries.png"),
        height=7, width=7, dpi=300
    )

    by_season_ts = 
        ggplot(
            data=dataset %>% filter(sensor != "res"), 
            mapping=aes(
                x=from_sn_start,
                y=value,
                color=factor(year),
                linetype=factor(sensor, c("beaco2n","quantaq"))
            )
        ) + 
        # scale_linetype_manual(values=c("dotdash","solid","longdash")) +
        facet_wrap(~ season) +
        geom_line() + theme_bw() +
        theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1)) +
        geom_hline(yintercept = 0, color="red") +
        labs(
            title=paste("BEACO2N vs QuantAQ PM2.5 at",site),
            subtitle=paste0("2022-2024 Faceted by Season. Data: ", noise_filter),
            x = "Days since first day of season",
            y = "PM2.5 (ppm)",
            color="Year",
            linetype="Sensor",
            caption=site
        )
    ggsave(
        plot=by_season_ts, 
        filename=paste0("./plots/pm_prelim_analysis/",noise_filter,"/",site,"/by_sn_timeseries.png"),
        height=7, width=7, dpi=300
    )

    year_by_season_box_nores = 
        ggplot(
            data=dataset %>% filter(sensor != "res"), # Residual scaled poorly on plot
            mapping=aes(
                x=sensor, 
                y=value,
                fill=sensor
            )
        ) + 
        facet_grid(rows=vars(year), cols=vars(season), scales="free") +
        geom_boxplot() + theme_bw() +
        theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1)) +
        labs(
            title="Myron: AQS vs BEACO2N Summary Boxplots",
            subtitle=paste0("2022-2024 Faceted by Year and Season. Data: ", noise_filter),
            x = "Sensor",
            y = "PM2.5 (ppm)",
            caption=site
        )
    ggsave(
        plot=year_by_season_box_nores, 
        filename=paste0("./plots/pm_prelim_analysis/",noise_filter,"/",site,"/yr_by_sn_box_nores.png"),
        height=7, width=7, dpi=300
    )

    year_by_season_box_onlyres = 
        ggplot(
            data=dataset %>% filter(sensor == "res"), 
            mapping=aes(
                x=sensor, 
                y=value
            )
        ) + 
        facet_grid(rows=vars(year), cols=vars(season), scales="free") +
        geom_boxplot() + theme_bw() +
        theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1)) +
        labs(
            title="Myron: Residual Summary Boxplots",
            subtitle=paste0("2022-2024 Faceted by Year and Season. Data: ", noise_filter),
            x = "Sensor",
            y = "PM2.5 (ppm)",
            caption=site
        )
    ggsave(
        plot=year_by_season_box_onlyres, 
        filename=paste0("./plots/pm_prelim_analysis/",noise_filter,"/",site,"/yr_by_sn_box_onlyres.png"),
        height=7, width=7, dpi=300
    )

    by_season_box_nores = 
        ggplot(
            data=dataset %>% filter(sensor != "res"), # Residual scaled poorly on plot
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
            caption=site
        )
    ggsave(
        plot=by_season_box_nores, 
        filename=paste0("./plots/pm_prelim_analysis/",noise_filter,"/",site,"/by_sn_box_nores.png"),
        height=7, width=7, dpi=300
    )

    by_season_box_onlyres = 
        ggplot(
            data=dataset %>% filter(sensor == "res"), 
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
            caption=site
        )
    ggsave(
        plot=by_season_box_onlyres, 
        filename=paste0("./plots/pm_prelim_analysis/",noise_filter,"/",site,"/by_sn_box_onlyres.png"),
        height=7, width=7, dpi=300
    )

    by_year_box_nores = 
        ggplot(
            data=dataset %>% filter(sensor != "res"), # Residual scaled poorly on plot
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
            caption=site
        )
    ggsave(
        plot=by_year_box_nores, 
        filename=paste0("./plots/pm_prelim_analysis/",noise_filter,"/",site,"/by_year_box_nores.png"),
        height=7, width=7, dpi=300
    )

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
            caption=site
        )
    ggsave(
        plot=by_year_box_onlyres, 
        filename=paste0("./plots/pm_prelim_analysis/",noise_filter,"/",site,"/by_year_box_onlyres.png"),
        height=7, width=7, dpi=300
    )

}

colocated_pm = colocated_pm %>% 
    pivot_wider(
        id_cols=c("date","location"),
        names_from=sensor,
        names_prefix="raw_",
        values_from="value"
    ) %>% mutate(
        raw_res = raw_beaco2n - raw_quantaq,
        savgol_beaco2n = sgolayfilt(raw_beaco2n, n=SAVGOL_FILTER_LEN, p=4),
        savgol_quantaq = sgolayfilt(raw_quantaq, n=SAVGOL_FILTER_LEN, p=4),
        savgol_res = savgol_beaco2n - savgol_quantaq,
        year=if_else(month(date)==12, year(date)+1, year(date)), 
        year=factor(year, levels=c(2022,2023,2024,2025)),
        season=season(date), 
        season=factor(season, levels = c("Winter", "Spring", "Summer", "Fall")),
        from_sn_start=count_from_season_start(date)
    ) %>% pivot_longer(
        cols=-c(date, location, year, season, from_sn_start),
        names_to=c("noise_filter","sensor"),
        names_pattern="^(raw|savgol)_(beaco2n|quantaq|res)$",
        values_to="value"
    )

# filter = "savgol"; site="dpw" 
for (filter in c("raw","savgol")) {
    for (site in c("dpw","pha","pema")) {
        plot_graphs(colocated_pm %>% filter(noise_filter==filter, location==site), noise_filter=filter, site=site)
        box = pm_boxplot(colocated_pm %>% filter(location==site), filter=filter, title=paste0("BEACO2N vs QuantAQ: PM2.5 at ", site))
        ggsave(plot=box, filename=paste0("./plots/pm_prelim_analysis/",filter,"/",site,"/box.png"))
    }
}

stat_table = colocated_pm %>%
pivot_wider (
    # id_cols = -sensor,
    names_from = sensor,
    values_from = value
) %>% 
filter(if_all(c(beaco2n, quantaq, res), ~ !is.na(.))) %>%
group_by(noise_filter, location) %>%
summarize(
    `R^2` = round(cor(beaco2n, quantaq)^2,digits=4),
    RMSE = round(sqrt(mean((beaco2n-quantaq)^2)), digits=4),
    MBE = round(mean(beaco2n-quantaq),digits=4),
    quantaq_mean = round(digits=4, mean(quantaq)),
    beaco2n_mean = round(digits=4,mean(beaco2n)),
    quantaq_sd = round(digits=4,sd(quantaq)),
    beaco2n_sd = round(digits=4,sd(beaco2n)) 
)
png(file="./plots/pm_prelim_analysis/raw/stats.png", height=8.5, width=11, units="in", res=300)
grid.table(stat_table)
dev.off()


## PLOT PATCHED HISTOGRAMS
bcn_dpw_hist = pm_histogram(
    colocated_pm %>% filter(location=="dpw",sensor=="beaco2n",noise_filter==filter),
    title="BEACO2N DPW",
    subtitle=paste0(
        "mean=", stat_table %>% filter(location=="dpw") %>% select(beaco2n_mean) %>% pull,
        " sd=", stat_table %>% filter(location=="dpw") %>% select(beaco2n_sd) %>% pull
    )
)
bcn_pha_hist = pm_histogram(
    colocated_pm %>% filter(location=="pha",sensor=="beaco2n",noise_filter==filter),
    title="BEACO2N PHA",
    subtitle=paste0(
        "mean=", stat_table %>% filter(location=="pha") %>% select(beaco2n_mean) %>% pull,
        " sd=", stat_table %>% filter(location=="pha") %>% select(beaco2n_sd) %>% pull
    )
)
bcn_pema_hist = pm_histogram(
    colocated_pm %>% filter(location=="pema",sensor=="beaco2n",noise_filter==filter),
    title="BEACO2N PEMA",
    subtitle=paste0(
        "mean=", stat_table %>% filter(location=="pema") %>% select(beaco2n_mean) %>% pull,
        " sd=", stat_table %>% filter(location=="pema") %>% select(beaco2n_sd) %>% pull
    )
)
qaq_dpw_hist = pm_histogram(
    colocated_pm %>% filter(location=="dpw",sensor=="quantaq",noise_filter==filter),
    title="QuantAQ DPW",
    subtitle=paste0(
        "mean=", stat_table %>% filter(location=="dpw") %>% select(quantaq_mean) %>% pull,
        " sd=", stat_table %>% filter(location=="dpw") %>% select(quantaq_sd) %>% pull
    )
)
qaq_pha_hist = pm_histogram(
    colocated_pm %>% filter(location=="pha",sensor=="quantaq",noise_filter==filter),
    title="QuantAQ PHA",
    subtitle=paste0(
        "mean=", stat_table %>% filter(location=="pha") %>% select(quantaq_mean) %>% pull,
        " sd=", stat_table %>% filter(location=="pha") %>% select(quantaq_sd) %>% pull
    )
)
qaq_pema_hist = pm_histogram(
    colocated_pm %>% filter(location=="pema",sensor=="quantaq",noise_filter==filter),
    title="QuantAQ PEMA",
    subtitle=paste0(
        "mean=", stat_table %>% filter(location=="pema") %>% select(quantaq_mean) %>% pull,
        " sd=", stat_table %>% filter(location=="pema") %>% select(quantaq_sd) %>% pull
    )
)
patch = (qaq_dpw_hist + qaq_pha_hist + qaq_pema_hist) / (bcn_dpw_hist + bcn_pha_hist + bcn_pema_hist)
ggsave(plot=patch, file=paste0("./plots/pm_prelim_analysis/",filter,"/patched_histogram.png"), height=7, width=7, dpi=300)
        