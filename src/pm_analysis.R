library(tidyverse)
library(sgolay)
# library(openair)
library(ggpmisc)
# library(gridExtra)
library(zoo)
# library(r2r)
library(checkmate)
source("./import/import_cleaned.R")

AVG_WINDOW = 24
SAVGOL_FILTER_LEN = 24+1

pm = import_pm()
# co_long = read_csv("./clean_data/beaco2n_drift_cleaned.csv")

dates_of_deployment = pm %>%
    filter(parameter %in% c("pm25","pm1","pm10"), sensor %in% c("quantaq", "beaco2n")) %>%
    arrange(date) %>%
    group_by(sensor, location) %>%
    filter(!is.na(value)) %>%
    slice(1) %>%
    ungroup() %>%
    select(sensor, location, deployment_start=date)

season = function(date_vec) {
    m = month(as.Date(date_vec))
    return(
        case_when(
            m %in% c(12,1,2) ~ "Winter",
            m %in% 3:5 ~ "Spring",
            m %in% 6:8 ~ "Summer",
            m %in% 9:11 ~ "Fall"
        )
    )
}

hours_into_season = function(date_vec) {
    d = as.POSIXct(date_vec)
    sn = season(date_vec)
    sn_start = 
        case_when(
            sn=="Winter" ~ make_datetime(year=if_else(month(d)==12, year(d), year(d)-1), month=12, day=1),
            sn=="Spring" ~ make_datetime(year=year(d), month=3, day=1),
            sn=="Summer" ~ make_datetime(year=year(d), month=6, day=1),
            sn=="Fall" ~ make_datetime(year=year(d), month=9, day=1)
        )
    hours_from_sn_start = interval(start=sn_start, end=d, tz="UTC") %/% hours(1)
    # days_from_sn_start = seconds_from_sn_start/86400 # divide by seconds per day
    return(hours_from_sn_start)
}

pm_long = pm %>% 
    filter(parameter %in% c("pm25","pm1","pm10"), sensor %in% c("quantaq", "beaco2n")) %>%
    pivot_wider(
        id_cols = c("date","location"),
        names_from = "sensor",
        values_from = "value",
        names_prefix = "original_"
    ) %>%
    group_by(location) %>%
    mutate(
        rolling_quantaq = rollmean(original_quantaq, k=AVG_WINDOW, align="center", fill=NA),
        rolling_beaco2n = rollmean(original_beaco2n, k=AVG_WINDOW, align="center", fill=NA), 
        savgol_quantaq = sgolayfilt(original_quantaq, n=SAVGOL_FILTER_LEN, p=4),
        savgol_beaco2n = sgolayfilt(original_beaco2n, n=SAVGOL_FILTER_LEN, p=4)
    ) %>%
    ungroup() %>%
    mutate(
        original_res = original_beaco2n - original_quantaq,
        rolling_res = rolling_beaco2n - rolling_quantaq,
        savgol_res = savgol_beaco2n - savgol_quantaq
    ) %>%
    pivot_longer(
        cols = -c(date, location),
        names_to = c("filter", "sensor"),
        names_pattern = "(original|rolling|savgol)_?(.*)",
        values_to = "value"
    ) %>% 
    semi_join( # Remove sensor/location artifacts
        y = pm %>% select(sensor, location) %>% distinct(), 
        by = c("sensor", "location")
    ) %>%
    mutate(
        sn_year=factor(if_else(month(date)==12, year(date)+1, year(date)), levels=2018:2030),
        season=factor(season(date), levels=c("Winter", "Spring", "Summer", "Fall")),
        hours_into_sn=hours_into_season(date),
    ) %>%
    filter(!is.na(value)) %>%
    left_join(dates_of_deployment, by = c("sensor", "location")) %>%
    mutate(
        mos_into_deployment = interval(deployment_start, date) %/% months(1),
        hrs_into_deployment = interval(deployment_start, date) %/% hours(1)
    ) %>%
    select(everything(), -deployment_start)


arrange_plot_data = function(dataset, noise_filter, meas_sensor, meas_location, self_ref=FALSE, ref_sensor, ref_location) {
    plot_data = dataset %>% filter(sensor==meas_sensor, filter==noise_filter, location==meas_location)
    if (!self_ref) {
        if (missing(ref_sensor) | missing(ref_location)) {
            stop("Must have self_ref=TRUE or provide ref_sensor and ref_location.")
        }
        ref_data = dataset %>% filter(sensor==ref_sensor, filter==noise_filter, location==ref_location)
    } else {
        first_year_ref = plot_data %>%
            filter(mos_into_deployment <= 12) %>%
            mutate(
                month = month(date),
                day = day(date),
                hour = hour(date)
            ) %>%
            group_by(month, day, hour) %>%
            slice_min(mos_into_deployment, with_ties = FALSE) %>%
            ungroup() %>%
            select(month, day, hour, value, date) %>%
            rename(value_ref = value, fy_ref_date = date)
        ref_data = plot_data %>%
            mutate(
                month=month(date),
                day=day(date),
                hour=hour(date)
            ) %>%
            left_join(first_year_ref, by=c("month","day","hour"), relationship="many-to-one") %>%
            mutate(value=value_ref) %>%
            select(date, everything(), -month, -day, -hour, -fy_ref_date)
    }
    plot_data = plot_data %>% 
        left_join(y=ref_data, by="date", suffix=c("_meas","_ref")) %>%
        mutate(
            value_resid = value_meas - value_ref,
            # Copy remaining data from measurement to residual. 
            # mos_into_deployment may need to depend on whether both sensors have data or not.
            sensor_resid = "residual",
            sn_year_resid = sn_year_meas,
            season_resid = season_meas,
            hours_into_sn_resid = hours_into_sn_meas,
            mos_into_deployment_resid = mos_into_deployment_meas,
            hrs_into_deployment_resid = hrs_into_deployment_meas,
            filter_resid = filter_meas,
            mos_into_deployment_ref=mos_into_deployment_meas,
            hrs_into_deployment_ref=hrs_into_deployment_meas,
            season_ref=season_meas,
            hours_into_sn_ref=hours_into_sn_meas
        ) %>%
        pivot_longer(
            cols=matches("_(meas|ref|resid)$"),
            names_to=c(".value", "plottype"),
            names_pattern = "^(.*)_(ref|meas|resid)$"
        )
    if (any(is.na(plot_data$sn_year)) | any(is.na(plot_data$season))) {
        warning("Dropping rows of plot data with NA faceting variables.")
        plot_data = plot_data %>% filter(!is.na(sn_year), !is.na(season))
    }
    return(plot_data %>% filter(!is.na(value)))
}

# Plot timeseries of measurement and reference sensor, faceted by season. 
# `dataset` must be in long format and contain both measurement and reference data.
timeseries_year_season = function(dataset, noise_filter, meas_sensor, meas_location, self_ref=FALSE, ref_sensor, ref_location, filepath) {
    plot_data = arrange_plot_data(dataset, noise_filter, meas_sensor, meas_location, self_ref, ref_sensor, ref_location)
    ts_year_season = 
        ggplot(
            data=plot_data %>% filter(!is.na(value)), 
            mapping=aes(x=hours_into_sn, y=value, color=plottype) 
        ) + 
        facet_grid(rows=vars(sn_year), cols=vars(season)) +
        geom_line(na.rm=TRUE) + theme_bw() +
        theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1)) +
        geom_hline(yintercept = 0, color="red") +
        labs(
            title="Timeseries faceted by Year and Season.",
            x = "Days from first day of season",
            y = "PM (ppm)",
        )
    ggsave(plot=ts_year_season, filename=filepath)
}
timeseries_season = function(dataset, noise_filter, meas_sensor, meas_location, self_ref=FALSE, ref_sensor, ref_location, filepath) {
    plot_data = arrange_plot_data(dataset, noise_filter, meas_sensor, meas_location, self_ref, ref_sensor, ref_location) 

    ts_season = 
        ggplot(
            data=plot_data, 
            mapping=aes(
                x=hours_into_sn,
                y=value,
                color=sn_year, 
                linetype=sensor
            )
        ) + 
        facet_wrap(~ season) +
        geom_line() + theme_bw() +
        theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1)) +
        geom_hline(yintercept = 0, color="red") +
        labs(
            title="Timeseries faceted by Season",
            x = "Days from first day of season",
            y = "PM (ppm)",
            color="Sensor",
            linetype="Year"
        )
    ggsave(plot=ts_season, filename=filepath)
 
}
# timeseries_year_season( 
#     co_long, noise_filter="original", meas_sensor="beaco2n", meas_location="myron", self_ref=TRUE, filepath="./ts_yr_sn.png"
# )
# timeseries_season(
#     co_long, noise_filter="original", meas_sensor="beaco2n", meas_location="myron", self_ref=TRUE, filepath="./ts_sn.png"
# )

box_year_season = function(dataset, noise_filter, meas_sensor, meas_location, self_ref=FALSE, ref_sensor, ref_location, filepath) {
    plot_data = arrange_plot_data(dataset, noise_filter, meas_sensor, meas_location, self_ref, ref_sensor, ref_location)
    box_year_season = 
        ggplot(
            data=plot_data %>% filter(!is.na(value)), 
            mapping=aes(x=sensor, y=value, color=plottype) 
        ) + 
        facet_grid(rows=vars(sn_year), cols=vars(season)) +
        geom_boxplot(na.rm=TRUE) + theme_bw() +
        theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1)) +
        # geom_hline(yintercept = 0, color="red") +
        labs(
            title="Boxplot faceted by Year and Season.",
            x = "Sensor type",
            y = "PM (ppm)",
        )
    ggsave(plot=box_year_season, filename=filepath)
}
box_season = function(dataset, noise_filter, meas_sensor, meas_location, self_ref=FALSE, ref_sensor, ref_location, filepath) {
    plot_data = arrange_plot_data(dataset, noise_filter, meas_sensor, meas_location, self_ref, ref_sensor, ref_location)
    box_sn = 
        ggplot(
            data=plot_data, # Residual scaled poorly on plot
            mapping=aes(
                x=sn_year, 
                y=value,
                fill=sensor,
            )
        ) + 
        facet_wrap(~ season) +
        geom_boxplot() + theme_bw() +
        theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1)) +
        labs(
            title="Boxplot faceted by season.",
            x = "Year",
            y = "PM (ppm)"
        )
    ggsave(plot=box_sn, filename=filepath)
}
# box_year_season(co_long, "original", "beaco2n", "myron", FALSE, "aqs", "myron", "./box_yr_sn.png")
# box_season(co_long, "original", "beaco2n", "myron", FALSE, "aqs", "myron", "./box_sn_notself.png")
# box_season(co_long, "original", "beaco2n", "myron", TRUE, filepath="./box_sn_self.png")

violin_year_season = function(dataset, noise_filter, meas_sensor, meas_location, self_ref=FALSE, ref_sensor, ref_location, filepath) {
    plot_data = arrange_plot_data(dataset, noise_filter, meas_sensor, meas_location, self_ref, ref_sensor, ref_location)
    violin_year_season = 
        ggplot(
            data=plot_data %>% filter(!is.na(value)), 
            mapping=aes(x=sensor, y=value, color=plottype) 
        ) + 
        facet_grid(rows=vars(sn_year), cols=vars(season)) +
        geom_violin(na.rm=TRUE) + theme_bw() +
        theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1)) +
        # geom_hline(yintercept = 0, color="red") +
        labs(
            title="Violin plot faceted by Year and Season.",
            x = "Sensor type",
            y = "PM (ppm)",
        )
    ggsave(plot=violin_year_season, filename=filepath)
}
violin_season = function(dataset, noise_filter, meas_sensor, meas_location, self_ref=FALSE, ref_sensor, ref_location, filepath) {
    plot_data = arrange_plot_data(dataset, noise_filter, meas_sensor, meas_location, self_ref, ref_sensor, ref_location)
    violin_sn = 
        ggplot(
            data=plot_data, # Residual scaled poorly on plot
            mapping=aes(
                x=sn_year, 
                y=value,
                fill=sensor,
            )
        ) + 
        facet_wrap(~ season) +
        geom_violin() + theme_bw() +
        theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1)) +
        labs(
            title="Boxplot faceted by season.",
            x = "Year",
            y = "PM (ppm)"
        )
    ggsave(plot=violin_sn, filename=filepath)
}
# violin_year_season(co_long, "original", "beaco2n", "myron", FALSE, "aqs", "myron", "./violin_yr_sn.png")
# violin_season(co_long, "original", "beaco2n", "myron", FALSE, "aqs", "myron", "./violin_sn_notself.png")
# violin_season(co_long, "original", "beaco2n", "myron", TRUE, filepath="./violin_sn_self.png")

deployment_correlation = function(dataset, noise_filter, meas_sensor, meas_location, self_ref=FALSE, ref_sensor, ref_location, filepath) {
    plot_data = arrange_plot_data(dataset, noise_filter, meas_sensor, meas_location, self_ref, ref_sensor, ref_location) %>%
        pivot_wider(
            id_cols=c(date, mos_into_deployment, hrs_into_deployment, sn_year, season),
            names_from=plottype,
            values_from=value
        ) %>%
        filter(!is.na(ref))

    deployment_plot = 
        ggplot(
            data=plot_data,
            mapping=aes(
                x=ref,
                y=meas
            )
        ) + 
        facet_wrap(
            ~ mos_into_deployment, 
            ncol=3, 
        ) +
        stat_poly_line() + stat_poly_eq() +
        geom_abline(slope=1, intercept=0, color="red") +
        geom_point(alpha=0.05) + 
        labs(
            title="Dep plot test"
            # subtitle="Correlation over time since July 2022"
        )
    ggsave(plot=deployment_plot, filename=filepath, width=16, height=22, units="in", dpi=300)
}

# deployment_correlation(
#     dataset=co_long,
#     noise_filter="savgol",
#     meas_sensor="beaco2n",
#     meas_location="myron",
#     self_ref=TRUE,
#     ref_sensor="aqs",
#     ref_location="myron",
#     filepath="./test_depcor.png"
# )

plot_all = function(dataset, noise_filter, meas_sensor, meas_location, self_ref=FALSE, ref_sensor, ref_location, basepath) {
    assert(endsWith(basepath, "/"))

    timeseries_year_season(
        dataset = dataset,
        noise_filter = noise_filter,
        meas_sensor = meas_sensor,
        meas_location = meas_location,
        self_ref = self_ref,
        ref_sensor = ref_sensor,
        ref_location = ref_location,
        filepath = paste0(basepath, "timeseries_year_season.png")
    )
    timeseries_season(
        dataset = dataset,
        noise_filter = noise_filter,
        meas_sensor = meas_sensor,
        meas_location = meas_location,
        self_ref = self_ref,
        ref_sensor = ref_sensor,
        ref_location = ref_location,
        filepath = paste0(basepath, "timeseries_season.png")
    )
    box_year_season(
        dataset = dataset,
        noise_filter = noise_filter,
        meas_sensor = meas_sensor,
        meas_location = meas_location,
        self_ref = self_ref,
        ref_sensor = ref_sensor,
        ref_location = ref_location,
        filepath = paste0(basepath, "box_year_season.png")
    )
    box_season(
        dataset = dataset,
        noise_filter = noise_filter,
        meas_sensor = meas_sensor,
        meas_location = meas_location,
        self_ref = self_ref,
        ref_sensor = ref_sensor,
        ref_location = ref_location,
        filepath = paste0(basepath, "box_season.png")
    )
    violin_year_season(
        dataset = dataset,
        noise_filter = noise_filter,
        meas_sensor = meas_sensor,
        meas_location = meas_location,
        self_ref = self_ref,
        ref_sensor = ref_sensor,
        ref_location = ref_location,
        filepath = paste0(basepath, "violin_year_season.png")
    )
    violin_season(
        dataset = dataset,
        noise_filter = noise_filter,
        meas_sensor = meas_sensor,
        meas_location = meas_location,
        self_ref = self_ref,
        ref_sensor = ref_sensor,
        ref_location = ref_location,
        filepath = paste0(basepath, "violin_season.png")
    )
}

locations = list("dpw","pema","pha")
for(location in locations) {
    plot_all(
    dataset=pm_long,
    noise_filter="original",
    meas_sensor="beaco2n",
    meas_location=location,
    self_ref=FALSE,
    ref_sensor="quantaq",
    ref_location=location,
    basepath=paste0("./plots/pm_analysis/", location, "/")
    )
}