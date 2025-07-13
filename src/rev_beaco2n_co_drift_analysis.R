library(tidyverse)
library(sgolay)
library(openair)
library(ggpmisc)
library(gridExtra)
library(zoo)
library(r2r)
source("./import/import_cleaned.R")

AVG_WINDOW = 24*7
SAVGOL_FILTER_LEN = 24*7-1

co = import_co()

dates_of_deployment = co %>%
    filter(parameter == "co", sensor %in% c("aqs", "beaco2n")) %>%
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

days_into_season = function(date_vec) {
    d = as.POSIXct(date_vec)
    sn = season(date_vec)
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

co_long = co %>% 
    filter(parameter=="co", sensor %in% c("aqs","beaco2n")) %>%
    pivot_wider(
        id_cols = c("date","location"),
        names_from = "sensor",
        values_from = "value",
        names_prefix = "original_"
    ) %>%
    group_by(location) %>%
    mutate(
        rolling_aqs = rollmean(original_aqs, k=AVG_WINDOW, align="center", fill=NA),
        rolling_beaco2n = rollmean(original_beaco2n, k=AVG_WINDOW, align="center", fill=NA), 
        savgol_aqs = sgolayfilt(original_aqs, n=SAVGOL_FILTER_LEN, p=4),
        savgol_beaco2n = sgolayfilt(original_beaco2n, n=SAVGOL_FILTER_LEN, p=4)
    ) %>%
    ungroup() %>%
    mutate(
        original_res = original_beaco2n - original_aqs,
        rolling_res = rolling_beaco2n - rolling_aqs,
        savgol_res = savgol_beaco2n - savgol_aqs
    ) %>%
    pivot_longer(
        cols = -c(date, location),
        names_to = c("filter", "sensor"),
        names_pattern = "(original|rolling|savgol)_?(.*)",
        values_to = "value"
    ) %>% 
    semi_join( # Remove sensor/location artifacts
        y = co %>% select(sensor, location) %>% distinct(), 
        by = c("sensor", "location")
    ) %>%
    mutate(
        sn_year=factor(if_else(month(date)==12, year(date)+1, year(date)), levels=2018:2030),
        season=factor(season(date), levels=c("Winter", "Spring", "Summer", "Fall")),
        days_into_sn=days_into_season(date),
    ) %>%
    filter(!is.na(value)) %>%
    left_join(dates_of_deployment, by = c("sensor", "location")) %>%
    mutate(mos_into_deployment = interval(deployment_start, date) %/% months(1))

print(head(co_long), width=Inf)


# Plot timeseries of measurement and reference sensor, faceted by season. 
# `dataset` must be in long format and contain both measurement and reference data.
timeseries_year_season = function(dataset, noise_filter, meas_sensor, meas_location, self_ref=FALSE, ref_sensor, ref_location, filepath) {
    plot_data = dataset %>% filter(sensor==meas_sensor, filter==noise_filter, location==meas_location)
    if (!self_ref) {
        if (missing(ref_sensor) | missing(ref_location)) {
            stop("Must have self_ref=TRUE or provide ref_sensor and ref_location.")
        }
        ref_data = dataset %>% filter(sensor==ref_sensor, filter==noise_filter, location==ref_location)
    } else {
        max_date = plot_data %>% arrange(desc(date)) %>% slice(1) %>% pull(date)
        ref_data = plot_data %>% 
            filter(year(date) <= year(ymd_hms(max_date))-1) %>%
            mutate(
                date = date %m+% years(1),
                sn_year = factor(as.numeric(as.character(sn_year)) + 1, levels = 2018:2030),
            )
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
            days_into_sn_resid = days_into_sn_meas,
            mos_into_deployment_resid = mos_into_deployment_meas,
            filter_resid = filter_meas
        ) %>%
        pivot_longer(
            cols=matches("_(meas|ref|resid)$"),
            names_to=c(".value", "plottype"),
            names_pattern = "^(.*)_(ref|meas|resid)$"
        )
    print(tail(plot_data), width=Inf); stop("CALLED STOP")

    year_by_season_ts = 
        ggplot(
            data=plot_data, 
            mapping=aes(x=days_into_sn, y=value, color=meas_or_ref) 
        ) + 
        facet_grid(rows=vars(sn_year), cols=vars(season)) +
        geom_line() + theme_bw() +
        theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1)) +
        geom_hline(yintercept = 0, color="red") +
        labs(
            title="Timeseries faceted by Year and Season.",
            # subtitle=paste0("Measurement="),
            x = "Days from first day of season",
            y = "CO (ppm)",
            # caption=dataset_name,
            # color=c(paste("Measurement:",meas_sensor,"@",meas_location), paste("Reference:",ref_sensor,"@",ref_location))
        )
    ggsave(plot=year_by_season_ts, filename=filepath)
}

timeseries_year_season(
    co_long, 
    noise_filter="original", 
    meas_sensor="beaco2n", 
    meas_location="myron", 
    self_ref=TRUE,
    # ref_sensor="aqs",
    # ref_location="myron",
    filepath="./test.png"
)