library(tidyverse)
library(sgolay)
library(ggpmisc)
library(zoo)
library(checkmate)
# source("./import/import_cleaned.R")

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
    return(hours_from_sn_start)
}


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
            hrs_into_deployment_month_resid = hrs_into_deployment_month_meas,
            filter_resid = filter_meas,
            mos_into_deployment_ref=mos_into_deployment_meas,
            hrs_into_deployment_ref=hrs_into_deployment_meas,
            hrs_into_deployment_month_ref = hrs_into_deployment_month_meas,
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

arrange_deployment_data = function(dataset, noise_filter, meas_sensor, meas_location, self_ref=FALSE, ref_sensor, ref_location) {
    ret = arrange_plot_data(dataset, noise_filter, meas_sensor, meas_location, self_ref, ref_sensor, ref_location) %>%
        pivot_wider(
            id_cols=c(date, mos_into_deployment, hrs_into_deployment, hrs_into_deployment_month, sn_year, season),
            names_from=plottype,
            values_from=value
        ) %>%
        filter(!is.na(ref))
    return(ret)
}

elongate_co_bcn_aqs = function(co) {
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
            hours_into_sn=hours_into_season(date),
        ) %>%
        filter(!is.na(value)) %>%
        left_join(dates_of_deployment, by = c("sensor", "location")) %>%
        mutate(
            mos_into_deployment = interval(deployment_start, date) %/% months(1),
            hrs_into_deployment = interval(deployment_start, date) %/% hours(1),
            hrs_into_deployment_month = interval(
                deployment_start %m+% months(mos_into_deployment),
                date,
                tz="UTC"
            ) %/% hours(1)
        )
    return(co_long)
}

elongate_pm_bcn_qaq = function(pm) {
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
    return(pm_long)
}

dates_of_deployment_co_bcn_aqs = function(co) {

}

dates_of_deployment_pm_bcn_qaq = function(pm) {
    dates_of_deployment = pm %>%
    filter(parameter %in% c("pm25","pm1","pm10"), sensor %in% c("quantaq", "beaco2n")) %>%
    arrange(date) %>%
    group_by(sensor, location) %>%
    filter(!is.na(value)) %>%
    slice(1) %>%
    ungroup() %>%
    select(sensor, location, deployment_start=date)
}
