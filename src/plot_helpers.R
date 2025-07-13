library(tidyverse)
library(sgolay)
library(ggpmisc)
library(zoo)
library(checkmate)
source("./import/import_cleaned.R")

dates_of_deployment = function(df, param, sensor_list) {
    ret = df %>%
        filter(parameter == param, sensor %in% sensor_list) %>%
        arrange(date) %>%
        group_by(sensor, location) %>%
        filter(!is.na(value)) %>%
        slice(1) %>%
        ungroup() %>%
        select(sensor, location, deployment_start=date)
    return(ret)
}
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
elongate = function(df, param, sensor_list) {
    co_long = co %>% 
        filter(parameter==param, sensor %in% sensor_list) %>%
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
            hrs_into_deployment = interval(deployment_start, date) %/% hours(1)
        ) %>%
        select(everything(), -deployment_start)


}