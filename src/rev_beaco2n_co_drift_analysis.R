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
print(head(co))

deployment_dates = hashmap()

first_dates = co %>%
    filter(parameter == "co", sensor %in% c("aqs", "beaco2n")) %>%
    arrange(date) %>%
    group_by(sensor, location) %>%
    filter(!is.na(value)) %>%
    slice(1) %>%
    ungroup()
walk2(
    .x = first_dates$sensor,
    .y = first_dates$location,
    .f = ~ {
        key = c(.x, .y)
        deployment_dates[[key]] = first_dates %>%
        filter(sensor == .x, location == .y) %>%
        pull(date) %>%
        first()
    }
)

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
    days_from_sn_start = round(seconds_from_sn_start/86400, digits=6) # divide by seconds per day
    return(days_from_sn_start)
}

# mos_into_deployment = function(date_vec, sensor, location) {
#     date = ymd_hms(format(date_vec, "%F %T"), tz="UTC")
#     dp_start = deployment_dates[[c(sensor, location)]]
#     mos_into_deployment = round(interval(dp_start, date) %/% months(1), digits=6) 
#     return(mos_into_deployment)
# }

mos_into_deployment = function(date_vec, sensor_vec, location_vec) {
    map2_dbl(sensor_vec, location_vec, ~ {
        key = c(.x, .y)
        dp_start = deployment_dates[[key]]
        date = ymd_hms(format(date_vec, "%F %T"), tz = "UTC")
        round(interval(dp_start, date) %/% months(1), digits = 6)
    })
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
        sn_year=factor(if_else(month(date)==12, year(date)+1, year(date)), levels=2022:2025),
        season=factor(season(date), levels=c("Winter", "Spring", "Summer", "Fall")),
        days_into_sn=days_into_season(date),
        mos_into_deployment = mos_into_deployment(date, sensor, location)
    )
# write.csv(co_long, "./test.csv")