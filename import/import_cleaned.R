library(tidyverse)

# Helper function to import data from "./clean_data/all_co_temp_rh_2022.csv"
import_co = function() {
    all_co = read.csv("./clean_data/all_co_temp_rh_2022.csv")

    all_co$date = as.POSIXct(all_co$date, tz="UTC")
    all_co = all_co %>% pivot_longer( # Convert combined_df to tidy format
        cols = -date,
        names_to = c("parameter","sensor","location"),
        values_to = "value",
        names_pattern = "([^_]+)_([^_]+)_(.+)"
    )
    return(all_co)
}

import_pm = function() {
    all_pm = read.csv("./clean_data/merged_pm.csv")

    all_pm$date = as.POSIXct(all_pm$date, tz="UTC")
    all_pm = all_pm %>% pivot_longer( # Convert combined_df to tidy format
        cols = -date,
        names_to = c("parameter","sensor","location"),
        values_to = "value",
        names_pattern = "([^_]+)_([^_]+)_(.+)"
    )
    return(all_pm)
}