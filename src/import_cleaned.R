library(tidyverse)

# Helper function to import data from "./clean_data/all_co_temp_rh_2022.csv"
import_cleaned = function() {
    all_params = read.csv("./clean_data/all_co_temp_rh_2022.csv")

    all_params$date = as.POSIXct(combined_df$date, tz="UTC")
    all_params = all_params %>% pivot_longer( # Convert combined_df to tidy format
        cols = -date,
        names_to = c("parameter","sensor","location"),
        values_to = "value",
        names_pattern = "([^_]+)_([^_]+)_(.+)"
    )
    return(all_params)
}