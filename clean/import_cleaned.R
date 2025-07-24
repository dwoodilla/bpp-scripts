library(tidyverse)

# NOTE: Clearing the cache does not actually force a refresh of data originating from uncleaned data,
# because the python cleaner also needs to run. This method should execute the cleaner when file read fails.
import_co = function(city="providence") {
    if (city=="berkeley") all_co = read.csv("./clean_data/merged_berkeley_co.csv")
    else all_co = read.csv("./clean_data/merged_co.csv")

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