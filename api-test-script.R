# **See ReadMe for QuantAQ's R API here: https://github.com/quant-aq/r-quantaq/blob/main/README.md**
# See docs for QuantAQ's HTTP API here: https://docs.quant-aq.com/software-apis-and-libraries/quantaq-cloud-api 

rm(list=ls()) # clear the R environment between script runs
library(jsonlite)
# install.packages("devtools") # uncomment to install devtools package, needed to install R API
# devtools::install_github(repo="quant-aq/r-quantaq") # Uncomment to install QuantAQ's R API from GitHub.
# install.packages("httr2") # uncomment to install httr2, needed to use QuantAQ's HTTP API.

library(QuantAQAPIClient)
library(httr2)
library(clock)
library(dplyr)
library(tidyr)

# Create data frames from CSV files in script directory
measurement_files = list.files( # Make a list of file names (locations) in BEACO2N_measurements folder in project directory.
  path = "./BEACO2N_measurements",
  pattern = "\\.csv$",
  full.names = TRUE
)
measurement = setNames( # Name each field of "measurement" for a file (location) in BEACO2N directory.
  lapply(measurement_files, read.csv),
  tools::file_path_sans_ext(basename(measurement_files))
)

# Analogous operations for QuantAQ reference data.
reference_files = list.files(
  path = "./reference_measurements",
  pattern = "\\.csv$",
  full.names = TRUE
)
reference = setNames(
  lapply(reference_files, read.csv),
  tools::file_path_sans_ext(basename(reference_files))
)

# Clean BEACO2N data by location. 
measurement = lapply(measurement, function(df) { # Apply this across all measurement locations within the "measurement" data frame. 
  colnames(df)[colnames(df)=="datetime"] = "timestamp" # rename "datetime" to "timestamp".
  colnames(df)[colnames(df)=="co2_raw"] = "co2" # rename "co2_raw" to "co2"
  df$timestamp = as.POSIXct(df$timestamp, tz = "UTC") # store "timestamp" as a POSIXct. 
  df$timestamp = as.POSIXct(round(df$timestamp, "hours"), tz = "UTC") # round "timestamp" to nearest minute.
  df = df %>% select(-(all_of(c("local_timestamp", "epoch", "node_file_id")))) # remove local_timestamp, epoch, and node_file_id fields.
  suffixed_cols = grepl("_wrk_aux$", names(df))  # Identify cols with suffix "_wrk_aux"
  df[suffixed_cols] = df[suffixed_cols] * 1000 # Convert measurements from V to mV.
  names(df)[suffixed_cols] <- sub("_wrk_aux$", "", names(df)[suffixed_cols]) # Remove "_wrk_aux" suffix from col name.
  # df = df %>% filter(!is.na(co))
  df %>% drop_na()
  return(df)
})

reference <- lapply(reference, function(df) {
  df = df %>% select(-(all_of(c("period_start", "period_end", "period_end_utc", "sn")))) 
  colnames(df)[colnames(df)=="period_start_utc"] = "timestamp"
  df$timestamp = date_time_parse_RFC_3339(df$timestamp, offset = "%Ez")
  df = df %>% filter(n_datapoints != 0)
  # df = df %>% filter(!is.na(co))
  df %>% drop_na()
  return(df)
})

