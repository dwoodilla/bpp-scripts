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

# Create dataframes from CSV files in script directory
measurement_files = list.files(
  path = "./BEACO2N_measurements",
  pattern = "\\.csv$",
  full.names = TRUE
)
measurement = setNames(
  lapply(measurement_files, read.csv),
  tools::file_path_sans_ext(basename(measurement_files))
)

reference_files = list.files(
  path = "./reference_measurements",
  pattern = "\\.csv$",
  full.names = TRUE
)
reference = setNames(
  lapply(reference_files, read.csv),
  tools::file_path_sans_ext(basename(reference_files))
)

measurement = lapply(measurement, function(df) { # Apply this across all measurement locations.
  colnames(df)[colnames(df)=="datetime"] = "timestamp" # rename "datetime" to "timestamp".
  colnames(df)[colnames(df)=="co2_raw"] = "co2" # rename "co2_raw" to "co2"
  df$timestamp = as.POSIXct(df$timestamp, tz = "UTC") # store "timestamp" as a POSIXct. 
  df$timestamp = as.POSIXct(round(df$timestamp, "mins"), tz = "UTC") # round "timestamp" to nearest minute.
  df = df %>% select(-(all_of(c("local_timestamp", "epoch", "node_file_id")))) # remove local_timestamp, epoch, and node_file_id fields.
  suffixed_cols = grepl("_wrk_aux$", names(df))  # Identify cols with suffix "_wrk_aux"
  df[suffixed_cols] = df[suffixed_cols] * 1000 # Convert measurements from V to mV.
  names(df)[suffixed_cols] <- sub("_wrk_aux$", "", names(df)[suffixed_cols]) # Remove "_wrk_aux" suffix from col name.
  
  return(df)
})

reference <- lapply(reference, function(df) {
  
})


# Clean QuantAQ data
QuantAQ_DPW %>% select(-(starts_with("bin")))
QuantAQ_DPW %>% select(-(all_of("id", "timestamp_local", "sn", "flag", "lat", "lon", "device_state")))
colnames(QuantAQ_DPW)[colnames(QuantAQ_DPW)=="co_diff"] = "co"

#Clean BEACO2N data
BEACO2N_DPW = subset(BEACO2N_DPW, select = c("datetime", "co_wrk_aux"))
colnames(BEACO2N_DPW)[colnames(BEACO2N_DPW)=="co_wrk_aux"] = "co"
colnames(BEACO2N_DPW)[colnames(BEACO2N_DPW)=="datetime"] = "timestamp"
BEACO2N_DPW$co = BEACO2N_DPW$co * 1000 # convert from V to mV to be consistent with QuantAQ data.

# Round timestamps to nearest minute and force POSIXct formatting. 
BEACO2N_DPW$timestamp = as.POSIXct(BEACO2N_DPW$timestamp, tz = "UTC")
BEACO2N_DPW$timestamp = as.POSIXct(round(BEACO2N_DPW$timestamp, "mins"), tz = "UTC")
QuantAQ_DPW$timestamp = as.POSIXct(QuantAQ_DPW$timestamp, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
QuantAQ_DPW$timestamp = as.POSIXct(round(QuantAQ_DPW$timestamp, "mins"), tz = "UTC")

merged_DPW = merge(QuantAQ_DPW, BEACO2N_DPW, by = "timestamp", suffixes = c("_Q", "_B"))
print(t.test(merged_DPW$co_Q, merged_DPW$co_B, paired = TRUE))


