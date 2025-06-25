rm(list=ls()) # clear the R environment between script runs
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
  df = df %>% drop_na()
  return(df)
})

reference = lapply(reference, function(df) {
  df = df %>% select(-(all_of(c("period_start", "period_end", "period_end_utc", "sn")))) 
  colnames(df)[colnames(df)=="period_start_utc"] = "timestamp"
  colnames(df)[colnames(df)=="pm25"] = "pm2_5"
  df$timestamp = date_time_parse_RFC_3339(df$timestamp, offset = "%Ez")
  df = df %>% drop_na()
  return(df)
})

merged = mapply(
  function(meas, ref) {
    merge(meas, ref, by = "timestamp", suffixes = c("_meas", "_ref"), all = TRUE)
  },
  measurement, 
  reference, 
  SIMPLIFY = FALSE
  )

merged = lapply(merged, function(df) {
  df = df %>% drop_na()
  return(df)
})

# Want to calculate set of timestamps for which RSD(co) < 10% across all reference locations. 

timestamps_10percent_rsd = 
  bind_rows(
  lapply(names(reference), function(site) {
    df = reference[[site]] %>% select(timestamp, co_ref = co) %>% mutate(location = site)
    return(df)
    })
  ) %>% 
  pivot_wider(names_from = location, values_from = co_ref) %>%
  drop_na() %>%
  rowwise() %>% 
  mutate(
    vals = list(c_across(-timestamp)),
    mean = mean(vals),
    sd = sd(vals),
    rsd = sd / mean
  ) %>% 
  ungroup() %>%
  filter (rsd < .10) %>%
  select(timestamp)
timestamps_10percent_rsd = as.vector(timestamps_10percent_rsd$timestamp)

merged = lapply(merged, function(df) {
  filter(df, timestamp %in% timestamps_10percent_rsd)
})










