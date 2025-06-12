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

API_KEY = "6XB3KQVJ62BKQR3DVKX1MMGR"
QUANTAQ_DPW_SN = "MOD-00811"

# Gave up trying to import hourly data and downloaded manually instead. 
QuantAQ_DPW = read.csv("/Users/mikewoodilla/Desktop/summer25/bpp-scripts/MOD-00811_May.csv")

# Get CO data from BEACO2N sensor at DPW for the month of May with 1-hour resolution
# Range: 2025-04-30 17:00:00-07:00 to 2025-05-31 16:00:00-07:00 == 2025-05-01 00:00:00-00:00 to 2025-05-31 23:00:00-00:00
# This URL came from manually specifying parameters for BEACO2N node and dataset at below URL:
# http://128.32.208.8/map/?ids=276&variables=co_wrk_aux&interval=1&chart_type=measurement&start=2025-04-30%2017:00:00&end=2025-05-31%2016:00:00&show_locations=true
BEACO2N_DPW_link = "http://128.32.208.8/node/276/measurements_all/csv?name=Department%20of%20Public%20Works&interval=1&variables=co_wrk_aux&start=2025-04-30%2017:00:00&end=2025-05-31%2016:00:00&chart_type=measurement"
BEACO2N_DPW = read.csv(BEACO2N_DPW_link)

# Clean data by removing impertinent fields. Also aligns names of dataframe cols.
QuantAQ_DPW = subset(QuantAQ_DPW, select = c("timestamp", "co_diff"))
colnames(QuantAQ_DPW)[colnames(QuantAQ_DPW)=="co_diff"] = "co"
BEACO2N_DPW = subset(BEACO2N_DPW, select = c("datetime", "co_wrk_aux"))
colnames(BEACO2N_DPW)[colnames(BEACO2N_DPW)=="co_wrk_aux"] = "co"
colnames(BEACO2N_DPW)[colnames(BEACO2N_DPW)=="datetime"] = "timestamp"

BEACO2N_DPW$timestamp = as.POSIXct(BEACO2N_DPW$timestamp, tz="UTC") # the default format in this case should align with given format
QuantAQ_DPW$timestamp = as.POSIXct(QuantAQ_DPW$timestamp, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

merged_DPW = merge(QuantAQ_DPW, BEACO2N_DPW, by = "timestamp", suffixes = c("_Q", "_B"))
print(t.test(merged_DPW$co_Q, merged_DPW$co_B, paired = TRUE))


