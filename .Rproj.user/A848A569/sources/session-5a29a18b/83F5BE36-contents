# **See ReadMe for QuantAQ's R API here: https://github.com/quant-aq/r-quantaq/blob/main/README.md**
# See docs for QuantAQ's HTTP API here: https://docs.quant-aq.com/software-apis-and-libraries/quantaq-cloud-api 

rm(list=ls()) # clear the R environment between script runs
library(jsonlite)
# install.packages("devtools") # uncomment to install devtools package, needed to install R API
# devtools::install_github(repo="quant-aq/r-quantaq") # Uncomment to install QuantAQ's R API from GitHub.
# install.packages("httr2") # uncomment to install httr2, needed to use QuantAQ's HTTP API.

library(QuantAQAPIClient)
library(httr2)

API_KEY = "6XB3KQVJ62BKQR3DVKX1MMGR"

setup_client(api_key = API_KEY)

# Get all data from QuantAQ sensor at DPW for the month of May with 1-hour resolution
# Apparently the R API has no method to get resampled data (needed for 1h resolution), so this must be done manually using CLI + httr2
# Make an HTTP request using httr2, following HTTP API specifications.
QuantAQ_DPW = request("https://api.quant-aq.com/v1/data/resampled") %>% # This is the URL which provides resampled data
  req_auth_basic(API_KEY, "") %>% #Authenticate the request w/ API_KEY (no password so second arg is blank)
  req_url_query(
    sn = "MOD-00811", # Serial number of QuantAQ Node at DPW site
    start_date = "2025-05-01", # Start and end dates; no times as per API docs
    end_date = "2025-05-31",
    period = "1h" #Specify 1-hour resolution
  ) %>%
  req_perform() # Actually make the HTTP request
QuantAQ_DPW = resp_body_json(QuantAQ_DPW) # Convert the received JSON into a list of lists of labeled data (which needs to be converted to a dataframe)
QuantAQ_DPW = do.call(rbind, lapply(QuantAQ_DPW$data, as.data.frame)) # Store QuantAQ_DPW as a data frame

# Get CO data from BEACO2N sensor at DPW for the month of May with 1-hour resolution
# This URL came from manually specifying parameters for BEACO2N node and dataset at below URL:
# http://128.32.208.8/map/?ids=276&variables=co_wrk_aux&interval=60&chart_type=measurement&start=2025-05-01%2000:00:00&end=2025-06-01%2000:00:00&show_locations=true
BEACO2N_DPW_link = "http://128.32.208.8/node/276/measurements_all/csv?name=Department%20of%20Public%20Works&interval=60&variables=co_wrk_aux&start=2025-05-01%2000:00:00&end=2025-06-01%2000:00:00&chart_type=measurement"
BEACO2N_DPW = read.csv(BEACO2N_DPW_link)

# Clean data by removing impertinent fields. Also aligns names of dataframe cols.
QuantAQ_DPW = subset(QuantAQ_DPW, select = c("period_start_utc", "co"))
colnames(QuantAQ_DPW)[colnames(QuantAQ_DPW)=="period_start_utc"] = "datetime"
BEACO2N_DPW = subset(BEACO2N_DPW, select = c("datetime", "co_wrk_aux"))
colnames(BEACO2N_DPW)[colnames(BEACO2N_DPW)=="co_wrk_aux"] = "co"

BEACO2N_DPW$datetime = as.POSIXct(BEACO2N_DPW$datetime, tz="GMT") # the default format in this case should align with given format
QuantAQ_DPW$datetime = as.POSIXct(QuantAQ_DPW$datetime, tz="GMT", format="%FT%T", tz="GMT")



