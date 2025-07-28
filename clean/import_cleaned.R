library(tidyverse)
library(worldmet)

beaco2n_site_list = c(
	"myron","zuccolo","wecc","rocklib","silverlake","unitedway","cfs","pha","reservoir","ccri",
	"mtpleasant","carnevale","martialarts","southprovlib","ecubed","ricollege","blackstone","rochambeaulib","provcollege","prek",
	"smithhilllib","pema","rockspot","medschool","dpw"
)
quantaq_site_list = c("dpw","pema","pha")
aqs_site_list = c("myron","cranston")
beaco2n_berkeley_site_list = c(
	"rfs","dejean","albany","korematsu","madera","nystrom","peres","washington"
)

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