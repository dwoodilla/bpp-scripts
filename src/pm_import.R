library(tidyverse)

source("./src/import_cleaned.R")

all_params = import_cleaned()

pm_df = all_params %>% filter(parameter=)