library(tidyverse)
library(openair)
library(checkmate) # type checking
# library(stringr) # Used for df_parser function

unlink("./plots/*.png", expand=TRUE) # Hit Ctrl-Enter on this line to clear temporary files.

combined_df = read.csv("./clean_data/all_co_temp_rh_2022.csv")
combined_df$date = as.POSIXct(combined_df$date, tz="UTC")
combined_df$date = with_tz(combined_df$date, tzone="America/New_York") # Allows OpenAir to account for EST/EDT
valid_cols = colnames(combined_df)

tidy_co_stats = function(df) {
    # Assert df is a non-empty DataFrame with double and POSIXct columns with names from combined_df
    assert_data_frame(df, types=c("double", "POSIXct"), all.missing=FALSE, min.cols=2, col.names=checkSubset(colnames(df), valid_cols))
    assert_names(df, must.include="date", subset.of=valid_cols)
    assert_posixct(df$date, any.missing=FALSE)
    assert_posixct(df[colnames(df)!="date"], negate=TRUE)
    
    # colnames = colnames(df)
    # ggplot(
    #     data=df,
    #     mapping=aes(

    #     )
    # )
}

tidy_co_stats(data.frame(date = c(as.POSIXct(Sys.Date()), as.POSIXct(Sys.Date())), co_aqs_myron=c(0, as.POSIXct(Sys.Date()))))