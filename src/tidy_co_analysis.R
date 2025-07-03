library(tidyverse)
library(openair)
library(checkmate) # type checking
# library(stringr) # Used for df_parser function

unlink("./plots/*.png", expand=TRUE) # Hit Ctrl-Enter on this line to clear temporary files.

combined_df = read.csv("./clean_data/all_co_temp_rh_2022.csv")
combined_df$date = as.POSIXct(combined_df$date, tz="UTC")
combined_df$date = with_tz(combined_df$date, tzone="America/New_York") # Allows OpenAir to account for EST/EDT
valid_cols = colnames(combined_df)

assert_df = function(df, ...) {
    # Assert df is a non-empty DataFrame with double and POSIXct columns with names from combined_df
    # NOTE: These checks do not perfectly filter out invalid dataframes, but do assert expectations that the rest of the script relies on.
    assert_data_frame(df, types=c("double", "POSIXct"), all.missing=FALSE, min.cols=2)
    assert_subset(colnames(df), valid_cols, empty.ok=FALSE)
    assert_posixct(df$date, any.missing=FALSE)
    for (col in ...) {
        assert_subset(col, colnames(df))
    }
}

tidy_co_stats = function(df, ...) {
    args = list(...)
    assert_df(df, args$x, args$y)
    ggplot(
        data=df,
        mapping=aes(
            x=args$x,
            y=args,
            color=if("color" in names(args)) args$color else NULL
        )
    ) +
    geom_boxplot(title=)
}
