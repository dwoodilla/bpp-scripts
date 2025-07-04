library(tidyverse)
library(openair)
library(checkmate) # type checking
# library(glue)
# library(stringr) # Used for df_parser function

unlink("./plots/*.png", expand=TRUE) # Hit Ctrl-Enter on this line to clear temporary files.

combined_df = read.csv("./clean_data/all_co_temp_rh_2022.csv")
combined_df$date = as.POSIXct(combined_df$date, tz="UTC")
tidy_combined_df = combined_df %>% pivot_longer( # Convert combined_df to tidy format
    cols = -date,
    names_to = c("parameter","sensor","location"),
    values_to = "value",
    names_pattern = "([^_]+)_([^_]+)_(.+)"
)
# rownames(combined_df) = combined_df$date
tidy_combined_df$date = with_tz(tidy_combined_df$date, tzone="America/New_York") # Allows OpenAir to account for EST/EDT
valid_cols = colnames(tidy_combined_df)

assert_df = function(df) {
    # Assert df is a non-empty DataFrame with double and POSIXct columns with names from combined_df
    # NOTE: These checks do not perfectly filter out invalid dataframes, but do assert expectations that the rest of the script relies on.
    assert_data_frame(df, ncols=5)
    assert_set_equal(colnames(df), valid_cols)
    assert_posixct(df$date, any.missing=FALSE)
}

tidy_co_stats = function(df, title) {
    assert_df(df)
    df = df %>% filter(parameter=="co")
    plt = 
        ggplot(
            data=df,
            mapping=aes(
                x=interaction(sensor, location, sep=" @ "),
                y=value,
                fill=sensor
            )
        ) + geom_boxplot() + 
        labs(
            title=title,
            x = "Sensor type @ Location",
            y = "CO (ppm)"
        )
    ggsave(plot=plt, glue::glue("./plots/co_stats_{title}.png"))
}

tidy_co_histogram = function(df, title) {
    assert_df(df)
    x_partitions = seq(0,1.5,by=0.05)
    y_partitions = seq(0,0.25,by=0.025)
    plt = 
        ggplot(
            data=df,
            mapping=aes(x=value, y=after_stat(count/sum(count)), fill=sensor)
        ) + 
        geom_histogram(
            color="white",
            binwidth=0.05,
            boundary=0,
            position="identity",
            alpha=0.6
        ) +
        scale_x_continuous(
            breaks = x_partitions,
            labels = x_partitions
        ) + 
        scale_y_continuous(
            breaks = y_partitions,
            labels = y_partitions
        ) +
        theme_bw() + theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1)) +
        labs(
            title=title,
            x="CO (ppm)",
            y="Relative Frequency"
        )

    ggsave(plot=plt, filename=glue::glue("./plots/co_hist_{title}.png"))
}

tidy_co_histogram(filter(tidy_combined_df, parameter=="co", location=="dpw"), title="dpw")
tidy_co_histogram(filter(tidy_combined_df, parameter=="co", location=="pha"), title="pha")
tidy_co_histogram(filter(tidy_combined_df, parameter=="co", location %in% c("dpw", "cranston")), title="vaqs")

