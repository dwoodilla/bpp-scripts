library(tidyverse)
library(openair)
library(checkmate)
library(ggbreak)
# library(glue)
# library(patchwork)
library(zoo)
library(pracma)

unlink(c("./plots/*.png", "./tidy_plots/*.png"), expand=TRUE) # Hit Ctrl-Enter on this line to clear temporary files.

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

assert_tidy = function(df) {
    # Assert df is a non-empty DataFrame with double and POSIXct columns with names from combined_df
    # NOTE: These checks do not perfectly filter out invalid dataframes, but do assert expectations that the rest of the script relies on.
    assert_data_frame(df, ncols=5)
    assert_set_equal(colnames(df), valid_cols)
    assert_posixct(df$date, any.missing=FALSE)
}

tidy_co_stats = function(df, filename, ...) {
    assert_tidy(df)
    df = df %>% filter(parameter=="co")
    y_partitions = seq(0,1.25,by=0.25)
    plt = 
        ggplot(
            data=df,
            mapping=aes(
                x=interaction(sensor, location, sep=" @ ", lex.order=TRUE),
                y=value,
                fill=sensor
            )
        ) + geom_boxplot() + 
        theme_bw() + theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1)) +
        scale_y_continuous(
            breaks = y_partitions,
            labels = y_partitions
        ) +
        labs(
            ...,
            x = "Sensor type @ Location",
            y = "CO (ppm)"
        )
    ggsave(plot=plt, filename=filename)
}

tidy_co_histogram = function(df, filename, filltype = NULL, ...) {
    assert_string(filltype, null.ok=TRUE)
    assert_tidy(df)
    x_partitions = seq(0,1.5,by=0.05)
    y_max=0.175
    y_partitions = seq(0,y_max,by=0.025)
    plt = 
        ggplot(
            data=df,
            mapping={
                if (is.null(filltype)) {
                    aes(x=value, y=after_stat(count/sum(count)), fill=sensor)
                } else {
                    aes(x=value, y=after_stat(count/sum(count)), fill=.data[[filltype]])
                }
            }
        ) + {
            if (is.null(filltype)) {
                facet_wrap(~sensor)
            } else {
                facet_wrap(vars(!!sym(filltype)))
            }
        } +
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
        coord_cartesian(ylim=c(0,y_max)) +
        # geom_density(alpha=0.5) +
        theme_bw() + theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1)) +
        labs(
            ...,
            x="CO (ppm)",
            y="Relative Frequency"
        )

    ggsave(plot=plt, filename=filename)
}
tidy_co_distribution = function(df, filepath, filltype = NULL, ...) {
    assert_string(filltype, null.ok=TRUE)
    assert_tidy(df)
    x_partitions = seq(0,1.5,by=0.05)
    y_partitions = seq(0,0.25,by=0.025)
    plt = 
        ggplot(
            data=df,
            mapping=aes(x=value, y=after_stat(count/sum(count)))
        ) + {
            if (is.null(filltype)) {aes(fill=sensor)} else {aes(fill=.data[[filltype]])}
        } +
        geom_density(
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
        # geom_density(alpha=0.5) +
        theme_bw() + theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1)) +
        labs(
            ...,
            x="CO (ppm)",
            y="Relative Frequency"
        )

    ggsave(plot=plt, filename=filename)
}


# BEACO2N DRIFT:
# 1) Plot BEACO2N vs AQS residual over time (Myron): residual, rolling avg residual, stat tests for residual.
# 2) Plot BEACO2N DPW vs AQS Cranston: residual, rolling avg residual, stat tests
myron_df_wide = tidy_combined_df %>% 
    filter(location=="myron", parameter=="co", sensor %in% c("aqs","beaco2n")) %>%
    pivot_wider(
        id_cols = "date",
        names_from = "sensor",
        values_from = "value",
        names_prefix = "co_"
    ) %>%
    filter(if_all(c(co_aqs, co_beaco2n), ~ !is.na(.)))

    
myron_df_long = myron_df_wide %>% 
pivot_longer(
        cols = -date,
        names_to = "sensor",
        names_prefix = "co_",
        values_to = "value"
)

myron_zoo_beaco2n = zoo(myron_df_wide[["co_beaco2n"]], myron_df_wide[["date"]])
myron_zoo_aqs = zoo(myron_df_wide[["co_aqs"]], myron_df_wide[["date"]])
myron_zoo = merge(myron_zoo_beaco2n, myron_zoo_aqs)
myron_wide = fortify(myron_zoo)
# Houly data * 24 hr/day * 31 days/month = 744 hours per
myron_rolling_wide = fortify(rollmean(myron_zoo, k=372, align="center")) 
colnames(myron_wide) = c("date","beaco2n","aqs")
colnames(myron_rolling_wide) = c("date","beaco2n","aqs")

myron_wide[["beaco2n"]] = savgol(myron_wide[["beaco2n"]], fl=101)
myron_wide[["aqs"]] = savgol(myron_wide[["aqs"]], fl=101)

myron_rolling_wide[["bcn-aqs"]] = myron_rolling_wide[["beaco2n"]] - myron_rolling_wide[["aqs"]]

myron_savgol_long = myron_wide %>% pivot_longer(
    cols=-date,
    names_to="sensor",
    values_to="value"
)
myron_rolling_long = myron_rolling_wide %>% pivot_longer(
    cols = -date,
    names_to = "sensor",
    values_to = "value"
)

myron_rolling_long_summers = myron_rolling_long %>% 
    filter(
        (date >= as.POSIXct("2022-06-01 00:00:00") & date <= as.POSIXct("2022-09-01 00:00:00")) |
        (date >= as.POSIXct("2023-06-01 00:00:00") & date <= as.POSIXct("2023-09-01 00:00:00")) |
        (date >= as.POSIXct("2024-06-01 00:00:00") & date <= as.POSIXct("2024-09-01 00:00:00"))
    )
myron_rolling_long_winters = myron_rolling_long %>% 
    filter(
        (date >= as.POSIXct("2022-12-01 00:00:00") & date <= as.POSIXct("2023-03-01 00:00:00")) |
        (date >= as.POSIXct("2023-12-01 00:00:00") & date <= as.POSIXct("2024-03-01 00:00:00")) |
        (date >= as.POSIXct("2024-12-01 00:00:00") & date <= as.POSIXct("2025-03-01 00:00:00"))
    )
print(head(myron_rolling_long_summers))
print(head(myron_rolling_long_winters))


summers_plt = ggplot(data=myron_rolling_long_summers, mapping=aes(x=date, y=value, color=sensor)) + geom_line() + geom_hline(yintercept=0, color="red") 
ggsave(plot=summers_plt, filename="./tidy_plots/myron_rolling_summers.png")

winters_plt = ggplot(data=myron_rolling_long_winters, mapping=aes(x=date, y=value, color=sensor)) + geom_line() + geom_hline(yintercept=0, color="red")
ggsave(plot=winters_plt, filename="./tidy_plots/myron_rolling_winters.png")

ggplot(data=myron_savgol_long, mapping=aes(x=date, y=value, color=sensor)) + geom_line() + 
    geom_hline(yintercept=0, color="red") 
ggsave(filename="./tidy_plots/myron_sevgol_timeseries.png", height=7.5, width=7.5, units="in")
ggplot(data=myron_rolling_long, mapping=aes(x=date, y=value, color=sensor)) + geom_line() + 
    geom_hline(yintercept=0, color="red") +
    scale_y_break(c(0.035,0.15))
ggsave(filename="./tidy_plots/myron_rolling_timeseries.png", height=7.5, width=7.5, units="in")

png(
    filename="./plots/myron_timeseries.png",
    width = 300*10, height = 300*10, res=300
)
timePlot(
    mydata=myron_df_wide, 
    pollutant = c("co_aqs", "co_beaco2n"),
    cols = c("salmon","green4"),
    stack=TRUE,
    smooth=TRUE,
    ci=TRUE
)
dev.off()

# tidy_co_stats(
#     filter(tidy_combined_df, parameter=="co", location %in% c("dpw", "pema", "pha", "cranston")), 
#     title="AQS Cranston vs QuantAQ: Summary Statistics", 
#     filename="./tidy_plots/co_stat_summary.png"
# )
# for(site in c("dpw", "pema", "pha")) {
#     tidy_co_histogram(
#         filter(tidy_combined_df, parameter=="co", location %in% c("cranston", site)), 
#         title=glue("Relative Frequency Histogram: {toupper(site)}"),
#         filename=glue("./tidy_plots/co_hist_{site}.png")
#     )
# }
# tidy_co_histogram(
#     filter(tidy_combined_df, parameter=="co", sensor=="quantaq"),
#     filename="./tidy_plots/co_hist_quantaq.png", 
#     filltype="location",
#     title="QuantAQ Relative Frequency Histogram"
# )
# tidy_co_histogram(
#     filter(tidy_combined_df, parameter=="co", sensor=="aqs", location=="cranston"),
#     filename="./tidy_plots/co_hist_aqs.png",
#     title="AQS RF Hist"
# )
