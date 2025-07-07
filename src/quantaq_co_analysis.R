library(tidyverse)
library(openair)
library(checkmate) 
library(glue)
library(patchwork)

unlink("./plots/*.png", expand=TRUE) # Hit Ctrl-Enter on this line to clear temporary files.

# combined_df = read.csv("./clean_data/all_co_temp_rh_2022.csv")
# combined_df$date = as.POSIXct(combined_df$date, tz="UTC")
# tidy_combined_df = combined_df %>% pivot_longer( # Convert combined_df to tidy format
#     cols = -date,
#     names_to = c("parameter","sensor","location"),
#     values_to = "value",
#     names_pattern = "([^_]+)_([^_]+)_(.+)"
# )

tidy_combined_df = import_cleaned()

tidy_combined_df$date = with_tz(tidy_combined_df$date, tzone="America/New_York") # Allows OpenAir to account for EST/EDT
valid_cols = colnames(tidy_combined_df)

assert_tidy = function(df) {
    # Assert df is a non-empty DataFrame with double and POSIXct columns with names from combined_df
    # NOTE: These checks do not perfectly filter out invalid dataframes, but do assert expectations that the rest of the script relies on.
    assert_data_frame(df, ncols=5)
    assert_set_equal(colnames(df), valid_cols)
    assert_posixct(df$date, any.missing=FALSE)
}

co_boxplot = function(df, ...) {
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
        ) + 
        geom_boxplot() + 
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
    return(plt)
}

co_histogram = function(df, ...) {
    assert_tidy(df)
    x_partitions = seq(0,1.5,by=0.1)
    yrange = c(0, 0.4)
    xrange = c(0, 1.35)
    y_partitions = seq(0,yrange[2],by=0.025)
    plt = 
        ggplot(
            data=df,
            mapping=aes(x=value, y=after_stat(count/sum(count)))
        ) +
        geom_histogram(
            color="black",
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
        coord_cartesian(ylim=yrange, xlim=xrange) +
        theme_bw() + theme(axis.text.x = element_text(angle=60, hjust=1, vjust=1)) +
        labs(
            ...,
            x="CO (ppm)",
            y="Relative Frequency"
        )

    return(plt)
}
co_distribution = function(df, ...) {
    assert_tidy(df)
    x_partitions = seq(0,1.5,by=0.05)
    y_partitions = seq(0,0.25,by=0.025)
    plt = 
        ggplot(
            data=df,
            mapping=aes(x=value, y=after_stat(count/sum(count)))
        ) +
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
        theme_bw() + theme(axis.text.x = element_text(angle=90, hjust=1, vjust=1)) +
        labs(
            ...,
            x="CO (ppm)",
            y="Relative Frequency"
        )

    return(plt)
}

ggsave(
    plot=co_boxplot(
        filter(tidy_combined_df, parameter=="co", location %in% c("dpw", "pema", "pha", "cranston")), 
        title="AQS Cranston vs QuantAQ: Summary Statistics",
    ),
    filename="./plots/quantaq_co_analysis/co_boxplot.png"
)
# co_boxplot(
#     filter(tidy_combined_df, parameter=="co", location %in% c("dpw", "pema", "pha", "cranston")), 
#     title="AQS Cranston vs QuantAQ: Summary Statistics", 
#     filename="./plots/co_stat_summary.png"
# )

co_stat_df = tidy_combined_df %>% 
    filter(parameter=="co", location %in% c("cranston","dpw","pha","pema")) %>%
    group_by(location, sensor) %>%
    summarise(mean = mean(value, na.rm=TRUE), sd = sd(value, na.rm=TRUE))

aqs_hist = co_histogram(
    filter(tidy_combined_df, parameter=="co", location=="cranston"), 
    title="AQS Cranston",
    subtitle=paste0(
        "mean=",
        round(digits=3, co_stat_df %>% filter(location=="cranston", sensor=="aqs") %>% select(mean) %>% pull),
        " sd=",
        round(digits=3, co_stat_df %>% filter(location=="cranston", sensor=="aqs") %>% select(sd) %>% pull)
    )
)

qaq_dpw_hist = co_histogram(
    filter(tidy_combined_df, parameter=="co", location=="dpw", sensor=="quantaq"), 
    title="QuantAQ DPW",
    subtitle=paste0(
        "mean=",
        round(digits=3, co_stat_df %>% filter(location=="dpw", sensor=="quantaq") %>% select(mean) %>% pull),
        " sd=",
        round(digits=3, co_stat_df %>% filter(location=="dpw", sensor=="quantaq") %>% select(sd) %>% pull)
    )
)
qaq_pha_hist = co_histogram(
    filter(tidy_combined_df, parameter=="co", location=="pha", sensor=="quantaq"), 
    title="QuantAQ PHA",
    subtitle=paste0(
        "mean=",
        round(digits=3, co_stat_df %>% filter(location=="pha", sensor=="quantaq") %>% select(mean) %>% pull),
        " sd=",
        round(digits=3, co_stat_df %>% filter(location=="pha", sensor=="quantaq") %>% select(sd) %>% pull) 
    )
)
qaq_pema_hist = co_histogram(
    filter(tidy_combined_df, parameter=="co", location=="pema", sensor=="quantaq"), 
    title="QuantAQ PEMA",
    subtitle=paste0(
        "mean=",
        round(digits=3, co_stat_df %>% filter(location=="pema", sensor=="quantaq") %>% select(mean) %>% pull),
        " sd=",
        round(digits=3, co_stat_df %>% filter(location=="pema", sensor=="quantaq") %>% select(sd) %>% pull)
    )
)
bcn_dpw_hist = co_histogram(
    filter(tidy_combined_df, parameter=="co", location=="dpw", sensor=="beaco2n"), 
    title="BEACO2N DPW",
    subtitle=paste0(
        "mean=",
        round(digits=3, co_stat_df %>% filter(location=="dpw", sensor=="beaco2n") %>% select(mean) %>% pull),
        " sd=",
        round(digits=3, co_stat_df %>% filter(location=="dpw", sensor=="beaco2n") %>% select(sd) %>% pull)
    )
)
bcn_pha_hist = co_histogram(
    filter(tidy_combined_df, parameter=="co", location=="pha", sensor=="beaco2n"), 
    title="BEACO2N PHA",
    subtitle=paste0(
        "mean=",
        round(digits=3, co_stat_df %>% filter(location=="pha", sensor=="beaco2n") %>% select(mean) %>% pull),
        " sd=",
        round(digits=3, co_stat_df %>% filter(location=="pha", sensor=="beaco2n") %>% select(sd) %>% pull) 
    )
)
bcn_pema_hist = co_histogram(
    filter(tidy_combined_df, parameter=="co", location=="pema", sensor=="beaco2n"), 
    title="BEACO2N PEMA",
    subtitle=paste0(
        "mean=",
        round(digits=3, co_stat_df %>% filter(location=="pema", sensor=="beaco2n") %>% select(mean) %>% pull),
        " sd=",
        round(digits=3, co_stat_df %>% filter(location=="pema", sensor=="beaco2n") %>% select(sd) %>% pull)
    )
)


patch = (qaq_dpw_hist + qaq_pha_hist + qaq_pema_hist) / (bcn_dpw_hist + bcn_pha_hist + bcn_pema_hist)
ggsave(
    plot=patch, 
    filename="./plots/quantaq_co_analysis/patched_histograms.png"
)
ggsave(plot=aqs_hist, file="./plots/quantaq_co_analysis/aqs_histogram.png")
