library(tidyverse)
library(openair)
library(checkmate)
# library(zoo)
library(pracma)

AVG_WINDOW = 24*7
SAVGOL_FILTER_LEN = 24*7-1

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
# tidy_combined_df$date = with_tz(tidy_combined_df$date, tzone="America/New_York") # Allows OpenAir to account for EST/EDT
valid_cols = colnames(tidy_combined_df)

assert_tidy = function(df) {
    # Assert df is a non-empty DataFrame with double and POSIXct columns with names from combined_df
    # NOTE: These checks do not perfectly filter out invalid dataframes, but do assert expectations that the rest of the script relies on.
    assert_data_frame(df, ncols=5)
    assert_set_equal(colnames(df), valid_cols)
    assert_posixct(df$date, any.missing=FALSE)
}

# Function to plot summary boxplot for tidy df
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

# Function to plot histogram for tidy dataframe, faceted on filltype
# NOTE: Count is total observation among all facets, which misrepresents the data.
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

# Function to plot emperical probability distribution for sensor observations, faceted on filltype
# NOTE: Same but as with tidy_co_histogram
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
        values_from = "value"
    ) %>%
    filter(if_all(c(aqs, beaco2n), ~ !is.na(.))) # Choose rows s.t. no measurement entry is NA

myron_rolling_wide = myron_df_wide %>%
    mutate(aqs = rollmean(aqs, k=AVG_WINDOW, align="center", fill=NA), 
           beaco2n = rollmean(beaco2n, k=AVG_WINDOW, align="center", fill=NA)) %>%
    mutate(res = beaco2n - aqs)

myron_savgol_wide = myron_df_wide %>%
    mutate(aqs = savgol(aqs, fl=SAVGOL_FILTER_LEN), beaco2n = savgol(beaco2n, fl=SAVGOL_FILTER_LEN)) %>%
    mutate(res = beaco2n - aqs)

myron_df_long = myron_df_wide %>% 
    mutate(res = beaco2n - aqs) %>%
    pivot_longer(
        cols = -date,
        names_to = "sensor",
        values_to = "value"
    )
myron_rolling_long = myron_rolling_wide %>%
    pivot_longer(
        cols=-date,
        names_to = "sensor",
        values_to = "value"
    )
myron_savgol_long = myron_savgol_wide %>%
    pivot_longer(
        cols=-date,
        names_to = "sensor",
        values_to = "value"
    )

season = function(vec) {
    m = month(as.Date(vec))
    return(
        case_when(
            m %in% c(12,1,2) ~ "Winter",
            m %in% c(3,4,5) ~ "Spring",
            m %in% c(6,7,8) ~ "Summer",
            m %in% c(9,10,11) ~ "Fall"
        )
    )
}

count_from_season_start = function(vec) {
    d = as.POSIXct(vec)
    sn = season(vec)
    sn_start = 
        case_when(
            sn=="Winter" ~ make_datetime(year=if_else(month(d)==12, year(d), year(d)-1), month=12, day=1),
            sn=="Spring" ~ make_datetime(year=year(d), month=3, day=1),
            sn=="Summer" ~ make_datetime(year=year(d), month=6, day=1),
            sn=="Fall" ~ make_datetime(year=year(d), month=9, day=1)
        )
    return(int_length(interval(start=sn_start, end=d, tz="UTC")))
}

plot_graphs = function(dataset, dataset_name) {
    year_by_season_ts = 
        ggplot(
            data=dataset, 
            mapping=aes(
                x=from_sn_start, 
                y=value, 
                color=sensor
            )
        ) + 
        facet_grid(rows=vars(year), cols=vars(season)) +
        geom_line() + theme_bw() +
        theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1)) +
        geom_hline(yintercept = 0, color="red") +
        labs(
            title="Myron: AQS vs BEACO2N Time Series",
            subtitle="2022-2024 Faceted by Year and Season",
            x = "Seconds from first day of season",
            y = "CO (ppm)",
            caption=dataset_name
        )
    ggsave(plot=year_by_season_ts, filename=paste0("./tidy_plots/", dataset_name, "/year_by_sn_timeseries.png"))

    by_season_ts = 
        ggplot(
            data=dataset, 
            mapping=aes(
                x=from_sn_start,
                y=value,
                color=sensor,
                linetype=factor(year)
            )
        ) + 
        facet_wrap(~ season) +
        geom_line() + theme_bw() +
        theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1)) +
        geom_hline(yintercept = 0, color="red") +
        labs(
            title="Myron: AQS vs BEACO2N Time Series",
            subtitle="2022-2024 Faceted by Season",
            x = "Seconds from first day of season",
            y = "CO (ppm)",
            color="Sensor",
            linetype="Year",
            caption=dataset_name
        )
    ggsave(plot=by_season_ts, filename=paste0("./tidy_plots/", dataset_name, "/by_season_timeseries.png"))

    year_by_season_box = 
        ggplot(
            data=dataset %>% filter(sensor %in% c("aqs","beaco2n")), # Residual scaled poorly on plot
            mapping=aes(
                x=sensor, 
                y=value,
                color=sensor
            )
        ) + 
        facet_grid(rows=vars(year), cols=vars(season), scales="fixed") +
        geom_boxplot() + theme_bw() +
        theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1)) +
        labs(
            title="Myron: AQS vs BEACO2N Summary Boxplots",
            subtitle="2022-2024 Faceted by Year and Season",
            x = "Sensor",
            y = "CO (ppm)",
            caption=dataset_name
        )
    ggsave(plot=year_by_season_box, filename=paste0("./tidy_plots/",dataset_name,"/myron_yr_by_sn_box.png"))

    by_year_box_nores = 
        ggplot(
            data=dataset %>% filter(sensor %in% c("aqs","beaco2n")), # Residual scaled poorly on plot
            mapping=aes(
                x=sensor, 
                y=value,
                color=sensor
            )
        ) + 
        facet_wrap(~ year) +
        geom_boxplot() + theme_bw() +
        theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1)) +
        labs(
            title="Myron: AQS vs BEACO2N Summary Boxplots",
            subtitle="2022-2024 Faceted by Year, no Residual",
            x = "Sensor",
            y = "CO (ppm)",
            color="Sensor",
            caption=dataset_name
        )
    ggsave(plot=by_year_box_nores, filename=paste0("./tidy_plots/",dataset_name,"/by_year_box_nores.png"))

    by_year_box_onlyres = 
        ggplot(
            data=dataset %>% filter(sensor=="res"),
            mapping=aes(
                x=sensor, 
                y=value
            )
        ) + 
        facet_wrap(~ year) +
        geom_boxplot() + theme_bw() +
        labs(
            title="Myron: AQS vs BEACO2N Summary Boxplots",
            subtitle="2022-2024 Faceted by Year, only Residual",
            x = "Sensor",
            y = "CO (ppm)",
            caption=dataset_name
        )
    ggsave(plot=by_year_box_onlyres, filename=paste0("./tidy_plots/",dataset_name,"/by_year_box_onlyres.png"))

}

myron_df_long_seasonal =  myron_df_long %>%
    mutate(
        year=if_else(month(date)==12, year(date)+1, year(date)), 
        season=season(date), 
        season=factor(season, levels = c("Winter", "Spring", "Summer", "Fall")),
        from_sn_start=count_from_season_start(date)
    )
myron_rolling_long_seasonal = myron_rolling_long %>%
    mutate(
        year=if_else(month(date)==12, year(date)+1, year(date)), 
        season=season(date), 
        season=factor(season, levels = c("Winter", "Spring", "Summer", "Fall")),
        from_sn_start=count_from_season_start(date)
    ) 
myron_savgol_long_seasonal = myron_savgol_long %>%
    mutate(
        year=if_else(month(date)==12, year(date)+1, year(date)), 
        season=season(date), 
        season=factor(season, levels = c("Winter", "Spring", "Summer", "Fall")),
        from_sn_start=count_from_season_start(date)
    ) 

plot_graphs(myron_df_long_seasonal, "original")
plot_graphs(myron_rolling_long_seasonal, "rolling")
plot_graphs(myron_savgol_long_seasonal, "savgol")