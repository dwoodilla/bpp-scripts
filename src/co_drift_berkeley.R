library(tidyverse)
library(sgolay)
library(ggpmisc)
library(zoo)
library(checkmate)
library(latex2exp)
library(gridExtra)

source("./clean/import_cleaned.R")
source("./src/data_arrangement_helpers.R")
source("./src/divergence_plot_helpers.R")
source("./src/timeseries_plot_helpers.R")

co_imp = import_co(city="berkeley")

co = elongate_df(
    df = co_imp,
    parameters = c("co"),
    sensors = c("beaco2n","super"),
    meteorology=c(Inf,Inf),
    read_from_cache = TRUE, 
    cache_file="cache/beaco2n_berkeley_co_drift_long.csv"
)
co_met = elongate_df(
    df = co_imp,
    parameters = c("co"),
    sensors = c("beaco2n","super"),
    meteorology = c(30,75),
    read_from_cache = TRUE,
    cache_file="cache/beaco2n_berkeley_co_drift_long_metfilter.csv"
)

for (meas_location_iter in beaco2n_berkeley_site_list) {
    ref_location = "rfs"
    basepath = paste0("./plots/berkeley_co_drift/",meas_location_iter,"/")
    print(paste0("meas_location_iter: ", meas_location_iter))

    # Data structures
    df_list = list(
        ext      = arrange_plot_df(co, "co", "beaco2n", meas_location_iter, FALSE, "super", "rfs"),
        ext_met  = arrange_plot_df(co_met, "co", "beaco2n", meas_location_iter, FALSE, "super", "rfs"),
        int      = arrange_plot_df(co, "co", "beaco2n", meas_location_iter, TRUE),
        int_met  = arrange_plot_df(co_met, "co", "beaco2n", meas_location_iter, TRUE)
    )
    df_list = lapply(df_list, facet_filter_helper)
    pdf_list = lapply(df_list, deployment_density_stats)

    # Plot configs
    plot_cfgs = list(
        ext = list(
            folder = "extref/",
            ref = toupper(ref_location),
            subtitle = paste0(toupper(meas_location_iter)," BEACO2N vs. ", toupper(ref_location)," Reference"),
            self_ref = FALSE
        ),
        int = list(
            folder = "intref/",
            ref = toupper(meas_location_iter),
            subtitle = paste0(toupper(meas_location_iter)," BEACO2N vs.\n",toupper(meas_location_iter)," BEACO2N at Operating-year 1"),
            self_ref = TRUE
        )
    )

    # Meteorology filter configs
    met_cfgs = list(
        nofilter = list(suffix = "nofilter", caption = "meteorology=(no filter)", met = FALSE),
        metfilter = list(suffix = "metfilter", caption = "meteorology=(t<30C, rh<0.75)", met = TRUE)
    )

    # Loop over external/internal, and met/no-met
    for (ref_type in c("ext", "int")) {
        for (met_type in c("nofilter", "metfilter")) {
            df_key = ifelse(met_type == "nofilter", ref_type, paste0(ref_type,"_met"))
            print(df_key)
            pdf_key = df_key
            cfg = plot_cfgs[[ref_type]]
            met_cfg = met_cfgs[[met_type]]
            plot_df = df_list[[df_key]]
            pdf_stats = pdf_list[[pdf_key]][[1]]

            deployment_correlation(
                plot_df = plot_df,
                filepath = paste0(basepath, cfg$folder, "deployment_correlation_", met_cfg$suffix, ".png"),
                title = "Deployment Time Series Correlation by Operating Month",
                subtitle = cfg$subtitle,
                caption = met_cfg$caption,
                x = "Reference [CO] (ppm)",
                y = "Measurement [CO] (ppm)"
            )

            deployment_corr_stat_lineplot(
                plot_df = plot_df,
                plot_filepath = paste0(basepath, cfg$folder, "deployment_corrstats_", met_cfg$suffix, ".png"),
                csv_filepath = paste0(basepath, cfg$folder, "deployment_corrstats_", met_cfg$suffix, ".csv"),
                title = "Deployment Correlation Statistics by Operating Month",
                subtitle = cfg$subtitle,
                caption = met_cfg$caption,
                x = "Months deployed",
                y = "Statistic value"
            )

            # Density plot
            deployment_density(
                plot_df = plot_df,
                filepath = paste0(basepath, cfg$folder, "deployment_pdfs_", met_cfg$suffix, ".png"),
                title = "Deployment Probability Density Functions by Operating Month",
                subtitle = cfg$subtitle,
                caption = met_cfg$caption,
                x = "[CO] (ppm)",
                y = TeX("$\\frac{d (Cumulative\\_density)}{d[CO]}=pdf$")
            )

            # Divergence line plot
            divergence_line_plot(
                pdf_stats = pdf_stats,
                filepath = paste0(basepath, cfg$folder, "divergence_line_plot_", met_cfg$suffix, ".png"),
                title = "Divergence Statistics over Months Deployed",
                subtitle = cfg$subtitle,
                caption = met_cfg$caption,
                x = "Months Deployed",
                y = "Statistic value",
                self_ref = cfg$self_ref
            )

            # Write divergence table
            write_csv(pdf_stats, file = paste0(basepath, cfg$folder, "divergence_table_", met_cfg$suffix, ".csv"))
        }

        # Difference plot between metfilter and nofilter
        pdf_nofilt = pdf_list[[ref_type]][[1]] %>% select(-pdf_resolution)
        pdf_metfilt = pdf_list[[paste0(ref_type,"_met")]][[1]] %>% select(-pdf_resolution)
        pdf_diff = left_join(
            x = pdf_nofilt,
            y = pdf_metfilt,
            by = join_by(mos_into_deployment, statistic, plottype),
            relationship = "one-to-one",
            suffix = c("_nofilt", "_metfilt")
        ) %>%
        mutate(value = value_metfilt - value_nofilt) %>%
        select(-c("value_nofilt", "value_metfilt"))

        divergence_line_plot(
            pdf_stats = pdf_diff,
            filepath = paste0(basepath, cfg$folder, "metfilter_divergence_difference.png"),
            self_ref = FALSE,
            title = "Change in Divergence after Meteorological Filtering",
            subtitle = cfg$subtitle,
            caption = "meteorology=NA, noise=\"original\"",
            y = "Meteorologically Filtered Statistic - Non-filtered Statistic",
            x = "Months into Deployment"
        )
    }
}
