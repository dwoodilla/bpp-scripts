library(tidyverse)
library(sgolay)
library(ggpmisc)
library(zoo)
library(checkmate)
library(latex2exp)
library(gridExtra)

source("./clean/import_cleaned.R")
source("./src/plot_helpers.R")



co = import_co()
co_dod = dates_of_deployment(df=co, parameter_arg="co", sensors=c("beaco2n","aqs"))
co_filtered = co %>% 
    pivot_wider(
        names_from="parameter",
        values_from="value"
    ) %>%
    filter(is.na(temp) | temp < 30, is.na(rh) | rh < 75) %>%
    pivot_longer(
        cols=c("co","temp","rh"),
        names_to = "parameter",
        values_to="value"
    )

co_long_unfiltered = elongate_wrapper(
    df=co,
    parameter="co",
    sensors=c("beaco2n","aqs"),
    dates_of_deployment=co_dod,
    filepath="./clean_data/beaco2n_drift_long.csv"
)
co_long_filtered = elongate_wrapper(
    df=co_filtered,
    parameter="co",
    sensors=c("beaco2n","aqs"),
    dates_of_deployment=co_dod,
    filepath="./clean_data/beaco2n_drift_long_metfiltered.csv"
)
print(head(co_long_filtered), width=Inf)
print(head(co_long_unfiltered), width=Inf)
print(head(co_dod), width=Inf)

for (meas_location_iter in c("zuccolo")) {
    ref_location = ""
    if (meas_location_iter=="myron") {
        ref_location = "myron"
    } else {
        ref_location = "cranston"
    }

    # Data structures for external reference
# co_unfiltered_plottable_extref = arrange_season_data(
#     dataset=co_long_unfiltered,
#     noise_filter="original",
#     meas_sensor="beaco2n",
#     meas_location=meas_location_iter,
#     self_ref=FALSE,
#     ref_sensor="aqs",
#     ref_location=ref_location
# )
# co_unfiltered_plottable_deployment_extref = arrange_deployment_data(
#     dataset=co_long_unfiltered,
#     noise_filter="original",
#     meas_sensor="beaco2n",
#     meas_location=meas_location_iter,
#     self_ref=FALSE,
#     ref_sensor="aqs",
#     ref_location="myron"
# )
# co_unfiltered_deployment_pdf_stats_extref = deployment_density_stats(
#     season_data=co_unfiltered_plottable_extref
# )
# co_filtered_plottable_extref = arrange_season_data(
#     dataset=co_long_filtered,
#     noise_filter="original",
#     meas_sensor="beaco2n",
#     meas_location=meas_location_iter,
#     self_ref=FALSE,
#     ref_sensor="aqs",
#     ref_location="myron"
# )
# co_filtered_plottable_deployment_extref = arrange_deployment_data(
#     dataset=co_long_filtered,
#     noise_filter="original",
#     meas_sensor="beaco2n",
#     meas_location=meas_location_iter,
#     self_ref=FALSE,
#     ref_sensor="aqs",
#     ref_location="myron"
# )
# co_filtered_deployment_pdf_stats_extref = deployment_density_stats(
#     season_data=co_filtered_plottable_extref
# )

# Data structures for internal reference 
print("co_unfiltered_plottable_intref")
co_unfiltered_plottable_intref = arrange_season_data(
    dataset=co_long_unfiltered,
    noise_filter="original",
    meas_sensor="beaco2n",
    meas_location=meas_location_iter,
    self_ref=TRUE
)
print("co_unfiltered_plottable_deployment_intref")
co_unfiltered_plottable_deployment_intref = arrange_deployment_data(
    dataset=co_long_unfiltered,
    noise_filter="original",
    meas_sensor="beaco2n",
    meas_location=meas_location_iter,
    self_ref=TRUE
)
print("co_unfiltered_deployment_pdf_stats_intref")
co_unfiltered_deployment_pdf_stats_intref = deployment_density_stats(
    season_data=co_unfiltered_plottable_intref
)
print("co_filtered_plottable_intref")
co_filtered_plottable_intref = arrange_season_data(
    dataset=co_long_filtered,
    noise_filter="original",
    meas_sensor="beaco2n",
    meas_location=meas_location_iter,
    self_ref=TRUE
)
print("co_filtered_plottable_deployment_intref")
co_filtered_plottable_deployment_intref = arrange_deployment_data(
    dataset=co_long_filtered,
    noise_filter="original",
    meas_sensor="beaco2n",
    meas_location=meas_location_iter,
    self_ref=TRUE
)
print("co_filtered_deployment_pdf_stats_intref")
co_filtered_deployment_pdf_stats_intref = deployment_density_stats(
    season_data=co_filtered_plottable_intref
)

basepath = paste0("./plots/co_drift/",meas_location_iter,"/")

# # Plots for external reference, no meteorological filtering
# deployment_correlation(
#     deployment_data = co_unfiltered_plottable_deployment_extref,
#     season_data = co_unfiltered_plottable_extref,
#     filepath=paste0(basepath, "extref/deployment_correlation_nofilter.png"),
#     title="Deployment Time Series Correlation by Operating Month",
#     subtitle=paste0(toupper(meas_location_iter)," BEACO2N vs. ", toupper(ref_location)," RIDEM"),
#     caption="meteorology=(no filter)  noise=\"original\"",
#     x="Reference [CO] (ppm)",
#     y="Measurement [CO] (ppm)"
# )
# deployment_corr_stat_lineplot(
#     deployment_data = co_unfiltered_plottable_deployment_extref,
#     filepath=paste0(basepath, "extref/deployment_corrstats_nofilter.png"),
#     title="Deployment Correlation Statistics by Operating Month",
#     subtitle=paste0(toupper(meas_location_iter)," BEACO2N vs. ", toupper(ref_location)," RIDEM"),
#     caption="meteorology=(no filter)  noise=\"original\"",
#     x="Months deployed",
#     y="Statistic value"

# )
# deployment_density(
#     season_data=co_unfiltered_plottable_extref,
#     filepath=paste0(basepath, "extref/deployment_pdfs_nofilter.png"),
#     title="Deployment Probability Density Functions by Operating Month",
#     subtitle=paste0(toupper(meas_location_iter)," BEACO2N vs. ", toupper(ref_location)," RIDEM"),
#     caption="meteorology=(no filter)  noise=\"original\"",
#     x="[CO] (ppm)",
#     y=TeX("$\\frac{d (Cumulative\\_density)}{d[CO]}=pdf$")
# )
# divergence_line_plot(
#     pdf_stats=co_unfiltered_deployment_pdf_stats_extref[[1]],
#     filepath=paste0(basepath,"extref/divergence_line_plot_nofilter.png"),
#     title="Divergence Statistics over Months Deployed",
#     subtitle=paste0(toupper(meas_location_iter)," BEACO2N vs. ", toupper(ref_location)," RIDEM"),
#     caption="meteorology=(no filter)  noise=\"original\"",
#     x="Months Deployed",
#     y="Statistic value",
#     self_ref=FALSE
# )

# png(filename=paste0(basepath,"extref/divergence_table_nofilter.png"), width=8.5, height=11, units="in", res=300)
# grid.table(
#     co_unfiltered_deployment_pdf_stats_extref[[1]] %>% 
#         filter(statistic %in% c("KL", "hellinger", "euclidean")) %>%
#         pivot_wider(
#             names_from="statistic",
#             values_from="value"
#         ) %>%
#         select(-c(pdf_resolution, plottype)) %>%
#         drop_na(c("KL","hellinger","euclidean"))
# )
# dev.off()

# # Plots for external reference, WITH meteorological filtering
# deployment_correlation(
#     deployment_data = co_filtered_plottable_deployment_extref,
#     season_data=co_filtered_plottable_extref,
#     filepath=paste0(basepath,"extref/deployment_correlation_metfilter.png"),
#     title="Deployment Time Series Correlation by Operating Month",
#     subtitle=paste0(toupper(meas_location_iter)," BEACO2N vs. ", toupper(ref_location)," RIDEM"),
#     caption="meteorology=(t<30C, rh<0.75)  noise=\"original\"",
#     x="Reference [CO] (ppm)",
#     y="Measurement [CO] (ppm)"
# )
# deployment_corr_stat_lineplot(
#     deployment_data = co_filtered_plottable_deployment_extref,
#     filepath=paste0(basepath,"extref/deployment_corrstats_metfilter.png"),
#     title="Deployment Correlation Statistics by Operating Month",
#     subtitle=paste0(toupper(meas_location_iter)," BEACO2N vs. ", toupper(ref_location)," RIDEM"),
#     caption="meteorology=(no filter)  noise=\"original\"",
#     x="Months deployed",
#     y="Statistic value"

# )
# deployment_density(
#     season_data=co_filtered_plottable_extref,
#     filepath=paste0(basepath,"extref/deployment_pdfs_metfilter.png"),
#     title="Deployment Probability Density Functions by Operating Month",
#     subtitle=paste0(toupper(meas_location_iter)," BEACO2N vs. ", toupper(ref_location)," RIDEM"),
#     caption="meteorology=(t<30C, rh<0.75)  noise=\"original\"",
#     x="[CO] (ppm)",
#     y=TeX("$\\frac{d (Cumulative\\_density)}{d[CO]}=pdf$")
# )
# divergence_line_plot(
#     pdf_stats=co_filtered_deployment_pdf_stats_extref[[1]],
#     filepath=paste0(basepath,"extref/divergence_line_plot_metfilter.png"),
#     title="Divergence Statistics over Months Deployed",
#     subtitle=paste0(toupper(meas_location_iter)," BEACO2N vs. ", toupper(ref_location)," RIDEM"),
#     caption="meteorology=(t<30C, rh<0.75)  noise=\"original\"",
#     x="Months Deployed",
#     y="Statistic value",
#     self_ref=FALSE
# )

# png(filename=paste0(basepath,"extref/divergence_table_metfilter.png"), width=8.5, height=11, units="in", res=300)
# grid.table(
#     co_filtered_deployment_pdf_stats_extref[[1]] %>% 
#         filter(statistic %in% c("KL", "hellinger", "euclidean")) %>%
#         pivot_wider(
#             names_from="statistic",
#             values_from="value"
#         ) %>%
#         select(-c(pdf_resolution, plottype)) %>%
#         drop_na(c("KL","hellinger","euclidean"))
# )
# dev.off()

# metfilter_divergence_diff_by_opmonth_extref = 
#     left_join(
#         x=co_unfiltered_deployment_pdf_stats_extref[[1]] %>% select(-pdf_resolution),
#         y=co_filtered_deployment_pdf_stats_extref[[1]] %>% select(-pdf_resolution),
#         by=join_by(mos_into_deployment, statistic, plottype),
#         relationship="one-to-one",
#         suffix=c("_nofilt","_metfilt")
#     ) %>%
#     mutate(
#         value = value_metfilt-value_nofilt
#     ) %>%
#     select(-c("value_nofilt","value_metfilt"))

# divergence_line_plot(
#     pdf_stats = metfilter_divergence_diff_by_opmonth_extref,
#     filepath=paste0(basepath,"extref/metfilter_divergence_difference.png"),
#     lims_y=c(-0.33,0.33),
#     self_ref=FALSE,
#     title="Change in Divergence after Meteorological Filtering",
#     subtitle=paste0(toupper(meas_location_iter)," BEACO2N vs. ", toupper(ref_location)," RIDEM"),
#     caption="meteorology=NA, noise=\"original\"",
#     y="Meteorologically Filtered Statistic - Non-filtered Statistic",
#     x="Months into Deployment"
# )

# Plots for internal reference, no meteorological filtering
print("deployment_correlation")
deployment_correlation(
    deployment_data = co_unfiltered_plottable_deployment_intref,
    season_data = co_unfiltered_plottable_intref,
    filepath=paste0(basepath,"intref/deployment_correlation_nofilter.png"),
    title="Deployment Time Series Correlation by Operating Month",
    subtitle=paste0(toupper(meas_location_iter)," BEACO2N vs. ",toupper(meas_location_iter)," BEACO2N at Operating-year 1"),
    caption="meteorology=(no filter)  noise=\"original\"",
    x="Reference [CO] (ppm)",
    y="Measurement [CO] (ppm)"
)
print("deployment_density")
deployment_density(
    season_data=co_unfiltered_plottable_intref,
    filepath=paste0(basepath,"intref/deployment_pdfs_nofilter.png"),
    title="Deployment Probability Density Functions by Operating Month",
    subtitle=paste0(toupper(meas_location_iter)," BEACO2N vs. ",toupper(meas_location_iter)," BEACO2N at Operating-year 1"),
    caption="meteorology=(no filter)  noise=\"original\"",
    x="[CO] (ppm)",
    y=TeX("$\\frac{d (Cumulative\\_density)}{d[CO]}=pdf$")
)
print("divergence_line_plot")
divergence_line_plot(
    pdf_stats=co_unfiltered_deployment_pdf_stats_intref[[1]],
    filepath=paste0(basepath,"intref/divergence_line_plot_nofilter.png"),
    title="Divergence Statistics over Months Deployed",
    subtitle=paste0(toupper(meas_location_iter)," BEACO2N vs. ",toupper(meas_location_iter)," BEACO2N at Operating-year 1"),
    caption="meteorology=(no filter)  noise=\"original\"",
    x="Months Deployed",
    y="Statistic value",
    self_ref=TRUE
)

print("png")
png(filename=paste0(basepath,"intref/divergence_table_nofilter.png"), width=8.5, height=11, units="in", res=300)
grid.table(
    co_unfiltered_deployment_pdf_stats_intref[[1]] %>% 
        filter(statistic %in% c("KL", "hellinger", "euclidean")) %>%
        pivot_wider(
            names_from="statistic",
            values_from="value"
        ) %>%
        select(-c(pdf_resolution, plottype)) %>%
        drop_na(c("KL","hellinger","euclidean"))
)
dev.off()

# Plots for internal reference, WITH meteorological filtering
print("deployment_correlation")
deployment_correlation(
    deployment_data = co_filtered_plottable_deployment_intref,
    season_data = co_filtered_plottable_intref,
    filepath=paste0(basepath,"intref/deployment_correlation_metfilter.png"),
    title="Deployment Time Series Correlation by Operating Month",
    subtitle=paste0(toupper(meas_location_iter)," BEACO2N vs. ",toupper(meas_location_iter)," BEACO2N at Operating-year 1"),
    caption="meteorology=(t<30C, rh<0.75)  noise=\"original\"",
    x="Reference [CO] (ppm)",
    y="Measurement [CO] (ppm)"
)
print("deployment_density")
deployment_density(
    season_data=co_filtered_plottable_intref,
    filepath=paste0(basepath,"intref/deployment_pdfs_metfilter.png"),
    title="Deployment Probability Density Functions by Operating Month",
    subtitle=paste0(toupper(meas_location_iter)," BEACO2N vs. ",toupper(meas_location_iter)," BEACO2N at Operating-year 1"),
    caption="meteorology=(t<30C, rh<0.75)  noise=\"original\"",
    x="[CO] (ppm)",
    y=TeX("$\\frac{d (Cumulative\\_density)}{d[CO]}=pdf$")
)
print("divergence_line_plot")
divergence_line_plot(
    pdf_stats=co_filtered_deployment_pdf_stats_intref[[1]],
    filepath=paste0(basepath,"intref/divergence_line_plot_metfilter.png"),
    title="Divergence Statistics over Months Deployed",
    subtitle=paste0(toupper(meas_location_iter)," BEACO2N vs. ",toupper(meas_location_iter)," BEACO2N at Operating-year 1"),
    caption="meteorology=(t<30C, rh<0.75)  noise=\"original\"",
    x="Months Deployed",
    y="Statistic value",
    self_ref=TRUE
)

print("png")
png(filename=paste0(basepath,"intref/divergence_table_metfilter.png"), width=8.5, height=11, units="in", res=300)
grid.table(
    co_filtered_deployment_pdf_stats_intref[[1]] %>% 
        filter(statistic %in% c("KL", "hellinger", "euclidean")) %>%
        pivot_wider(
            names_from="statistic",
            values_from="value"
        ) %>%
        select(-c(pdf_resolution, plottype)) %>%
        drop_na(c("KL","hellinger","euclidean"))
)
dev.off()


print("metfilter_divergence_diff_by_opmonth_intref")
metfilter_divergence_diff_by_opmonth_intref = 
    left_join(
        x=co_unfiltered_deployment_pdf_stats_intref[[1]] %>% select(-pdf_resolution),
        y=co_filtered_deployment_pdf_stats_intref[[1]] %>% select(-pdf_resolution),
        by=join_by(mos_into_deployment, statistic, plottype),
        relationship="one-to-one",
        suffix=c("_nofilt","_metfilt")
    ) %>%
    mutate(
        value = value_metfilt-value_nofilt
    ) %>%
    select(-c("value_nofilt","value_metfilt"))

print("divergence_line_plot")
divergence_line_plot(
    pdf_stats = metfilter_divergence_diff_by_opmonth_intref,
    filepath=paste0(basepath,"intref/metfilter_divergence_difference.png"),
    lims_y=c(-0.33,0.33),
    self_ref=FALSE,
    title="Change in Divergence after Meteorological Filtering",
    subtitle=paste0(toupper(meas_location_iter)," BEACO2N vs. ",toupper(meas_location_iter)," BEACO2N at Operating-year 1"),
    caption="meteorology=NA, noise=\"original\"",
    y="Meteorologically Filtered Statistic - Non-filtered Statistic",
    x="Months into Deployment"
)


}


