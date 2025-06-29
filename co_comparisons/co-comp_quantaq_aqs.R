# install.packages("openair")
# install.packages("dplyr")
# install.packages("Metrics")
library(openair)
library(dplyr)
library(clock)
usethis::use_readme_rmd()

combined_df = read.csv("./combined_dataset.csv")
combined_df$date = as.POSIXct(combined_df$date, tz="UTC")

print_stats = function(qaq, ref, filename) {
    stopifnot(is.double(qaq), is.double(ref), length(qaq)==length(ref))
    stopifnot(is.character(filename))

    res = qaq - ref
    MAE = mean(abs(res))
    RMSE = sqrt(mean(res^2))
    rcor = cor(qaq, ref)
    Rsq = rcor^2 # NOTE: assuming this is a linear least-squares regression with a single independent variable
    ttest = t.test(qaq, ref)

    # Capture printed outputs as text lines
    qaq_txt     =  capture.output(summary(qaq))
    ref_txt     =  capture.output(summary(ref))
    res_txt     =  capture.output(summary(res))
    ttest_txt   =  capture.output(print(ttest))
    
    sink(filename)
    on.exit(sink())
    # Combine everything for printing
    out = c(
        "QuantAQ Summary:",
        qaq_txt,
        "",
        "Reference Summary:",
        ref_txt,
        "",
        "Residual Summary:",
        res_txt,
        "",
        "Residual Statistics:",
        sprintf("\tMAE:       %.6f", MAE),
        sprintf("\tRMSE:      %.6f", RMSE),
        sprintf("\tPearson r: %.6f", rcor),
        sprintf("\tR^2:       %.6f", Rsq),
        "",
        "t-test:",
        ttest_txt # TODO: format more nicely (get rid of centered heading included in the t.test output)
    )
    cat(paste0(out, collapse = "\n"), "\n")
}

co_time_series = function(df, qaq_pol, qaq_site, ref_pol, ref_site, filepath) {

    png(
        filename=filepath,
        width = 10*300, height=10*300, res=300
    )
    timePlot(
        df, 
        pollutant=c(ref_pol, qaq_pol, "residual"),
        name.pol=c(ref_site, qaq_site, paste(qaq_site, "–", ref_site)), 
        main=paste(qaq_site, "vs", ref_site),
        smooth=TRUE, 
        ci=TRUE,
        group=TRUE,
        ylim = c(-0.2,1.4),
        ylab="CO (ppm)",
        y.relation = "same",
        lty=1,
        scales = list(
            y = list(
                at = seq(-0.2, 1.4, by = 0.2),    # tick positions every 0.2
                labels = seq(-0.2, 1.4, by = 0.2),# matching labels
                tck = c(1, 0)                     # draw ticks into the plotting area
            )
        )
    ) # nolint
    dev.off() 
}

co_time_variation = function(df, qaq_pol, qaq_site, ref_pol, ref_site, filepath) {
    # Plot time variations
    png(
        filename=filepath,
        width = 10*300, height=10*300, res=300
    )
    timeVariation(
        df, 
        pollutant=c(ref_pol, qaq_pol, "residual"),
        name.pol=c(ref_site, qaq_site, paste(qaq_site, "–", ref_site)),
        ylab="CO (ppm)",
        main=paste(qaq_site, "vs", ref_site)
    )
    dev.off()
}

# COMPARE QUANTAQ TO AQS (BEACO2N BELOW)

merged_vaqs_df = read.csv("./intermediary_datasets/merge_quantaq_aqs.csv")
merged_vaqs_df$date = as.POSIXct(merged_vaqs_df$date, tz="UTC")

# dec_vaqs_df_all = merged_vaqs_df %>% filter(if_all(everything(), ~ !is.na(.x))) # Filter for times when AQS and DPW have data # nolint
dec_vaqs_df_dpw = merged_vaqs_df %>% filter(!is.na(merged_vaqs_df$co_quantaq_dpw) & !is.na(merged_vaqs_df$co_aqs_cranston)) # Filter for times when AQS and DPW have data # nolint
dec_vaqs_df_pha = merged_vaqs_df %>% filter(!is.na(merged_vaqs_df$co_quantaq_pha) & !is.na(merged_vaqs_df$co_aqs_cranston)) # Filter for times when AQS and PHA have data # nolint
dec_vaqs_df_pema = merged_vaqs_df %>% filter(!is.na(merged_vaqs_df$co_quantaq_pema) & !is.na(merged_vaqs_df$co_aqs_cranston)) # Filter for times when AQS and PEMA have data # nolint

# Make residual columns for plotting
dec_vaqs_df_dpw$residual = dec_vaqs_df_dpw$co_quantaq_dpw - dec_vaqs_df_dpw$co_aqs_cranston
dec_vaqs_df_pha$residual = dec_vaqs_df_pha$co_quantaq_pha - dec_vaqs_df_pha$co_aqs_cranston
dec_vaqs_df_pema$residual = dec_vaqs_df_pema$co_quantaq_pema - dec_vaqs_df_pema$co_aqs_cranston

# Write dataframes to CSVs for manual inspection
# write.csv(dec_vaqs_df_all, file="./co_comparisons/dec_vaqs_df_all.csv", row.names=FALSE)
write.csv(dec_vaqs_df_dpw, file="./co_comparisons/dec_vaqs_df_dpw.csv", row.names=FALSE)
write.csv(dec_vaqs_df_pha, file="./co_comparisons/dec_vaqs_df_pha.csv", row.names=FALSE)
write.csv(dec_vaqs_df_pema, file="./co_comparisons/dec_vaqs_df_pema.csv", row.names=FALSE)


# Compare the CO values of Cranston and QuantAQ sites (disregarding temp and rh)
print_stats(ref=dec_vaqs_df_dpw$co_aqs_cranston, qaq=dec_vaqs_df_dpw$co_quantaq_dpw, filename="./co_comparisons/stats_vaqs_dpw.txt")
print_stats(ref=dec_vaqs_df_pema$co_aqs_cranston, qaq=dec_vaqs_df_pema$co_quantaq_pema, filename="./co_comparisons/stats_vaqs_pema.txt")
print_stats(ref=dec_vaqs_df_pha$co_aqs_cranston, qaq=dec_vaqs_df_pha$co_quantaq_pha, filename="./co_comparisons/stats_vaqs_pha.txt")


co_time_series(
    df=dec_vaqs_df_dpw, 
    filepath="./co_comparisons/ts_dpw_aqs.png", 
    qaq_pol="co_quantaq_dpw",
    ref_pol="co_aqs_cranston",
    qaq_site="QuantAQ @ DPW",
    ref_pol="AQS @ Cranston"
)

# # Plot time series with residual for each QuantAQ/AQS pairing (only for times when QAQ and AQS are both active)
# png(
#     filename="./co_comparisons/ts_dpw_aqs.png",
#     width = 10*300, height=10*300, res=300
# )
# timePlot(
#     dec_vaqs_df_dpw, 
#     pollutant=c("co_aqs_cranston", "co_quantaq_dpw", "residual"),
#     name.pol=c("AQS @ Cranston", "QuantAQ @ DPW", "QuantAQ - AQS"), 
#     main="QuantAQ DPW vs. AQS Cranston\nOverlapping timestamps",
#     smooth=TRUE, 
#     ci=TRUE,
#     group=TRUE,
#     ylim = c(-0.2,1.4),
#     ylab="CO (ppm)",
#     y.relation = "same",
#     lty=1,
#     scales = list(
#         y = list(
#             at = seq(-0.2, 1.4, by = 0.2),    # tick positions every 0.2
#             labels = seq(-0.2, 1.4, by = 0.2),# matching labels
#             tck = c(1, 0)                     # draw ticks into the plotting area
#         )
#     )
# ) # nolint
# dev.off()

co_time_series(
    df=dec_vaqs_df_pha, 
    filename="./co_comparisons/ts_pha_aqs.png",
    qaq_pol="co_quantaq_pha"
)

png(
    filename="./co_comparisons/ts_pha_aqs.png",
    width = 10*300, height=10*300, res=300
)
timePlot(
    dec_vaqs_df_pha, 
    pollutant=c("co_aqs_cranston", "co_quantaq_pha", "residual"),
    name.pol=c("AQS @ Cranston", "QuantAQ @ PHA", "QuantAQ - AQS"), 
    main="QuantAQ PHA vs. AQS Cranston",
    smooth=TRUE, 
    ci=TRUE,
    group=TRUE,
    ylim = c(-0.2,1.4),
    ylab="CO (ppm)",
    y.relation = "same",
    lty=1,
    scales = list(
        y = list(
            at = seq(-0.2, 1.4, by = 0.2),    # tick positions every 0.2
            labels = seq(-0.2, 1.4, by = 0.2),# matching labels
            tck = c(1, 0)                     # draw ticks into the plotting area
        )
    )
) # nolint
dev.off()

png(
    filename="./co_comparisons/ts_pema_aqs.png",
    width = 10*300, height=10*300, res=300
)
timePlot(
    dec_vaqs_df_pema, 
    pollutant=c("co_aqs_cranston", "co_quantaq_pema", "residual"),
    name.pol=c("AQS @ Cranston", "QuantAQ @ PEMA", "QuantAQ - AQS"), 
    main="QuantAQ PEMA vs. AQS Cranston",
    smooth=TRUE, 
    ci=TRUE,
    group=TRUE,
    ylim = c(-0.2,1.4),
    ylab="CO (ppm)",
    y.relation = "same", 
    lty=1,
    scales = list(
        y = list(
            at = seq(-0.2, 1.4, by = 0.2),    # tick positions every 0.2 ppm
            labels = seq(-0.2, 1.4, by = 0.2),# matching labels
            tck = c(1, 0)                     # draw ticks into the plotting area
        )
    )
) # nolint
dev.off()

# Make dataframes to plot QuantAQ/AQS pairings with AQS data going back to 11-2022 (roughly 2 yrs before QuantAQ sensors were set up).

combined_qaq_df = combined_df %>%
    select(date, co_aqs_cranston, co_quantaq_dpw, co_quantaq_pema, co_quantaq_pha) %>%
    filter(date > as.POSIXct("2022-11-01", tx="UTC"))

# Plot DPW versus extended AQS data
png(
    filename="./co_comparisons/tv_dpw_aqs.png",
    width = 10*300, height=10*300, res=300
)
timeVariation(
    combined_qaq_df, 
    pollutant=c("co_aqs_cranston", "co_quantaq_dpw"),
    name.pol=c("AQS @ Cranston (from 11/2022)", "QuantAQ @ DPW (from 11/2024)"),
    ylab="CO (ppm)",
    main="QuantAQ DPW vs. AQS Cranston (extended)"
)
dev.off()

# Plot pha versus extended AQS data
png(
    filename="./co_comparisons/tv_pha_aqs.png",
    width = 10*300, height=10*300, res=300
)
timeVariation(
    combined_qaq_df, 
    pollutant=c("co_aqs_cranston", "co_quantaq_pha"),
    name.pol=c("AQS @ Cranston (from 11/2022)", "QuantAQ @ pha (from 11/2024)"),
    ylab="CO (ppm)",
    main="QuantAQ pha vs. AQS Cranston (extended)"
)
dev.off()

# Plot pema versus extended AQS data
png(
    filename="./co_comparisons/tv_pema_aqs.png",
    width = 10*300, height=10*300, res=300
)
timeVariation(
    combined_qaq_df, 
    pollutant=c("co_aqs_cranston", "co_quantaq_pema"),
    name.pol=c("AQS @ Cranston (from 11/2022)", "QuantAQ @ pema (from 11/2024)"),
    ylab="CO (ppm)",
    main="QuantAQ pema vs. AQS Cranston (extended)"
)
dev.off()

# COMPARE QUANTAQ TO BEACO2N (AQS ABOVE)

merged_vbcn_df = combined_df %>% select(date, co_quantaq_dpw, co_quantaq_pema, co_quantaq_pha, co_beaco2n_dpw, co_beaco2n_pema, co_beaco2n_pha)

vbcn_df_dpw = merged_vbcn_df %>% filter(!is.na(merged_vbcn_df$co_quantaq_dpw) & !is.na(merged_vbcn_df$co_beaco2n_dpw)) # Filter for times when qaq and bcn at DPW have data # nolint
vbcn_df_pha = merged_vbcn_df %>% filter(!is.na(merged_vbcn_df$co_quantaq_pha) & !is.na(merged_vbcn_df$co_beaco2n_pha)) # Filter for times when qaq and bcn at PHA have data # nolint
vbcn_df_pema = merged_vbcn_df %>% filter(!is.na(merged_vbcn_df$co_quantaq_pema) & !is.na(merged_vbcn_df$co_beaco2n_pema)) # Filter for times when qaq and bcn at PEMA have data # nolint

# Make residual columns for plotting
vbcn_df_pha$residual = vbcn_df_pha$co_quantaq_pha - vbcn_df_pha$co_beaco2n_pha
vbcn_df_pema$residual = vbcn_df_pema$co_quantaq_pema - vbcn_df_pha$co_beaco2n_pema
vbcn_df_dpw$residual = vbcn_df_dpw$co_quantaq_dpw - vbcn_df_pha$co_beaco2n_dpw

# Write dataframes to CSVs for manual inspection
write.csv(vbcn_df_dpw, file="./co_comparisons/vbcn_df_dpw.csv", row.names=FALSE)
write.csv(vbcn_df_pha, file="./co_comparisons/vbcn_df_pha.csv", row.names=FALSE)
write.csv(vbcn_df_pema, file="./co_comparisons/vbcn_df_pema.csv", row.names=FALSE)

# Compare the CO values of colocated bcn/qaq sites (disregarding temp and rh)
print_stats(ref=vbcn_df_dpw$co_beaco2n_dpw, qaq=vbcn_df_dpw$co_quantaq_dpw, filename="./co_comparisons/stats_vbcn_dpw.txt")
print_stats(ref=vbcn_df_pema$co_beaco2n_pema, qaq=vbcn_df_pema$co_quantaq_pema, filename="./co_comparisons/stats_vbcn_pema.txt")
print_stats(ref=vbcn_df_pha$co_beaco2n_pha, qaq=vbcn_df_pha$co_quantaq_pha, filename="./co_comparisons/stats_vbcn_pha.txt")

# Plot time series with residual for each QuantAQ/BEACO2N pairing (only for times when QAQ and BEACO2N are both active)
png(
    filename="./co_comparisons/ts_dpw_bcn.png",
    width = 10*300, height=10*300, res=300
)
timePlot(
    vbcn_df_dpw, 
    pollutant=c("co_beaco2n_dpw", "co_quantaq_dpw", "residual"),
    name.pol=c("BEACO2N @ DPW", "QuantAQ @ DPW", "QuantAQ - BEACO2N"), 
    main="QuantAQ DPW vs. BEACO2N DPW",
    smooth=TRUE, 
    ci=TRUE,
    group=TRUE,
    ylim = c(-0.2,1.4),
    ylab="CO (ppm)",
    y.relation = "same",
    lty=1,
    scales = list(
        y = list(
            at = seq(-0.2, 1.4, by = 0.2),    # tick positions every 0.2
            labels = seq(-0.2, 1.4, by = 0.2),# matching labels
            tck = c(1, 0)                     # draw ticks into the plotting area
        )
    ),
    date.pad=TRUE
) # nolint
dev.off()

png(
    filename="./co_comparisons/ts_pha_bcn.png",
    width = 10*300, height=10*300, res=300
)
timePlot(
    vbcn_df_pha, 
    pollutant=c("co_beaco2n_pha", "co_quantaq_pha", "residual"),
    name.pol=c("BEACO2N @ PHA", "QuantAQ @ PHA", "QuantAQ - BEACO2N"), 
    main="QuantAQ PHA vs. BEACO2N PHA",
    smooth=TRUE, 
    ci=TRUE,
    group=TRUE,
    ylim = c(-0.2,1.4),
    ylab="CO (ppm)",
    y.relation = "same",
    lty=1,
    scales = list(
        y = list(
            at = seq(-0.2, 1.4, by = 0.2),    # tick positions every 0.2
            labels = seq(-0.2, 1.4, by = 0.2),# matching labels
            tck = c(1, 0)                     # draw ticks into the plotting area
        )
    ), 
    date.pad=TRUE
) # nolint
dev.off()

png(
    filename="./co_comparisons/ts_pema_bcn.png",
    width = 10*300, height=10*300, res=300
)
timePlot(
    vbcn_df_pema, 
    pollutant=c("co_beaco2n_pema", "co_quantaq_pema", "residual"),
    name.pol=c("BEACO2N @ PEMA", "QuantAQ @ PEMA", "QuantAQ - BEACO2N"), 
    main="QuantAQ PEMA vs. BEACO2N PEMA",
    smooth=TRUE, 
    ci=TRUE,
    group=TRUE,
    ylim = c(-0.2,1.4),
    ylab="CO (ppm)",
    y.relation = "same",
    lty=1,
    scales = list(
        y = list(
            at = seq(-0.2, 1.4, by = 0.2),    # tick positions every 0.2
            labels = seq(-0.2, 1.4, by = 0.2),# matching labels
            tck = c(1, 0)                     # draw ticks into the plotting area
        )
    ), 
    date.pad=TRUE
) # nolint
dev.off()

# Plot time variations
png(
    filename="./co_comparisons/tv_dpw_bcn.png",
    width = 10*300, height=10*300, res=300
)
timeVariation(
    vbcn_df_dpw, 
    pollutant=c("co_beaco2n_dpw", "co_quantaq_dpw", "residual"),
    name.pol=c("BEACO2N @ DPW", "QuantAQ @ DPW", "QuantAQ - BEACO2N"),
    ylab="CO (ppm)",
    main="QuantAQ DPW vs. BEACO2N DPW"
)
dev.off()

png(
    filename="./co_comparisons/tv_pema_bcn.png",
    width = 10*300, height=10*300, res=300
)
timeVariation(
    vbcn_df_pema, 
    pollutant=c("co_beaco2n_pema", "co_quantaq_pema", "residual"),
    name.pol=c("BEACO2N @ PEMA", "QuantAQ @ PEMA", "QuantAQ - BEACO2N"),
    ylab="CO (ppm)",
    main="QuantAQ PEMA vs. BEACO2N PEMA"
)
dev.off()

png(
    filename="./co_comparisons/tv_pha_bcn.png",
    width = 10*300, height=10*300, res=300
)
timeVariation(
    vbcn_df_pha, 
    pollutant=c("co_beaco2n_pha", "co_quantaq_pha", "residual"),
    name.pol=c("BEACO2N @ PHA", "QuantAQ @ PHA", "QuantAQ - BEACO2N"),
    ylab="CO (ppm)",
    main="QuantAQ PHA vs. BEACO2N PHA"
)
dev.off()


# TODO: IN PROGRESS
# co_time_series <- function(df) {
#   # Extract the name of the dataframe passed to the function
#   df_name <- deparse(substitute(df))
  
#   # Extract sensor type (chars 2-4) and site code (everything after last '_')
#   ref_type <- substr(df_name, 2, 4)  # characters 2-4
#   ref_column_label = if(ref_type=="aqs") "aqs" else "beaco2n"
#   qaq_site <- grep(".*_", "", df_name)  # everything after last '_'

#   qaq_ledgend_label = paste("QuantAQ @", toupper(qaq_site))
#   ref_ledgend_label = paste(toupper(ref_column_label), "@", if(ref_type=="aqs") "Cranston" else toupper(ref_site))
  
#   # Set reference and QA/QC site names
#   if (ref_type == "aqs") {
#     ref_site <- "cranston"
#   } else if (ref_type == "bcn") {
#     ref_site <- qaq_site
#   } else {
#     stop("Unknown sensor type in dataframe name. Expected 'vaqs' or 'vbcn'.")
#   }
  
#   # Pollutant variable names with new format
#   qaq_pol <- paste0("co_quantaq_", qaq_site)
#   ref_pol = paste0("co_",ref_column_label,ref_site)
#   # ref_pol <- if(ref_type=="bcn") paste0("co_beaco2n_",ref_site) else paste0("co_aqs_",ref_site)
  
#   # Create output filepath
#   filepath <- paste0("./co_comparisons/","ts_",qaq_site,"_",ref_type,".png")
  
#   png(
#     filename = filepath,
#     width = 10 * 300, height = 10 * 300, res = 300
#   )
  
#   timePlot(
#     df,
#     pollutant = c(ref_pol, qaq_pol, "residual"),
#     name.pol = c(
#         ref_ledgend_label,
#         qaq_ledgend_label, 
#         paste(qaq_ledgend_label, "–", ref_ledgend_label)
#     )
#     main = paste(qaq_site, "vs", ref_site),
#     smooth = TRUE,
#     ci = TRUE,
#     group = TRUE,
#     ylim = c(-0.2, 1.4),
#     ylab = "CO (ppm)",
#     y.relation = "same",
#     lty = 1,
#     scales = list(
#       y = list(
#         at = seq(-0.2, 1.4, by = 0.2),
#         labels = seq(-0.2, 1.4, by = 0.2),
#         tck = c(1, 0)
#       )
#     )
#   )
  
#   dev.off()
# }