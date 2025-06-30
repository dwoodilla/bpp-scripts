library(stringr)
library(openair)
library(dplyr)
library(clock)

combined_df = read.csv("./combined_dataset.csv")
combined_df$date = as.POSIXct(combined_df$date, tz="UTC")

print_stats = function(qaq, ref, filename) {
    stopifnot(is.double(qaq), is.double(ref), length(qaq)==length(ref), is.character(filename))

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

df_parser = function(df) {
  # Extract the variable name of the dataframe passed to the function
  df_name <- deparse(substitute(df))
  # Extract sensor type (chars [2-4))
  ref_type <- substr(df_name, 2, 4)  # characters 2-4
  ref_column_label = if(ref_type=="aqs") "aqs" else "beaco2n"

  # from df_name, search from end, extract everything not an underscore to end. (i.e. get the qaq site from df_name)
  qaq_site <- str_extract(df_name, "(?<=_)[^_]+$") 

  qaq_ledgend_label = paste("QuantAQ", toupper(qaq_site))
  ref_ledgend_label = paste(toupper(ref_column_label), if(ref_type=="aqs") "Cranston" else toupper(ref_site))
  
  if (ref_type == "aqs") {
    ref_site <- "cranston"
  } else if (ref_type == "bcn") {
    ref_site <- qaq_site
  } else {
    stop(paste("Unexpected dataframe name:",df_name,"\tExpected [vaqs,vbcn]_df_[site]"))
  }

  qaq_pol <- paste0("co_quantaq_", qaq_site)
  ref_pol = paste0("co_",ref_column_label,"_",ref_site) 

  return(
    list(
        qaq_pol           = qaq_pol,
        qaq_ledgend_label = qaq_ledgend_label,
        qaq_site          = qaq_site,
        ref_type          = ref_type,
        ref_pol           = ref_pol,
        ref_ledgend_label = ref_ledgend_label,
        ref_column_label  = ref_column_label
    )
  )
}

co_time_series = function(df, parsed) {
  
  filepath <- paste0("./plots/","ts_",parsed$qaq_site,"_",parsed$ref_type,".png")
  
  png(
    filename = filepath,
    width = 10 * 300, height = 10 * 300, res = 300
  )
  timePlot(
    df,
    pollutant = c(parsed$ref_pol, parsed$qaq_pol, "residual"),
    cols=c("#fc4e54", "#4e54fc", "#14ba01"),
    name.pol = c(
        parsed$ref_ledgend_label,
        parsed$qaq_ledgend_label, 
        paste("QuantAQ", "–", toupper(parsed$ref_column_label))
    ),
    main = paste(parsed$qaq_ledgend_label, "vs", parsed$ref_ledgend_label),
    smooth = TRUE,
    ci = TRUE,
    group = TRUE,
    ylim = c(-0.2, 1.4),
    ylab = "CO (ppm)",
    y.relation = "same",
    lty = 1,
    scales = list(
      y = list(
        at = seq(-0.2, 1.4, by = 0.2),
        labels = seq(-0.2, 1.4, by = 0.2),
        tck = c(1, 0)
      )
    )
  )
  dev.off()
}

co_time_variation = function(df, parsed) {
    # Plot time variations
    png(
        filename=paste0("./plots/","tv_",parsed$qaq_site,"_",parsed$ref_type,".png"),
        width = 10*300, height=10*300, res=300
    )
    timeVariation(
        df, 
        pollutant=c(parsed$ref_pol, parsed$qaq_pol, "residual"),
        cols=c("#fc4e54", "#4e54fc", "#14ba01"),
        name.pol=c(
            parsed$ref_ledgend_label,
            parsed$qaq_ledgend_label,
            paste("QuantAQ", "–", toupper(parsed$ref_column_label))
        ),
        ylab="CO (ppm)",
        main=paste(parsed$qaq_ledgend_label, "vs", parsed$ref_ledgend_label)
    )
    dev.off()
}

# COMPARE QUANTAQ TO AQS (BEACO2N BELOW)

merged_vaqs_df = read.csv("./intermediary_datasets/merge_quantaq_aqs.csv")
merged_vaqs_df$date = as.POSIXct(merged_vaqs_df$date, tz="UTC")

# dec_vaqs_df_all = merged_vaqs_df %>% filter(if_all(everything(), ~ !is.na(.x))) # Filter for times when AQS and DPW have data # nolint
vaqs_df_dpw = merged_vaqs_df %>% filter(!is.na(merged_vaqs_df$co_quantaq_dpw) & !is.na(merged_vaqs_df$co_aqs_cranston)) # Filter for times when AQS and DPW have data # nolint
vaqs_df_pha = merged_vaqs_df %>% filter(!is.na(merged_vaqs_df$co_quantaq_pha) & !is.na(merged_vaqs_df$co_aqs_cranston)) # Filter for times when AQS and PHA have data # nolint
vaqs_df_pema = merged_vaqs_df %>% filter(!is.na(merged_vaqs_df$co_quantaq_pema) & !is.na(merged_vaqs_df$co_aqs_cranston)) # Filter for times when AQS and PEMA have data # nolint

# Make residual columns for plotting
vaqs_df_dpw$residual = vaqs_df_dpw$co_quantaq_dpw - vaqs_df_dpw$co_aqs_cranston
vaqs_df_pha$residual = vaqs_df_pha$co_quantaq_pha - vaqs_df_pha$co_aqs_cranston
vaqs_df_pema$residual = vaqs_df_pema$co_quantaq_pema - vaqs_df_pema$co_aqs_cranston

# Write dataframes to CSVs for manual inspection
# write.csv(dec_vaqs_df_all, file="./co_comparisons/dec_vaqs_df_all.csv", row.names=FALSE)
write.csv(vaqs_df_dpw, file="./co_comparisons/dec_vaqs_df_dpw.csv", row.names=FALSE)
write.csv(vaqs_df_pha, file="./co_comparisons/dec_vaqs_df_pha.csv", row.names=FALSE)
write.csv(vaqs_df_pema, file="./co_comparisons/dec_vaqs_df_pema.csv", row.names=FALSE)


# Compare the CO values of Cranston and QuantAQ sites (disregarding temp and rh)
print_stats(ref=vaqs_df_dpw$co_aqs_cranston, qaq=vaqs_df_dpw$co_quantaq_dpw, filename="./co_comparisons/stats_vaqs_dpw.txt")
print_stats(ref=vaqs_df_pema$co_aqs_cranston, qaq=vaqs_df_pema$co_quantaq_pema, filename="./co_comparisons/stats_vaqs_pema.txt")
print_stats(ref=vaqs_df_pha$co_aqs_cranston, qaq=vaqs_df_pha$co_quantaq_pha, filename="./co_comparisons/stats_vaqs_pha.txt")

co_time_series(vaqs_df_dpw, df_parser(vaqs_df_dpw))
co_time_series(vaqs_df_pha, df_parser(vaqs_df_pha))
co_time_series(vaqs_df_pema, df_parser(vaqs_df_pema))

co_time_variation(vaqs_df_dpw, df_parser(vaqs_df_dpw))
co_time_variation(vaqs_df_pha, df_parser(vaqs_df_pha))
co_time_variation(vaqs_df_pema, df_parser(vaqs_df_pema))

# stop("Stop")
# # Make dataframes to plot QuantAQ/AQS pairings with AQS data going back to 11-2022 (roughly 2 yrs before QuantAQ sensors were set up).

# combined_qaq_df = combined_df %>%
#     select(date, co_aqs_cranston, co_quantaq_dpw, co_quantaq_pema, co_quantaq_pha) %>%
#     filter(date > as.POSIXct("2022-11-01", tx="UTC"))

# # Plot DPW versus extended AQS data
# png(
#     filename="./co_comparisons/tv_dpw_aqs.png",
#     width = 10*300, height=10*300, res=300
# )
# timeVariation(
#     combined_qaq_df, 
#     pollutant=c("co_aqs_cranston", "co_quantaq_dpw"),
#     name.pol=c("AQS @ Cranston (from 11/2022)", "QuantAQ @ DPW (from 11/2024)"),
#     ylab="CO (ppm)",
#     main="QuantAQ DPW vs. AQS Cranston (extended)"
# )
# dev.off()

# # Plot pha versus extended AQS data
# png(
#     filename="./co_comparisons/tv_pha_aqs.png",
#     width = 10*300, height=10*300, res=300
# )
# timeVariation(
#     combined_qaq_df, 
#     pollutant=c("co_aqs_cranston", "co_quantaq_pha"),
#     name.pol=c("AQS @ Cranston (from 11/2022)", "QuantAQ @ pha (from 11/2024)"),
#     ylab="CO (ppm)",
#     main="QuantAQ pha vs. AQS Cranston (extended)"
# )
# dev.off()

# # Plot pema versus extended AQS data
# png(
#     filename="./co_comparisons/tv_pema_aqs.png",
#     width = 10*300, height=10*300, res=300
# )
# timeVariation(
#     combined_qaq_df, 
#     pollutant=c("co_aqs_cranston", "co_quantaq_pema"),
#     name.pol=c("AQS @ Cranston (from 11/2022)", "QuantAQ @ pema (from 11/2024)"),
#     ylab="CO (ppm)",
#     main="QuantAQ pema vs. AQS Cranston (extended)"
# )
# dev.off()

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

# TODO: DEBUG
co_time_series(vbcn_df_dpw, df_parser(vbcn_df_dpw))
co_time_series(vbcn_df_pha, df_parser(vbcn_df_pha))
co_time_series(vbcn_df_pema, df_parser(vbcn_df_pema))

co_time_variation(vbcn_df_dpw, df_parser(vbcn_df_dpw))
co_time_variation(vbcn_df_pha, df_parser(vbcn_df_pha))
co_time_variation(vbcn_df_pema, df_parser(vbcn_df_pema))
stop("Called stop()")

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