# install.packages("openair")
# install.packages("dplyr")
# install.packages("Metrics")
library(openair)
library(dplyr)
library(clock)

merged_df = read.csv("./intermediary_datasets/merge_quantaq_aqs.csv")
merged_df$date = as.POSIXct(merged_df$date, tz="UTC")

dec_df_all = merged_df %>% filter(if_all(everything(), ~ !is.na(.x))) # Filter for times when AQS and DPW have data # nolint
dec_df_dpw = merged_df %>% filter(!is.na(merged_df$co_quantaq_dpw) & !is.na(merged_df$co_aqs_cranston)) # Filter for times when AQS and DPW have data # nolint
dec_df_pha = merged_df %>% filter(!is.na(merged_df$co_quantaq_pha) & !is.na(merged_df$co_aqs_cranston)) # Filter for times when AQS and PHA have data # nolint
dec_df_pema = merged_df %>% filter(!is.na(merged_df$co_quantaq_pema) & !is.na(merged_df$co_aqs_cranston)) # Filter for times when AQS and PEMA have data # nolint

# Write dataframes to CSVs for manual inspection
write.csv(dec_df_all, file="./co_comparisons/dec_df_all.csv", row.names=FALSE)
write.csv(dec_df_dpw, file="./co_comparisons/dec_df_dpw.csv", row.names=FALSE)
write.csv(dec_df_pha, file="./co_comparisons/dec_df_pha.csv", row.names=FALSE)
write.csv(dec_df_pema, file="./co_comparisons/dec_df_pema.csv", row.names=FALSE)

# Make residual columns for plotting
dec_df_dpw$residual = dec_df_dpw$co_quantaq_dpw - dec_df_dpw$co_aqs_cranston
dec_df_pha$residual = dec_df_pha$co_quantaq_pha - dec_df_pha$co_aqs_cranston
dec_df_pema$residual = dec_df_pema$co_quantaq_pema - dec_df_pema$co_aqs_cranston

print_stats = function(qaq, aqs, filename) {
    stopifnot(is.double(qaq), is.double(aqs), length(qaq)==length(aqs))
    stopifnot(is.character(filename))

    res = qaq - aqs
    MAE = mean(abs(res))
    RMSE = sqrt(mean(res^2))
    rcor = cor(qaq, aqs)
    Rsq = rcor^2 # NOTE: assuming this is a linear least-squares regression with a single independent variable
    ttest = t.test(qaq, aqs)

    # Capture printed outputs as text lines
    qaq_txt     =  capture.output(summary(qaq))
    aqs_txt     =  capture.output(summary(aqs))
    res_txt     =  capture.output(summary(res))
    ttest_txt   =  capture.output(print(ttest))
    
    sink(filename)
    on.exit(sink())
    # Combine everything for printing
    out = c(
        "QuantAQ Summary:",
        qaq_txt,
        "",
        "AQS Summary:",
        aqs_txt,
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
        ttest_txt
    )
    cat(paste0(out, collapse = "\n"), "\n")
}
print_stats(aqs=dec_df_dpw$co_aqs_cranston, qaq=dec_df_dpw$co_quantaq_dpw, filename="./text.txt")

# Plot time series with residual for each QuantAQ/AQS pairing.
png(
    filename="./co_comparisons/dpw_aqs.png",
    width = 10*300, height=10*300, res=300
)
timePlot(
    dec_df_dpw, 
    pollutant=c("co_aqs_cranston", "co_quantaq_dpw", "residual"),
    name.pol=c("AQS @ Cranston", "QuantAQ @ DPW", "QuantAQ - AQS"), 
    smooth=TRUE, 
    ci=TRUE,
    group=TRUE,
    ylim = c(-0.2,1.4),
    ylab="CO (ppm)",
    y.relation = "same",
    lty=1
) # nolint
dev.off()

png(
    filename="./co_comparisons/pha_aqs.png",
    width = 10*300, height=10*300, res=300
)
timePlot(
    dec_df_pha, 
    pollutant=c("co_aqs_cranston", "co_quantaq_pha", "residual"),
    name.pol=c("AQS @ Cranston", "QuantAQ @ PHA", "QuantAQ - AQS"), 
    smooth=TRUE, 
    ci=TRUE,
    group=TRUE,
    ylim = c(-0.2,1.4),
    ylab="CO (ppm)",
    y.relation = "same",
    lty=1
) # nolint
dev.off()

png(
    filename="./co_comparisons/pema_aqs.png",
    width = 10*300, height=10*300, res=300
)
timePlot(
    dec_df_pema, 
    pollutant=c("co_aqs_cranston", "co_quantaq_pema", "residual"),
    name.pol=c("AQS @ Cranston", "QuantAQ @ PEMA", "QuantAQ - AQS"), 
    smooth=TRUE, 
    ci=TRUE,
    group=TRUE,
    ylim = c(-0.2,1.4),
    ylab="CO (ppm)",
    y.relation = "same", 
    lty=1
) # nolint
dev.off()

png(
    filename="./co_comparisons/dpw_aqs_2023-_seasonal.png",
    width = 10*300, height=10*300, res=300
)
timeVariation(
    merged_df, 
    pollutant=c("co_aqs_cranston", "co_quantaq_dpw"),
    name.pol=c("AQS @ Cranston", "QuantAQ @ DPW"),
    ylab="CO (ppm)"
)
dev.off()