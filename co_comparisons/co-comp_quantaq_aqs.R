# install.packages("openair")
# install.packages("dplyr")
# install.packages("ggplot2")
library(openair)
library(dplyr)
library(clock)

merged_df = read.csv("./intermediary_datasets/merge_quantaq_aqs.csv")
merged_df$date = as.POSIXct(merged_df$date, tz="UTC")

dec_df_all = merged_df %>% filter(if_all(everything(), ~ !is.na(.x))) # All QuantAQ sites have readings with AQS # nolint
dec_df_dpw = merged_df %>% filter(!is.na(merged_df$co_quantaq_dpw) & !is.na(merged_df$co_aqs_cranston)) # DPW has readings with AQS # nolint
dec_df_pha = merged_df %>% filter(!is.na(merged_df$co_quantaq_pha) & !is.na(merged_df$co_aqs_cranston)) # PHA has readings with AQS # nolint
dec_df_pema = merged_df %>% filter(!is.na(merged_df$co_quantaq_pema) & !is.na(merged_df$co_aqs_cranston)) # PEMA has readings with AQS # nolint

# Write dataframes to CSVs for manual inspection
write.csv(dec_df_all, file="./co_comparisons/dec_df_all.csv", row.names=FALSE)
write.csv(dec_df_dpw, file="./co_comparisons/dec_df_dpw.csv", row.names=FALSE)
write.csv(dec_df_pha, file="./co_comparisons/dec_df_pha.csv", row.names=FALSE)
write.csv(dec_df_pema, file="./co_comparisons/dec_df_pema.csv", row.names=FALSE)

# Make residual columns for plotting
dec_df_dpw$residual = dec_df_dpw$co_quantaq_dpw - dec_df_dpw$co_aqs_cranston
dec_df_pha$residual = dec_df_pha$co_quantaq_pha - dec_df_pha$co_aqs_cranston
dec_df_pema$residual = dec_df_pema$co_quantaq_pema - dec_df_pema$co_aqs_cranston

# Plot time series with residual for each pairing.
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
    ylim = c(0,1.4),
    ylab="CO (ppm)",
    y.relation = "same"
) # nolint
dev.off()

png(
    filename="./co_comparisons/pha_aqs.png",
    width = 10*300, height=10*300, res=300
)
timePlot(
    dec_df_pha, 
    pollutant=c("co_aqs_cranston", "co_quantaq_pha", "residual"),
    name.pol=c("AQS @ Cranston", "QuantAQ @ pha", "QuantAQ - AQS"), 
    smooth=TRUE, 
    ci=TRUE,
    group=TRUE,
    ylim = c(0,1.4),
    ylab="CO (ppm)",
    y.relation = "same"
) # nolint
dev.off()

png(
    filename="./co_comparisons/pema_aqs.png",
    width = 10*300, height=10*300, res=300
)
timePlot(
    dec_df_pema, 
    pollutant=c("co_aqs_cranston", "co_quantaq_pema", "residual"),
    name.pol=c("AQS @ Cranston", "QuantAQ @ pema", "QuantAQ - AQS"), 
    smooth=TRUE, 
    ci=TRUE,
    group=TRUE,
    ylim = c(0,1.4),
    ylab="CO (ppm)",
    y.relation = "same"
) # nolint
dev.off()