library(tidyverse)
library(openair)
# library(stringr) # Used for df_parser function

unlink("./plots/*.png", expand=TRUE) # Hit Ctrl-Enter on this line to clear temporary files.

combined_df = read.csv("./clean_data/all_co_temp_rh_2022.csv")
combined_df$date = as.POSIXct(combined_df$date, tz="UTC")
combined_df$date = with_tz(combined_df$date, tzone="America/New_York") # Allows OpenAir to account for EST/EDT

# co_stats = function(qaq, ref, filename, qaq_site, ref_site, ref_type) {
#     stopifnot(is.double(qaq), is.double(ref), length(qaq)==length(ref), is.character(filename))
#     stopifnot(is.character(qaq_site), is.character(ref_site), is.character(ref_type))
#
#   plt = ggplot() +
#     geom_boxplot(aes(x = paste0("QuantAQ @ ", qaq_site), y = qaq), fill = "skyblue", width = 0.6) +
#     geom_boxplot(aes(x = paste(ref_type, "@", ref_site), y = ref), fill = "salmon", width = 0.6) +
#     xlab("Site") +
#     ylab("CO (ppm)") +
#     scale_y_continuous(
#         limits=c(0, 1.2),
#         breaks=waiver(),
#         minor_breaks=NULL
#     ) +
#     ggtitle(paste0("Summary statistics for QuantAQ @ ", qaq_site, ", ", ref_type, " @ ", ref_site)) +
#     theme_bw()
#   ggsave(filename=filename, plot=plt, width=6, height=6, dpi=300, units="in")
# }

# co_distribution = function(qaq, ref, filename, qaq_site, ref_site, ref_type) {
#     plt = ggplot() +
#     geom_histogram(aes(x = qaq, y = after_stat(count/sum(count))),
#                  fill = "skyblue", binwidth = 0.01) +
#     geom_histogram(aes(x = ref, y = after_stat(count/sum(count))),
#                  fill = "salmon", binwidth = 0.01) +
#     xlab("CO (ppm)") +
#     ylab("Frequency") +
#     theme_bw()
#     ggsave(filename=filename, plot=plt,  width=6, height=6, dpi=300, units="in")
# }

co_time_series = function(df, qaq_pol, qaq_display_label, ref_pol, ref_display_label, ref_type, filename) {
  
  png(
    filename = filename,
    width = 10 * 300, height = 10 * 300, res = 300
  )
  timePlot(
    df,
    pollutant = c(ref_pol, qaq_pol, "residual"),
    cols=c("salmon", "skyblue", "#14ba01"),
    name.pol = c(
        ref_display_label,
        qaq_display_label, 
        paste("QuantAQ", "–", if(ref_type=="aqs") "AQS" else "BEACO2N")
    ),
    main = paste(qaq_display_label, "vs", ref_display_label),
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
    ),
    avg.time="2 hour" # Looking at plots, this cannot possibly be 2 hours... is way too smooth.
  )
  dev.off()
}

co_time_variation = function(df, qaq_pol, qaq_display_label, ref_pol, ref_display_label, ref_type, filename) {
    png(
        filename=filename,
        width = 10*300, height=10*300, res=300
    )
    timeVariation(
        df, 
        pollutant=c(ref_pol, qaq_pol, "residual"),
        cols=c("salmon", "skyblue", "green4"),
        name.pol=c(
            ref_display_label,
            qaq_display_label,
            paste("QuantAQ", "–", if(ref_type=="aqs") "AQS" else "BEACO2N")
        ),
        ylab="CO (ppm)",
        main=paste(qaq_display_label, "vs", ref_display_label),
        period="2 hour"
    )
    dev.off()
}

# COMPARE QUANTAQ TO AQS (BEACO2N BELOW)

merged_vaqs_df = read.csv("./clean_data/aqs_qaq-notallna.csv")
merged_vaqs_df$date = as.POSIXct(merged_vaqs_df$date, tz="UTC")

vaqs_df_dpw = merged_vaqs_df %>% filter(!is.na(merged_vaqs_df$co_quantaq_dpw) & !is.na(merged_vaqs_df$co_aqs_cranston)) # Filter for times when AQS and DPW have data 
vaqs_df_pha = merged_vaqs_df %>% filter(!is.na(merged_vaqs_df$co_quantaq_pha) & !is.na(merged_vaqs_df$co_aqs_cranston)) # Filter for times when AQS and PHA have data 
vaqs_df_pema = merged_vaqs_df %>% filter(!is.na(merged_vaqs_df$co_quantaq_pema) & !is.na(merged_vaqs_df$co_aqs_cranston)) # Filter for times when AQS and PEMA have data 

# Make residual columns for plotting
vaqs_df_dpw$residual = vaqs_df_dpw$co_quantaq_dpw - vaqs_df_dpw$co_aqs_cranston
vaqs_df_pha$residual = vaqs_df_pha$co_quantaq_pha - vaqs_df_pha$co_aqs_cranston
vaqs_df_pema$residual = vaqs_df_pema$co_quantaq_pema - vaqs_df_pema$co_aqs_cranston

co_time_series(
    df=vaqs_df_dpw, 
    qaq_pol = "co_quantaq_dpw", 
    ref_pol = "co_aqs_cranston",
    qaq_display_label = "QuantAQ @ DPW",
    ref_display_label = "AQS @ Cranston",
    ref_type = "aqs",
    filename = "./plots/time_series_aqs-v-dpw.png"
)
co_time_series(
    df=vaqs_df_pha, 
    qaq_pol = "co_quantaq_pha", 
    ref_pol = "co_aqs_cranston",
    qaq_display_label = "QuantAQ @ PHA",
    ref_display_label = "AQS @ Cranston",
    ref_type = "aqs",
    filename = "./plots/time_series_aqs-v-pha.png"
)
co_time_series(
    df=vaqs_df_pema, 
    qaq_pol = "co_quantaq_pema", 
    ref_pol = "co_aqs_cranston",
    qaq_display_label = "QuantAQ @ PEMA",
    ref_display_label = "AQS @ Cranston",
    ref_type = "aqs",
    filename = "./plots/time_series_aqs-v-pema.png"
)

co_time_variation(
    df=vaqs_df_dpw, 
    qaq_pol = "co_quantaq_dpw", 
    ref_pol = "co_aqs_cranston",
    qaq_display_label = "QuantAQ @ DPW",
    ref_display_label = "AQS @ Cranston",
    ref_type = "aqs",
    filename = "./plots/time_var_aqs-v-dpw.png"
)
co_time_variation(
    df=vaqs_df_pha, 
    qaq_pol = "co_quantaq_pha", 
    ref_pol = "co_aqs_cranston",
    qaq_display_label = "QuantAQ @ PHA",
    ref_display_label = "AQS @ Cranston",
    ref_type = "aqs",
    filename = "./plots/time_var_aqs-v-pha.png"
)
co_time_variation(
    df=vaqs_df_pema, 
    qaq_pol = "co_quantaq_pema", 
    ref_pol = "co_aqs_cranston",
    qaq_display_label = "QuantAQ @ PEMA",
    ref_display_label = "AQS @ Cranston",
    ref_type = "aqs",
    filename = "./plots/time_var_aqs-v-pema.png"
)

# COMPARE QUANTAQ TO BEACO2N (AQS ABOVE)

merged_vbcn_df = combined_df %>% select(date, co_quantaq_dpw, co_quantaq_pema, co_quantaq_pha, co_beaco2n_dpw, co_beaco2n_pema, co_beaco2n_pha)

vbcn_df_dpw = merged_vbcn_df %>% filter(!is.na(merged_vbcn_df$co_quantaq_dpw) & !is.na(merged_vbcn_df$co_beaco2n_dpw)) # Filter for times when qaq and bcn at DPW have data # nolint
vbcn_df_pha = merged_vbcn_df %>% filter(!is.na(merged_vbcn_df$co_quantaq_pha) & !is.na(merged_vbcn_df$co_beaco2n_pha)) # Filter for times when qaq and bcn at PHA have data # nolint
vbcn_df_pema = merged_vbcn_df %>% filter(!is.na(merged_vbcn_df$co_quantaq_pema) & !is.na(merged_vbcn_df$co_beaco2n_pema)) # Filter for times when qaq and bcn at PEMA have data # nolint

# Make residual columns for plotting
vbcn_df_pha$residual = vbcn_df_pha$co_quantaq_pha - vbcn_df_pha$co_beaco2n_pha
vbcn_df_pema$residual = vbcn_df_pema$co_quantaq_pema - vbcn_df_pha$co_beaco2n_pema
vbcn_df_dpw$residual = vbcn_df_dpw$co_quantaq_dpw - vbcn_df_pha$co_beaco2n_dpw

# Compare the CO values of colocated bcn/qaq sites (disregarding temp and rh)
co_stats(ref=vbcn_df_dpw$co_beaco2n_dpw, qaq=vbcn_df_dpw$co_quantaq_dpw, filename="./plots/stats_vbcn_dpw.png", qaq_site="dpw", ref_site="dpw", ref_type="bcn")
co_stats(ref=vbcn_df_pema$co_beaco2n_pema, qaq=vbcn_df_pema$co_quantaq_pema, filename="./plots/stats_vbcn_pema.png", qaq_site="pema", ref_site="pema", ref_type="bcn")
co_stats(ref=vbcn_df_pha$co_beaco2n_pha, qaq=vbcn_df_pha$co_quantaq_pha, filename="./plots/stats_vbcn_pha.png", qaq_site="pha", ref_site="pha", ref_type="bcn")

co_time_series(
    vbcn_df_dpw, 
    qaq_pol="co_quantaq_dpw", 
    ref_pol="co_beaco2n_dpw", 
    qaq_display_label="QuantAQ @ DPW",
    ref_display_label = "BEACO2N @ DPW",
    ref_type = "bcn",
    filename="./plots/time_series_bcn_dpw.png"
)
co_time_series(
    vbcn_df_pha, 
    qaq_pol="co_quantaq_pha", 
    ref_pol="co_beaco2n_pha", 
    qaq_display_label="QuantAQ @ PHA",
    ref_display_label = "BEACO2N @ PHA",
    ref_type = "bcn",
    filename="./plots/time_series_bcn_pha.png"
)
co_time_series(
    vbcn_df_pema, 
    qaq_pol="co_quantaq_pema", 
    ref_pol="co_beaco2n_pema", 
    qaq_display_label="QuantAQ @ PEMA",
    ref_display_label = "BEACO2N @ PEMA",
    ref_type = "bcn",
    filename="./plots/time_series_bcn_pema.png"
)
co_time_variation(
    vbcn_df_dpw, 
    qaq_pol="co_quantaq_dpw", 
    ref_pol="co_beaco2n_dpw", 
    qaq_display_label="QuantAQ @ DPW",
    ref_display_label = "BEACO2N @ DPW",
    ref_type = "bcn",
    filename="./plots/time_var_bcn_dpw.png" 
)
co_time_variation(
    vbcn_df_pha, 
    qaq_pol="co_quantaq_pha", 
    ref_pol="co_beaco2n_pha", 
    qaq_display_label="QuantAQ @ PHA",
    ref_display_label = "BEACO2N @ PHA",
    ref_type = "bcn",
    filename="./plots/time_var_bcn_pha.png" 
)
co_time_variation(
    vbcn_df_pema, 
    qaq_pol="co_quantaq_pema", 
    ref_pol="co_beaco2n_pema", 
    qaq_display_label="QuantAQ @ PEMA",
    ref_display_label = "BEACO2N @ PEMA",
    ref_type = "bcn",
    filename="./plots/time_var_bcn_pema.png" 
)

# BEACO2N DRIFT
# New df with only beaco2n CO and aqs_cranston cols, dropping rows that do not have at least one non-NA column other than date.
bcn_aqs_df = combined_df %>% 
    select(matches("^(?:co_beaco2n_(?:dpw|pema|pha)|co_aqs_(?:cranston|myron)|date)$")) %>% 
    filter(if_any(-date, ~ !is.na(.)))
# write.csv(bcn_aqs_df, "./test.csv")
# png(
#     filename="./plots/bcn_drift.png",
#     width = 10 * 300, height = 10 * 300, res = 300
# )
# bcn_trends = smoothTrend(
#     bcn_aqs_df, 
#     pollutant=c("co_beaco2n_dpw","co_beaco2n_pha","co_beaco2n_pema"),
#     pol.names=c("BEACO2N at DPW","BEACO2N at PHA","BEACO2N at PEMA"),
#     cols=c("skyblue","salmon","green4"),
#     type="year"
# )$data
# bcn_trends_long = as.tibble(bcn_trends) %>% pivot_longer(
#     cols=c("co_beaco2n_dpw", "co_beaco2n_pha", "co_beaco2n_pema"),
#     names_to = "variable",
#     values_to = "concentration"
# )
# plt = ggplot(bcn_trends_long, aes(x = date, y = concentration, color = variable)) +
#   geom_point() +
#   geom_line() +                   # optional if you want lines connecting points
#   labs(x = "Date", y = "Concentration",
#        color = "Variable") +
#   theme_minimal()
# ggsave(filename="./plots/bcn_drift.png", plot=plt, width=6, height=6, units="in", dpi=300)

# # Plot BEACO2N PEMA vs both AQS sensors for as long as there is BEACO2N data.
# png(
#     filename="./plots/drift_bcn_pema.png",
#     width = 10 * 300, height = 10 * 300, res = 300
# )
# timePlot(
#     bcn_aqs_df,
#     pollutant=c("co_beaco2n_pema","co_aqs_myron","co_aqs_cranston"),
#     pol.names=c("BEACO2N at PEMA", "AQS at Myron", "AQS at Cranston"),
#     main = paste("BEACO2N Drift"),
#     smooth = TRUE,
#     ci = TRUE,
#     group = TRUE,
#     stack=TRUE,
#     ylim = c(-0.2, 1.4),
#     ylab = "CO (ppm)",
#     y.relation = "same",
#     lty = 1
#     # scales = list(
#     #   y = list(
#     #     at = seq(-0.2, 1.4, by = 0.2),
#     #     labels = seq(-0.2, 1.4, by = 0.2),
#     #     tck = c(1, 0)
#     #   )
#     # )
# )
# dev.off()

# Comparing QuantAQ with each other:
quantaq_df = combined_df %>% 
    select(matches("^(?:co_quantaq_(?:dpw|pema|pha)|date)$")) %>%
    filter(if_all(-date, ~ !is.na(.))) # filter for times when all are operating

# x_lim = range(quantaq_df[, c("co_quantaq_dpw", "co_quantaq_pha", "co_quantaq_pema")])
# y_lim = c(0,0.20)

# dpw_plt = ggplot(quantaq_df, aes(co_quantaq_dpw)) +
#     geom_histogram(,
#         color = "black", fill="steelblue", binwidth = 0.1, alpha=0.6) +
#     scale_x_continuous(limits=c(0,1.6), breaks=seq(0,1.6,by=0.1)) +
#     theme(axis.text.x = element_text(angle = 40, hjust = 1)) +
#     xlab("CO (ppm)") +
#     ylab("Frequency (%)") +
#     ylim(y_lim) +
#     ggtitle("DPW")

# pha_plt = ggplot() +
#     geom_histogram(aes(x = quantaq_df$co_quantaq_pha, y = after_stat(count/sum(count))),
#         color = "black", fill="steelblue", binwidth=0.1, alpha=0.6) +
#     scale_x_continuous(limits=c(0,1.6), breaks=seq(0,1.6,by=0.1)) +
#     theme(axis.text.x = element_text(angle = 40, hjust = 1)) +
#     xlab("CO (ppm)") +
#     ylab("Frequency (%)") +
#     ylim(y_lim) +
#     ggtitle("PHA")

# pema_plt = ggplot() +
#     geom_histogram(aes(x = quantaq_df$co_quantaq_pema, y = after_stat(count/sum(count))),
#         color = "black", fill="steelblue", binwidth=0.1, alpha=0.6) +
#     scale_x_continuous(limits=c(0,1.6), breaks=seq(0,1.6,by=0.1)) +
#     theme(axis.text.x = element_text(angle = 40, hjust = 1)) +
#     xlab("CO (ppm)") +
#     ylab("Frequency (%)") +
#     ylim(y_lim) +
#     ggtitle("PEMA")

# ggsave(plot=wrap_plots(dpw_plt, pha_plt, pema_plt, nrow=1, guides="collect"), filename="./plots/ts_qaq_comp.png", width=11, height=8.5, units="in")

hist = ggplot() +
    geom_histogram(aes(x = quantaq_df$co_quantaq_dpw, y = after_stat(count/sum(count))),
                 fill = "skyblue", bins = 30, alpha=0.6) +
    geom_histogram(aes(x = quantaq_df$co_quantaq_pha, y = after_stat(count/sum(count))),
                 fill = "salmon", bins = 30, alpha=0.6) +
    geom_histogram(aes(x = quantaq_df$co_quantaq_pema, y = after_stat(count/sum(count))),
                 fill = "green4", bins = 30, alpha=0.6) +
    xlab("CO (ppm)") +
    ylab("Frequency") +
    theme_bw()
ggsave(filename="./plots/qaq_hist.png", plot=hist,  width=6, height=6, dpi=300, units="in")

box = ggplot() +
    geom_boxplot(aes(x = "QuantAQ @ DPW", y = quantaq_df$co_quantaq_dpw), fill = "skyblue", width = 0.6) +
    geom_boxplot(aes(x = "QuantAQ @ PHA", y = quantaq_df$co_quantaq_pha), fill = "salmon", width = 0.6) +
    geom_boxplot(aes(x = "QuantAQ @ PEMA", y = quantaq_df$co_quantaq_pema), fill = "green4", width = 0.6) +
    xlab("Site") +
    ylab("CO (ppm)") +
    scale_y_continuous(
        limits=c(0.5, 1.2),
        breaks=waiver(),
        minor_breaks=NULL
    ) +
    ggtitle("QuantAQ Site comparison") +
    theme_bw()
ggsave(filename="./plots/qaq_box.png", plot=box, width=6, height=6, dpi=300, units="in")



png(
    filename="./plots/qaq_ts.png",
    width = 10 * 300, height = 10 * 300, res = 300
)
timePlot(
    quantaq_df,
    pollutant=c("co_quantaq_dpw","co_quantaq_pha","co_quantaq_pema"),
    pol.names=c("DPW", "PHA", "PEMA"),
    main = "QuantAQ Network Comparison",
    smooth = TRUE,
    ci = TRUE,
    group = TRUE,
    # stack=TRUE,
    # ylim = c(-0.2, 1.4),
    ylab = "CO (ppm)",
    y.relation = "same",
    lty = 1
    # scales = list(
    #   y = list(
    #     at = seq(-0.2, 1.4, by = 0.2),
    #     labels = seq(-0.2, 1.4, by = 0.2),
    #     tck = c(1, 0)
    #   )
    # )
)
dev.off()

# png(
#     filename="./plots/qaq_trend.png",
#     width = 10 * 300, height = 10 * 300, res = 300
# )
# dpw_trend = smoothTrend(
#     quantaq_df, 
#     pollutant=c("co_quantaq_dpw","co_quantaq_pha","co_quantaq_pema"),
#     plot=TRUE
# )
