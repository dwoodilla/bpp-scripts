library(plyr, include.only="round_any")
library(tidyverse)
library(sgolay)
library(ggpmisc)
library(zoo)
library(checkmate)
library(moments) # skew, kurtosis
library(philentropy) # divergences
library(patchwork)

source("./src/data_arrangement_helpers.R")

#' Plot timeseries of measurement and reference sensor, faceted by season. 
#' @param `plot_df` must be in long format and contain both measurement and reference data.
timeseries_year_season = function(
	plot_df, 
	filepath, 
	...
) {
	ts_year_season = 
		ggplot(
			data=plot_df %>% filter(!is.na(value)), 
			mapping=aes(x=hours_into_sn, y=value, color=plottype) 
		) + 
		facet_grid(rows=vars(sn_year), cols=vars(season)) +
		geom_line(na.rm=TRUE) + theme_bw() +
		theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1)) +
		geom_hline(yintercept = 0, color="red") +
		labs(...)
	ggsave(plot=ts_year_season, filename=filepath, width=8.5, height=11, units="in", dpi=300, create.dir=TRUE)
}

timeseries_season = function(
	plot_df, 
	filepath, 
	...
) {
	ts_season = 
		ggplot(
			data=plot_df, 
			mapping=aes(
				x=hours_into_sn,
				y=value,
				color=sn_year, 
				linetype=sensor
			)
		) + 
		facet_wrap(~ season) +
		geom_line() + theme_bw() +
		theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1)) +
		geom_hline(yintercept = 0, color="red") +
		labs(...)
	ggsave(plot=ts_season, filename=filepath, width=8.5, height=11, units="in", dpi=300, create.dir=TRUE)
}

box_year_season = function(
	plot_df, 
	filepath, 
	...
) {
	box_year_season = 
		ggplot(
			data=plot_df %>% filter(!is.na(value)), 
			mapping=aes(x=sensor, y=value, color=plottype) 
		) + 
		facet_grid(rows=vars(sn_year), cols=vars(season)) +
		geom_boxplot(na.rm=TRUE) + theme_bw() +
		theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1)) +
		# geom_hline(yintercept = 0, color="red") +
		labs(...)
	ggsave(plot=box_year_season, filename=filepath, width=8.5, height=11, units="in", dpi=300, create.dir=TRUE)
}

box_season = function(
	plot_df, 
	filepath,
	...
) {
	box_sn = 
		ggplot(
			data=plot_df,
			mapping=aes(
				x=sn_year, 
				y=value,
				fill=sensor,
			)
		) + 
		facet_wrap(~ season) +
		geom_boxplot() + theme_bw() +
		theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1)) +
		labs(...)
	ggsave(plot=box_sn, filename=filepath, width=8.5, height=11, units="in", dpi=300, create.dir=TRUE)
}

violin_year_season = function(
	plot_df, 
	filepath, 
	...
) {
	violin_year_season = 
		ggplot(
			data=plot_df, 
			mapping=aes(x=sensor, y=value, fill=plottype) 
		) + 
		facet_grid(rows=vars(sn_year), cols=vars(season), scales="free_y") +
		geom_violin(na.rm=TRUE) + theme_bw() +
		theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1)) +
		labs(...)
	ggsave(plot=violin_year_season, filename=filepath, width=8.5, height=11, units="in", dpi=300, create.dir=TRUE)
}

violin_season = function(
	plot_df, 
	filepath, 
	...
) {
	violin_sn = 
		ggplot(
			data=plot_df, # Residual scaled poorly on plot
			mapping=aes(
				x=sn_year, 
				y=value,
				fill=sensor,
			)
		) + 
		facet_wrap(~ season, scales="free_y") +
		geom_violin() + theme_bw() +
		theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1)) +
		labs(...)
	ggsave(plot=violin_sn, filename=filepath, width=8.5, height=11, units="in", dpi=300, create.dir=TRUE)
}

timeseries_deployment_residual = function(
	plot_df,
	filepath,
	...
) {
	wide_plot_df = wide_plot_df_helper(plot_df)
	deployment_plot = 
		ggplot(
			data=wide_plot_df,
			mapping=aes(
				x=hrs_into_deployment_month,
				y=resid
			)
		) + 
		facet_wrap(
			~ mos_into_deployment, 
			ncol=3, 
			labeller = labeller(
				mos_into_deployment = function(x) {
					return(deployment_density_labeller(
						plot_df=plot_df, 
						dep_months=x
					))
				}
			)
		) +
		geom_line() + 
		labs(...)
	ggsave(plot=deployment_plot, filename=filepath, width=8.5, height=11, units="in", dpi=300, create.dir=TRUE)
}

violin_deployment_residual = function(
	plot_df, 
	filepath,
	...
) {
	deployment_plot = 
		ggplot(
			data=plot_df %>% filter(plottype=="resid"),
			mapping=aes(
				x=plottype,
				y=value
			)
		) + 
		facet_wrap(
			~ mos_into_deployment, 
			ncol=3, 
		) +
		geom_violin() + 
		labs(...)
	ggsave(plot=deployment_plot, filename=filepath, width=8.5, height=11, units="in", dpi=300, create.dir=TRUE)
}

deployment_corr_stat_lineplot = function(
	plot_df,
	filepath,
	...
) {
	wide_plot_df = wide_plot_df_helper(plot_df)
	deployment_mos_range = wide_plot_df %>% pull(mos_into_deployment) %>% range(na.rm=TRUE)
	deployment_mos_range = seq(deployment_mos_range[1], deployment_mos_range[2], by=1)
	month_stats = wide_plot_df %>%
		group_by(mos_into_deployment) %>%
		summarize(
			`R^2` = cor(meas, ref, use="pairwise.complete.obs")^2,
			RMSE = sqrt(mean((resid)^2, na.rm=TRUE)),
			MBE = mean(resid, na.rm=TRUE),
			Median_Bias_Error = median(resid, na.rm=TRUE),
			residual_kurtosis = kurtosis(resid, na.rm=TRUE),
			residual_skewness = skewness(resid, na.rm=TRUE)
		) %>%
		ungroup() %>%
		pivot_longer(
			cols = -mos_into_deployment,
			names_to = "statistic",
			values_to = "value"
		) %>%
		complete(
			mos_into_deployment=deployment_mos_range, 
			statistic=c("R^2","RMSE","MBE","Median_Bias_Error","residual_kurtosis","residual_skewness")
		)
	month_corr_stats_lineplot = 
		ggplot(
			data = month_stats %>% filter(!(statistic %in% c("residual_kurtosis","residual_skewness"))),
			mapping=aes(
				x=mos_into_deployment,
				y=value,
				color=statistic
			)
		) + 
		geom_line() +
		scale_x_continuous(
			breaks = seq(
				from = min(month_stats$mos_into_deployment),
				to   = max(month_stats$mos_into_deployment),
				by   = 6
			)
		) +
		scale_y_continuous(
			breaks = seq(from=0, to=1, by=0.05),
			limits=c(-1,1)
		) +
		labs(...)
	month_dist_stats_lineplot = 
		ggplot(
			data = month_stats %>% filter(statistic %in% c("residual_kurtosis","residual_skewness")),
			mapping=aes(
				x=mos_into_deployment,
				y=value,
				color=statistic
			)
		) + 
		geom_line() +
		scale_x_continuous(
			breaks = seq(
				from = min(month_stats$mos_into_deployment),
				to   = max(month_stats$mos_into_deployment),
				by   = 6
			)
		) +
		labs(...)	
	
	patch = month_corr_stats_lineplot / month_dist_stats_lineplot
	ggsave(plot=patch, filename=filepath, width=8.5, height=11, units="in", dpi=300, create.dir=TRUE)
}