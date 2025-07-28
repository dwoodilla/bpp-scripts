library(plyr, include.only="round_any")
library(tidyverse)
library(sgolay)
library(ggpmisc)
library(zoo)
library(checkmate)
library(moments) # skew, kurtosis
library(patchwork)

source("./src/data_arrangement_helpers.R")

deployment_correlation = function(
	plot_df,
	filepath,
	...
) {
	wide_plot_df = wide_plot_df_helper(plot_df)
	residual_plot = 
		ggplot(
			data=wide_plot_df,
			mapping=aes(
				x=ref,
				y=meas
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
		# stat_poly_line() + stat_poly_eq() +
		geom_abline(slope=1, intercept=0, color="red") +
		geom_point(alpha=0.05) + 
		labs(...)
	ggsave(plot=residual_plot, filename=filepath, width=8.5, height=11, units="in", dpi=300, create.dir=TRUE)
}

deployment_density = function(
	plot_df, 
	filepath,
	...
) {
	deployment_plot = 
		ggplot(
			data=plot_df %>% filter(plottype %in% c("meas","ref")),
			mapping=aes(
				x=value,
				fill=plottype
			)
		) + 
		facet_wrap(
			~ mos_into_deployment, 
			ncol=3, 
			scales="free_y",
			labeller = labeller(
				mos_into_deployment = function(x) {
					return(deployment_density_labeller(
						plot_df=plot_df, 
						dep_months=x
					))
				}
			)
		) +
		geom_density(alpha=0.5) +
		labs(...)
	ggsave(plot=deployment_plot, filename=filepath, width=8.5, height=11, units="in", dpi=300, create.dir=TRUE)
}

deployment_density_labeller = function(
	plot_df, 
	dep_months
) {
	label_stats = plot_df
	label_stats = label_stats %>% select(plottype, mos_into_deployment, date, value)
	label_stats = label_stats %>% 
		group_by(plottype, mos_into_deployment) %>%
		summarize(
			n=sum(!is.na(value)),
			mo_min = month(min(date), label=TRUE, abbr=TRUE),
			mo_max = month(max(date), label=TRUE, abbr=TRUE),
			yr_min = year(min(date)),
			yr_max = year(max(date))
		) %>% ungroup()

	label_stats = label_stats %>% 
		pivot_wider(
			names_from="plottype",
			values_from="n"
		) 
	
	label_stats = label_stats %>% 
		mutate(
			label= if_else(
				yr_min==yr_max,
				paste0(
					mos_into_deployment, ": ", 
					mo_min, "-",
					mo_max, " ", yr_max,
					" r=", ref, " m=", meas
				),
				paste0(
					mos_into_deployment, ": ", 
					mo_min, " ", yr_min, "-",
					mo_max, " ", yr_max,
					" r=", ref, " m=", meas
				)
			) 
		) 
	ret = t(setNames(label_stats$label, label_stats$mos_into_deployment))
	return(ret)
}

divergence_line_plot = function(
	pdf_stats,
	filepath,
	self_ref=FALSE,
	lims_y=c(-2,2),
	...
) {
	if (self_ref) pdf_stats = filter(.data=pdf_stats, mos_into_deployment>=12)
	# else pdf_stats = filter(.data=pdf_stats, !is.na(value))

	divergences_plot = 
		ggplot(
			data=pdf_stats %>% filter(
				statistic %in% c("KL","hellinger","euclidean")
			),
			mapping=aes(
				x=mos_into_deployment,
				y=value,
				shape=statistic,
				color=statistic
			)
		) +
		geom_line() + theme_bw() +
		coord_cartesian(ylim=lims_y) +
		scale_x_continuous(
			breaks = seq(
				from = min(pdf_stats$mos_into_deployment),
				to   = max(pdf_stats$mos_into_deployment),
				by   = 6
			)
		) +
		labs(...)

	ggsave(plot=divergences_plot, filename=filepath, width=5, height=5, units="in", dpi=300, create.dir=TRUE)
}