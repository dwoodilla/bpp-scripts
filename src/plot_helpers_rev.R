library(plyr, include.only="round_any")
library(tidyverse)
library(sgolay)
library(ggpmisc)
library(zoo)
library(checkmate)
library(moments) # skew, kurtosis
library(philentropy) # divergences
library(patchwork)

# library(statip) # alternative hellinger

# Errors: rochambeaulib, zuccolo, smithhilllib, pema, rockspot
beaco2n_site_list = c(
	"myron","zuccolo","wecc","rocklib","silverlake","unitedway","cfs","pha","reservoir","ccri",
	"mtpleasant","carnevale","martialarts","southprovlib","ecubed","ricollege","blackstone","rochambeaulib","provcollege","prek",
	"smithhilllib","pema","rockspot","medschool","dpw"
)
quantaq_site_list = c("dpw","pema","pha")
aqs_site_list = c("myron","cranston")
beaco2n_berkeley_site_list = c(
	"rfs","dejean","albany","korematsu","madera","nystrom","peres","washington"
)


elongate_df = function(
	df,
	parameters,
	sensors,
	meteorology,
	read_from_cache = TRUE,
	cache_file
) {
	met_filter = FALSE
	if (!missing(meteorology)) {
		if (length(meteorology)!=2) {
			stop("Length of meteorology arg must be 2 if provided [ i.e., c(temp,rh) ]")
		}
		met_filter = TRUE
	}
	df_long = tibble()
    if (read_from_cache & file.exists(cache_file)) {
		if(missing(cache_file)) {stop("read_from_cache==TRUE but missing cache file.")}
        df_long = read_csv(cache_file)
    } else {
		dods = dates_of_deployment(df=df, parameters=parameters, sensors=sensors)
        df_long = df %>%
			filter(parameter %in% c(parameters, "temp","rh"), sensor %in% sensors) %>%
			mutate(
				sn_year = factor(if_else(month(date) == 12, year(date) + 1, year(date)), levels = 2000:2030),
				season = factor(season(date), levels = c("Winter", "Spring", "Summer", "Fall")),
				hours_into_sn = hours_into_season(date)
			) %>%
			left_join(y=dods, by = c("sensor", "location"), relationship="many-to-one") %>%
			mutate(
				mos_into_deployment = interval(deployment_start, date) %/% months(1),
				hrs_into_deployment = interval(deployment_start, date) %/% hours(1),
				hrs_into_deployment_month = interval(
					deployment_start %m+% months(mos_into_deployment),
					date,
					tz="UTC"
				) %/% hours(1)
			) %>% 
			filter(hrs_into_deployment >= 0)
		if (met_filter) {
			df_long = df_long %>%
				group_by(sensor, location) %>%
				pivot_wider(
					names_from="parameter",
					values_from="value"
				) %>%
				mutate(
					met_flag = (is.na(temp) | is.na(rh)) | (temp>meteorology[1] | rh>meteorology[2])
				) %>%
				mutate(
					across(
						.cols = any_of("co","pm25","pm01","pm10","pm"), # want to change this to refer to parameters argument at some point
						.fns = ~ if_else(met_flag, NA, .x)
					)
				)
		}
        write_csv(x=df_long, file=cache_file, col_names=TRUE, append=FALSE)
    }
	return(df_long)
}

arrange_plot_df = function(
	dataset, 
	parameter_arg,
	meas_sensor, 
	meas_location, 
	self_ref=FALSE, 
	ref_sensor, 
	ref_location
) {
	dataset = dataset %>% filter(parameter==parameter_arg)

	plot_data = filter(.data=dataset, sensor==meas_sensor, location==meas_location) 
	if (nrow(plot_data)==0) { stop(paste0("Filter arguments yield empty measurement dataset:\nmeas_sensor=",meas_sensor,"\nmeas_location=",meas_location)) }
	if (!self_ref) {
		if (missing(ref_sensor) | missing(ref_location)) { stop("Must have self_ref=TRUE or provide ref_sensor and ref_location.") }
		ref_data = filter(.data=dataset, sensor==ref_sensor, location==ref_location)
		if (nrow(ref_data)==0) { stop(paste0("Filter arguments yield empty reference dataset:\nref_sensor=",ref_sensor,"\nref_location=",ref_location)) }
	} else {
		first_year_ref = plot_data %>%
			filter(mos_into_deployment < 12) %>%
			mutate(month = month(date), day = day(date), hour = hour(date)) %>%
			group_by(month, day, hour) %>%
			slice_min(mos_into_deployment, with_ties = FALSE) %>%
			ungroup() %>%
			select(month, day, hour, value, date) %>%
			rename(value_ref = value, fy_ref_date = date)
		ref_data = plot_data %>%
			mutate(month = month(date), day = day(date), hour = hour(date)) %>%
			left_join(y=first_year_ref, by = c("month", "day", "hour"), relationship = "many-to-one") %>%
			mutate(value = value_ref) %>%
			select(!c(month, day, hour, fy_ref_date))
	}
	plot_data = left_join(x=plot_data, y=ref_data, by="date", suffix=c("_meas","_ref"))
	plot_data = mutate( # could use data %>% mutate() %>% rename_with(...)
		.data=plot_data, 
		value_resid = value_meas - value_ref,
		# Copy remaining data from measurement to residual.
		# mos_into_deployment may need to depend on whether both sensors have data or not.
		sensor_resid = "residual",
		sn_year_resid = sn_year_meas,
		season_resid = season_meas,
		hours_into_sn_resid = hours_into_sn_meas,
		mos_into_deployment_resid = mos_into_deployment_meas,
		hrs_into_deployment_resid = hrs_into_deployment_meas,
		hrs_into_deployment_month_resid = hrs_into_deployment_month_meas,
		mos_into_deployment_ref=mos_into_deployment_meas,
		hrs_into_deployment_ref=hrs_into_deployment_meas,
		hrs_into_deployment_month_ref = hrs_into_deployment_month_meas,
		season_ref=season_meas,
		hours_into_sn_ref=hours_into_sn_meas
	)
	plot_data = pivot_longer(
		data = plot_data,
		cols=matches("_(meas|ref|resid)$"),
		names_to=c(".value", "plottype"),
		names_pattern = "^(.*)_(ref|meas|resid)$"
	)
	if (any(is.na(plot_data$sn_year)) | any(is.na(plot_data$season))) {
		warning("Dropping rows of plot data with NA faceting variables.")
		plot_data = plot_data %>% filter(!is.na(sn_year), !is.na(season))
	}
	return(plot_data)
}



season = function(date_vec) {
	m = month(as.Date(date_vec))
	return(
		case_when(
			m %in% c(12,1,2) ~ "Winter",
			m %in% 3:5 ~ "Spring",
			m %in% 6:8 ~ "Summer",
			m %in% 9:11 ~ "Fall"
		)
	)
}

hours_into_season = function(date_vec) {
	d = as.POSIXct(date_vec)
	sn = season(date_vec)
	sn_start =
		case_when(
			sn=="Winter" ~ make_datetime(year=if_else(month(d)==12, year(d), year(d)-1), month=12, day=1),
			sn=="Spring" ~ make_datetime(year=year(d), month=3, day=1),
			sn=="Summer" ~ make_datetime(year=year(d), month=6, day=1),
			sn=="Fall" ~ make_datetime(year=year(d), month=9, day=1)
		)
	hours_from_sn_start = interval(start=sn_start, end=d, tz="UTC") %/% hours(1)
	return(hours_from_sn_start)
}

wide_plot_df_helper = function(df) {
	ret = df %>% pivot_wider(
		id_cols=c(date, mos_into_deployment, hrs_into_deployment, hrs_into_deployment_month, sn_year, season),
		names_from=plottype,
		values_from=value
	)
	return(ret)
}

dates_of_deployment = function(
	df,
	parameters,
	sensors
) {
	dates_of_deployment = df %>%
		filter(parameter %in% parameters, sensor %in% sensors) %>%
		arrange(date) %>%
		group_by(sensor, location) %>%
		filter(!is.na(value)) %>%
		slice(1) %>%
		ungroup() %>%
		select(sensor, location, deployment_start=date)
}

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
			Pearson_R = cor(meas, ref, use="pairwise.complete.obs"),
			mean_residual = mean(resid, na.rm=TRUE),
			median_residual = median(resid, na.rm=TRUE),
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
			statistic=c("R^2","Pearson_R","mean_residual","median_residual","residual_kurtosis","residual_skewness")
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
		labs(...)
	month_dist_stats_lineplot = 
		ggplot(
			data = month_stats %>% filter(statistic %in% c("residual_kurtosis","residual_skewness"), !is.na(value)),
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
	label_stats = label_stats %>% select(plottype, mos_into_deployment, date)
	label_stats = label_stats %>% 
		group_by(plottype, mos_into_deployment) %>%
		summarize(
			n=n(),
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
	label_stats = label_stats %>% 
		select(mos_into_deployment, label) %>%
		complete(
			mos_into_deployment = full_seq(mos_into_deployment, 1)
		) 

	if (any(is.na(label_stats %>% pull(label)))) {
		print(label_stats %>% filter(is.na(label)))
		print("bp")
	}
	label_stats = label_stats %>% 
		mutate(label = if_else(
			is.na(label),
			paste0(mos_into_deployment, ": Insufficient Information"),
			label
		))
	ret = t(setNames(label_stats$label, label_stats$mos_into_deployment))
	return(ret)
}

deployment_density_stats = function(
	plot_df,
	bin_width = 0.05,
	log_eps_cutoff = 0.383
) {
	eps_cutoff = .Machine$double.eps^log_eps_cutoff
	mos_range = plot_df %>% 
    	pull(mos_into_deployment) %>% 
    	range(na.rm = TRUE)
	all_mos = seq(mos_range[1], mos_range[2])

	stats_by_month = plot_df %>%
		filter(plottype %in% c("meas","ref")) %>%
		group_by(mos_into_deployment, plottype) %>%
		summarize(
			mean = mean(value, na.rm=TRUE), 
			sd = sd(value, na.rm=TRUE),
			kurtosis = kurtosis(value, na.rm=TRUE),
			skewness = skewness(value, na.rm=TRUE)
		) %>%
		pivot_wider(
			id_cols=mos_into_deployment,
			names_from=plottype,
			values_from=c(mean,sd,kurtosis,skewness)
		) %>%
		ungroup() %>%
		complete(mos_into_deployment=all_mos)
		
	divergence_by_month = plot_df %>%
		filter(plottype %in% c("meas", "ref")) %>%
		select(date, mos_into_deployment, plottype, value)

	divergence_by_month = divergence_by_month %>%
		group_by(mos_into_deployment, plottype) %>%
		select(-date) %>%
		nest(data = c(value)) %>%
		pivot_wider(names_from = plottype, values_from = data)

	divergence_by_month = mutate(.data=divergence_by_month,
		density = map2(meas, ref, function(meas_arg, ref_arg) {
			meas_arg$value[meas_arg$value<0] = 0
			ref_arg$value[ref_arg$value<0] = 0

			meas_len = length(na.omit(meas_arg$value))
			ref_len = length(na.omit(ref_arg$value))

			if (meas_len<2 | ref_len<2) {
				warning("Measurement or Reference has <2 observations; returning NA")
				return(NA)
			}

			est_n = min(c(meas_len, ref_len))
			values = c(meas_arg$value, ref_arg$value)
			if (is.na(max(min(values), 0)) | is.na(max(values))) {
				warning("min(values) or max(values) is NA, returning NA")
				return(NA)
			}
			est_grid = seq(
				round_any(max(min(values), 0), accuracy=bin_width, f=floor),
				round_any(max(values), accuracy=bin_width, f=ceiling),
				by=bin_width
			)
			# est_grid = est_grid[est_grid>=0]

			if (length(na.omit(est_grid))<=1) {stop("Histogram est_grid has length<=1")}
			h_meas = hist(meas_arg$value, breaks=est_grid, right=FALSE, plot=FALSE)
			h_ref  = hist(ref_arg$value,  breaks=est_grid, right=FALSE, plot=FALSE)
			return(tibble(
				est_grid=est_grid[-length(est_grid)],
				meas=h_meas$density,
				ref=h_ref$density
			))
		}))
	divergence_by_month = mutate(
		.data = divergence_by_month,
		KL = map_dbl(density, ~ {
			if (any(is.na(.x))) {return(NA)}
			P=.x$ref*bin_width
			Q=.x$meas*bin_width
			P[abs(P)<eps_cutoff]=eps_cutoff
			Q[abs(Q)<eps_cutoff]=eps_cutoff

			kl = sum((P)*log((P)/(Q)))
			return(kl)
		}))
	divergence_by_month = mutate(
		.data = divergence_by_month,
		hellinger = map_dbl(density, ~ {
			if (any(is.na(.x))) {return(NA)}
			P=.x$ref*bin_width
			Q=.x$meas*bin_width
			P[abs(P)<eps_cutoff]=0
			Q[abs(Q)<eps_cutoff]=0

			hellinger = (1/sqrt(2))*norm(sqrt(P)-sqrt(Q), type="2")
			return(hellinger)
		}))
	divergence_by_month = mutate(
		.data = divergence_by_month,
		euclidean = map_dbl(density, ~ {
			if (any(is.na(.x))) {return(NA)}
			P=.x$ref*bin_width
			Q=.x$meas*bin_width
			P[abs(P)<eps_cutoff]=0
			Q[abs(Q)<eps_cutoff]=0

			euclidean = sqrt(sum((P-Q)^2))
			return(euclidean)
		}))
	divergence_by_month = mutate(
		.data = divergence_by_month,
		pdf_resolution = map_dbl(
			density, 
			~{ if (length(nrow(.x))==0) {return(0)} else {return(nrow(.x))} }
		)
	)
	divergence_by_month = rename(.data=divergence_by_month, obs_meas=meas, obs_ref=ref, pdf=density) %>%
		ungroup() %>%
		complete(mos_into_deployment=all_mos)

	stats_by_month = 
		stats_by_month %>% 
		left_join(
			y=divergence_by_month %>% select(-c(obs_meas,obs_ref,pdf)), 
			by=join_by(mos_into_deployment),
			relationship="one-to-one"
		) %>%
		pivot_longer(
			cols=matches("^(mean|sd|kurtosis|skewness|KL|hellinger|euclidean)(?:_(meas|ref))?$"),
			names_pattern="^(mean|sd|kurtosis|skewness|KL|hellinger|euclidean)(?:_(meas|ref))?$",
			names_to=c("statistic","plottype"),
			values_to="value"
		) %>%
		mutate(plottype=na_if(plottype, ""))

	divergence_by_month = select(
		.data=divergence_by_month,
		c(mos_into_deployment, obs_meas, obs_ref, pdf, pdf_resolution)
	)
	return(list(stats_by_month, divergence_by_month))
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