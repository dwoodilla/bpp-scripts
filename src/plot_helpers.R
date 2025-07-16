library(tidyverse)
library(sgolay)
library(ggpmisc)
library(zoo)
library(checkmate)
library(moments) # skew, kurtosis
library(philentropy) # divergences

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

arrange_season_data = function(
	dataset, 
	noise_filter, 
	meas_sensor, 
	meas_location, 
	self_ref=FALSE, 
	ref_sensor, 
	ref_location
) {
	plot_data = dataset %>% filter(sensor==meas_sensor, filter==noise_filter, location==meas_location)
	if (!self_ref) {
		if (missing(ref_sensor) | missing(ref_location)) {
			stop("Must have self_ref=TRUE or provide ref_sensor and ref_location.")
		}
		ref_data = dataset %>% filter(sensor==ref_sensor, filter==noise_filter, location==ref_location)
	} else {
		first_year_ref = plot_data %>%
			filter(mos_into_deployment <= 12) %>%
			mutate(
				month = month(date),
				day = day(date),
				hour = hour(date)
			) %>%
			group_by(month, day, hour) %>%
			slice_min(mos_into_deployment, with_ties = FALSE) %>%
			ungroup() %>%
			select(month, day, hour, value, date) %>%
			rename(value_ref = value, fy_ref_date = date)
		ref_data = plot_data %>%
			mutate(
				month=month(date),
				day=day(date),
				hour=hour(date)
			) %>%
			left_join(first_year_ref, by=c("month","day","hour"), relationship="many-to-one") %>%
			mutate(value=value_ref) %>%
			select(date, everything(), -month, -day, -hour, -fy_ref_date)
	}
	plot_data = plot_data %>%
		left_join(y=ref_data, by="date", suffix=c("_meas","_ref")) %>%
		mutate(
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
			filter_resid = filter_meas,
			mos_into_deployment_ref=mos_into_deployment_meas,
			hrs_into_deployment_ref=hrs_into_deployment_meas,
			hrs_into_deployment_month_ref = hrs_into_deployment_month_meas,
			season_ref=season_meas,
			hours_into_sn_ref=hours_into_sn_meas
		) %>%
		pivot_longer(
			cols=matches("_(meas|ref|resid)$"),
			names_to=c(".value", "plottype"),
			names_pattern = "^(.*)_(ref|meas|resid)$"
		)
	if (any(is.na(plot_data$sn_year)) | any(is.na(plot_data$season))) {
		warning("Dropping rows of plot data with NA faceting variables.")
		plot_data = plot_data %>% filter(!is.na(sn_year), !is.na(season))
	}
	return(plot_data %>% filter(!is.na(value)))
}

arrange_deployment_data = function(
	dataset, 
	noise_filter, 
	meas_sensor, 
	meas_location, 
	self_ref=FALSE, 
	ref_sensor, 
	ref_location
) {
	ret = arrange_season_data(dataset, noise_filter, meas_sensor, meas_location, self_ref, ref_sensor, ref_location) %>%
		pivot_wider(
			id_cols=c(date, mos_into_deployment, hrs_into_deployment, hrs_into_deployment_month, sn_year, season),
			names_from=plottype,
			values_from=value
		)
	return(ret)
}

elongate_df = function(
	df,
	parameter_arg,
	sensors,
	avg_window = 24,
	savgol_len = 25,
	dates_of_deployment
) {
	meas_sensor = sensors[1]
	ref_sensor = sensors[2]

	ret = df %>%
		filter(parameter == parameter_arg, sensor %in% sensors) %>%
		pivot_wider(
			id_cols = c(date, location),
			names_from = sensor,
			values_from = value,
			names_prefix = "original_"
		) %>%
		group_by(location) %>%
		mutate(
			!!paste0("rolling_", meas_sensor) := rollmean(get(paste0("original_", meas_sensor)), k = avg_window, align = "center", fill = NA),
			!!paste0("rolling_", ref_sensor) := rollmean(get(paste0("original_", ref_sensor)), k = avg_window, align = "center", fill = NA),
			!!paste0("savgol_", meas_sensor) := sgolayfilt(get(paste0("original_", meas_sensor)), n = savgol_len, p = 4),
			!!paste0("savgol_", ref_sensor) := sgolayfilt(get(paste0("original_", ref_sensor)), n = savgol_len, p = 4)
		) %>%
		ungroup() %>%
		mutate(
			original_res = .data[[paste0("original_", meas_sensor)]] - .data[[paste0("original_", ref_sensor)]],
			rolling_res  = .data[[paste0("rolling_",  meas_sensor)]] - .data[[paste0("rolling_",  ref_sensor)]],
			savgol_res   = .data[[paste0("savgol_",   meas_sensor)]] - .data[[paste0("savgol_",   ref_sensor)]]
		) %>%
		pivot_longer(
			cols = -c(date, location),
			names_to = c("filter", "sensor"),
			names_pattern = "(original|rolling|savgol)_?(.*)",
			values_to = "value"
		) %>%
		semi_join(
			y = df %>% select(sensor, location) %>% distinct(),
			by = c("sensor", "location")
		) %>%
		mutate(
			sn_year = factor(if_else(month(date) == 12, year(date) + 1, year(date)), levels = 2018:2030),
			season = factor(season(date), levels = c("Winter", "Spring", "Summer", "Fall")),
			hours_into_sn = hours_into_season(date)
		) %>%
		filter(!is.na(value)) %>%
		left_join(dates_of_deployment, by = c("sensor", "location")) %>%
		mutate(
			mos_into_deployment = interval(deployment_start, date) %/% months(1),
			hrs_into_deployment = interval(deployment_start, date) %/% hours(1),
			hrs_into_deployment_month = interval(
				deployment_start %m+% months(mos_into_deployment),
				date,
				tz="UTC"
			) %/% hours(1)
		)
	return(ret)
}

dates_of_deployment = function(
	df,
	parameter_arg,
	sensors
) {
	dates_of_deployment = df %>%
		filter(parameter == parameter_arg, sensor %in% sensors) %>%
		arrange(date) %>%
		group_by(sensor, location) %>%
		filter(!is.na(value)) %>%
		slice(1) %>%
		ungroup() %>%
		select(sensor, location, deployment_start=date)
}

#' Plot timeseries of measurement and reference sensor, faceted by season. 
#' @param `season_data` must be in long format and contain both measurement and reference data.
timeseries_year_season = function(
	season_data, 
	filepath, 
	...
) {
	ts_year_season = 
		ggplot(
			data=season_data %>% filter(!is.na(value)), 
			mapping=aes(x=hours_into_sn, y=value, color=plottype) 
		) + 
		facet_grid(rows=vars(sn_year), cols=vars(season)) +
		geom_line(na.rm=TRUE) + theme_bw() +
		theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1)) +
		geom_hline(yintercept = 0, color="red") +
		labs(...)
	ggsave(plot=ts_year_season, filename=filepath, width=8.5, height=11, units="in", dpi=300)
}

timeseries_season = function(
	season_data, 
	filepath, 
	...
) {
	ts_season = 
		ggplot(
			data=season_data, 
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
	ggsave(plot=ts_season, filename=filepath, width=8.5, height=11, units="in", dpi=300)
}

box_year_season = function(
	season_data, 
	filepath, 
	...
) {
	box_year_season = 
		ggplot(
			data=season_data %>% filter(!is.na(value)), 
			mapping=aes(x=sensor, y=value, color=plottype) 
		) + 
		facet_grid(rows=vars(sn_year), cols=vars(season)) +
		geom_boxplot(na.rm=TRUE) + theme_bw() +
		theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1)) +
		# geom_hline(yintercept = 0, color="red") +
		labs(...)
	ggsave(plot=box_year_season, filename=filepath, width=8.5, height=11, units="in", dpi=300)
}

box_season = function(
	season_data, 
	filepath,
	...
) {
	box_sn = 
		ggplot(
			data=season_data,
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
	ggsave(plot=box_sn, filename=filepath, width=8.5, height=11, units="in", dpi=300)
}

violin_year_season = function(
	season_data, 
	filepath, 
	...
) {
	violin_year_season = 
		ggplot(
			data=season_data, 
			mapping=aes(x=sensor, y=value, color=plottype) 
		) + 
		facet_grid(rows=vars(sn_year), cols=vars(season)) +
		geom_violin(na.rm=TRUE) + theme_bw() +
		theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1)) +
		labs(...)
	ggsave(plot=violin_year_season, filename=filepath, width=8.5, height=11, units="in", dpi=300)
}

violin_season = function(
	season_data, 
	filepath, 
	...
) {
	violin_sn = 
		ggplot(
			data=season_data, # Residual scaled poorly on plot
			mapping=aes(
				x=sn_year, 
				y=value,
				fill=sensor,
			)
		) + 
		facet_wrap(~ season) +
		geom_violin() + theme_bw() +
		theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1)) +
		labs(...)
	ggsave(plot=violin_sn, filename=filepath, width=8.5, height=11, units="in", dpi=300)
}

deployment_correlation = function(
	deployment_data, 
	filepath,
	...
) {
	deployment_plot = 
		ggplot(
			data=deployment_data,
			mapping=aes(
				x=ref,
				y=meas
			)
		) + 
		facet_wrap(
			~ mos_into_deployment, 
			ncol=3, 
		) +
		stat_poly_line() + stat_poly_eq() +
		geom_abline(slope=1, intercept=0, color="red") +
		geom_point(alpha=0.05) + 
		labs(...)
	ggsave(plot=deployment_plot, filename=filepath, width=8.5, height=11, units="in", dpi=300)
}

timeseries_deployment_residual = function(
	deployment_data, 
	filepath,
	...
) {
	deployment_plot = 
		ggplot(
			data=deployment_data,
			mapping=aes(
				x=hrs_into_deployment_month,
				y=resid
			)
		) + 
		facet_wrap(
			~ mos_into_deployment, 
			ncol=3, 
		) +
		geom_line() + 
		labs(...)
	ggsave(plot=deployment_plot, filename=filepath, width=8.5, height=11, units="in", dpi=300)
}

violin_deployment_residual = function(
	season_data, 
	filepath,
	...
) {
	deployment_plot = 
		ggplot(
			data=season_data %>% filter(plottype=="resid"),
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
	ggsave(plot=deployment_plot, filename=filepath, width=8.5, height=11, units="in", dpi=300)
}

deployment_density = function(
	season_data, 
	filepath,
	...
) {
	deployment_plot = 
		ggplot(
			data=season_data %>% filter(plottype %in% c("meas","ref")),
			mapping=aes(
				x=value,
				fill=plottype
			)
		) + 
		facet_wrap(
			~ mos_into_deployment, 
			ncol=3, 
			scales="free_y"
		) +
		geom_density(alpha=0.5) +
		labs(...)
	ggsave(plot=deployment_plot, filename=filepath, width=8.5, height=11, units="in", dpi=300)
}

deployment_density_stats = function(
	season_data
) {
	stats_by_month = season_data %>%
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
		)
	divergence_by_month = 
		season_data %>%
		filter(plottype %in% c("meas","ref")) %>%
		select(date, mos_into_deployment, plottype, value) %>%
		pivot_wider(names_from=plottype, values_from=value) %>%
		filter(if_all(c(meas, ref), ~ !is.na(.))) %>%
		pivot_longer(cols=c(meas, ref), names_to="plottype",values_to="value") %>%
		group_by(mos_into_deployment, plottype) %>%
		select(-date) %>%
		nest(data = c(value)) %>%
		pivot_wider(names_from=plottype, values_from=data) %>%
		mutate( 
			density = map2(meas, ref, function(m,r) {
				est_n = length(m$value)
				values = c(m$value, r$value)
				est_grid = seq(
					min(values, na.rm=TRUE), 
					max(values, na.rm=TRUE), 
					length.out=est_n
				)
				est_min = min(est_grid)
				est_max = max(est_grid)
				dm = density(m$value, from=est_min, to=est_max, n=est_n, na.rm=TRUE)
				dr = density(r$value, from=est_min, to=est_max, n=est_n, na.rm=TRUE)
				return(tibble(
					x=est_grid, 
					m=dm$y/sum(dm$y),
					r=dr$y/sum(dr$y)
				))
			}),
		  	KL = map_dbl(density, ~ {
				distance(
					x=t(as.matrix(select(.x, m, r))), 
					method="kullback-leibler",
					mute.message=TRUE
				)
			}),
			hellinger = map_dbl(density, ~ {
				distance(
					x=t(as.matrix(select(.x, m, r))), 
					method="hellinger",
					mute.message=TRUE
				)
			}),
			euclidean = map_dbl(density, ~ {
				distance(
					x=t(as.matrix(select(.x, m, r))), 
					method="euclidean",
					mute.message=TRUE
				)
			}),
			pdf_resolution = map_dbl(density, ~ nrow(.x))
		) %>%
		rename(obs_meas=meas, obs_ref=ref, pdf=density)
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
	...
) {
	if (self_ref) pdf_stats = filter(.data=pdf_stats, mos_into_deployment>=12)
	else pdf_stats = filter(.data=pdf_stats, !is.na(value))

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
		labs(...)

	ggsave(
		plot=divergences_plot, 
		filename=filepath, 
		width=5, 
		height=5, 
		units="in", 
		dpi=300
	)
}