library(plyr, include.only="round_any")
library(tidyverse)
library(sgolay)
library(ggpmisc)
library(zoo)
library(checkmate)
library(moments) # skew, kurtosis
library(patchwork)


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
			stop("length of meteorology arg must be 2 if provided [ i.e., c(temp,rh) ]")
		}
		met_filter = TRUE
	}
	df_long = tibble()
    if (read_from_cache & file.exists(cache_file)) {
		if(missing(cache_file)) {stop("read_from_cache==true but missing cache file.")}
        df_long = read_csv(cache_file)
    } else {
		dods = dates_of_deployment(df=df, parameters=parameters, sensors=sensors)
        df_long = df %>% filter(parameter %in% c(parameters, "temp","rh"), sensor %in% sensors) 
		df_long = df_long %>% 
			mutate(
				sn_year = factor(if_else(month(date) == 12, year(date) + 1, year(date)), levels = 2000:2030),
				season = factor(season(date), levels = c("Winter", "Spring", "Summer", "Fall")),
				hours_into_sn = hours_into_season(date)
			) 
		df_long = df_long %>% 
			left_join(y=dods, by = c("sensor", "location"), relationship="many-to-one") 
		df_long = df_long %>% 
			mutate(
				mos_into_deployment = interval(deployment_start, date) %/% months(1),
				hrs_into_deployment = interval(deployment_start, date) %/% hours(1),
				hrs_into_deployment_month = interval(
					deployment_start %m+% months(mos_into_deployment),
					date,
					tz="utc"
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
						.cols = any_of(c("co","pm25","pm01","pm10","pm")), # want to change this to refer to parameters argument at some point
						.fns = ~ if_else(met_flag, NA, .x)
					)
				) %>% select(-met_flag) %>%
				pivot_longer(
					cols=any_of(c("co","pm25","pm01","pm10","pm")),
					names_to="parameter",
					values_to="value"
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

deployment_density_stats = function(
	plot_df,
	bin_width = 0.05,
	log_eps_cutoff = 0.383
) {
	eps_cutoff = .Machine$double.eps^log_eps_cutoff
	mos_range = plot_df %>% 
    	pull(mos_into_deployment) %>% 
    	range(na.rm = TRUE)
	if (is.infinite(mos_range[1]) | is.infinite(mos_range[2])) {
		browser()
	}
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

			grid_from = max(min(values, na.rm=TRUE), 0)
			grid_to = max(values, na.rm=TRUE)

			if (is.na(grid_from) | is.na(grid_to)) { stop("grid_from or grid_to is NA") }
			est_grid = seq(
				round_any(grid_from, accuracy=bin_width, f=floor),
				round_any(grid_to, accuracy=bin_width, f=ceiling),
				by=bin_width
			)

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

#' The purpose of this function is to filter out mos_into_deployment groups of plot_df
#' that contain no meas observations, but do contain reference observations, unless that 
#' mos_into_deployment group is surrounded by op-months that do have measurement observations.
facet_filter_helper = function(
	plot_df
) {
	label_stats = plot_df
	label_stats = label_stats %>% select(plottype, mos_into_deployment, date, value)
	label_stats = label_stats %>% 
		group_by(plottype, mos_into_deployment) %>%
		filter(!is.na(value)) %>% 
		summarize(n=n()) %>% ungroup() %>%
		pivot_wider(
			names_from="plottype",
			values_from="n"
		) 
	opmonth_range = label_stats %>%
		filter(!is.na(meas)) %>%
		pull(mos_into_deployment) %>%
		range(.)
	ret = plot_df %>% filter(mos_into_deployment >= opmonth_range[1] & mos_into_deployment <= opmonth_range[2])
	return(ret)
}