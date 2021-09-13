# Plot COVID-19 spread in North America (including Mexico)

# For data manipulation
library(tidyverse)
library(lubridate)
# For cartography
library(tmap)
library(tigris)
library(ggplot2)
# library(USAboundaries)
# library(USAboundariesData)
library(urbnmapr)
library(tidycensus)

source("functions_useful.R")

# Enable caching in tigris
options(tigris_use_cache = TRUE)

REFRESH_DATA = TRUE
PROCESS_DATA = TRUE
PLOT_MAPS = FALSE

date_today = today()
#date_today = "2021-06-27"

if (REFRESH_DATA) {
  # The list in which we store everything
  DATA_RAW = list()
  ###
  ### CAN
  ### 
  DATA_RAW$CAN = list()
  DATA_RAW$CAN$local = read.csv("https://raw.githubusercontent.com/ccodwg/Covid19Canada/master/timeseries_hr/cases_timeseries_hr.csv")
  # CAN dates are weird, but easy to deal with
  DATA_RAW$CAN$local$date_report = as.character(dmy(DATA_RAW$CAN$local$date_report))
  ###
  ### MEX
  ###
  # File names for MEX data include the date and are typically day before
  date_yesterday = date_today-1
  MEX_date_str = sprintf("%s", gsub("-", "", date_yesterday))
  DATA_RAW$MEX = list()
  DATA_RAW$MEX$local = 
    read.csv(sprintf("https://datos.covid-19.conacyt.mx/Downloads/Files/Casos_Diarios_Municipio_Confirmados_%s.csv",
                     MEX_date_str))
  DATA_RAW$MEX$states_or_provinces = 
    read.csv(sprintf("https://datos.covid-19.conacyt.mx/Downloads/Files/Casos_Diarios_Estado_Nacional_Confirmados_%s.csv",
                     MEX_date_str))
  # MEX data dates come out weird
  tmp = colnames(DATA_RAW$MEX$local)
  # All we need to fix is what starts with an X
  idx_X = grep("X", tmp)
  # Now deal with the X's..
  tmp[idx_X] = gsub("X", "", tmp[idx_X])
  # Dates in tmp are now D.M.Y. Split on ".", and make these into proper ymd dates
  tmp_dates = strsplit(tmp[idx_X], "\\.")
  tmp[idx_X] = unlist(lapply(tmp_dates, function(x) sprintf("%s-%s-%s", x[3], x[2], x[1])))
  colnames(DATA_RAW$MEX$local) = tmp
  # Do the same for state level
  tmp = colnames(DATA_RAW$MEX$states_or_provinces)
  # All we need to fix is what starts with an X
  idx_X = grep("X", tmp)
  # Now deal with the X's..
  tmp[idx_X] = gsub("X", "", tmp[idx_X])
  # Dates in tmp are now D.M.Y. Split on ".", and make these into proper ymd dates
  tmp_dates = strsplit(tmp[idx_X], "\\.")
  tmp[idx_X] = unlist(lapply(tmp_dates, function(x) sprintf("%s-%s-%s", x[3], x[2], x[1])))
  colnames(DATA_RAW$MEX$states_or_provinces) = tmp
  ###
  ### USA
  ###
  DATA_RAW$USA = list()
  DATA_RAW$USA$local = read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv")
  # Do some tidying up (dates, etc.)
  # JHU data for the USA comes with silly date format, fix this. Get all column names
  tmp = colnames(DATA_RAW$USA$local)
  # All we need to fix is what starts with an X
  idx_X = grep("X", tmp)
  # Now deal with the X's..
  tmp[idx_X] = gsub("X", "", tmp[idx_X])
  # Dates in tmp are now M.D.Y, with Y just two digits. Split on ".", add "20" to Y and make these into proper ymd dates
  tmp_dates = strsplit(tmp[idx_X], "\\.")
  tmp[idx_X] = unlist(lapply(tmp_dates, function(x) sprintf("20%s-%02d-%02d", x[3], as.numeric(x[1]), as.numeric(x[2]))))
  colnames(DATA_RAW$USA$local) = tmp
  ### 
  ### GLOBAL
  ### 
  # While we're at it, we get the global data from JHU
  DATA_RAW$global = read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
  # Do some tidying up (dates, etc.)
  # JHU data for the USA comes with silly date format, fix this. Get all column names
  tmp = colnames(DATA_RAW$global)
  # All we need to fix is what starts with an X
  idx_X = grep("X", tmp)
  # Now deal with the X's..
  tmp[idx_X] = gsub("X", "", tmp[idx_X])
  # Dates in tmp are now M.D.Y, with Y just two digits. Split on ".", add "20" to Y and make these into proper ymd dates
  tmp_dates = strsplit(tmp[idx_X], "\\.")
  tmp[idx_X] = unlist(lapply(tmp_dates, function(x) sprintf("20%s-%02d-%02d", x[3], as.numeric(x[1]), as.numeric(x[2]))))
  colnames(DATA_RAW$global) = tmp
  ##
  ## SAVE THE RAW DATA
  ##
  saveRDS(DATA_RAW, file = sprintf("CAN_MEX_USA_raw_data_%s.Rds", date_today))
} else {
  DATA_RAW = readRDS(file = sprintf("CAN_MEX_USA_raw_data_%s.Rds", date_today))
}

if (PROCESS_DATA) {
  # Cannot remember if this is needed
  USA_local_pop = get_estimates("county", product = "population") %>%
    filter(variable == "POP") %>%
    select(-"variable")
  # Where we store everything
  DATA = list()
  ###
  ### CAN
  ###
  DATA$CAN = list()
  # Total number of counties in the USA (for percentage active)
  DATA$CAN$nb_local_units = 125
  # Get range of dates for all jurisdictions
  DATA$CAN$date_range_local = range(ymd(DATA_RAW$CAN$local$date_report))
  # Make full list of dates
  DATA$CAN$dates_local = seq(from = DATA$CAN$date_range_local[1],
                             to = DATA$CAN$date_range_local[2],
                             by = "day")
  # Make a table like the other jurisdictions
  DATA$CAN$local = unique(DATA_RAW$CAN$local[,c("province", "health_region")]) %>%
    filter(health_region != "Not Reported")
  rownames(DATA$CAN$local) = 1:dim(DATA$CAN$local)[1]
  tmp = mat.or.vec(nr = dim(DATA$CAN$local)[1], nc = length(DATA$CAN$dates_local))
  colnames(tmp) = sprintf("%s", DATA$CAN$dates_local)
  DATA$CAN$local = cbind(DATA$CAN$local, tmp)
  DATA$CAN$idx_dates_local = c(grep("2019", colnames(DATA$CAN$local)),
                               grep("202", colnames(DATA$CAN$local)))
  # Fill in the table
  for (i in 1:dim(DATA_RAW$CAN$local)[1]) {
    writeLines(paste0(i, "/", dim(DATA_RAW$CAN$local)[1]))
    idx_PT = which(DATA$CAN$local$province == DATA_RAW$CAN$local$province[i])
    idx_HR = which(DATA$CAN$local$health_region == DATA_RAW$CAN$local$health_region[i])
    idx_row = intersect(idx_PT, idx_HR)
    idx_col = which(colnames(DATA$CAN$local) == DATA_RAW$CAN$local$date_report[i])
    DATA$CAN$local[idx_row, idx_col] = DATA_RAW$CAN$local$cases[i]
  }
  ###
  ### MEX
  ###
  DATA$MEX = list()
  # Total number of counties in the USA (for percentage active)
  DATA$MEX$nb_local_units = 2454
  DATA$MEX$local = DATA_RAW$MEX$local
  # Get range of dates for all jurisdictions
  DATA$MEX$idx_dates_local = c(grep("2019", colnames(DATA$MEX$local)),
                               grep("202", colnames(DATA$MEX$local)))
  DATA$MEX$dates_local = colnames(DATA_RAW$MEX$local)[DATA$MEX$idx_dates_local]
  DATA$MEX$date_range_local = range(ymd(DATA$MEX$dates_local))
  ###
  ### USA
  ###
  DATA$USA = list()
  # Total number of counties in the USA (for percentage active)
  DATA$USA$nb_local_units = 3143
  # To animate the map, make a table in incidence per 100K. Get rid of a few places not on the map, as well
  DATA$USA$local = DATA_RAW$USA$local %>%
    filter(! Province_State %in% c("American Samoa", 
                                   "Diamond Princess", 
                                   "Grand Princess", 
                                   "Guam",
                                   "Northern Mariana Islands",
                                   "Puerto Rico",
                                   "Virgin Islands"))
  idx_data = grep("202", colnames(DATA$USA$local))
  idx_to_remove = c()
  for (i in 1:dim(DATA$USA$local)[1]) {
    idx_fips_in_countydata = which(as.numeric(USA_local_pop$GEOID) == DATA$USA$local$FIPS[i])
    if (length(idx_fips_in_countydata) > 0) {
      writeLines(sprintf("i=%d/%d", i, dim(DATA$USA$local)[1]))
      # Data is cumulative, need to decumulate
      tmp_in = as.numeric(DATA$USA$local[i, idx_data])
      tmp_out = c(tmp_in[1], diff(tmp_in)) / USA_local_pop$value[idx_fips_in_countydata] * 100000
      tmp_out[which(tmp_out < 0)] = 0
      DATA$USA$local[i, idx_data] = tmp_out
      # writeLines(sprintf("%d", USA_local_pop$value[idx_fips_in_countydata]))
    } else {
      # writeLines(paste0("Missing population info for FIPS=", fips, 
      #                   " (", DATA$USA$local$Admin2[idx_fips_in_data], ")"))
      idx_to_remove = c(idx_to_remove, i)
    }
  }
  DATA$USA$local = DATA$USA$local[setdiff(1:dim(DATA$USA$local)[1], idx_to_remove),]
  # Get the spatial data
  local_sf <- get_urbn_map("counties", sf = TRUE)
  local_sf$county_fips = as.numeric(local_sf$county_fips)
  # Concatenate
  spatial_data = merge(x = local_sf,
                       y = DATA$USA$local,
                       by.x = "county_fips",
                       by.y = "FIPS",
                       all.x = TRUE)
  # If there are some rows without data, put zeros in there instead
  idx_data_in_spatial_data = grep("202", colnames(spatial_data))
  #which(is.na(spatial_data[,idx_data_in_spatial_data]))
  # Get range of dates for all jurisdictions
  DATA$USA$idx_dates_local = grep("202", colnames(DATA$USA$local))
  DATA$USA$dates_local = colnames(DATA_RAW$USA$local)[DATA$USA$idx_dates_local]
  DATA$USA$date_range_local = range(ymd(DATA$USA$dates_local))
  ###
  ### GLOBAL (minus USA)
  ###
  DATA$global = list()
  DATA$global$idx_dates = grep("202", colnames(DATA_RAW$global))
  DATA$global$dates = colnames(DATA_RAW$global)[DATA$global$idx_dates]
  DATA$global$date_range = range(ymd(DATA$global$dates))
  DATA$global$data = mat.or.vec(nr = length(unique(tmp$Country.Region)),
                                nc = length(DATA$global$dates))
  for (i in 1:length(unique(tmp$Country.Region))) {
    CT = unique(tmp$Country.Region)[i]
    tmp_table = DATA_RAW$global[which(tmp$Country.Region == CT), DATA$global$idx_dates]
    DATA$global$data[i,] = colSums(tmp_table)
  }
  colnames(DATA$global$data) = DATA$global$dates
  rownames(DATA$global$data) = unique(tmp$Country.Region)
  # Find active C/T through time
  DATA$global$active_CT = DATA$global$data
  DATA$global$active_CT[which(DATA$global$active_CT>0)] = 1
  DATA$global$nb_active_CT = colSums(DATA$global$active_CT)
  # Look for times when a C/T decreased, by computing diffs
  DATA$global$active_CT_delta = DATA$global$active_CT[,2:dim(DATA$global$active_CT)[2]]
  for (i in 1:dim(DATA$global$active_CT)[1]) {
    DATA$global$active_CT_delta[i,] = diff(DATA$global$active_CT[i,])
  }
  ###
  ### Determine periods without new cases
  ###
  # Determine, at a given point in time, which jurisdictions had a case in the past look_back days
  look_back = 21
  for (loc in names(DATA)) {
    t_start = DATA[[loc]]$date_range_local[1] + look_back
    t_end = DATA[[loc]]$date_range_local[2]
    dates_checked = seq(from = t_start, to = t_end, by = "days")
    DATA[[loc]]$dates_local_activity = dates_checked
    DATA[[loc]]$nb_local_units_with_cases_period = c()
    DATA[[loc]]$nb_states_or_provinces_with_cases_period = c()
    # Now go through the data
    for (dd in dates_checked) {
      dd = as_date(dd)
      past_N_days = seq(from = (dd-look_back), to = dd, by = "days")
      idx_past_N_days = which(colnames(DATA[[loc]]$local) %in% as.character(past_N_days))
      tmp = DATA[[loc]]$local[,idx_past_N_days]
      tmp_local = as.numeric(rowSums(tmp))
      DATA[[loc]]$nb_local_units_with_cases_period = c(DATA[[loc]]$nb_local_units_with_cases_period,
                                  length(tmp_local[tmp_local>0]))
    }
    DATA[[loc]]$pct_local_units_with_cases_period = 
      DATA[[loc]]$nb_local_units_with_cases_period / DATA[[loc]]$nb_local_units * 100
  }
  ###
  ### Last bits of info regarding dates
  ###
  DATA$overall = list()
  DATA$overall$range = c(min(c(DATA$CAN$date_range_local, DATA$MEX$date_range_local, DATA$USA$date_range_local)),
                         max(c(DATA$CAN$date_range_local, DATA$MEX$date_range_local, DATA$USA$date_range_local)))
  ###
  ### SAVE THE DATA
  ###
  saveRDS(DATA, file = sprintf("CAN_MEX_USA_data_%s.Rds", date_today))
  saveRDS(spatial_data, file = sprintf("CAN_MEX_USA_spatialdata_%s.Rds", date_today))
} else {
  DATA = readRDS(file = sprintf("CAN_MEX_USA_data_%s.Rds", date_today))
  spatial_data = readRDS(file = sprintf("CAN_MEX_USA_spatialdata_%s.Rds", date_today))
  idx_data = grep("202", colnames(DATA$USA$local))
  idx_data_in_spatial_data = grep("202", colnames(spatial_data))
  look_back = 21
}

## 
## Get geography info
## 

# US_states = unique(DATA_RAW$USA$local$Province_State)
# GEOG = counties(state = US_states)
# # FIPS codes >=60 are not in Alaska+lower 48, nor is 15 (Hawaii). Remove them..
# idx_to_remove = c(which(GEOG$STATEFP >= 60), which(GEOG$STATEFP == 15))
# idx_to_keep = setdiff(1:length(GEOG$STATEFP), idx_to_remove)
# GEOG = GEOG[idx_to_keep,]

# contemporary_us <- us_counties()

if (PLOT_MAPS) {
  max_incidence = max(DATA$USA$local[,idx_data])
  col_names = colnames(DATA$USA$local)[idx_data]
  tmp = as.numeric(unlist(DATA$USA$local[,idx_data]))
  quantiles_incidence = quantile(tmp, probs = seq(0, 1, 0.01))
  quantiles_incidence = c(0, as.numeric(quantiles_incidence[which(quantiles_incidence>0)]))
  
  for (c in col_names) {
    writeLines(paste0(c, " max incidence=", max(DATA$USA$local[,c])))
    ggplot() +
      geom_sf(spatial_data,
              mapping = aes(fill = get(c)),
              color = "#ffffff", size = 0.05) +
      # scale_fill_gradient2(midpoint = 5,
      #                      limits = c(0, max_incidence)) +
      scale_fill_gradient(low = "white", high = "red",
                          limits = c(0, 100),
                          oob = scales::squish) +
      ggtitle(c) +
      theme(plot.title = element_text(hjust = 0.5)) +
      labs(fill = "Incidence (/100K)")
    # Note the slightly weird form factor.. this is so that after cropping, the image has even width and height
    # (needed for simplicity in ffmpeg)
    ggsave(filename = sprintf("FIGS/USA_incidence_%s.png", c), width = 1921, height = 1081, units = "px")
    crop_figure(sprintf("FIGS/USA_incidence_%s.png", c))
  }
  
  command_str = "ffmpeg -r 5 -pattern_type glob -i 'FIGS/USA_incidence_202*.png' -c:v libx264 -vf 'loop=-1,fps=25,format=yuv420p' USA_incidence.mp4"
}
  
###
### PLOT ACTIVE JURISDICTIONS
###
###


# pdf(file = sprintf("%s/pct_active_3weeks_PT_HR.pdf", DIRS$FIGS),
#     width =8, height = 6)
png(file = sprintf("pct_active_%ddays.png", look_back),
    width = 1200, height = 800)
# tiff(file = sprintf("%s/pct_active_3weeks_PT_HR.tif", DIRS$FIGS),
#      width = 8, height = 6, res = 300, units = "in",
#      compression = "zip")
par(mar = c(7, 7, 7, 7))
plot(DATA$CAN$dates_local_activity, DATA$CAN$pct_local_units_with_cases_period,
     type = "l",
     lwd = 5,
     col = "darkorange4",
     ylim = c(0, 100),
     cex.lab = 2, cex.axis = 2,
     xaxt = "n",
     xlab = "Date", 
     ylab = sprintf("Jurisdictions with new cases in past %d days (%%)", look_back))
lines(DATA$MEX$dates_local_activity, DATA$MEX$pct_local_units_with_cases_period,
      type = "l",
      lwd = 5,
      pch = 17,
      col = "dodgerblue4")
lines(DATA$USA$dates_local_activity, DATA$USA$pct_local_units_with_cases_period,
      type = "l",
      lwd = 5,
      pch = 17,
      col = "black")
for (h in c(0, 100)) {
  abline(h = h, lty = 1, lwd = 1)
}
for (h in seq(from = 10, to = 90, by = 10)) {
  abline(h = h, lty = 3, lwd = 0.5)
}
legend("bottomright", 
       legend = c("CAN", "MEX", "USA"),
       col = c("darkorange4", "dodgerblue4", "black"), bg = "white",
       lwd = c(5, 5, 5),
       cex = 2,
       inset = 0.01)
axis(1, 
     at = pretty(ymd(DATA$CAN$dates_local_activity)),
     labels = sprintf("%s", pretty(ymd(DATA$CAN$dates_local_activity))),
     cex.axis = 2)
dev.off()
crop_figure(sprintf("pct_active_%ddays.png", look_back))

