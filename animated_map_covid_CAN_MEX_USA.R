# Plot COVID-19 spread in North America (including Mexico)

# For data manipulation
library(tidyverse)
library(dplyr)
library(readr)
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

REFRESH_DATA = FALSE
PROCESS_DATA = FALSE

date_today = today()
date_today = "2021-06-26"

if (REFRESH_DATA) {
  date_yesterday = date_today-1
  MEX_date_str = sprintf("%s", gsub("-", "", date_yesterday))
  # First, just download everything
  DATA_RAW = list()
  # Data for CAN
  DATA_RAW$CAN = list()
  DATA_RAW$CAN$health_regions = read.csv("https://raw.githubusercontent.com/ccodwg/Covid19Canada/master/timeseries_hr/cases_timeseries_hr.csv")
  # Data for MEX
  DATA_RAW$MEX = list()
  DATA_RAW$MEX$municipalities = 
    read.csv(sprintf("https://datos.covid-19.conacyt.mx/Downloads/Files/Casos_Diarios_Municipio_Confirmados_%s.csv",
                     MEX_date_str))
  DATA_RAW$MEX$states_national = 
    read.csv(sprintf("https://datos.covid-19.conacyt.mx/Downloads/Files/Casos_Diarios_Estado_Nacional_Confirmados_%s.csv",
                     MEX_date_str))
  # Data for USA
  DATA_RAW$USA = list()
  DATA_RAW$USA$counties = read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv")
  # While we're at it..
  DATA_RAW$global = read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
  
  # Do some tidying up (dates, etc.)
  # JHU data for the USA comes with silly date format, fix this. Get all column names
  tmp = colnames(DATA_RAW$USA$counties)
  # All we need to fix is what starts with an X
  idx_X = grep("X", tmp)
  # Now deal with the X's..
  tmp[idx_X] = gsub("X", "", tmp[idx_X])
  # Dates in tmp are now M.D.Y, with Y just two digits. Split on ".", add "20" to Y and make these into proper ymd dates
  tmp_dates = strsplit(tmp[idx_X], "\\.")
  tmp[idx_X] = unlist(lapply(tmp_dates, function(x) sprintf("20%s-%02d-%02d", x[3], as.numeric(x[1]), as.numeric(x[2]))))
  colnames(DATA_RAW$USA$counties) = tmp
  
  # MEX data dates also come out weird
  tmp = colnames(DATA_RAW$MEX$municipalities)
  # All we need to fix is what starts with an X
  idx_X = grep("X", tmp)
  # Now deal with the X's..
  tmp[idx_X] = gsub("X", "", tmp[idx_X])
  # Dates in tmp are now D.M.Y. Split on ".", and make these into proper ymd dates
  tmp_dates = strsplit(tmp[idx_X], "\\.")
  tmp[idx_X] = unlist(lapply(tmp_dates, function(x) sprintf("%s-%s-%s", x[3], x[2], x[1])))
  colnames(DATA_RAW$MEX$municipalities) = tmp
  # Do the same for state level
  tmp = colnames(DATA_RAW$MEX$states_national)
  # All we need to fix is what starts with an X
  idx_X = grep("X", tmp)
  # Now deal with the X's..
  tmp[idx_X] = gsub("X", "", tmp[idx_X])
  # Dates in tmp are now D.M.Y. Split on ".", and make these into proper ymd dates
  tmp_dates = strsplit(tmp[idx_X], "\\.")
  tmp[idx_X] = unlist(lapply(tmp_dates, function(x) sprintf("%s-%s-%s", x[3], x[2], x[1])))
  colnames(DATA_RAW$MEX$states_national) = tmp
  
  # CAN dates are also silly, but easier to deal with
  DATA_RAW$CAN$health_regions$date_report = as.character(dmy(DATA_RAW$CAN$health_regions$date_report))
  
  ##
  ## SAVE THE RAW DATA
  ##
  saveRDS(DATA_RAW, file = sprintf("CAN_MEX_USA_raw_data_%s.Rds", date_today))
} else {
  DATA_RAW = readRDS(file = sprintf("CAN_MEX_USA_raw_data_%s.Rds", date_today))
}

USA_counties_pop = get_estimates("county", product = "population") %>%
  filter(variable == "POP") %>%
  select(-"variable")

if (PROCESS_DATA) {
  DATA = list()
  DATA$USA = list()
  # To animate the map, make a table in incidence per 100K. Get rid of a few places not on the map, as well
  DATA$USA$counties = DATA_RAW$USA$counties %>%
    filter(! Province_State %in% c("American Samoa", 
                                   "Diamond Princess", 
                                   "Grand Princess", 
                                   "Guam",
                                   "Northern Mariana Islands",
                                   "Puerto Rico",
                                   "Virgin Islands"))
  idx_data = grep("202", colnames(DATA$USA$counties))
  idx_to_remove = c()
  for (i in 1:dim(DATA$USA$counties)[1]) {
    idx_fips_in_countydata = which(as.numeric(USA_counties_pop$GEOID) == DATA$USA$counties$FIPS[i])
    if (length(idx_fips_in_countydata) > 0) {
      writeLines(sprintf("i=%d",i))
      # Data is cumulative, need to decumulate
      tmp_in = as.numeric(DATA$USA$counties[i, idx_data])
      tmp_out = c(tmp_in[1], diff(tmp_in)) / USA_counties_pop$value[idx_fips_in_countydata] * 100000
      tmp_out[which(tmp_out < 0)] = 0
      DATA$USA$counties[i, idx_data] = tmp_out
      # writeLines(sprintf("%d", USA_counties_pop$value[idx_fips_in_countydata]))
    } else {
      # writeLines(paste0("Missing population info for FIPS=", fips, 
      #                   " (", DATA$USA$counties$Admin2[idx_fips_in_data], ")"))
      idx_to_remove = c(idx_to_remove, i)
    }
  }
  DATA$USA$counties = DATA$USA$counties[setdiff(1:dim(DATA$USA$counties)[1], idx_to_remove),]
  # Get the spatial data
  counties_sf <- get_urbn_map("counties", sf = TRUE)
  counties_sf$county_fips = as.numeric(counties_sf$county_fips)
  # Concatenate
  spatial_data = merge(x = counties_sf,
                       y = DATA$USA$counties,
                       by.x = "county_fips",
                       by.y = "FIPS",
                       all.x = TRUE)
  # If there are some rows without data, put zeros in there instead
  idx_data_in_spatial_data = grep("202", colnames(spatial_data))
  #which(is.na(spatial_data[,idx_data_in_spatial_data]))
  ##
  ## SAVE THE DATA
  ##
  saveRDS(DATA, file = sprintf("CAN_MEX_USA_data_%s.Rds", date_today))
  saveRDS(spatial_data, file = sprintf("CAN_MEX_USA_spatialdata_%s.Rds", date_today))
} else {
  DATA = readRDS(file = sprintf("CAN_MEX_USA_data_%s.Rds", date_today))
  spatial_data = readRDS(file = sprintf("CAN_MEX_USA_spatialdata_%s.Rds", date_today))
  idx_data = grep("202", colnames(DATA$USA$counties))
  idx_data_in_spatial_data = grep("202", colnames(spatial_data))
}

## 
## Get geography info
## 

# US_states = unique(DATA_RAW$USA$counties$Province_State)
# GEOG = counties(state = US_states)
# # FIPS codes >=60 are not in Alaska+lower 48, nor is 15 (Hawaii). Remove them..
# idx_to_remove = c(which(GEOG$STATEFP >= 60), which(GEOG$STATEFP == 15))
# idx_to_keep = setdiff(1:length(GEOG$STATEFP), idx_to_remove)
# GEOG = GEOG[idx_to_keep,]

# contemporary_us <- us_counties()

max_incidence = max(DATA$USA$counties[,idx_data])
col_names = colnames(DATA$USA$counties)[idx_data]
tmp = as.numeric(unlist(DATA$USA$counties[,idx_data]))
quantiles_incidence = quantile(tmp, probs = seq(0, 1, 0.01))
quantiles_incidence = c(0, as.numeric(quantiles_incidence[which(quantiles_incidence>0)]))

for (c in col_names) {
  writeLines(paste0(c, " max incidence=", max(DATA$USA$counties[,c])))
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

# tm_shape(spatial_data, asp = 1) +
#   tm_fill()

# Determine, at a given point in time, which jurisdictions had a case in the past look_back days
look_back = 21
t_start = dates_data[1] + look_back
t_end = dates_data[length(dates_data)]
dates_checked = seq(from = t_start, to = t_end, by = "days")

nb_HR_with_cases_2weeks = c()
nb_PT_with_cases_2weeks = c()

for (dd in dates_checked) {
  dd = as_date(dd)
  past_2_weeks = seq(from = (dd-delay), to = dd, by = "days")
  tmp_HR = rowSums(daily_activity_HR[,as.character(past_2_weeks)])
  nb_HR_with_cases_2weeks = c(nb_HR_with_cases_2weeks,
                              length(tmp_HR[tmp_HR>0]))
  tmp_PT = rowSums(daily_activity_PT[,as.character(past_2_weeks)])
  nb_PT_with_cases_2weeks = c(nb_PT_with_cases_2weeks,
                              length(tmp_PT[tmp_PT>0]))
}

nb_PT_with_cases_2weeks = nb_PT_with_cases_2weeks/13*100
nb_HR_with_cases_2weeks = nb_HR_with_cases_2weeks/112*100
y_max = max(max(nb_PT_with_cases_2weeks), max(nb_HR_with_cases_2weeks))


# pdf(file = sprintf("%s/pct_active_3weeks_PT_HR.pdf", DIRS$FIGS),
#     width =8, height = 6)
tiff(file = sprintf("%s/pct_active_3weeks_PT_HR.tif", DIRS$FIGS),
     width = 8, height = 6, res = 300, units = "in",
     compression = "zip")
plot(dates_checked, nb_PT_with_cases_2weeks,
     type = "b",
     lwd = 2,
     col = "red",
     pch = 21, 
     ylim = c(0, 100),
     xlab = "Date", ylab = "Jurisdictions with new cases in past 3 weeks (%)")
lines(dates_checked, nb_HR_with_cases_2weeks,
      type = "b",
      lwd = 2,
      pch = 17,
      col = "dodgerblue4")
for (h in seq(from = 0, to = 100, by = 10)) {
  abline(h = h, lty = 3, lwd = 0.5)
}
legend("bottomright", 
       legend = c("Provinces and Territories", "Health regions"),
       col = c("red", "dodgerblue4"), bg = "white",
       pch = c(21, 17),
       inset = 0.01)
dev.off()
crop_figure(sprintf("%s/pct_active_3weeks_PT_HR.tif", DIRS$FIGS))
