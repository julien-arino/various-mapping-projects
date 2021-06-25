# Plot COVID-19 spread in North America (including Mexico)

# For data manipulation
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

# Enable caching in tigris
options(tigris_use_cache = TRUE)

date_today = today()
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
counties_sf <- get_urbn_map("counties", sf = TRUE)

counties_sf %>% 
  ggplot(aes()) +
  geom_sf(fill = "grey", color = "#ffffff")
