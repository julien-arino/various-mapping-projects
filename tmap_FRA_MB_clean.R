# Get data on France and Manitoba and plot road networks and cities in both, as
# well as (the numerous) lakes in Manitoba

library(dplyr)
library(tmap)    # for static and interactive maps

source("functions_useful.R")

# Get metropolitan France from {rnaturalearth}
france <- rnaturalearth::ne_states(country = "France", returnclass = "sf") %>% 
  filter(!name %in% c("Guyane française", 
                      "Martinique", 
                      "Guadeloupe", 
                      "La Réunion", 
                      "Mayotte"))

# Get Manitoba
manitoba <- rnaturalearth::ne_states(country = "Canada", returnclass = "sf") %>%
  filter(name == "Manitoba")

# Use world.cities from the maps package
# Keep only cities with at least 10K inhabitants
cities <- maps::world.cities[maps::world.cities$pop >= 3000, ]
# turn it into an sf object
cities <- cities %>%
  sf::st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
  sf::st_cast("POINT")
# keep only the cities that are in France
cities_france = cities %>%
  filter(country.etc == "France")
# keep only the cities that are in Canada
cities_canada = cities %>%
  filter(country.etc == "Canada")

# MB in detail
# Manitoba
# Get data on roads from 
#   https://geo.statcan.gc.ca/nrn_rrn/mb/nrn_rrn_mb_SHAPE.zip
MB_roads = sf::read_sf("geography/shape-files/trn_lrs_highway_network_2018_shp/LRS_HIGHWAY_NETWORK_2018.shp")
MB_roads = sf::st_transform(MB_roads)
# For large roads, keep only Provincial Trunk Highways and Provincial Roads
idx = grep("H", MB_roads$ROAD_TYPE)
MB_large_roads = MB_roads[idx,]
idx = which(!is.na(MB_large_roads$NATIONAL_H))
MB_large_roads = MB_large_roads[idx,]
# Read MB lakes
MB_water = sf::read_sf("geography/shape-files/500k_shp/500k_hyd-py.shp")
sf::st_crs(MB_water) = sf::st_crs(MB_roads)
MB_water = sf::st_transform(MB_water)

# Use a nice little file I found..
municipalities_MB = read.csv("demography/cities-CAN.csv") %>%
  filter(province_id == "MB") %>%
  sf::st_as_sf(coords = c("lng", "lat"), crs = 4326) %>%
  sf::st_cast("POINT")

municipalities_MB_large = municipalities_MB %>%
  filter(population >= 4000) 

# Do fly in communities in MB
fly_in_MB = read.csv("geography/cgn_mb_csv_eng.csv") %>%
  filter(Generic.Category == "Populated Place") %>%
  filter(Generic.Term == "Northern Community" | Generic.Term == "Town" | 
           Geographical.Name == "Tadoule Lake" | Geographical.Name == "Lac Brochet") %>%
  sf::st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  sf::st_cast("POINT")

# Tadoule Lake: 58.706111, -98.512222
# Lac Brochet: 58.62, -101.483889


# Get localisation of cities in FRA
list_cities_FRA = readr::read_csv("demography/cities-FRA.csv", col_names = FALSE)
list_cities_FRA_cols = read.csv("demography/list_cities_FRA_col_names.txt", 
                                header = FALSE)
col_names =  c("id", list_cities_FRA_cols[1:12,1], 
               "no_se", list_cities_FRA_cols[13:dim(list_cities_FRA_cols)[1],1])
colnames(list_cities_FRA) = col_names

list_cities_FRA = list_cities_FRA %>%
  filter(pop_2012 >= 4000) %>%
  sf::st_as_sf(coords = c("lon_deg", "lat_deg"), crs = 4326) %>%
  sf::st_cast("POINT")

# Get road networks. France first
FRA_roads = sf::read_sf("geography/shape-files/roads-FRA//road.shp")
FRA_roads = sf::st_transform(FRA_roads, crs = 4326)

#st_bbox(MB_water) = c(xmin = -110, ymin = 48.95, xmax = -100, ymax = 60.5)
bbox_MB = sf::st_bbox(c(xmin = -102.1, xmax = -89, ymin = 48.95, ymax = 60.5), 
                      crs = sf::st_crs(4326))
bbox_FRA = sf::st_bbox(c(xmin = -5, xmax = 10, ymin = 41, ymin = 52), 
                       crs = sf::st_crs(4326))

# Add fill layer to france shape
w1 = tm_shape(france, asp = 1) +
    tm_fill() +
    tm_style("bw") +
    tm_layout(frame = FALSE) +
  tm_shape(FRA_roads$geometry) +
     tm_lines(lwd = 0.2) +
  tm_shape(list_cities_FRA) +
    tm_symbols(size = "pop_2012", 
               scale = 1.25, 
               title.size = "Population") +
  tm_scale_bar(position = c("right", "top")) +
  tm_graticules(lwd = 0.5) +
  tm_layout(title = "FRA")
# Add fill layer to Manitoba shape
w2 = tm_shape(manitoba) + 
    tm_fill() +
    tm_style("bw") +
    tm_layout(frame = FALSE) +
  tm_shape(MB_water) +
  tm_fill("white") +
  tm_shape(MB_roads$geometry) +
  tm_lines(lwd = 0.1) +
  tm_shape(MB_large_roads$geometry) +
  tm_lines(lwd = 0.5) +
  tm_shape(municipalities_MB_large) +
  tm_symbols(size = "population", 
             title.size = "Population", 
             scale = 1.25) +
  tm_scale_bar(position = c("right", "top")) +
  tm_graticules(lwd = 0.5) +
  tm_layout(title = "CAN-MB")

p = tmap_arrange(w1, w2)
tmap_save(p, filename = "FRA_and_MB_to_scale.png", 
          #height = 4)
          width = 2000, height = 1200)
crop_figure("FRA_and_MB_to_scale.png")

tmap_save(w1, filename = "cities_roads_FRA.png", asp = 1)
crop_figure("cities_roads_FRA.png")



# Add fill layer to Manitoba shape
w3 = tm_shape(manitoba) + 
  tm_fill() +
  tm_layout(frame = FALSE) +
  tm_shape(MB_water) +
  tm_fill("white") +
  #tm_shape(MB_water) +
  #tm_fill("dodgerblue4", alpha = 0.25) +
  tm_shape(MB_roads$geometry) +
  tm_lines(lwd = 0.3) +
  tm_shape(MB_large_roads$geometry) +
  tm_lines(lwd = 1) +
  tm_shape(fly_in_MB) +
  tm_symbols(col = "dodgerblue4", border.col = "dodgerblue4", scale = 0.3) +
  tm_shape(municipalities_MB_large) +
  tm_symbols(scale = 0.5, 
             col = "red3", border.col = "red3") +
  tm_legend(show = FALSE) +
  tm_scale_bar(position = c("right", "top")) +
  tm_graticules(lwd = 0.5) +
  tm_layout(title = "CAN-MB") +
  tm_grid(x = seq(-5, 10, by = 2), y = seq(42, 50, by = 2))
  
tmap_save(w3, filename = "cities_roads_CAN-MB_detail.png", asp = 1)
crop_figure("cities_roads_CAN-MB_detail.png")

