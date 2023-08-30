# Get data on a Canadian P/T and plot road networks and cities. If available, 
# also get water bodies (lakes, etc.)
# Road networks come from
#   https://open.canada.ca/data/en/dataset/3d282116-e556-400c-9306-ca1a3cada77f

library(dplyr)
library(sf)
library(tmap)

source("functions_useful.R")

# Are we using shapefiles or geopackage files for road networks? Set following to
# SHAPE or GEOPACKAGE (this is grep-ed)
NRN_file_type = "GEOPACKAGE"
# Set the P/T under consideration
PT = "Manitoba"

# Get the list of available files from 
#   https://open.canada.ca/data/en/dataset/3d282116-e556-400c-9306-ca1a3cada77f
# Get the information from a JSON file there
raw_data <- RCurl::getURL("https://open.canada.ca/data/api/action/package_show?id=3d282116-e556-400c-9306-ca1a3cada77f")
tmp = rjson::fromJSON(raw_data)$result$resources
spatial_data = list()
for (i in 1:length(tmp)) {
  spatial_data[[i]] = list()
  spatial_data[[i]]$name = tmp[[i]]$name
  spatial_data[[i]]$url = tmp[[i]]$url
}
spatial_data = plyr::ldply(spatial_data, data.frame)
# Keep only the links (and names) to road networks, and select type (SHAPE or 
# GEOPACKAGE) of file used
spatial_data = spatial_data %>%
  filter(grepl("NRN", spatial_data$name)) 
spatial_data = spatial_data %>%
  filter(grepl(NRN_file_type, spatial_data$name))
# Get NRN for the selected geography
geography_NRN = spatial_data %>%
  filter(grepl(PT, spatial_data$name))
geography_NRN = read_shape_URL(geography_NRN$url, type = NRN_file_type)

# Get map of the P/T
geography <- rnaturalearth::ne_states(country = "Canada", returnclass = "sf") %>%
  filter(name == PT)

# Use world.cities from the maps package
# Keep only cities with at least 3K inhabitants
cities <- maps::world.cities[maps::world.cities$pop >= 3000, ]
# turn it into an sf object
cities <- cities %>%
  sf::st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
  sf::st_cast("POINT")
# keep only the cities that are in Canada
cities_canada = cities %>%
  filter(country.etc == "Canada")

# Geography in detail
# Get data on roads from 
#   https://open.canada.ca/data/en/dataset/3d282116-e556-400c-9306-ca1a3cada77f
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
  filter(population >= 3000) 

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
# Add fill layer to geography shape
w2 = tm_shape(geography) + 
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



# Add fill layer to geography shape
w3 = tm_shape(geography) + 
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

