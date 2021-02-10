library(sf)
library(raster)
library(dplyr)
library(spData)
library(spDataLarge)
library(rnaturalearth)
#devtools::install_github("ropensci/rnaturalearthhires")
library(rnaturalearthhires)


library(tmap)    # for static and interactive maps
library(leaflet) # for interactive maps
library(mapview) # for interactive maps
library(ggplot2) # tidyverse vis package
library(shiny)   # for web applications
library(rgeos)
library(maps)
library(WikidataQueryServiceR)
library(osmdata)

# To scrape a bit
library(rvest)
#devtools::install_url('https://cran.r-project.org/src/contrib/Archive/htmltab/htmltab_0.7.1.tar.gz')
library(htmltab)

# Load some general data
data(World, metro, rivers, land)

# Get metropolitan France from {rnaturalearth}
france <- ne_states(country = "France", returnclass = "sf") %>% 
  filter(!name %in% c("Guyane française", "Martinique", "Guadeloupe", "La Réunion", "Mayotte"))
# Get Canada
canada <- ne_states(country = "Canada", returnclass = "sf")
# Get Manitoba
manitoba <- ne_states(country = "Canada", returnclass = "sf") %>%
  filter(name == "Manitoba")

# Use world.cities from the maps package
# Keep only cities with at least 10K inhabitants
cities <- world.cities[world.cities$pop >= 5000, ]
# turn it into an sf object
cities <- cities %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
  st_cast("POINT")
# keep only the cities that are in France
cities_france = cities %>%
  filter(country.etc == "France")
#cities_france <- st_intersection(cities, st_union(france))
# keep only the cities that are in France
cities_canada = cities %>%
  filter(country.etc == "Canada")
# cities <- world.cities[world.cities$pop >= 10000, ]
# turn it into an sf object
# cities <- cities %>%
#   st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
#   st_cast("POINT")
# cities_manitoba = st_intersection(cities, st_union(manitoba))

# Do pruning brute force: get list of municipalities in MB and see which ones are in cities_canada
# Urban municipalities
municipalities_urban_MB = htmltab("https://en.wikipedia.org/wiki/List_of_municipalities_in_Manitoba", 1)
to_remove = grep("Total", municipalities_urban_MB$Name)
to_remove = c(to_remove, grep("Province", municipalities_urban_MB$Name))
municipalities_urban_MB = municipalities_urban_MB[setdiff(1:dim(municipalities_urban_MB)[1], to_remove),]
# Rural municipalities
municipalities_rural_MB = htmltab("https://en.wikipedia.org/wiki/List_of_municipalities_in_Manitoba", 2)
to_remove = grep("Total", municipalities_rural_MB$Name)
to_remove = c(to_remove, grep("Province", municipalities_rural_MB$Name))
municipalities_rural_MB = municipalities_rural_MB[setdiff(1:dim(municipalities_rural_MB)[1], to_remove),]
# List of names only
municipalities_MB = c(municipalities_urban_MB$Name, municipalities_rural_MB$Name)

MB_in_CAN_data = intersect(cities_canada$name, municipalities_MB)
cities_manitoba = cities_canada[which(cities_canada$name %in% MB_in_CAN_data),]

# Get road networks
FRA_roads = read_sf("/home/jarino/DATA_local/GEOGRAPHY/road-networks/FRA/road.shp")
st_crs(FRA_roads) = 2154 # 4326
st_crs(FRA_roads$geometry) = 2154
FRA_roads = st_transform(FRA_roads, crs = 4326)

#france = st_transform(france, crs = 2192)
#manitoba = st_transform(manitoba, crs = 3348)

# Add fill layer to france shape
w1 = tm_shape(france) +
    tm_fill() +
    tm_style("bw") +
    tm_layout(frame = FALSE) +
  # tm_shape(FRA_roads$geometry) +
  #   tm_lines(lwd = 2) +
  tm_shape(cities_france) +
    tm_symbols(size = "pop", scale = 1) +
  tm_compass(position = c("right", "bottom")) +
  tm_scale_bar(position = c("right", "bottom"))
# Add fill layer to Manitoba shape
w2 = tm_shape(manitoba) + 
    tm_fill() +
    tm_style("bw") +
    tm_layout(frame = FALSE) +
  tm_shape(cities_manitoba) +
    tm_symbols(size = "pop", scale = 1) +
  tm_scale_bar()

# tmap_mode("plot")
# tmap_mode("view")
tmap_arrange(w1, w2)

# r <- query_wikidata('
#     SELECT ?item ?itemLabel (MIN(?_date) AS ?date) (MIN(?_year) AS ?year) {
#         ?item wdt:P31 wd:Q7889 ; wdt:P577 ?_date .
#         BIND(YEAR(?_date) AS ?_year) .
#         SERVICE wikibase:label { bd:serviceParam wikibase:language "en" . }
#     }
#     GROUP BY ?item ?itemLabel
#     HAVING (?year > 1)
# ')
# 
# r <- query_wikidata('
#      SELECT ?item {
#          ?item wd:Q17110241.
#          SERVICE wikibase:label { bd:serviceParam wikibase:language "[AUTO_LANGUAGE]". }
#      }
#      GROUP BY ?item
# ')
# 
