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
cities <- world.cities[world.cities$pop >= 10000, ]
# turn it into an sf object
cities <- cities %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
  st_cast("POINT")
cities_manitoba = st_intersection(cities, st_union(manitoba))

#france = st_transform(france, crs = 2192)
#manitoba = st_transform(manitoba, crs = 3348)

# Add fill layer to france shape
w1 = tm_shape(france) +
    tm_fill() +
    tm_style("bw") +
    tm_layout(frame = FALSE) +
  tm_shape(cities_france) +
    tm_symbols(size = "pop", scale = 2) 
# Add fill layer to Manitoba shape
w2 = tm_shape(manitoba) +
    tm_fill() +
    tm_style("bw") +
    tm_layout(frame = FALSE) +
  tm_shape(cities_canada) +
    tm_symbols(size = "pop", scale = 2)

#current.mode <- tmap_mode("plot")
#tmap_mode("view")
tmap_arrange(w1, w2)
#tmap_mode(current.mode)
