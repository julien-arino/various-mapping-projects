# Libraries that are "easy" to install from CRAN.
# Under Linux, this may require to install debs. Watch the output
# of the install process...
# The list could use some cleaning...
required_CRAN_libraries = c(
  "sf",
  "raster",
  "dplyr",
  "readr",
  "spData",
  "rnaturalearth",
  "tmap",    # for static and interactive maps
  "leaflet", # for interactive maps
  "mapview",  # for interactive maps
  "ggplot2", # tidyverse vis package
  # "shiny",   # for web applications
  #"rgeos", # rgeos is now deprecated
  "maps",
  "WikidataQueryServiceR",
  "osmdata"
)

for (lib in required_CRAN_libraries) {
  if (!require(lib, character.only = TRUE)) {
    install.packages(lib, Ncpus = (parallel::detectCores()-1))
    library(lib, character.only = TRUE)
  }
}

# Libraries that come from elsewhere
# if (!require(spDataLarge)) {
#   install.packages('spDataLarge', 
#                    repos='https://nowosad.github.io/drat/', 
#                    type='source')
#   library(spDataLarge)
# }
if (!require(rnaturalearthhires)) {
  devtools::install_github("ropensci/rnaturalearthhires")
  library(rnaturalearthhires)
}



# To scrape a bit
library(rvest)
if (!require(htmltab)) {
  devtools::install_url('https://cran.r-project.org/src/contrib/Archive/htmltab/htmltab_0.7.1.tar.gz')
  library(htmltab)
}

# Load some general data
data(World, metro, rivers, land)

# Get countries from {rnaturalearth}
cameroon <- ne_states(country = "Cameroon", returnclass = "sf")
chad <- ne_states(country = "Chad", returnclass = "sf")
niger <- ne_states(country = "Niger", returnclass = "sf")
nigeria <- ne_states(country = "Nigeria", returnclass = "sf")

# Get Canada
canada <- ne_states(country = "Canada", returnclass = "sf")
# Get Manitoba
manitoba <- ne_states(country = "Canada", returnclass = "sf") %>%
  filter(name == "Manitoba")

# Use world.cities from the maps package
# Keep only cities with at least 10K inhabitants
cities <- world.cities[world.cities$pop >= 3000, ]
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

# # Do pruning brute force: get list of municipalities in MB and see which ones are in cities_canada
# # Urban municipalities
# municipalities_urban_MB = htmltab("https://en.wikipedia.org/wiki/List_of_municipalities_in_Manitoba", 1)
# to_remove = grep("Total", municipalities_urban_MB$Name)
# to_remove = c(to_remove, grep("Province", municipalities_urban_MB$Name))
# municipalities_urban_MB = municipalities_urban_MB[setdiff(1:dim(municipalities_urban_MB)[1], to_remove),]
# colnames(municipalities_urban_MB)[4] = "pop_2016"
# # Rural municipalities
# municipalities_rural_MB = htmltab("https://en.wikipedia.org/wiki/List_of_municipalities_in_Manitoba", 2)
# to_remove = grep("Total", municipalities_rural_MB$Name)
# to_remove = c(to_remove, grep("Province", municipalities_rural_MB$Name))
# municipalities_rural_MB = municipalities_rural_MB[setdiff(1:dim(municipalities_rural_MB)[1], to_remove),]
# colnames(municipalities_rural_MB)[3] = "pop_2016"
# # List of names only
# municipalities_MB = rbind(municipalities_urban_MB[,c("Name", "pop_2016")], 
#                           municipalities_rural_MB[,c("Name", "pop_2016")])
# # Simplify populations
# municipalities_MB = municipalities_MB %>%
#   mutate(pop_2016 = as.numeric(gsub(",", "", municipalities_MB$pop_2016))) %>%
#   arrange(desc(pop_2016)) %>% 
#   filter(pop_2016 >= 4000) 

# MB in detail
# Manitoba
MB_roads = read_sf("/home/jarino/DATA/GEOGRAPHY/road-networks/MB/trn_lrs_highway_network_2018_shp/LRS_HIGHWAY_NETWORK_2018.shp")
MB_roads = st_transform(MB_roads)
# For large roads, keep only Provincial Trunk Highways and Provincial Roads
idx = grep("H", MB_roads$ROAD_TYPE)
MB_large_roads = MB_roads[idx,]
idx = which(!is.na(MB_large_roads$NATIONAL_H))
MB_large_roads = MB_large_roads[idx,]
# Read MB lakes
MB_water = read_sf("/home/jarino/DATA/GEOGRAPHY/rivers-lakes/MB/500k_shp/500k_hyd-py.shp")
st_crs(MB_water) = st_crs(MB_roads)
MB_water = st_transform(MB_water)

# Use a nice little file I found..
municipalities_MB = read.csv("/home/jarino/DATA/GEOGRAPHY/Canada/canadacities.csv") %>%
  filter(province_id == "MB") %>%
  st_as_sf(coords = c("lng", "lat"), crs = 4326) %>%
  st_cast("POINT")

municipalities_MB_large = municipalities_MB %>%
  filter(population >= 4000) 

# Do fly in communities in MB
fly_in_MB = read.csv("/home/jarino/DATA/GEOGRAPHY/Canada/cgn_mb_csv_eng.csv") %>%
  filter(Generic.Category == "Populated Place") %>%
  filter(Generic.Term == "Northern Community" | Generic.Term == "Town" | 
           Geographical.Name == "Tadoule Lake" | Geographical.Name == "Lac Brochet") %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  st_cast("POINT")

# Tadoule Lake: 58.706111, -98.512222
# Lac Brochet: 58.62, -101.483889


# # Get population in French cities
# municipalities_20kplus_FRA = rbind(htmltab("https://en.wikipedia.org/wiki/List_of_communes_in_France_with_over_20,000_inhabitants", 1),
#                                    htmltab("https://en.wikipedia.org/wiki/List_of_communes_in_France_with_over_20,000_inhabitants", 2))
# Get localisation of cities in FRA
list_cities_FRA = read_csv("/home/jarino/DATA/GEOGRAPHY/France/villes_france.csv", col_names = FALSE)
list_cities_FRA_cols = read.csv("/home/jarino/DATA/GEOGRAPHY/France/list_cities_FRA_col_names.txt", header = FALSE)
col_names =  c("id", list_cities_FRA_cols[1:12,1], "no_se", list_cities_FRA_cols[13:dim(list_cities_FRA_cols)[1],1])
colnames(list_cities_FRA) = col_names
# # Add department numbers
# list_departments_FRA = htmltab("https://fr.wikipedia.org/wiki/Liste_des_d%C3%A9partements_fran%C3%A7ais_class%C3%A9s_par_population_et_superficie", 1)
# list_departments_FRA = htmltab("https://en.wikipedia.org/wiki/List_of_French_departments_by_population", 1) %>%
#   filter(`INSEE Dept. No.` <900) %>%
#   arrange(`INSEE Dept. No.`)
# list_cities_FRA = merge(x = list_departments_FRA, y = list_cities_FRA, by.x = "INSEE Dept. No.", by.y = "departement")
# # Merge info
# list_cities_FRA_tmp = merge(x = list_cities_FRA, y = municipalities_20kplus_FRA, 
#                         by.x = "nom_reel", by.y = "Commune") %>%
#   filter(Department.x == Department.y) %>%
#   select(nom_reel, "Population, 2017", lon_deg, lat_deg) %>%
#   mutate(`Population, 2017` = gsub(",", "", list_cities_FRA$`Population, 2017`))
list_cities_FRA = list_cities_FRA %>%
  filter(pop_2012 >= 4000) %>%
  st_as_sf(coords = c("lon_deg", "lat_deg"), crs = 4326) %>%
  st_cast("POINT")

# Get road networks. France first
FRA_roads = read_sf("/home/jarino/DATA/GEOGRAPHY/road-networks/FRA/road.shp")
#st_crs(FRA_roads) = 2154 # 4326
#st_crs(FRA_roads$geometry) = 2154
FRA_roads = st_transform(FRA_roads, crs = 4326)


#st_bbox(MB_water) = c(xmin = -110, ymin = 48.95, xmax = -100, ymax = 60.5)
bbox_MB = st_bbox(c(xmin = -102.1, xmax = -89, ymin = 48.95, ymax = 60.5), crs = st_crs(4326))
bbox_FRA = st_bbox(c(xmin = -5, xmax = 10, ymin = 41, ymin = 52), crs = st_crs(4326))

# france = st_transform(france, 
#                     crs = "+proj=laea +x_0=0 +y_0=0 +lon_0=2 +lat_0=47")
# FRA_roads = st_transform(FRA_roads, 
#                          crs = "+proj=laea +x_0=0 +y_0=0 +lon_0=2 +lat_0=47")
# manitoba = st_transform(manitoba, 
#                         crs = "+proj=laea +x_0=0 +y_0=0 +lon_0=-98 +lat_0=54")
# MB_roads = st_transform(MB_roads, 
#                         crs = "+proj=laea +x_0=0 +y_0=0 +lon_0=-98 +lat_0=54")
# cities_manitoba = st_transform(cities_manitoba, 
#                                crs = "+proj=laea +x_0=0 +y_0=0 +lon_0=-98 +lat_0=54")
#tm_shape(MB_water) +
#  tm_fill()

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

#print(w1, return.asp = TRUE, mode = "plot")
#print(w2, return.asp = TRUE, mode = "plot")

p = tmap_arrange(w1, w2)#, 
                 #widths = c(0.45, 0.55), heights = c(0.4, 0.6))

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
