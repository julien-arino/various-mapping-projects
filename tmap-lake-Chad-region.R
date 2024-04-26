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
  "osmdata",
  "rvest"
)

for (lib in required_CRAN_libraries) {
  if (!require(lib, character.only = TRUE)) {
    install.packages(lib, Ncpus = (parallel::detectCores()-1))
    library(lib, character.only = TRUE)
  }
}

# Libraries that come from elsewhere
if (!require(spDataLarge)) {
  install.packages('spDataLarge',
                   repos='https://nowosad.github.io/drat/',
                   type='source')
  library(spDataLarge)
}
if (!require(rnaturalearthhires)) {
  devtools::install_github("ropensci/rnaturalearthhires")
  library(rnaturalearthhires)
}
# To scrape a bit
if (!require(htmltab)) {
  devtools::install_url('https://cran.r-project.org/src/contrib/Archive/htmltab/htmltab_0.8.2.tar.gz')
  library(htmltab)
}

# Load some general data
data(World, metro, rivers, land)

# List countries (useful at a few places)
countries_considered = c("Cameroon", 
                         "Chad",
                         "Niger",
                         "Nigeria")

# Get countries from {rnaturalearth}
CCNN <- ne_states(country = countries_considered, 
                  returnclass = "sf")
# For map colouring, we give an id to the countries
colours = viridis::viridis(dim(CCNN)[1])
CCNN$colour_country = rep(0, dim(CCNN)[1])
for (i in 1:length(countries_considered)) {
  CCNN$colour_country[which(CCNN$admin == countries_considered[i])] = 
    colours[i]
}

# Use world.cities from the maps package
# Keep only cities with at least 10K inhabitants
cities <- world.cities[world.cities$pop >= 3000, ]
# turn it into an sf object
cities <- cities %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
  st_cast("POINT")
# keep only the cities that are in the selected countries
cities_CCNN = cities %>%
  filter(country.etc %in% countries_considered)

bbox_lake_chad = 
  st_bbox(c(xmin = 12, 
            xmax = 15, 
            ymin = 12, 
            ymax = 15), 
          crs = st_crs(4326))

# Get water bodies information
Africa_water = sf::read_sf("geography/shape-files/waterbodies_africa/waterbodies_africa.shp")
Africa_water = sf::st_transform(Africa_water)

# Add fill layer to shape
w1 = tm_shape(CCNN, asp = 1, bbox = bbox_lake_chad) +
  tm_fill("admin", title = "Country") +
  #tm_borders("colour_country") +
  tm_borders() +
  # tm_text("woe_label") +
  tm_text("admin") +
  # tm_style("bw") +
  tm_shape(Africa_water) +
  tm_fill("dodgerblue4") +
  tm_layout(frame = FALSE) +
  tm_shape(cities_CCNN) +
  tm_symbols(size = "pop", 
             scale = 1.25, 
             title.size = "Population") +
  tm_scale_bar(position = c("right", "top")) +
  tm_graticules(lwd = 0.5) +
  tm_layout(title = "Lake Chad")

p = tmap_arrange(w1)
tmap_save(p, filename = "Lake-Chad.png", 
          #height = 4)
          width = 2000, height = 1200)
# crop_figure("Lake-Chad.png")




# # Add fill layer to Manitoba shape
# w3 = tm_shape(manitoba) + 
#   tm_fill() +
#   tm_layout(frame = FALSE) +
#   tm_shape(MB_water) +
#   tm_fill("white") +
#   #tm_shape(MB_water) +
#   #tm_fill("dodgerblue4", alpha = 0.25) +
#   tm_shape(MB_roads$geometry) +
#   tm_lines(lwd = 0.3) +
#   tm_shape(MB_large_roads$geometry) +
#   tm_lines(lwd = 1) +
#   tm_shape(fly_in_MB) +
#   tm_symbols(col = "dodgerblue4", border.col = "dodgerblue4", scale = 0.3) +
#   tm_shape(municipalities_MB_large) +
#   tm_symbols(scale = 0.5, 
#              col = "red3", border.col = "red3") +
#   tm_legend(show = FALSE) +
#   tm_scale_bar(position = c("right", "top")) +
#   tm_graticules(lwd = 0.5) +
#   tm_layout(title = "CAN-MB") +
#   tm_grid(x = seq(-5, 10, by = 2), y = seq(42, 50, by = 2))
#   
# tmap_save(w3, filename = "cities_roads_CAN-MB_detail.png", asp = 1)
# crop_figure("cities_roads_CAN-MB_detail.png")
# 
# # r <- query_wikidata('
# #     SELECT ?item ?itemLabel (MIN(?_date) AS ?date) (MIN(?_year) AS ?year) {
# #         ?item wdt:P31 wd:Q7889 ; wdt:P577 ?_date .
# #         BIND(YEAR(?_date) AS ?_year) .
# #         SERVICE wikibase:label { bd:serviceParam wikibase:language "en" . }
# #     }
# #     GROUP BY ?item ?itemLabel
# #     HAVING (?year > 1)
# # ')
# # 
# # r <- query_wikidata('
# #      SELECT ?item {
# #          ?item wd:Q17110241.
# #          SERVICE wikibase:label { bd:serviceParam wikibase:language "[AUTO_LANGUAGE]". }
# #      }
# #      GROUP BY ?item
# # ')
# # 
