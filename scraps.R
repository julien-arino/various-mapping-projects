CAN_provinces_to_plot <- c("Manitoba")
CAN <- getData("GADM", country="CAN", level=1)
CAN.provinces <- CAN[CAN$NAME_1 %in% CAN_provinces_to_plot,]

ca.bbox <- bbox(CAN.provinces)
xlim <- c(min(ca.bbox[1,1]),max(ca.bbox[1,2]))
ylim <- c(min(ca.bbox[2,1]),max(ca.bbox[2,2]))
# plot(us.states, xlim=xlim, ylim=ylim)
# plot(ca.provinces, xlim=xlim, ylim=ylim, add=T)
plot(CAN.provinces, xlim=xlim, ylim=ylim)


World$highlighted <- ifelse(World$iso_a3 %in% c("CAN", "FRA"), "gold", "gray75")
tm_shape(World, projection=3857, ylim=c(.1, 1), relative = TRUE) + 
  tm_polygons("highlighted") + 
  tm_layout("Web Mercator projection. Although widely used, it is discouraged for
statistical purposes. In reality, Australia is 3 times larger than Greenland!",
            inner.margins=c(0,0,.1,0), title.size=.6)

us_states_map = tm_shape(us_states, projection = 2163) + tm_polygons() + 
  tm_layout(frame = FALSE)
hawaii_map = tm_shape(hawaii) + tm_polygons() + 
  tm_layout(title = "Hawaii", frame = FALSE, bg.color = NA, 
            title.position = c("LEFT", "BOTTOM"))
alaska_map = tm_shape(alaska) + tm_polygons() + 
  tm_layout(title = "Alaska", frame = FALSE, bg.color = NA)
us_states_map
print(hawaii_map, vp = grid::viewport(0.35, 0.1, width = 0.2, height = 0.1))
print(alaska_map, vp = grid::viewport(0.15, 0.15, width = 0.3, height = 0.3))

tm_shape(World) +
  tm_polygons() + 
  tm_layout("Eckhart IV projection. Recommended in statistical maps for its equal-area property.",
            inner.margins=c(0,0,.1,0), title.size=.8)



library(maps)
usa = st_as_sf(map('usa', plot = FALSE, fill = TRUE))
laea = st_crs("+proj=laea +lat_0=30 +lon_0=-95") # Lambert equal area
usa <- st_transform(usa, laea)
g = st_graticule(usa)
plot(st_geometry(g), axes = TRUE)
plot(usa, graticule = TRUE, key.pos = NULL, axes = TRUE)


france = st_as_sf(map('france', plot = FALSE, fill = TRUE))
laea = st_crs("+proj=laea +lat_0=30 +lon_0=10") # Lambert equal area
france <- st_transform(france, laea)
g = st_graticule(france)
plot(st_geometry(g), axes = TRUE)
plot(france, graticule = TRUE, key.pos = NULL, axes = TRUE)


canada = st_as_sf(map('canada', plot = FALSE, fill = TRUE))
laea = st_crs("+proj=laea +lat_0=50 +lon_0=-90") # Lambert equal area
france <- st_transform(france, laea)
g = st_graticule(france)
plot(st_geometry(g), axes = TRUE)
plot(france, graticule = TRUE, key.pos = NULL, axes = TRUE)

# Canadian provinces
CAN_prov <- rnaturalearth::ne_states(c("canada"))
