library(ggplot2)
theme_set(theme_bw())
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)

visualise_errors <- function(com_errors, om_errors) {
  world <- ne_countries(scale = "medium", returnclass = "sf")
  
  com_errors_sf <- st_as_sf(x=com_errors,
                            coords=c("centroid_x", "centroid_y"),
                            crs="WGS84")
  om_errors_sf <- st_as_sf(x=om_errors,
                           coords=c("centroid_x", "centroid_y"),
                           crs="WGS84")
  
  ggplot() +
    geom_sf(data = world) +
    geom_sf(data = om_errors_sf, color = "red") +
    geom_sf(data = com_errors_sf, color = "blue") +
    labs(title = "Commission and omission errors of full Random Forest model")
}
