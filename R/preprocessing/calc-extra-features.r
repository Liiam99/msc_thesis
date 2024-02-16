library(dplyr)
library(sf)
library(zoo)

source("./R/utils/calc-index-metrics.r")
source("./R/utils/utils.r")

calc_extra_features <- function(reference_data, start, end) {
  extra_indices <- c("DVI", "EVI", "MNDBI", "MNDWI", "NBR", "NDMI", "NDVI", 
                     "TCW", "TCWVI")
  extra_indices_metrics <- lapply(extra_indices, calc_index_metrics, start=start, end=end)
  spatial_stats <- calc_spatial_stats(reference_data)
  
  # Creates one large data frame by merging all metrics and spatial stats.
  extra_features <- plyr::join_all(c(extra_indices_metrics, list(spatial_stats)), by="location_id")
}

calc_spatial_stats <- function(reference_data) {
  spatial_stats <- reference_data %>%
    select(location_id, centroid_x, centroid_y) %>%
    distinct()
  
  spatial_stats$abs_lat <- abs(spatial_stats$centroid_y)
  
  spatial_stats <- rename(spatial_stats, long=centroid_x, lat=centroid_y)
}
