library(dplyr)
library(sf)
library(zoo)

source("./R/utils/calc-index-metrics.r")
source("./R/utils/utils.r")

calc_extra_features <- function(reference_data, indices, start, end) {
  extra_indices_names <- c("DVI", "EVI", "MNDBI", "MNDWI", "NBR", "NDMI", "NDVI", 
                           "TCW", "TCWVI")
  extra_indices <-  indices[names(indices) %in% extra_indices_names]
  extra_indices_metrics <- mapply(calc_index_metrics, extra_indices, extra_indices_names, MoreArgs=list(start=start, end=end), SIMPLIFY=F)
  
  spatial_stats <- calc_spatial_stats(reference_data)

  # Creates one large data frame by merging all metrics and spatial stats.
  extra_features <- plyr::join_all(c(extra_indices_metrics, list(spatial_stats)), by="location_id")
}

calc_spatial_stats <- function(reference_data) {
  spatial_stats <- reference_data %>%
    select(location_id, centroid_x, centroid_y)
  
  spatial_stats$abs_lat <- abs(spatial_stats$centroid_y)
  
  spatial_stats <- rename(spatial_stats, long=centroid_x, lat=centroid_y)
}
