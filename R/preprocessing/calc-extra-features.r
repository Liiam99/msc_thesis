library(dplyr)
library(sf)

source("./R/utils/calc-index-metrics.r")

calc_extra_features <- function(reference_data, time_series) {
  extra_time_series_names<- c("DVI", "EVI", "MNDBI", "MNDWI", "NBR", "NDMI", 
                              "NDVI", "TCW", "TCWVI")
  extra_time_series <-  time_series[names(time_series) %in% extra_time_series_names]
  extra_indices_metrics <- mapply(calc_index_metrics, extra_time_series, extra_time_series_names, SIMPLIFY=F)
  
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
