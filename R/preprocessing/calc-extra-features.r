# Credit to: https://github.com/GreatEmerald/postprocessing-bfast/blob/main/src/015_preprocess_dense/10_CalcTemporalHarmonics.r

library(dplyr)
library(sf)
library(zoo)

source("./R/preprocessing/calc-base-features.r")
source("./R/utils/harm_utils.r")
source("./R/utils/utils.r")

calc_extra_features <- function(reference_data, location_ids, start, end) {
  extra_indices <- calc_features(reference_data, names=c("DVI", "MNDBI", "TCW", "TCWVI"), start, end)
  temporal_stats <- calc_temporal_stats(location_ids)
  spatial_stats <- calc_spatial_stats(reference_data, location_ids)

  extra_features <- plyr::join_all(list(extra_indices, temporal_stats, spatial_stats), by="location_id")
}

calc_temporal_stats <- function(location_ids) {
  NIRv <- st_read("./data/global/processed/temporal_indices/NIRv.gpkg", quiet=T)
  NIRv <- NIRv[NIRv$location_id %in% location_ids, ]
  
  # Center harmonics around first year (July 2016 - June 2017)
  NIRvz_first_year <- window(SFToZoo(NIRv), start=as.Date("2015-07-01"), end=as.Date("2018-6-30"))
  
  # Center harmonics around second year (July 2017 - June 2018)
  NIRvz_second_year <- window(SFToZoo(NIRv), start=as.Date("2016-07-01"), end=as.Date("2019-6-30"))
  
  out_layers = c("min", "max", "intercept", "co", "si", "co2", "si2", "trend",
                "phase1", "amplitude1", "phase2", "amplitude2")
  
  harm_coefs_first_year <- lapply(NIRvz_first_year, GetHarmonics)
  
  temporal_stats_first_year <- data.frame(
    names(harm_coefs_first_year),
    bind_rows(harm_coefs_first_year),
    row.names=NULL
  )
  colnames(temporal_stats_first_year)[1] <- "location_id"
  
  harm_coefs_second_year <- lapply(NIRvz_second_year, GetHarmonics)
  
  temporal_stats_second_year <- data.frame(
    names(harm_coefs_second_year),
    bind_rows(harm_coefs_second_year),
    row.names=NULL
  )
  colnames(temporal_stats_second_year)[1] <- "location_id"
  
  temporal_stats_changes <- temporal_stats_first_year[, -1] - temporal_stats_second_year[, -1]
  temporal_stats_changes <- cbind(temporal_stats_changes, location_id=temporal_stats_first_year[, 1])

  # temporal_stats_changes <- subset(
  #   temporal_stats_changes,
  #   select=c("location_id", "amplitude1","co", "si", "amplitude2", "si2")
  # )
}

calc_spatial_stats <- function(reference_data, location_ids) {
  spatial_stats <- reference_data %>%
    filter(location_id %in% location_ids) %>%
    select(location_id, centroid_x, centroid_y) %>%
    distinct()
  
  spatial_stats$abs_lat <- abs(spatial_stats$centroid_y)
  
  spatial_stats <- rename(spatial_stats, long=centroid_x, lat=centroid_y)
}
