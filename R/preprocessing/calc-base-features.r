library(dplyr)
library(sf)
library(zoo)

source("./R/utils/calc-index-metrics.r")
source("./R/utils/harm_utils.r")
source("./R/utils/utils.r")

calc_base_features <- function(reference_data, NIRv_ts, brazil=F) {
  NIRv_metrics <- calc_index_metrics(NIRv_ts, "NIRv")
  NIRv_harmonics <- calc_NIRv_harmonics(reference_data$location_id, brazil)

  reference_data <- reference_data %>%
    select(location_id, is_change)
  base_features <- plyr::join_all(list(reference_data, NIRv_metrics, NIRv_harmonics), by="location_id")
}

# Credit to: https://github.com/GreatEmerald/postprocessing-bfast/blob/main/src/015_preprocess_dense/10_CalcTemporalHarmonics.r
calc_NIRv_harmonics <- function(location_ids, brazil) {
  if (brazil == T) {
    folder <- "brazil"
  } else {
    folder <- "global"
  }
  
  # Needs more years than targeted date range so got to load it in again
  NIRv_file_path <- paste0("./data/", folder, "/processed/temporal_indices/NIRv.gpkg")
  NIRv <- st_read(NIRv_file_path, quiet=T)
  NIRv <- NIRv[NIRv$location_id %in% location_ids, ]
  NIRv_ts <- SFToZoo(NIRv)
  
  # Center harmonics around first year (July 2016 - June 2017)
  NIRvz_first_year <- window(NIRv_ts, start=as.Date("2015-07-01"), end=as.Date("2018-07-01"))
  
  # Center harmonics around second year (July 2017 - June 2018)
  NIRvz_second_year <- window(NIRv_ts, start=as.Date("2016-07-01"), end=as.Date("2019-07-01"))
  
  out_layers = c("min", "max", "intercept", "co", "si", "co2", "si2", "trend",
                 "phase1", "amplitude1", "phase2", "amplitude2")
  
  # Creates data frame with harmonics of first year.
  harm_coefs_first_year <- lapply(NIRvz_first_year, GetHarmonics)
  temporal_stats_first_year <- data.frame(
    names(harm_coefs_first_year),
    bind_rows(harm_coefs_first_year),
    row.names=NULL
  )
  colnames(temporal_stats_first_year)[1] <- "location_id"
  
  # Creates data frame with harmonics of second year.
  harm_coefs_second_year <- lapply(NIRvz_second_year, GetHarmonics)
  temporal_stats_second_year <- data.frame(
    names(harm_coefs_second_year),
    bind_rows(harm_coefs_second_year),
    row.names=NULL
  )
  colnames(temporal_stats_second_year)[1] <- "location_id"
  
  # Relative difference in harmonics between the two years.
  temporal_stats_changes <- (temporal_stats_second_year[, -1] - temporal_stats_first_year[, -1])/temporal_stats_first_year[, -1]
  temporal_stats_changes <- cbind(temporal_stats_changes, location_id=temporal_stats_first_year[, 1])
  
  # Subset based on recommendations of Masiliūnas et al. (2021).
  temporal_stats_changes <- subset(
    temporal_stats_changes,
    select=c("location_id", "amplitude1","co", "si", "amplitude2", "si2")
  )
}
