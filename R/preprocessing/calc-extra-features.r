# Credit to: https://github.com/GreatEmerald/postprocessing-bfast/blob/main/src/015_preprocess_dense/10_CalcTemporalHarmonics.r

library(dplyr)
library(sf)
library(zoo)

source("./R/utils/harm_utils.r")
source("./R/utils/utils.r")

calc_extra_features <- function(reference_data, location_ids, start, end) {
  nirv_stats <- calc_nirv_stats(location_ids, start, end)
  #temporal_stats <- calc_temporal_stats(location_ids)
  spatial_stats <- calc_spatial_stats(reference_data, location_ids)
  
  extra_features <- merge(nirv_stats, spatial_stats)
}

calc_nirv_stats <- function(location_ids, start, end) {
  NIRv <- st_read("./data/global/processed/temporal_indices/NIRv.gpkg", quiet=T)
  NIRv <- NIRv[NIRv$location_id %in% location_ids, ]
  NIRvz <- window(SFToZoo(NIRv), start=start, end=end)
  
  year_cutoff_date <- ((end - start)/2) + start
  NIRvz_year_1 <- window(NIRvz, start=start, end=year_cutoff_date)
  NIRvz_year_2 <- window(NIRvz, start=year_cutoff_date, end=end)
  
  # Relative yearly change in NIRv minimum (10th percentile).
  NIRv_min_year_1 <- apply(NIRvz_year_1, 2, quantile, probs=0.1, na.rm=T, names=F)
  NIRv_min_year_2 <- apply(NIRvz_year_2, 2, quantile, probs=0.1, na.rm=T, names=F)
  NIRv_rel_yearly_min_change <- (NIRv_min_year_2 - NIRv_min_year_1)/NIRv_min_year_1
  min_change <- data.frame(
    names(NIRv_rel_yearly_min_change), 
    NIRv_rel_yearly_min_change, 
    row.names=NULL
  )
  colnames(min_change)[1] <- "location_id"
  
  # Relative yearly change in NIRv maximum (90th percentile).
  NIRv_max_year_1 <- apply(NIRvz_year_1, 2, quantile, probs=0.9, na.rm=T, names=F)
  NIRv_max_year_2 <- apply(NIRvz_year_2, 2, quantile, probs=0.9, na.rm=T, names=F)
  NIRv_rel_yearly_max_change <- (NIRv_max_year_2 - NIRv_max_year_1)/NIRv_max_year_1
  max_change <- data.frame(
    names(NIRv_rel_yearly_max_change), 
    NIRv_rel_yearly_max_change, 
    row.names=NULL
  )
  colnames(max_change)[1] <- "location_id"
  
  # Relative yearly change in NIRv median.
  NIRv_year_1_median <- apply(NIRvz_year_1, 2, safe_median)
  NIRv_year_2_median <- apply(NIRvz_year_2, 2, safe_median)
  NIRv_rel_yearly_median_change <- (NIRv_year_2_median - NIRv_year_1_median)/NIRv_year_1_median
  median_change <- data.frame(
    names(NIRv_rel_yearly_median_change), 
    NIRv_rel_yearly_median_change, 
    row.names=NULL
  )
  colnames(median_change)[1] <- "location_id"
  
  NIRv_stats <- merge(min_change, max_change)
  NIRv_stats <- merge(NIRv_stats, median_change)
  
  return(NIRv_stats)
}

calc_temporal_stats <- function(location_ids) {
  NIRv <- st_read("./data/global/processed/temporal_indices/NIRv.gpkg", quiet=T)
  NIRv <- NIRv[NIRv$location_id %in% location_ids, ]
  # Calculating harmonics requires three years of data.
  NIRvz <- window(SFToZoo(NIRv), start=as.Date("2015-07-01"), end=as.Date("2018-6-30"))
  
  out_layers = c("min", "max", "intercept", "co", "si", "co2", "si2", "trend",
                "phase1", "amplitude1", "phase2", "amplitude2")
  
  # 68 observations resemble three years of data.
  harm_coefs <- rollapply(NIRvz, width=68, GetHarmonics, partial=TRUE, coredata=FALSE)
  
  harm_coefs <- as.matrix(harm_coefs)
  dim(harm_coefs) <- c(dim(NIRvz)[1], length(out_layers), dim(NIRvz)[2])
                     
  harm_coefs <- aperm(harm_coefs, c(1,3,2))
  
  missing_data <- is.na(NIRvz)
  harm_coefs[missing_data] <- NA
  harm_coefs[!is.finite(harm_coefs)] <- NA
  
  # order 1 amp
  # order 1 sine
  # order 1 cosine
  # order 2 amp
  # order 2 sine
  
  return(harm_coefs)
}

# location features function
calc_spatial_stats <- function(reference_data, location_ids) {
  spatial_stats <- reference_data %>%
    filter(location_id %in% location_ids) %>%
    select(location_id, centroid_x, centroid_y) %>%
    distinct()
  
  spatial_stats$abs_lat <- abs(spatial_stats$centroid_y)
  
  spatial_stats <- rename(spatial_stats, long=centroid_x, lat=centroid_y)
  
  return(spatial_stats)
}
