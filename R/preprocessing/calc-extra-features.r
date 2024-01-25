source("./R/utils/utils.r")

calc_extra_features <- function(base_features, start, end) {
  location_ids <- unique(base_features$location_id)
  nirv_stats <- calc_nirv_stats(location_ids, start, end)
  #temporal_stats <- calc_temporal_stats(location_ids, start, end)
  #spatial_stats <- calc_spatial_stats(location_ids, start, end)
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

# harmonics function
calc_temporal_stats <- function(location_ids) {
  
}

# location features function
calc_spatial_stats <- function(location_ids) {
  
}
