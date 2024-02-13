source("./R/utils/utils.r")

library(parallel)
library(sf)
library(zoo)


calc_features <- function(reference_data, names=c("NDVI", "NIRv", "NDMI", "EVI", "MNDWI"), start, end) {
  yearly_change_stats <- lapply(names, FUN=calc_metrics, start=start, end=end)
  yearly_change_stats <- plyr::join_all(yearly_change_stats, by="location_id")
  
  features <- reference_data %>%
    distinct(location_id, is_change)
  features <- merge(features, yearly_change_stats)
}

calc_metrics <- function(VIname, start, end, InFile="./data/global/processed/temporal_indices/") {
  VI <- st_read(paste0(InFile, VIname, ".gpkg"), quiet=T)
  VI <- VI %>%
    arrange(location_id)
  VIzoo <- SFToZoo(VI)
  
  year_cutoff_date <- ((end - start)/2) + start
  VIzoo_year_1 <- window(VIzoo, start=start, end=year_cutoff_date)
  VIzoo_year_2 <- window(VIzoo, start=year_cutoff_date, end=end)
  
  # Relative yearly change in VI minimum (10th percentile).
  min_year_1 <- apply(VIzoo_year_1, 2, quantile, probs=0.1, na.rm=T, names=F)
  min_year_2 <- apply(VIzoo_year_2, 2, quantile, probs=0.1, na.rm=T, names=F)
  rel_yearly_min_change <- (min_year_2 - min_year_1)/min_year_1

  # Relative yearly change in VI maximum (90th percentile).
  max_year_1 <- apply(VIzoo_year_1, 2, quantile, probs=0.9, na.rm=T, names=F)
  max_year_2 <- apply(VIzoo_year_2, 2, quantile, probs=0.9, na.rm=T, names=F)
  rel_yearly_max_change <- (max_year_2 - max_year_1)/max_year_1
  
  # Relative yearly change in VI median.
  year_1_median <- apply(VIzoo_year_1, 2, safe_median)
  year_2_median <- apply(VIzoo_year_2, 2, safe_median)
  rel_yearly_median_change <- (year_2_median - year_1_median)/year_1_median
  
  VIzoo <- window(VIzoo, start=start, end=end)
  VI_min <- apply(VIzoo, 2, quantile, probs=0.1, na.rm=T, names=F)
  VI_max <- apply(VIzoo, 2, quantile, probs=0.9, na.rm=T, names=F)
  VI_mean <- apply(VIzoo, 2, safe_mean)
  VI_var <- apply(VIzoo, 2, var, na.rm=T)
  
  metrics <- list(rel_yearly_min_change=rel_yearly_min_change, 
                  rel_yearly_max_change=rel_yearly_max_change, 
                  rel_yearly_median_change=rel_yearly_median_change,
                  min=VI_min,
                  max=VI_max,
                  mean=VI_mean,
                  var=VI_var)
  
  for (i in seq_along(metrics)) {
    metric <- metrics[[i]]
    metric_df <- ZooToDf(metric, VIname, names(metrics)[i])
    metrics[[i]] <- metric_df
  }
  
  # Creates one data frame with each metrics as a column.
  metrics <- plyr::join_all(metrics, by="location_id")
  
  metrics[sapply(metrics, is.infinite)] <- NA
  
  return(metrics)
}

ZooToDf <- function(zoo_obj, VIname, df_name) {
  df <- data.frame(
    location_id=names(zoo_obj), 
    zoo_obj, 
    row.names=NULL
  )
  
  colnames(df)[2] <- paste(VIname, df_name, sep="_")
  
  return(df)
}
