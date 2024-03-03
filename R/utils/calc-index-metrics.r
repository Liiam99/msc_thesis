library(dplyr)
library(sf)
library(zoo)

source("./R/utils/utils.r")

calc_index_metrics <- function(index_ts, index_name) {
  start_date <- start(index_ts)
  end_date <- end(index_ts)
  year_cutoff_date <- start_date + (end_date - start_date) / 2
  index_zoo_year_1 <- window(index_ts, start=start_date, end=year_cutoff_date)
  index_zoo_year_2 <- window(index_ts, start=year_cutoff_date + 1)
  
  # Relative yearly change in index minimum (10th percentile).
  min_year_1 <- apply(index_zoo_year_1, 2, quantile, probs=0.1, na.rm=T, names=F)
  min_year_2 <- apply(index_zoo_year_2, 2, quantile, probs=0.1, na.rm=T, names=F)
  rel_yearly_min_change <- (min_year_2 - min_year_1)/min_year_1
  
  # Relative yearly change in index maximum (90th percentile).
  max_year_1 <- apply(index_zoo_year_1, 2, quantile, probs=0.9, na.rm=T, names=F)
  max_year_2 <- apply(index_zoo_year_2, 2, quantile, probs=0.9, na.rm=T, names=F)
  rel_yearly_max_change <- (max_year_2 - max_year_1)/max_year_1
  
  # Relative yearly change in index median.
  year_1_median <- apply(index_zoo_year_1, 2, safe_median)
  year_2_median <- apply(index_zoo_year_2, 2, safe_median)
  rel_yearly_median_change <- (year_2_median - year_1_median)/year_1_median
  
  # Relative yearly change in IQR.
  year_1_IQR <- apply(index_zoo_year_1, 2, safe_median)
  year_2_IQR <- apply(index_zoo_year_2, 2, safe_median)
  rel_yearly_IQR_change <- (year_2_IQR - year_1_IQR)/year_1_IQR
  
  # Descriptive metrics of the time series as a whole.
  #index_zoo <- window(index, start=start, end=end)
  index_min <- apply(index_ts, 2, quantile, probs=0.1, na.rm=T, names=F)
  index_max <- apply(index_ts, 2, quantile, probs=0.9, na.rm=T, names=F)
  index_mean <- apply(index_ts, 2, safe_mean)
  index_IQR <- apply(index_ts, 2, IQR, na.rm=T)
  
  index_metrics <- list(rel_yearly_min_change=rel_yearly_min_change, 
                        rel_yearly_max_change=rel_yearly_max_change, 
                        rel_yearly_median_change=rel_yearly_median_change,
                        rel_yearly_IQR_change=rel_yearly_IQR_change,
                        min=index_min,
                        max=index_max,
                        mean=index_mean,
                        IQR=index_IQR)
  
  # Converts all metrics from zoo objects to data frames.
  for (i in seq_along(index_metrics)) {
    index_metric <- index_metrics[[i]]
    metric_df <- ZooToDf(index_metric, index_name, names(index_metrics)[i])
    index_metrics[[i]] <- metric_df
  }

  # Creates one data frame by merging all index metric data frames.
  index_metrics <- plyr::join_all(index_metrics, by="location_id")
  
  index_metrics[sapply(index_metrics, is.infinite)] <- NA
  
  return(index_metrics)
}
