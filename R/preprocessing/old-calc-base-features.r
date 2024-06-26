source("./R/utils/utils.r")

library(parallel)
library(sf)
library(tibble)
library(zoo)

old_calc_base_features <- function(reference_data, start, end) {
  VI_names = c("NIRv")
  
  l <- expand.grid(VIname=VI_names, segment_size=6, stringsAsFactors=FALSE)
  RollingStats = do.call(mcmapply, c(FUN=calc_segments_sums_of_changes, as.list(l), start=start, end=end, mc.cores=10))
  
  # Creates a list of data frames of VIs.
  VIs <- split_matrix(RollingStats)
  
  return(VIs)
  
  # Calculates the yearly change of the three proposed metrics per VI.
  VI_metrics <- lapply(VIs, calc_yearly_change_stats)
  
  base_features <- reference_data %>%
    distinct(location_id, is_change)
  
  # Adds all columns including a prefix with the VI name.
  for (i in seq_along(VI_metrics)) {
    base_features <- add_columns(base_features, VI_metrics[[i]], names(VI_metrics)[i])
  }
  
  return(base_features)
}

calc_segments_sums_of_changes <- function(VIname, start, end, segment_size=6, InFile="./data/global/processed/temporal_indices/") {
  VI = st_read(paste0(InFile, VIname, ".gpkg"))
  VI = VI %>%
    arrange(location_id)
  
  VIzoo = SFToZoo(VI)
  VIzoo = window(VIzoo, start=start, end=end)
  VI_stat = rollapply(VIzoo, width=segment_size, calc_segment_sum_of_change, by=segment_size, partial=TRUE, align="left")
  VI_stat = as.matrix(VI_stat) # Workaround for a bug in zoo
  
  return(VI_stat)
}

calc_segment_sum_of_change <- function(values_with_dates) {
  values = unname(values_with_dates)
  values = sum(!is.na(values))
  return(values)
  
  if (length(values) < 2)
  {
    sum = NA
  } else {
    sum = sum(abs(diff(values)))
  }
  
  return(sum)
}

split_matrix <- function(mat, num_segments=8) {
  num_rows <- nrow(mat)
  segment_size <- num_rows / num_segments
  
  # Reshapes the matrix into a 3D array.
  mat_3d <- array(mat, dim=c(nrow(mat), ncol(mat), num_segments))
  
  # Converts each original matrix column to a data frame.
  col_dfs <- lapply(1:ncol(mat), function(j) {
    column_segments <- mat_3d[, j, ]
    colnames(column_segments) <- paste("segment_", 1:num_segments, sep="")
    
    # Reshapes each column segment to have the same number of rows and same number of columns.
    reshaped_segment <- matrix(column_segments, nrow=segment_size, ncol=num_segments, byrow=TRUE)
    
    as.data.frame(reshaped_segment)
  })
  
  names(col_dfs) <- colnames(mat)
  
  return(col_dfs)
}

calc_yearly_change_stats <- function(df) {
  num_columns <- ncol(df)
  year_length <- num_columns/2
  
  # Splits the data frame into two halves vertically.
  year_1 <- df[, 1:year_length]
  year_2 <- df[, (year_length + 1):num_columns]
  
  year_1_means <- apply(year_1, 1, safe_mean)
  year_1_mins <- apply(year_1, 1, safe_min)
  year_1_maxs <- apply(year_1, 1, safe_max)
  
  year_2_means <- apply(year_2, 1, safe_mean)
  year_2_mins <- apply(year_2, 1, safe_min)
  year_2_maxs <- apply(year_2, 1, safe_max)
  
  yearly_change_mean <- year_1_means - year_2_means
  yearly_change_min <- year_1_mins - year_2_mins
  yearly_change_max <- year_1_maxs - year_2_maxs
  
  result <- data.frame(
    yearly_change_mean=yearly_change_mean,
    yearly_change_min=yearly_change_min,
    yearly_change_max=yearly_change_max
  )
  
  return(result)
}

add_columns <- function(df1, df2, prefix) {
  colnames(df2) <- paste(prefix, colnames(df2), sep="_")
  
  df <- cbind(df1, df2)
}
