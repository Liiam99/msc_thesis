library(dplyr)
library(parallel)
library(tibble)

source("./R/preprocessing/xu-functions.r")
source("./R/utils/load-sampling-data.r")
source("./R/utils/utils.r")

load_data <- function() {
  IIASA_path <- "./data/global/raw/IIASA_reference_data.csv"
  
  WUR_change_path <- "./data/global/raw/ref_change_4year.csv"
  WUR_nochange_path <-"./data/global/raw/ref_nochange_4year_additional.csv"
  
  # Burned locations from Google Earth Engine.
  IIASA_burned_path <- "./data/global/raw/IIASA_burned_sample_ids.csv"
  WUR_burned_path <- "./data/global/raw/WUR_burned_location_ids.csv"
  
  # Loads and cleans up the IIASA data set.
  IIASA <- read.csv(IIASA_path)
  IIASA <- TidyData(IIASA)
  
  # Combines the no-changes and the changes into one WUR data set.
  WUR_change <- read.csv(WUR_change_path)
  WUR_change <- subset(WUR_change, select= -c(incl.p_com, des_weight))
  WUR_nochange <- read.csv(WUR_nochange_path)
  WUR <- rbind(WUR_change, WUR_nochange)
  rm(WUR_change, WUR_nochange)
  
  # Renames and cleans the WUR data.
  WUR <- RenameReferenceData(WUR)
  WUR <- add_column(WUR, wetland_herbaceous=0, .after="water")
  WUR <- TidyData(WUR)
  
  # Fuses both data sets together into one global reference data set.
  common_cols <- intersect(names(IIASA), names(WUR))
  IIASA_subset <- IIASA[common_cols]
  WUR_subset <- WUR[common_cols]
  reference_data <- merge(IIASA_subset, WUR_subset, all=T)
  reference_data <- subset(reference_data, select= -burnt)
  rm(IIASA, IIASA_subset, WUR, WUR_subset)
  
  # Removes the points labelled burned anywhere from 2015 to 2018.
  IIASA_burned <- read.csv(IIASA_burned_path)
  WUR_burned <- read.csv(WUR_burned_path)
  reference_data <- reference_data[!reference_data$sample_id %in% IIASA_burned$sample_id, ]
  reference_data <- reference_data[!reference_data$location_id %in% WUR_burned$location_id, ]
  rm(IIASA_burned, WUR_burned)
  
  reference_data <- filter_changes(reference_data)
}

calc_base_features <- function() {
  VI_names = c("NDVI", "NIRv", "NDMI", "EVI", "MNDWI")
  
  l <- expand.grid(VIname=VI_names, segment_size=6, stringsAsFactors=FALSE)
  RollingStats = do.call(mcmapply, c(FUN=calc_segments_sums_of_changes, as.list(l), mc.cores=10))

  # Creates a list of dataframes of VIs.
  VIs <- split_matrix(RollingStats)
  
  # Calculates the yearly change of the three proposed metrics per VI.
  VI_metrics <- lapply(VIs, calculate_yearly_change_stats)
  
  base_features <- reference_data %>%
    distinct(location_id, is_change)

  # Adds all columns including a prefix with the VI name.
  for (i in seq_along(VI_metrics)) {
    base_features <- add_columns(base_features, names(VI_metrics)[i], VI_metrics[[i]])
  }
  
  return(base_features)
}

calc_segments_sums_of_changes <- function(VIname=VIs, segment_size=6, OutFile="./data/global/processed/temporal_indices/") {
  OutName = paste0(OutFile, "_", paste(VIname, segment_size, sep="_"), ".gpkg")
  VIzoo = SFToZoo(st_read(paste0(OutFile, VIname, ".gpkg")))
  VI_stat = rollapply(VIzoo, width=segment_size, calc_segment_sum_of_change, by=segment_size, partial=TRUE, align="left")
  VI_stat = as.matrix(VI_stat) # Workaround for a bug in zoo
  
  return(VI_stat)
}

calc_segment_sum_of_change <- function(values_with_dates) {
  values = unname(values_with_dates)
  values = na.omit(values)
  
  if (length(values) < 2)
  {
    sum = NA
  } else {
    sum = sum(diff(values))
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

calculate_yearly_change_stats <- function(df) {
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
  
  # Some EVI calculations can produce Inf values.
  result[sapply(result, is.infinite)] <- NA
  
  return(result)
}

add_columns <- function(df1, prefix, df2) {
  colnames(df2) <- paste(prefix, colnames(df2), sep="_")
  
  df <- cbind(df1, df2)
}

safe_mean <- function(x) ifelse(all(is.na(x)), NA, mean(x, na.rm=TRUE))
safe_max <- function(x) ifelse(all(is.na(x)), NA, max(x, na.rm=TRUE))
safe_min <- function(x) ifelse(all(is.na(x)), NA, min(x, na.rm=TRUE))
