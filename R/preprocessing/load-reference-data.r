library(dplyr)
library(tibble)

source("./R/utils/load-sampling-data.r")
source("./R/utils/utils.r")

load_reference_data <- function() {
  IIASA_path <- "./data/global/raw/IIASA_reference_data.csv"
  
  WUR_change_path <- "./data/global/raw/ref_change_4year.csv"
  WUR_nochange_path <-"./data/global/raw/ref_nochange_4year_additional.csv"
  
  # Loads and cleans up the IIASA data set.
  IIASA <- read.csv(IIASA_path)
  IIASA <- TidyData(IIASA)
  
  # Combines the no-changes and the changes into one WUR data set.
  WUR_change <- read.csv(WUR_change_path)
  WUR_change <- subset(WUR_change, select= -c(incl.p_com, des_weight))
  WUR_nochange <- read.csv(WUR_nochange_path)
  WUR <- rbind(WUR_change, WUR_nochange)
  
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
}
