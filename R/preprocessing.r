library(dplyr)
library(tibble)

source("./postprocessing/load-sampling-data.r")

#### Loading and combining the data. ####

IIASA_path <- file.path("..", "data", "global", "IIASA_reference_data.csv")

WUR_change_path <- file.path("..", "data", "global", "ref_change_4year.csv")
WUR_nochange_path <- file.path("..", "data", "global", "ref_nochange_4year_additional.csv")

# Burned locations from Google Earth Engine.
IIASA_burned_path <- file.path("..", "data", "global", "IIASA_burned_sample_ids.csv")
WUR_burned_path <- file.path("..", "data", "global", "WUR_burned_location_ids.csv")

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

# Removes the points labelled burned anywhere from 2015 to 2018.
IIASA_burned <- read.csv(IIASA_burned_path)
WUR_burned <- read.csv(WUR_burned_path)
IIASA <- IIASA[!IIASA$sample_id %in% IIASA_burned$sample_id, ]
WUR <- WUR[!WUR$location_id %in% WUR_burned$location_id, ]

# Fuses both data sets together into one global reference data set.
common_cols <- intersect(names(IIASA), names(WUR))
IIASA_subset <- IIASA[common_cols]
WUR_subset <- WUR[common_cols]
reference_data <- merge(IIASA_subset, WUR_subset, all=T)
reference_data <- subset(reference_data, select= -burnt)



#### Change and no-change determination. ####

# Calculates the differences between subsequent years for each class and each location.
classes = GetCommonClassNames()
fraction_changes <- reference_data %>%
  group_by(location_id) %>%
  arrange(reference_year) %>%
  reframe(across(all_of(classes), ~c(diff(.), NA), .names = "{col}")) %>%
  na.omit() # Removes the fourth row of differences as there can only be 3 between 4 years.

# Determines for each location if there is a change (1), no-change (0) or 
# in-between (NA) according to the definitions.
location_changes <- fraction_changes %>%
  group_by(location_id) %>%
  filter(n() == 3) %>% # Some locations do not have four years of data.
  mutate(
    is_change = ifelse(
      all(across(-1, ~. == 0)),  # All yearly total fraction changes are 0 for one location.
        0,
        ifelse(
          any(rowSums(abs(across(-1)), na.rm = TRUE) >= 70),  # At least one total yearly fraciton difference is >= 70
          1,
          NA
        )
    )
  ) %>%
  select(location_id, is_change) %>%
  distinct()

reference_data <- merge(reference_data, location_changes)
reference_data <- reference_data[complete.cases(reference_data$is_change), ]



#### Removing breaks outside targeted date range. ####
# https://github.com/GreatEmerald/supervised-bfast/blob/main/src/015_preprocess_dense/00_CalcTemporalIndices.r
# Copy the code but remove the features I don't want and add the VIs that should be added.
# ALl data point, no convolutions!

# Remove the unused points from the GPKG?

# https://github.com/GreatEmerald/supervised-bfast/blob/main/src/030_bfast/20_bfast-on-vis.r
