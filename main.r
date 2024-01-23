library(caret)
library(smotefamily)

# Preprocessing imports
source("./R/preprocessing/calc-base-features.r")
source("./R/preprocessing/calc-temporal-indices.r")
source("./R/preprocessing/load-reference-data.r")
source("./R/preprocessing/load-surface-reflectances.r")
source("./R/preprocessing/xu-functions.r")

# Select targeted date range
# Dates must be between 2015-01-01 and 2018-12-31
START = as.Date("2016-07-01")
END = as.Date("2018-6-30")

#### PREPROCESSING ####
reference_data <- load_reference_data()

# Filtering based on methods of Xu et al. (2022)
# Removes sites with only fraction change between 0 and 70 in any of the years.
reference_data <- filter_changes(reference_data)
reference_data <- remove_sites_with_breaks(reference_data, start=START, end=END)

# Writes the indices' values to files in ./data/global/processed/temporal_indices
SRs <- load_SRs(reference_data)
calc_temporal_indices(SRs)

base_features <- calc_base_features(start=START, end=END)
base_features <- na.omit(base_features)

# Performs oversampling on the number of changes as there are many more No Changes
#   than changes in the dataset.
oversampled_data <- SMOTE(select(base_features, c=-(is_change)), base_features$is_change)$data
oversampled_data$is_change <- as.factor(as.factor(oversampled_data$class))
oversampled_data <- subset(oversampled_data, select=-c(class))
