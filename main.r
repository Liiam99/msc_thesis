library(caret)

# Preprocessing imports
source("./R/preprocessing/calc-base-features.r")
source("./R/preprocessing/calc-extra-features.r")
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

# Calculating the features for the base model.
base_features <- calc_base_features(start=START, end=END)
base_features <- na.omit(base_features)

# Calculating the extra features for the full model.
location_ids <- unique(base_features$location_id)
extra_features <- calc_extra_features(reference_data, location_ids, start=START, end=END)
full_features <- merge(base_features, extra_features)
full_features <- na.omit(full_features)

#### MODELS ####
train_control <- trainControl(method="cv", number=10, sampling="up")

# Base Randomforest model.
rf <- train(is_change ~ . - location_id, data=base_features, trControl=train_control, ntree=128)
base_conf <- confusionMatrix(rf)$table
confusionMatrix(base_conf, positive="Change", mode="everything")

# Full Randomforest model.
rf <- train(is_change ~ . - location_id, data=full_features, trControl=train_control, ntree=128)
full_conf <- confusionMatrix(rf)$table
confusionMatrix(full_conf, positive="Change", mode="everything")

