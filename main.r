library(caret)

# Preprocessing imports
source("./R/preprocessing/calc-base-features.r")
source("./R/preprocessing/calc-extra-features.r")
source("./R/preprocessing/calc-temporal-indices.r")
source("./R/preprocessing/load-reference-data.r")
source("./R/preprocessing/load-surface-reflectances.r")
source("./R/preprocessing/xu-functions.r")

# Processing imports
source("./R/processing/calc-performance-metrics.r")
source("./R/processing/train-random-forest.r")

# Select targeted date range
# Dates must be between 2015-01-01 and 2018-12-31
START = as.Date("2016-07-01")
END = as.Date("2018-6-30")

#### PREPROCESSING ####
reference_data <- load_reference_data()

# Filtering based on methods of Xu et al. (2022).
# Removes sites with only fraction change between 0 and 70 in any of the years.
reference_data <- filter_changes(reference_data)
reference_data <- remove_sites_with_breaks(reference_data, start=START, end=END)

# Writes the VI values to files in ./data/global/processed/temporal_indices
SRs <- load_SRs(reference_data)
calc_temporal_indices(SRs)

# Calculating the features for the base model.
base_features <- calc_base_features(reference_data, start=START, end=END)
base_features <- na.omit(base_features)

# Calculating the extra features for the full model.
extra_features <- calc_extra_features(reference_data, start=START, end=END)
full_features <- merge(base_features, extra_features)
full_features <- na.omit(full_features)



#### MODELS ####
# Base random forest model.
base_rf <- train_rf(base_features)
base_conf <- confusionMatrix(base_rf)$table
base_rf_performance_metrics <- calc_performance_metrics(base_conf)
base_var_imp <- varImp(base_rf, type=1)
plot(base_var_imp, top=dim(base_var_imp$importance[1]))

# Full random forest model.
full_rf <- train_rf(full_features)
full_conf <- confusionMatrix(full_rf)$table
full_rf_performance_metrics <- calc_performance_metrics(full_conf)
full_var_imp <- varImp(full_rf, type=1)
plot(full_var_imp, top=dim(full_var_imp$importance[1]))



#### VISUALISATION ####
change_distribution <- st_as_sf(x=full_features,
                                coords=c("long", "lat"),
                                crs="WGS84")
st_write(change_distribution, "./change_distribution.gpkg", append=F)
