library(caret)
library(treeshap)

# Preprocessing imports
source("./R/preprocessing/calc-base-features.r")
source("./R/preprocessing/calc-extra-features.r")
source("./R/preprocessing/calc-temporal-indices.r")
source("./R/preprocessing/condense-reference-data.r")
source("./R/preprocessing/load-reference-data.r")
source("./R/preprocessing/load-surface-reflectances.r")
source("./R/preprocessing/xu-functions.r")

# Processing imports
source("./R/processing/assess-errors.r")
source("./R/processing/calc-error-shaps.r")
source("./R/processing/calc-performance-metrics.r")
source("./R/processing/derive-errors.r")
source("./R/processing/train-random-forest.r")

# Visualisation imports
source("./R/visualisation/visualise-errors.r")

source("./R/utils/utils.r")

# Select targeted date range (2 years range)
# Dates must be between 2015-01-01 and 2018-12-31
START = as.Date("2016-07-01")
END = as.Date("2018-6-30")


################################### GLOBAL #####################################
#### PREPROCESSING ####
reference_data <- load_reference_data()

# Filtering based on methods of Xu et al. (2022).
# Removes sites with only fraction change between 0 and 70 in any of the years.
reference_data <- filter_changes(reference_data)
reference_data <- remove_sites_with_breaks(reference_data, start=START, end=END)

reference_data <- assign_lcc_categories(reference_data)
reference_data_condensed <- condense_global_data(reference_data)

# Writes the VI values to files in ./data/global/processed/temporal_indices
global_SRs <- load_SRs(reference_data_condensed)
global_indices <- calc_temporal_indices(global_SRs)

# Creates time series objects with the specified range.
global_indices_ts <- lapply(global_indices, SFToZoo)
global_indices_ts <- lapply(global_indices_ts, window, start=START, end=END)

# Calculating the features for the base model.
base_features <- calc_base_features(reference_data_condensed, global_indices_ts$NIRv)
base_features <- na.omit(base_features)

# Calculating the extra features for the full model.
extra_features <- calc_extra_features(reference_data_condensed, global_indices_ts)
full_features <- merge(base_features, extra_features)
full_features <- na.omit(full_features)



#### MODELS ####
# Base random forest model.
base_rf_models <- train_rf(base_features, k=10)
base_rf_metrics <- sapply(base_rf_models, calc_performance_metrics)
print(rowMeans(base_rf_metrics))

# Full random forest model.
full_rf_models <- train_rf(full_features, k=10)
full_rf_metrics <- sapply(full_rf_models, calc_performance_metrics)
print(rowMeans(full_rf_metrics))



#### ERROR EXPLORATION ####
global_errors <- lapply(full_rf_models, derive_errors)
global_errors <- do.call(rbind, global_errors)
global_errors_shaps <- lapply(full_rf_models, calc_error_shaps, full_features)

# commission errors = 100 - user accuracy
# errors where the class was predicted as change but was not in reality.
global_com_errors <- assess_errors(
  global_errors[global_errors$error_type == "commission", ], 
  global_errors_shaps, 
  full_features,
  global_indices_ts)
global_com_errors_sf <- st_as_sf(x=global_com_errors[global_com_errors$is_drawn == T, ],
                                coords=c("centroid_x", "centroid_y"),
                                crs="WGS84")
st_write(global_com_errors_sf, "results/global_com_errors.kml")


# omission errors = 100 - producer's accuracy
# errors where the class was not predicted as change but was a change in reality.
global_om_errors <- assess_errors(
  global_errors[global_errors$error_type == "omission", ], 
  global_errors_shaps, 
  full_features,
  global_indices_ts)
global_om_errors_sf <- st_as_sf(x=global_om_errors[global_om_errors$is_drawn == T, ],
                         coords=c("centroid_x", "centroid_y"),
                         crs="WGS84")
st_write(global_om_errors_sf, "results/global_om_errors.kml")

#### VISUALISATION ####
visualise_errors(global_com_errors, global_om_errors)





################################### BRAZIL #####################################
#### PREPROCESSING ####
brazil <- T
brazil_reference_data <- read.csv("./data/brazil/raw/brazil_reference_data.csv")
names(brazil_reference_data)[names(brazil_reference_data) == "TARGETID"] <- "location_id"
brazil_reference_data$is_change <- factor(brazil_reference_data$is_change, 
                                          levels = c(0, 1), 
                                          labels = c("No Change", "Change"))

brazil_reference_data <- remove_sites_with_breaks(brazil_reference_data, start=START, end=END, brazil)

brazil_reference_data_condensed <- condense_brazil_data(brazil_reference_data)

# Writes the VI values to files in ./data/global/processed/temporal_indices
brazil_SRs <- load_SRs(brazil_reference_data_condensed, brazil)
brazil_indices <- calc_temporal_indices(brazil_SRs, brazil)

# Creates time series objects with the specified range.
brazil_indices_ts <- lapply(brazil_indices, SFToZoo)
brazil_indices_ts <- lapply(brazil_indices_ts, window, start=START, end=END)

brazil_base_features <- calc_base_features(brazil_reference_data_condensed, brazil_indices_ts$NIRv, brazil)
brazil_base_features <- na.omit(brazil_base_features)

brazil_extra_features <- calc_extra_features(brazil_reference_data_condensed, brazil_indices_ts)
brazil_full_features <- merge(brazil_base_features, brazil_extra_features)
brazil_full_features <- na.omit(brazil_full_features)

#### MODELS ####
brazil_preds <- predict(full_rf, newdata=brazil_full_features)
brazil_conf_matrix <- confusionMatrix(brazil_preds, brazil_full_features$is_change)$table
print(calc_performance_metrics(brazil_conf_matrix))

#### ERROR EXPLORATION ####
brazil_errors_features <- brazil_full_features[brazil_preds != brazil_full_features$is_change, ]
unified_brazil <- custom.unify(full_rf$finalModel, brazil_full_features)
treeshap_brazil <- treeshap(unified_brazil, brazil_errors_features)
plot_feature_importance(treeshap_brazil)

brazil_errors <- brazil_reference_data_condensed[brazil_reference_data_condensed$location_id %in% brazil_errors_features$location_id, ]
brazil_com_errors <- assess_errors(unified_brazil, brazil_errors, "omission", global_indices_ts)
