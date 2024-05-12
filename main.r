library(dplyr)
library(sf)
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
source("./R/processing/calc-errors-shaps.r")
source("./R/processing/calc-performance-metrics.r")
source("./R/processing/derive-errors.r")
source("./R/processing/train-random-forest.r")

# Visualisation imports
source("./R/visualisation/plot-feature-importance-mod.r")
source("./R/visualisation/visualise-class-changes.r")
source("./R/visualisation/visualise-obs-counts.r")
source("./R/visualisation/visualise-pred-probs.r")

# Utilities
source("./R/utils/custom-unify.r")
source("./R/utils/utils.r")

# Select targeted date range (2 years range)
# Dates must be between 2015-01-01 and 2018-12-31
START = as.Date("2016-07-01")
END = as.Date("2018-6-30")



# GLOBAL -----------------------------------------------------------------------
#### PREPROCESSING ####
reference_data <- load_reference_data()
nrow(reference_data)

# Burned locations from Google Earth Engine.
IIASA_burned_path <- "./data/global/raw/IIASA_burned_sample_ids.csv"
WUR_burned_path <- "./data/global/raw/WUR_burned_location_ids.csv"

# The thee preprocessing methods based on methods of Xu et al. (2022):
  # 1. Removes the points labelled burned anywhere from 2015 to 2018.
IIASA_burned <- read.csv(IIASA_burned_path)
WUR_burned <- read.csv(WUR_burned_path)
reference_data <- reference_data[!reference_data$sample_id %in% IIASA_burned$sample_id, ]
reference_data <- reference_data[!reference_data$location_id %in% WUR_burned$location_id, ]
  # 2. Removes sites with only fraction change between 0 and 70 in any of the years.
reference_data <- filter_changes(reference_data)
  # 3. Removes sites with breaks detected outside the targeted date range.
reference_data <- remove_sites_with_breaks(reference_data, start=START, end=END)

reference_data <- reference_data %>%
  group_by(location_id) %>%
  mutate(
    from = case_when(
      reference_year == 2015 ~ dominant_lc,
      TRUE ~ NA_character_ # If none of the conditions are met, assign NA
    ),
    to = case_when(
      reference_year == 2018 ~ dominant_lc,
      TRUE ~ NA_character_ # If none of the conditions are met, assign NA
    )
  ) %>%
  ungroup()

reference_data_condensed <- condense_global_data(reference_data)
reference_data_condensed <- assign_lcc_categories(reference_data_condensed)

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
base_rf_results <- train_rf(base_features, k=10)
base_rf_metrics <- sapply(base_rf_results, calc_performance_metrics)
print(rowMeans(base_rf_metrics))

# Full random forest model.
full_rf_results <- train_rf(full_features, k=10)
full_rf_metrics <- sapply(full_rf_results, calc_performance_metrics)
print(rowMeans(full_rf_metrics))

# Calculates for each result the metrics per category and then calculates the
# average across these results.
global_lcc_metrics <- lapply(full_rf_results, calc_lcc_metrics, reference_data_condensed)
global_lcc_metrics <- Reduce("+", global_lcc_metrics)/length(global_lcc_metrics)



#### ERROR EXPLORATION ####
global_errors <- lapply(full_rf_results, derive_errors)
global_errors <- do.call(rbind, global_errors)
global_errors$shap_idx <- 1:nrow(global_errors)
global_errors$location_id <- full_features[global_errors$val_idx, "location_id"]
global_errors <- merge(reference_data_condensed, global_errors, by="location_id")
global_errors_shaps <- lapply(full_rf_results, calc_errors_shaps, full_features)
global_errors_shaps <- do.call(rbind, global_errors_shaps)

global_shaps <- lapply(full_rf_results, function(result, features) {
  val_features <- features[result$val_idx, ]
  
  unified <- custom.unify(result$model, features)
  treeshap_obj <- treeshap(unified, val_features)
  
  return(treeshap_obj$shaps)
  }, full_features)
global_shaps <- do.call(rbind, global_shaps)

# commission errors = 100 - user accuracy
# errors where the class was predicted as change but was not in reality.
global_com_errors <- assess_errors(
  global_errors[global_errors$error_type == "commission", ], 
  global_errors_shaps, 
  full_features,
  global_indices_ts
)

# For further analysis in QGIS.
global_com_errors_sf <- st_as_sf(x=global_com_errors,
                                 coords=c("centroid_x", "centroid_y"),
                                 crs="WGS84")
st_write(global_com_errors_sf, "results/global_com_errors.gpkg", append=F)

# For further analysis in Google Earth Pro.
global_com_errors_sf <- st_as_sf(x=global_com_errors[global_com_errors$is_drawn == T, ],
                                 coords=c("centroid_x", "centroid_y"),
                                 crs="WGS84")
st_write(global_com_errors_sf, "results/20_global_com_errors.kml", append=F)

# omission errors = 100 - producer's accuracy
# errors where the class was not predicted as change but was a change in reality.
global_om_errors <- assess_errors(
  global_errors[global_errors$error_type == "omission", ], 
  global_errors_shaps, 
  full_features,
  global_indices_ts
)

# For further analysis in QGIS.
global_om_errors_sf <- st_as_sf(x=global_om_errors,
                                coords=c("centroid_x", "centroid_y"),
                                crs="WGS84")
st_write(global_om_errors_sf, "results/global_om_errors.gpkg", append=F)

# For further analysis in Google Earth Pro.
global_om_errors_sf <- st_as_sf(x=global_om_errors[global_om_errors$is_drawn == T, ],
                                coords=c("centroid_x", "centroid_y"),
                                crs="WGS84")
st_write(global_om_errors_sf, "results/20_global_om_errors.kml", append=F)



# BRAZIL -----------------------------------------------------------------------
#### PREPROCESSING ####
brazil <- T
brazil_reference_data <- read.csv("./data/brazil/raw/brazil_reference_data.csv")
names(brazil_reference_data)[names(brazil_reference_data) == "TARGETID"] <- "location_id"
brazil_reference_data$is_change <- factor(brazil_reference_data$is_change, 
                                          levels=c(0, 1), 
                                          labels=c("NoChange", "Change"))

brazil_reference_data <- remove_sites_with_breaks(brazil_reference_data, start=START, end=END, brazil)

brazil_reference_data_condensed <- condense_brazil_data(brazil_reference_data)
brazil_reference_data_condensed <- na.omit(brazil_reference_data_condensed)
brazil_reference_data_condensed <- assign_lcc_categories(brazil_reference_data_condensed)

# Writes the spectral index values to files in ./data/global/processed/temporal_indices
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
best_model <- full_rf_results[[which.max(full_rf_metrics["F1_change", ])]]$model
brazil_pred <- predict(best_model, newdata=brazil_full_features)
brazil_prob_pred <- predict(best_model, newdata=brazil_full_features, type="prob")

brazil_result <- data.frame(pred=brazil_pred, 
                            obs=brazil_full_features$is_change,
                            val_idx=1:nrow(brazil_full_features),
                            val_location_id=brazil_full_features$location_id)
brazil_metrics <- calc_performance_metrics(brazil_result)
brazil_lcc_metrics <- calc_lcc_metrics(brazil_result, brazil_reference_data_condensed)



#### ERROR EXPLORATION ####
brazil_errors_idx <- brazil_result$pred != brazil_result$obs
brazil_unified <- custom.unify(best_model, brazil_full_features)
brazil_errors_shaps <- treeshap(brazil_unified, brazil_full_features[brazil_errors_idx, ])$shaps

brazil_errors <- brazil_result[brazil_errors_idx, ]
brazil_errors$error_type <- ifelse(brazil_errors$pred == "Change", "commission", "omission")
brazil_errors$location_id <- brazil_full_features[brazil_errors_idx, "location_id"]
brazil_errors$shap_idx <- 1:nrow(brazil_errors)
brazil_errors$Change <- brazil_prob_pred[brazil_errors_idx, "Change"]
brazil_errors$NoChange <- brazil_prob_pred[brazil_errors_idx, "NoChange"]
brazil_errors <- merge(brazil_reference_data_condensed, brazil_errors, by="location_id")

brazil_com_errors <- assess_errors(
  brazil_errors[brazil_errors$error_type == "commission", ], 
  brazil_errors_shaps, 
  brazil_full_features,
  brazil_indices_ts
)

# For further analysis in QGIS.
brazil_com_errors_sf <- st_as_sf(x=brazil_com_errors,
                                 coords=c("centroid_x", "centroid_y"),
                                 crs="WGS84")
st_write(brazil_com_errors_sf, "results/brazil_com_errors.gpkg", append=F)

# For further analysis in Google Earth Pro.
brazil_com_errors_sf <- st_as_sf(x=brazil_com_errors[brazil_com_errors$is_drawn == T, ],
                                 coords=c("centroid_x", "centroid_y"),
                                 crs="WGS84")
st_write(brazil_com_errors_sf, "results/20_brazil_com_errors.kml", append=F)

brazil_om_errors <- assess_errors(
  brazil_errors[brazil_errors$error_type == "omission", ], 
  brazil_errors_shaps, 
  brazil_full_features,
  brazil_indices_ts
)

# For further analysis in QGIS.
brazil_om_errors_sf <- st_as_sf(x=brazil_om_errors,
                                 coords=c("centroid_x", "centroid_y"),
                                 crs="WGS84")
st_write(brazil_om_errors_sf, "results/brazil_om_errors.gpkg", append=F)

# For further analysis in Google Earth Pro.
brazil_om_errors_sf <- st_as_sf(x=brazil_om_errors[brazil_om_errors$is_drawn == T, ],
                                 coords=c("centroid_x", "centroid_y"),
                                 crs="WGS84")
st_write(brazil_om_errors_sf, "results/20_brazil_om_errors.kml", append=F)

visualise_class_changes(brazil_reference_data_condensed)



# VISUALISATION ----------------------------------------------------------------
# Exports to create the overview maps showing the number of changes and classes.
global_points <- st_as_sf(x=reference_data_condensed,
                          coords=c("centroid_x", "centroid_y"),
                          crs="WGS84")
st_write(global_points, "results/global_points.gpkg", append=F)
regional_points <- st_as_sf(x=brazil_reference_data_condensed,
                          coords=c("centroid_x", "centroid_y"),
                          crs="WGS84")
st_write(regional_points, "results/regional_points.gpkg", append=F)

#### Sankey diagrams showing the class transitions in the datasets.####
visualise_class_changes(reference_data_condensed)
visualise_class_changes(brazil_reference_data_condensed)

#### Sankey diagrams showing the class transitions of the omission errors.####
visualise_class_changes(global_om_errors)
visualise_class_changes(brazil_om_errors)

#### Histograms of the relative frequency of the change probability prediction values per result type ####
global_preds <- lapply(full_rf_results, function(result) {
  result <- data.frame(
    pred=result$pred,
    obs=result$obs,
    NoChange=result$prob_pred[, "NoChange"],
    Change=result$prob_pred[, "Change"],
    location_id=result$val_location_id
  )}
)
global_preds <- bind_rows(global_preds)
visualise_pred_probs(global_preds, "")

brazil_preds <- data.frame(
  pred=brazil_pred,
  obs=brazil_full_features$is_change,
  NoChange=brazil_prob_pred[, "NoChange"],
  Change=brazil_prob_pred[, "Change"],
  location_id=brazil_full_features$location_id
)
visualise_pred_probs(brazil_preds, "")

#### Examples of time series ####
par(cex=2)
plot(global_indices_ts$NIRv[, names(global_indices_ts$NIRv) == 2768185], ylab="NIRv", main="NIRv time series of a true positive", xlab="Date", sub="Change probability prediction value = 1")
plot(global_indices_ts$NIRv[, names(global_indices_ts$NIRv) == 2781020], ylab="NIRv", main="NIRv time series of a false negative", xlab="Date", sub="Change probability prediction value = 0")
