# Preprocessing imports
source("./R/preprocessing/calc-base-features.r")
source("./R/preprocessing/calc-extra-features.r")
source("./R/preprocessing/calc-temporal-indices.r")
source("./R/preprocessing/load-reference-data.r")
source("./R/preprocessing/load-surface-reflectances.r")
source("./R/preprocessing/xu-functions.r")

# Processing imports
source("./R/processing/calc-performance-metrics.r")
source("./R/processing/explore-errors.r")
source("./R/processing/train-random-forest.r")

source("./R/utils/custom-unify.r")
source("./R/utils/utils.r")
library(treeshap)

# Select targeted date range (2 years range)
# Dates must be between 2015-01-01 and 2018-12-31
START = as.Date("2016-07-01")
END = as.Date("2018-6-30")

#### PREPROCESSING ####
reference_data <- load_reference_data()

# Filtering based on methods of Xu et al. (2022).
# Removes sites with only fraction change between 0 and 70 in any of the years.
reference_data <- filter_changes(reference_data)
reference_data <- remove_sites_with_breaks(reference_data, start=START, end=END)

reference_data <- assign_lcc_categories(reference_data)

library(dplyr)
# Condenses the data frame into one row per location, ignoring the years and fractions.
reference_data_condensed <- reference_data %>%
  select(location_id, sample_id, centroid_x, centroid_y, is_change, from, to) %>%
  group_by(location_id) %>%
  summarise(across(everything(), ~ if(all(is.na(.))) NA else first(.[!is.na(.)]))) %>%
  ungroup()

# Writes the VI values to files in ./data/global/processed/temporal_indices
SRs <- load_SRs(reference_data_condensed)
indices <- calc_temporal_indices(SRs)

# Creates time series objects with the specified range.
indices_ts <- lapply(indices, SFToZoo)
indices_ts <- lapply(indices_ts, window, start=START, end=END)

# Calculating the features for the base model.
base_features <- calc_base_features(reference_data_condensed, indices_ts$NIRv)
base_features <- na.omit(base_features)

# Calculating the extra features for the full model.
extra_features <- calc_extra_features(reference_data_condensed, indices_ts)
full_features <- merge(base_features, extra_features)
full_features <- na.omit(full_features)



#### MODELS ####
# Base random forest model.
base_rf <- train_rf(base_features)
base_rf_performance_metrics <- calc_performance_metrics(base_rf)
print(base_rf_performance_metrics)

# Full random forest model.
full_rf <- train_rf(full_features)
full_rf_performance_metrics <- calc_performance_metrics(full_rf)
print(full_rf_performance_metrics)



#### ERROR EXPLORATION ####
errors <- full_rf$pred[full_rf$pred$pred != full_rf$pred$obs, ]
errors$location_id <- full_rf$trainingData$location_id[errors$rowIndex]
errors <- errors[, !colnames(errors) %in% c("mtry", "Resample")]
errors <- merge(reference_data_condensed, errors, by="location_id")

unified <- custom.unify(full_rf$finalModel, full_rf$trainingData)
treeshap_object <- treeshap(unified, features[errors$rowIndex, ])

plot_feature_importance(treeshap_object)

# commission errors = 100 - user accuracy
# errors where the class was predicted as change but was not in reality.
com_errors <- assess_errors(unified, errors, "commission", indices_ts)

com_errors_sf <- st_as_sf(x=com_errors,
                          coords=c("centroid_x", "centroid_y"),
                          crs="WGS84")
st_write(com_errors_sf, "com_errors.kml")

# omission errors = 100 - producer's accuracy
# errors where the class was not predicted as change but was a change in reality.
om_errors <- assess_errors(unified, errors, "omission", indices_ts)

om_errors_sf <- st_as_sf(x=om_errors,
                          coords=c("centroid_x", "centroid_y"),
                          crs="WGS84")
st_write(om_errors_sf, "om_errors.kml")





# 
# nandika_errors <- errors[errors$location_id %in% unique(lol$location_id), ]
# nandika_errors <- nandika_errors %>%
#   mutate(type_of_error=ifelse(location_id %in% com_errors$location_id, "commission", "omission")) %>%
#   select(location_id, sample_id, centroid_x, centroid_y, type_of_error)
# 
# errors_sf <- st_as_sf(x=nandika_errors,
#                       coords=c("centroid_x", "centroid_y"),
#                       crs="WGS84")
# st_write(errors_sf, "errors.gpkg")

#### VISUALISATION ####
library(ggplot2)
theme_set(theme_bw())
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)

world <- ne_countries(scale = "medium", returnclass = "sf")

ggplot() +
  geom_sf(data = world) +
  geom_sf(data = om_errors_sf, color = "red") +
  geom_sf(data = com_errors_sf, color = "blue") +
  labs(title = "Commission and omission errors of full Random Forest model")

com_errors_sf <- st_as_sf(x=com_errors_reference,
                          coords=c("centroid_x", "centroid_y"),
                          crs="WGS84")
plot(st_geometry(com_errors_sf))

om_errors_sf <- st_as_sf(x=om_errors_reference,
                         coords=c("centroid_x", "centroid_y"),
                         crs="WGS84")
plot(st_geometry(om_errors_sf), add=T)

