library(dplyr)

condense_global_data <- function(global_reference_data) {
  # Condenses the data frame into one row per location, ignoring the years and fractions.
  reference_data_condensed <- reference_data %>%
    select(location_id, sample_id, centroid_x, centroid_y, is_change, from, to) %>%
    group_by(location_id) %>%
    summarise(across(everything(), ~ if(all(is.na(.))) NA else first(.[!is.na(.)]))) %>%
    ungroup()
}

condense_brazil_data <- function(brazil_reference_data) {
  brazil_reference_data_condensed <- brazil_reference_data %>%
    mutate(from=CLASS_2016) %>%
    mutate(to=CLASS_2018) %>%
    select(location_id, LAT, LON, is_change, from, to) %>%
    rename(centroid_x=LON, centroid_y=LAT)
}

map_class_nr_to_label <- function(class_nr, class_map) {
  class_match <- which(mapply("%in%", class_nr, class_map))
  
  if (length(class_match) == 0) {
    return(NA)
  } 
  
  return(names(class_map)[class_match])
}
