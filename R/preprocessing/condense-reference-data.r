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
  # Maps the numbers of the Mapbiomas legend to the classes of the global reference data set.
  # https://staging-brasil.mapbiomas.org/wp-content/uploads/sites/4/2023/08/Legenda-Colecao-8-LEGEND-CODE.pdf
  class_map <- list(
    bare=c(23, 25, 29, 30),
    crops=c(19, 20, 35, 36, 39, 40, 41, 46, 47, 48, 62),
    grassland=c(4, 12, 15),
    shrub=c(13),
    tree=c(1, 3, 49, 9),
    urban_built_up=c(24),
    water=c(26, 31, 33),
    wetland_herbaceous=c(5, 6, 11, 32, 50)
  )
  
  # brazil_reference_data_condensed <- brazil_reference_data %>%
  #   mutate(from=sapply(CLASS_2016, map_class_nr_to_label, class_map)) %>%
  #   mutate(to=sapply(CLASS_2018, map_class_nr_to_label, class_map)) %>%
  #   select(location_id, LAT, LON, is_change, from, to) %>%
  #   rename(centroid_x=LAT, centroid_y=LON)

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
