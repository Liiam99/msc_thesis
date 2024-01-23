library(dplyr)

load_SRs <- function(reference_data) {
  # Reads all layers into a single list "IIASA_SR".
  IIASA_gpkg = "./data/global/raw/IIASAChange20152018_Landsat8_TS.gpkg"
  SRNames = st_layers(IIASA_gpkg)$name
  IIASA_SR = lapply(SRNames, function(name) st_read(IIASA_gpkg, layer=name, quiet=T))
  
  # Reads all layers into a single list "WUR_SR".
  WUR_gpkg = "./data/global/raw/WURChange20152019_Landsat8_TS.gpkg"
  WUR_SR = lapply(SRNames, function(name) st_read(WUR_gpkg, layer=name, quiet=T))
  
  # Extracts the SR values for the points in the reference data from both data sets.
  IIASA_SR_filtered <- lapply(IIASA_SR, filter_IIASA_SR, reference_data)
  WUR_SR_filtered <- lapply(WUR_SR, filter_WUR_SR, reference_data)
  
  SR <- Map(bind_rows, IIASA_SR_filtered, WUR_SR_filtered)
  names(SR) = SRNames
  
  return(SR)
}

filter_IIASA_SR <- function(IIASA_SR, reference_data) {
  IIASA_SR$sample_id <- as.integer(IIASA_SR$sample_id)
  
  IIASA_SR_filtered <- IIASA_SR %>%
    filter(sample_id %in% reference_data$sample_id) %>%
    left_join(reference_data %>% select(location_id, sample_id), by="sample_id") %>%
    select(-sample_id) %>%
    relocate(location_id) %>%
    distinct(location_id, .keep_all=TRUE)
}

filter_WUR_SR <- function(WUR_SR, reference_data) {
  WUR_SR$location_id <- as.integer(WUR_SR$location_id)
  WUR_SR_filtered <- WUR_SR %>%
    filter(location_id %in% reference_data$location_id) %>%
    left_join(reference_data %>% select(location_id, centroid_x, centroid_y), by="location_id") %>%
    select(-sample_x, -sample_y) %>%
    relocate(c("centroid_x", "centroid_y"), .after="location_id") %>%
    distinct(location_id, .keep_all=TRUE)
}
