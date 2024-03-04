library(dplyr)

load_SRs <- function(reference_data, brazil=F) {
  if (brazil == T) {
    brazil_gpkg = "./data/brazil/raw/BrazilChange20152018_Landsat8_TS.gpkg"
    SRNames = st_layers(brazil_gpkg)$name
    SRs = lapply(SRNames, function(name) st_read(brazil_gpkg, layer=name, quiet=T))
    names(SRs) = SRNames
    
    return(SRs)
  }
  
  # Reads all surface reflectances into a single list "IIASA_SRs".
  IIASA_gpkg = "./data/global/raw/IIASAChange20152018_Landsat8_TS.gpkg"
  SRNames = st_layers(IIASA_gpkg)$name
  IIASA_SRs = lapply(SRNames, function(name) st_read(IIASA_gpkg, layer=name, quiet=T))
  
  # Reads all surface reflectances into a single list "WUR_SRs".
  WUR_gpkg = "./data/global/raw/WURChange20152019_Landsat8_TS.gpkg"
  WUR_SRs = lapply(SRNames, function(name) st_read(WUR_gpkg, layer=name, quiet=T))
  
  # Extracts the SR values for the points in the reference data from both data sets.
  IIASA_SRs_filtered <- lapply(IIASA_SRs, filter_IIASA_SRs, reference_data)
  WUR_SRs_filtered <- lapply(WUR_SRs, filter_WUR_SRs, reference_data)
  
  SRs <- Map(bind_rows, IIASA_SRs_filtered, WUR_SRs_filtered)
  names(SRs) = SRNames
  
  return(SRs)
}

filter_IIASA_SRs <- function(IIASA_SRs, reference_data) {
  IIASA_SRs$sample_id <- as.integer(IIASA_SRs$sample_id)
  
  IIASA_SRs_filtered <- IIASA_SRs %>%
    filter(sample_id %in% reference_data$sample_id) %>%
    left_join(reference_data %>% select(location_id, sample_id), by="sample_id") %>%
    select(-sample_id) %>%
    relocate(location_id)
}

filter_WUR_SRs <- function(WUR_SRs, reference_data) {
  WUR_SRs$location_id <- as.integer(WUR_SRs$location_id)
  WUR_SRs_filtered <- WUR_SRs %>%
    filter(location_id %in% reference_data$location_id) %>%
    left_join(reference_data %>% select(location_id, centroid_x, centroid_y), by="location_id") %>%
    select(-sample_x, -sample_y) %>%
    relocate(c("centroid_x", "centroid_y"), .after="location_id")
}
