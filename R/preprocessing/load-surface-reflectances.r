load_SRs <- function(reference_data) {
  # Reads all layers into a single list "IIASA_SR".
  IIASA_gpkg = "../../data/global/IIASAChange20152018_Landsat8_TS.gpkg"
  SRNames = st_layers(IIASA_gpkg)$name
  IIASA_SR = lapply(SRNames, function(name) st_read(IIASA_gpkg, layer=name, quiet=T))
  
  # Reads all layers into a single list "WUR_SR".
  WUR_gpkg = "../../data/global/WURChange20152019_Landsat8_TS.gpkg"
  SRNames = st_layers(WUR_gpkg)$name
  WUR_SR = lapply(SRNames, function(name) st_read(WUR_gpkg, layer=name, quiet=T))
  
  # Extracts the SR values for the points in the reference data from both data sets.
  IIASA_SR_filtered <- lapply(IIASA_SR, filter_IIASA_SR, reference_data)
  WUR_SR_filtered <- lapply(WUR_SR, filter_WUR_SR, reference_data)
  
  SR <- Map(bind_rows, IIASA_SR_filtered, WUR_SR_filtered)
}

filter_IIASA_SR <- function(df, reference_data) {
  df$sample_id <- as.integer(df$sample_id)
  
  df_filtered <- df %>%
    filter(sample_id %in% reference_data$sample_id) %>%
    left_join(reference_data %>% select(location_id, sample_id), by="sample_id") %>%
    select(-sample_id) %>%
    relocate(location_id)
}

filter_WUR_SR <- function(df, reference_data) {
  df$location_id <- as.integer(df$location_id)
  df_filtered <- df %>%
    filter(location_id %in% reference_data$location_id) %>%
    left_join(reference_data %>% select(location_id, centroid_x, centroid_y), by="location_id") %>%
    select(-sample_x, -sample_y) %>%
    relocate(c("centroid_x", "centroid_y"), .after="location_id")
}
