library(bfast)
library(dplyr)
library(lubridate)
library(pbapply)
library(ROSE)
library(sf)

source("./R/preprocessing/load-surface-reflectances.r")
source("./R/utils/utils.r")

filter_changes <- function(reference_data) {
  # Calculates the differences between subsequent years for each class and each location.
  classes = GetCommonClassNames()
  fraction_changes <- reference_data %>%
    group_by(location_id) %>%
    arrange(reference_year) %>%
    reframe(across(all_of(classes), ~c(diff(.), NA), .names = "{col}")) %>%
    na.omit() # Removes the fourth row of differences as there can only be 3 between 4 years.
  
  # Determines for each location if there is a change (1), no-change (0) or
  # in-between (NA) according to the definitions.
  location_changes <- fraction_changes %>%
    group_by(location_id) %>%
    filter(n() == 3) %>% 
    mutate(
      is_change = factor(
        ifelse(
          all(across(everything(), ~. == 0)),
          0,
          ifelse(
            any(rowSums(abs(across(everything())), na.rm = TRUE)/2 >= 70),
            1,
            NA
          )
        ),
        levels = c(0, 1),  # Specify the levels for the factor variable
        labels = c("No Change", "Change")  # Specify labels for the levels
      )
    ) %>%
    na.omit %>%
    select(location_id, is_change) %>%
    distinct()
  
  reference_data <- merge(reference_data, location_changes)
}

remove_sites_with_breaks <- function(reference_data, start, end) {
  SRs <- load_SRs(reference_data)
  
  # Saves a template to include location id and proper date notations.
  Template = SRs[[1]]
  names(Template)[datecols(Template)] = strtrim(names(Template)[datecols(Template)], 11)
  
  # Converts into a zoo matrix.
  SRZ = lapply(lapply(SRs, SFToMatrix), MatrixToZoo)
  NIR = SRZ[["SR_B5"]]
  RED = SRZ[["SR_B4"]]
  
  NIRvz = ((NIR - RED)/(NIR + RED))*NIR
  NIRv = ZooToSF(NIRvz, Template)
  
  # Breakpoints are in decimal date form.
  start_decimal <- decimal_date(start)
  end_decimal <- decimal_date(end)
  
  BFL = function(location_id, start=start_decimal, end=end_decimal) {
    NIRvTS = SFToZoo(NIRv[NIRv$location_id == location_id,])
    NIRvTS = window(NIRvTS, start=as.Date("2015-01-01"), end=as.Date("2018-12-31"))
    InData = ts(as.ts(as.zoo(NIRvTS[,1])), start=c(2015, 1), frequency = 365.25/16)
    
    breaks = try(bfastlite(InData, h=0.33), silent=T)
    if (class(breaks) != "bfastlite") {
      return(NULL)
    }

    breakpoints = breaks$breakpoints$breakpoints
    
    if (is.na(breakpoints[1])) {
      return(NULL)
    }
    
    # Makes a vector with dates of the time series.
    dates.no.na <- as.numeric(time(InData))
    dates.no.na[is.na(InData)] <- NA
    dates.no.na <- na.omit(dates.no.na)
    
    for (breakpoint in breakpoints) {
      breakpoint_date <- dates.no.na[breakpoint]

      if (breakpoint_date < start_decimal || breakpoint_date > end_decimal)
      {
        return(location_id)
      }
    }

    return(NULL)
  }
  
  # Removes locations that contain a break outside the targeted date range.
  locations_with_breaks <- pblapply(reference_data$location_id, BFL, cl=10)
  locations_with_breaks <- locations_with_breaks[lengths(locations_with_breaks) != 0]
  reference_data <- reference_data[!reference_data$location_id %in% locations_with_breaks, ]
}

assign_lcc_categories <- function(reference_data) {
  lcc_map <- c(crops="herbaceous_vegetation",
               grassland="herbaceous_vegetation",
               tree="forest",
               wetland_herbaceous="wetland",
               water="water",
               bare="bare",
               urban_built_up="urban",
               shrub="shrub")
  
  reference_data <- reference_data %>%
    group_by(location_id) %>%
    mutate(
      from = ifelse(
        reference_year == 2015,
        lcc_map[levels(dominant_lc)[dominant_lc]],
        NA
      )
    ) %>%
    mutate(
      to = ifelse(
        reference_year == 2018,
        lcc_map[levels(dominant_lc)[dominant_lc]],
        NA
      )
    ) %>%
    ungroup()
}
