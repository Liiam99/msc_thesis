library(bfast)
library(dplyr)
library(lubridate)
library(pbapply)

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
    select(location_id, is_change) %>%
    distinct()

  # Removes the locations that do not conform to the definitions.
  reference_data <- merge(reference_data, location_changes)
  reference_data <- reference_data[complete.cases(reference_data$is_change), ]
}

remove_sites_with_breaks <- function(reference_data, start, end) {
  NIRv = st_read("./data/global/processed/temporal_indices/NIRv.gpkg", quiet=T)
  
  start_decimal <- decimal_date(start)
  end_decimal <- decimal_date(end)
  
  BFL = function(location_id, start=start_decimal, end=end_decimal) {
    NIRvTS = SFToZoo(NIRv[NIRv$location_id == location_id,])
    NIRvTS = window(NIRvTS, start=as.Date("2015-01-01"), end=as.Date("2018-12-31"))
    InData = ts(as.ts(as.zoo(NIRvTS[,1])), start=c(2015, 1), frequency = 365.25/16)
    
    # Run model
    breaks = try(bfastlite(InData, h=0.33), silent=T)
    if (class(breaks) != "bfastlite") {
      return(NULL)
    }
    
    # Only return location id if there is a break outside the targeted date range.
    
    # Check for breaks
    # Check if break happened before start or after end
    #    if yes, remove location id from the reference_data (ALL ROWS)
    
    breakpoints = breaks$breakpoints$breakpoints
    
    if (is.na(breakpoints[1])) {
      return(NULL)
    }
    
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
  
  locations_with_breaks = pblapply(unique(reference_data$location_id), BFL)
}
