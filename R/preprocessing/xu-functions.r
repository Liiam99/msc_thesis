library(dplyr)

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

# https://github.com/GreatEmerald/supervised-bfast/blob/main/src/030_bfast/20_bfast-on-vis.r

#### BFAST EXPERIMENTATION ####
# # Run BFAST and BFASTlite to remove the breaks before June 2016 and after July 2018 and check the numbers
# library(bfast)
# 
# OutTemplate = SRs[[1]]
# names(OutTemplate)[datecols(OutTemplate)] = strtrim(names(OutTemplate)[datecols(OutTemplate)], 11)
# 
# SRZ = lapply(lapply(SRs, SFToMatrix), MatrixToZoo)
# test <- ((SRZ[["SR_B5"]] - SRZ[["SR_B4"]])/(SRZ[["SR_B5"]] + SRZ[["SR_B4"]]))*SRZ[["SR_B5"]]
# 
# NDVITS = window(test, start=as.Date("2015-01-01"), end=as.Date("2018-12-31"))
# 
# InData = ts(as.ts(as.zoo(NDVITS)), frequency = 365.25/16)
# 
# bro <- bfastlite(InData)
# NIRvsf = ZooToSF(test, OutTemplate)
# 
# BFL = function(location_id, scaled=TRUE, mag_threshold=0, plot=FALSE, formula=response~trend, h=10, ...)
# {
#   NDVITS = test[test$location_id == location_id,]
#   NDVITS = window(test, start=as.Date("2015-01-01"), end=as.Date("2018-12-31"))
#   
#   InData = ts(as.ts(as.zoo(NDVITS[, 1])), frequency = 365.25/16)
#   
#   # Run model
#   bfl = try(bfastlite(InData, h=h, formula=formula))
#   if (class(bfl) != "bfastlite") # If it crashed, don't return any prediction
#     return(NULL)
#   
#   return(bfl)
# }
# 
# lol <- lapply(unique(reference_data$location_id), BFL)

