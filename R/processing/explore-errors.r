library(dplyr)
library(tools)
library(treeshap)

source("./R/utils/utils.r")

assess_errors <- function(unified, errors, type_of_error, time_series) {
  # Only two options: commission and omission.
  if (type_of_error == "commission") {
    errors <- errors[errors$pred == "Change" & errors$obs == "NoChange", ]
  } else if (type_of_error == "omission") {
    errors <- errors[errors$pred == "NoChange" & errors$obs == "Change", ]
  } else {
    stop("Invalid type of error. Error options are [commission] or [omission]")
  }
  
  # Samples random (c)om errors and collects all relevant data for assessment.
  set.seed(123)
  random_errors <- sample_n(errors, 20)
  random_errors_features <- unified$data[random_errors$rowIndex, ]
  random_errors_treeshap <- treeshap(unified, random_errors_features)
  random_errors_shaps <- random_errors_treeshap$shaps
  
  # These harmonics are derived from NIRv time series.
  NIRv_harmonics <- c("amplitude1", "co", "si", "amplitude2", "si2")
  
  # Allows user to inspect the time series of the most influential Shap value for each error.
  for (error_nr in 1:nrow(random_errors)) {
    print("--------------------------------")
    error <- random_errors[error_nr, ]
    error_shaps <- random_errors_shaps[error_nr, ]
    
    # Checks the feature with the highest Shapley value and what index it used.
    highest_shap_value_idx <- which.max(abs(error_shaps))
    feature_name <- colnames(error_shaps)[highest_shap_value_idx]
    index_name <- sub("_.*", "", feature_name)
    
    print(paste("Location ID:", error$location_id))
    print(paste("From", error$from, "to", error$to))
    print(paste("Most influential feature:", feature_name))
    print(paste("Shapley value:", error_shaps[highest_shap_value_idx]))
    
    if (index_name %in% NIRv_harmonics) {
      index_name <- "NIRv"
    } 
  
    if (index_name == "long" || index_name == "lat" || index_name == "abs_lat") {
      print("No time series for long or lat")
      plot.new()
    } else {
      # Retrieves the time series of the index used to calculate the feature.
      index_ts <- time_series[[index_name]]
      index_ts_error <- index_ts[, names(index_ts) == error$location_id]
      plot(index_ts_error, xlab="Time", ylab=index_name, main=feature_name, sub=error$location_id)
      print(plot_contribution(random_errors_treeshap, obs=error_nr))
    }
    
    # User can either continue to assess next error or quit.
    prompt_text <- paste("Next", type_of_error, "error? (y/n) ")
    prompt_answer <- readline(prompt=prompt_text)
    confirmation <- regexpr(prompt_answer, 'y', ignore.case = TRUE) == 1
    if (!confirmation) {
      break
    }
  }
  
  # Marks the errors that were randomly drawn
  errors <- errors %>%
    mutate(is_drawn=ifelse(location_id %in% random_errors$location_id, T, F))
  
  return(errors)
}
