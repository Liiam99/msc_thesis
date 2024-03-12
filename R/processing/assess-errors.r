library(dplyr)
library(tools)

source("./R/utils/utils.r")
source("./R/visualisation/plot-contribution-mod.r")

assess_errors <- function(errors, errors_shaps, features, time_series) {
  # Samples random (c)om errors and collects all relevant data for assessment.
  set.seed(123)
  random_indices <- sample.int(nrow(errors), 20)
  random_errors <- errors[random_indices, ]
  
  # These harmonics are derived from NIRv time series.
  NIRv_harmonics <- c("amplitude1", "co", "si", "amplitude2", "si2")
  
  # Allows user to inspect the time series of the most influential Shap value for each error.
  for (error_nr in 1:nrow(random_errors)) {
    print("--------------------------------")
    error <- random_errors[error_nr, ]
    error_features <- features[error$val_idx, ]
    error_shaps <- errors_shaps[error$shap_idx ,]
    
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
    }
    
    # Displays a plot that breaks down the contribution of each feature to
    # the predicted value.
    print(plot_contribution_mod(
      error_shaps, 
      error_features, 
      error$Change,
      subtitle=error$location_id))
    
    # User can either continue to assess next error or quit.
    prompt_text <- paste("Next error? (y/n) ")
    prompt_answer <- readline(prompt=prompt_text)
    confirmation <- regexpr(prompt_answer, 'y', ignore.case = TRUE) == 1
    if (!confirmation) {
      break
    }
  }
  
  # Marks the errors that were randomly drawn.
  errors <- errors %>%
    mutate(is_drawn=ifelse(row_number() %in% random_indices, T, F))
  
  return(errors)
}
