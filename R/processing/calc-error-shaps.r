library(treeshap)

source("./R/utils/custom-unify.r")

calc_error_shaps <- function(result, features) {
  val_features <- features[result$val_index, ]
  errors <- result$pred != result$obs
  errors_features <- val_features[errors, ]
  
  unified <- custom.unify(result$model, features)
  treeshap_obj <- treeshap(unified, errors_features)
  
  return(treeshap_obj$shaps)
}
