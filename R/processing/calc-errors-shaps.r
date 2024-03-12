library(treeshap)

source("./R/utils/custom-unify.r")

calc_errors_shaps <- function(result, features) {
  val_features <- features[result$val_idx, ]
  errors <- result$pred != result$obs
  errors_features <- val_features[errors, ]
  
  unified <- custom.unify(result$model, features)
  treeshap_obj <- treeshap(unified, errors_features)
  
  return(treeshap_obj$shaps)
}
