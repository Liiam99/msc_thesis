library(dplyr)

derive_errors <- function(result) {
  errors_idx <- result$pred != result$obs
  
  errors <- data.frame(
    pred=result$pred[errors_idx],
    obs=result$obs[errors_idx],
    NoChange=result$prob_pred[errors_idx, "NoChange"],
    Change=result$prob_pred[errors_idx, "Change"],
    feature_idx=result$val_index[errors_idx],
    fold=rep.int(result$fold, sum(errors_idx))
  )
  
  errors <- errors %>%
    mutate(error_type=ifelse(pred == "Change", "commission", "omission")) %>%
    mutate(shap_idx=row_number())
}
