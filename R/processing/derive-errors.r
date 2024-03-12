library(dplyr)

derive_errors <- function(result) {
  errors_idx <- result$pred != result$obs
  
  errors <- data.frame(
    pred=result$pred[errors_idx],
    obs=result$obs[errors_idx],
    NoChange=result$prob_pred[errors_idx, "NoChange"],
    Change=result$prob_pred[errors_idx, "Change"],
    val_idx=result$val_idx[errors_idx],
    fold=rep.int(result$fold, sum(errors_idx)),
    error_type=ifelse(result$pred[errors_idx] == "Change", "commission", "omission")
  )
}
