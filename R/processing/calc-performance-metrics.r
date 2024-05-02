library(caret)

calc_performance_metrics <- function(result) {
  conf_matrix <- confusionMatrix(result$pred, result$obs)$table
  
  TN <- conf_matrix[1, 1]
  FN <- conf_matrix[1, 2]
  FP <- conf_matrix[2, 1]
  TP <- conf_matrix[2, 2]
  
  OA <- (TP + TN)/(TP + FP + TN + FN)*100
  
  PA_no_change <- TN/(TN + FP)*100
  UA_no_change <- TN/(TN + FN)*100
  
  PA_change <- TP/(TP + FN)*100
  UA_change <- TP/(TP + FP)*100
  F1_change <- (UA_change * PA_change)/(UA_change + PA_change)*2

  Gmean <- sqrt(PA_change*PA_no_change)
  
  performance_metrics <- c(OA=OA, 
                           PA_change=PA_change,
                           PA_no_change=PA_no_change,
                           UA_change=UA_change, 
                           UA_no_change=UA_no_change,
                           F1_change=F1_change, 
                           Gmean=Gmean)
}

calc_lcc_metrics <- function(result, ref_data) {
  ref <- ref_data[ref_data$location_id %in% result$val_location_id, c("location_id", "from_lcc", "to_lcc")]
  ref <- cbind(ref, pred=result$pred, obs=result$obs)

  lccs <- c("forest", "herbaceous_vegetation", "water", "wetland")
  lcc_metrics <- list()
  
  for (lcc in lccs) {
    lcc_ref <- ref[ref$from_lcc == lcc | ref$to_lcc == lcc, ]
    accuracies <- calc_performance_metrics(lcc_ref)
    lcc_metrics[[lcc]] <- calc_performance_metrics(lcc_ref)
  }
  
  return(t(as.data.frame(lcc_metrics)))
}
