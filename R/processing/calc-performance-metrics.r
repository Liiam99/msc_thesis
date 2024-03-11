library(caret)

calc_performance_metrics <- function(result) {
  conf_matrix <- confusionMatrix(result$preds, result$obs)$table
  
  TN <- conf_matrix[1, 1]
  FN <- conf_matrix[1, 2]
  FP <- conf_matrix[2, 1]
  TP <- conf_matrix[2, 2]
  
  OA <- (TP + TN)/(TP + FP + TN + FN)*100
  PA_change <- TP/(TP + FN)*100
  UA_change <- TP/(TP + FP)*100
  F1_change <- (UA_change * PA_change)/(UA_change + PA_change)*2
  
  PA_no_change <- TN/(TN + FP)*100
  Gmean <- sqrt(PA_change*PA_no_change)
  
  performance_metrics <- c(OA=OA, 
                           PA_change=PA_change, 
                           UA_change=UA_change, 
                           F1_change=F1_change, 
                           Gmean=Gmean)
}
