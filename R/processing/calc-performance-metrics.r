library(caret)

calc_performance_metrics <- function(rf_model) {
  conf_matrix <- confusionMatrix(rf_model)$table
  
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
  
  var_imp <- varImp(rf_model, type=1)
  print(plot(var_imp, top=dim(var_imp$importance[1]), main="Relative Permutation Feature Importance"))
  
  performance_metrics <- c(OA=OA, 
                           PA_change=PA_change, 
                           UA_change=UA_change, 
                           F1_change=F1_change, 
                           Gmean=Gmean)
}
