library(caret)

train_rf <- function(features, sampling="up", cutoff=0.5, ntree=128) {
  train_control <- trainControl(method="cv", number=10, sampling=sampling, 
                                preProcOptions=c(cutoff=cutoff))
  
  rf <- train(is_change ~ . - location_id, data=features, 
              trControl=train_control, ntree=ntree, importance=T, preProcess="corr")
}
