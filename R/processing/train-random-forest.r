library(caret)

train_rf <- function(features, sampling="up", cutoff=0.5, ntree=128) {
  set.seed(123)
  seeds <- vector(mode="list", length=11)
  
  # Ensures reproducibility by setting seeds for each resample.
  k = 10
  for(i in 1:k) {
    seeds[[i]]<- sample.int(n=1000, 3)
  }
  
  seeds[[k + 1]]<-sample.int(1000, 1)
  
  train_control <- trainControl(method="cv", 
                                number=k, 
                                sampling=sampling, 
                                preProcOptions=c(cutoff=cutoff), 
                                savePredictions="final",
                                seeds=seeds)
  
  rf <- train(is_change ~ . - location_id, data=features, 
              trControl=train_control, ntree=ntree, importance=T, preProcess="corr")
}
