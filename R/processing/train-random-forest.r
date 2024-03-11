library(caret)
library(randomForest)
library(treeshap)

train_rf <- function(data, k, cutoff=0.5) {
  correlated_features <- findCorrelation(
    cor(data[, !colnames(data) %in% c("location_id", "is_change")]), 
    cutoff=cutoff,
    names=T
  )
  
  filtered_features <- data[, !colnames(data) %in% correlated_features]
  set.seed(123)
  folds <- createFolds(filtered_features$is_change, k)
  
  results <- vector(mode='list', length=k)
  
  for (i in 1:k) {
    print(paste("Fold:", i))
    
    # Creates oversampled training data.
    train_index <- unlist(folds[-i])
    train_data <- filtered_features[train_index, ]
    train_data <- upSample(train_data, train_data$is_change, yname="is_change")
    
    # All other data is validation data.
    val_index <- unlist(folds[i])
    val_data <- filtered_features[val_index, ]

    rf_model <- randomForest(is_change ~ . - location_id, data=train_data, ntree=128) 
    
    prob_preds <- predict(rf_model, newdata=val_data, type="prob")
    preds <- predict(rf_model, newdata=val_data)
  
    results[[i]] <- list(
      model=rf_model,
      preds=preds,
      obs=val_data$is_change,
      prob_preds=prob_preds,
      train_index=train_index,
      val_index=val_index
    )
  }
  
  return(results)
}
