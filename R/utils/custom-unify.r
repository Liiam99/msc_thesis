# Credit to https://github.com/ModelOriented/treeshap/blob/master/R/unify_randomForest.R

library(data.table)

custom.unify <- function(rf_model, data) {
  if(!inherits(rf_model,'randomForest')){stop('Object rf_model was not of class "randomForest"')}
  if(any(attr(rf_model$terms, "dataClasses")[-1] != "numeric")) {
    stop('Models built on data with categorical features are not supported - please encode them before training.')
  }
  n <- rf_model$ntree
  ret <- data.table()
  prediction <- NULL
  x <- lapply(1:n, function(tree){
    tree_data <- as.data.table(randomForest::getTree(rf_model, k = tree, labelVar = TRUE))
    tree_data <- tree_data[ , prediction:=ifelse(prediction == "Change", 1, 0)]
    tree_data[, c("left daughter", "right daughter", "split var", "split point", "prediction")]
  })
  times_vec <- sapply(x, nrow)
  y <- rbindlist(x)
  y[, Tree := rep(0:(n - 1), times = times_vec)]
  y[, Node := unlist(lapply(times_vec, function(x) 0:(x - 1)))]
  setnames(y, c("Yes", "No", "Feature", "Split",  "Prediction", "Tree", "Node"))
  y[, Feature := as.character(Feature)]
  y[, Yes := Yes - 1]
  y[, No := No - 1]
  y[y$Yes < 0, "Yes"] <- NA
  y[y$No < 0, "No"] <- NA
  y[, Missing := NA]
  y[, Missing := as.integer(Missing)] # seems not, but needed
  
  ID <- paste0(y$Node, "-", y$Tree)
  y$Yes <- match(paste0(y$Yes, "-", y$Tree), ID)
  y$No <- match(paste0(y$No, "-", y$Tree), ID)
  
  y$Cover <- 0
  
  y$Decision.type <- factor(x = rep("<=", times = nrow(y)), levels = c("<=", "<"))
  y[is.na(Feature), Decision.type := NA]
  
  # Here we lose "Quality" information
  y[!is.na(Feature), Prediction := NA]
  
  # treeSHAP assumes, that [prediction = sum of predictions of the trees]
  # in random forest [prediction = mean of predictions of the trees]
  # so here we correct it by adjusting leaf prediction values
  y[is.na(Feature), Prediction := Prediction / n]
  
  setcolorder(y, c("Tree", "Node", "Feature", "Decision.type", "Split", "Yes", "No", "Missing", "Prediction", "Cover"))
  feature_names <- rownames(rf_model$importance)
  data <- data[,colnames(data) %in% feature_names]
  
  ret <- list(model = as.data.frame(y), data = as.data.frame(data), feature_names = feature_names)
  class(ret) <- "model_unified"
  attr(ret, "missing_support") <- FALSE
  attr(ret, "model") <- "randomForest"
  return(set_reference_dataset(ret, as.data.frame(data)))
}