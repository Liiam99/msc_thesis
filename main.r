library(caret)
library(smotefamily)

source("./R/preprocessing/preprocessing.r")
source("./R/utils/calc-temporal-indices.r")
source("./R/utils/load-surface-reflectances.r")
source("./R/utils/utils.r")

reference_data <- load_data()
SRs <- load_SRs(reference_data)
SRs <- filter_by_dates(SRs, "2016-07-01", "2018-06-30")

# Writes the indices' values to files in ./data/global/processed/temporal_indices
calc_temporal_indices(SRs)
 
base_features <- calc_base_features()

data <- base_features
data <- na.omit(data)

# Excludes location_id from oversampling.
data_subset <- data[, -1]

# Performs oversampling on the number of changes as there are many more No Changes
#   than changes in the dataset.
oversampled_data <- SMOTE(data_subset[, -1], data_subset$is_change)$data
oversampled_data$is_change <- as.factor(as.factor(oversampled_data$class))
oversampled_data <- subset(oversampled_data, select=-c(class))

set.seed(123)
sample_indices <- createDataPartition(oversampled_data$is_change, p=0.8, list=FALSE)
train_data <- oversampled_data[sample_indices, ]
test_data <- oversampled_data[-sample_indices, ]

train_control <- trainControl(method="cv", number=10)
rf_model_cv <- train(is_change ~ . - is_change, data=train_data,
                     method="rf", trControl=train_control, ntree=500)

predictions <- predict(rf_model_cv, newdata=test_data)

conf_matrix <- confusionMatrix(predictions, test_data$is_change, mode="prec_recall")
conf_matrix <- conf_matrix$table
