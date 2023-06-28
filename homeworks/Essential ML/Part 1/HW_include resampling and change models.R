# HW : 

library(mlbench)
library(caret)
library(rpart) # decision tree
library(randomForest)
library(class) # knn
library(glmnet) # ridge lasso
library(tidyverse)
library(MLmetrics)
library(forcats)

# 1. split data 80:20

t_t_s <- function(data, ratio=0.8) {
  set.seed(42)
  n <- nrow(data)
  id <- sample(1:n, size=ratio*n)
  train_data <- data[id,]
  test_data <- data[-id,]
  list(train=train_data, test=test_data)
}

set.seed(42)
t_t_s_HPI <- t_t_s(House_Price_India, 0.8)
train_data_HPI <- t_t_s_HPI$train
test_data_HPI <- t_t_s_HPI$test

# 2. train model

ctrl = trainControl(
  method = "cv",
  number = 5,
  verboseIter = TRUE
)

lm_model_HPI <- train(
  Price ~ `number of bedrooms` + `living area` + `lot area` + `Distance from the airport` + `grade of the house` + `Renovation Year`,
  data = train_data_HPI,
  method = "lm",
  trControl = ctrl,
  preProcess = c("center", "scale")
)

rf_model_HPI <- train(
  Price ~ `number of bedrooms` + `living area` + `lot area` + `Distance from the airport` + `grade of the house` + `Renovation Year`,
  data = train_data_HPI,
  method = "rf",
  trControl = ctrl,
  preProcess = c("center", "scale")
)

knn_model_HPI <- train(
  Price ~ `number of bedrooms` + `living area` + `lot area` + `Distance from the airport` + `grade of the house` + `Renovation Year`,
  data = train_data_HPI,
  method = "knn",
  trControl = ctrl,
  preProcess = c("center", "scale")
)

# comparing lm vs rf vs knn (Rsquared) found in this scenario rf > lm > knn
# so in this analysis will use rf model

# 3. score model
HPI_pre_price_rf <- predict(rf_model_HPI, newdata = test_data_HPI)

actual <- test_data_HPI$Price
prediction <- HPI_pre_price_rf

# 4. evaluate model
rmse_metric(actual, prediction)
mse_metric(actual, prediction)
MSE(prediction, actual)
MAE(prediction, actual)
