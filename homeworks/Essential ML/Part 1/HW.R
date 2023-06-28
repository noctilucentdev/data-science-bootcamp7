library(tidyverse)
library(caret)

glimpse(House_Price_India)

# split data
# train : test = 80:20

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

# train model
# linear regression
train_model_HPI <- lm(Price ~ `number of bedrooms` + `living area` + `lot area` + `Distance from the airport` + `grade of the house` + `Renovation Year`, data = train_data_HPI )

# score
HPI_pre_price <- predict(train_model_HPI, newdata = test_data_HPI)

# evaluate
test_HPI_actual = test_data_HPI$Price
test_HPI_pred = HPI_pre_price

mae_metric <- function(actual, prediction){
  # mean absolute error
  abs_error <- abs(actual - prediction)
  mean(abs_error)
}

mae_metric(test_data$mpg, mpg_pred)

mse_metric <- function(actual, prediction){
  # mean square error
  sq_error <- (actual - prediction)**2
  mean(sq_error)
}

rmse_metric <- function(actual, prediction){
  # mean square error
  sq_error <- (actual - prediction)**2
  sqrt(mean(sq_error)) # back to normal unit
}

mae_metric(test_HPI_actual, test_HPI_pred)
mse_metric(test_HPI_actual, test_HPI_pred)
rmse_metric(test_HPI_actual, test_HPI_pred)
