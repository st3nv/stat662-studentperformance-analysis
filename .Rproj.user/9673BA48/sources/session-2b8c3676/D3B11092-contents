# Predictive performance comparison
# Load required libraries
library(caret)
library(randomForest)
library(xgboost)
library(nnet)
library(MASS)
library(dplyr)
library(Metrics)

set.seed(42)

data <- read.csv("data/student-merge-processed.csv", header = TRUE)
data_nodrop <- data %>% 
  filter(G3_math != 0 & G3_por != 0 & G1_math != 0 & G1_por != 0 & G2_math != 0 & G2_por != 0) 

target_vars <- c("G3_math", "G3_por")

train_index <- createDataPartition(data_nodrop$G3_math, p = 0.8, list = FALSE)
train_data <- data_nodrop[train_index, ]
test_data <- data_nodrop[-train_index, ]

predictors <- c("famsize", "Pstatus", "Medu", "Fedu", "Mjob", "Fjob", "famrel", "famsup",
                "guardian", "internet", "school", "sex", "age", "address", "traveltime",
                "freetime", "nursery", "schoolsup", "activities", "higher", "romantic",
                "goout")
formula_math <- as.formula(paste("G3_math ~", paste(predictors, collapse = " + ")))
formula_por <- as.formula(paste("G3_por ~", paste(predictors, collapse = " + ")))

# Model 1: Multivariate Linear Regression
mlm1 <- lm(cbind(G3_math, G3_por) ~ ., data = train_data[, c(target_vars, predictors)])
preds_lm <- predict(mlm1, newdata = test_data)
mspe_lm_math <- mse(test_data$G3_math, preds_lm[,1])
mspe_lm_por <- mse(test_data$G3_por, preds_lm[,2])
print(mspe_lm_math)
print(mspe_lm_por)

# Model 2: XGBoost
ctrl <- trainControl(method = "cv", number = 5)

xgb_grid <- expand.grid(
  nrounds = c(50, 100, 150),
  max_depth = c(3, 5, 7),
  eta = c(0.01, 0.1, 0.3),
  gamma = c(0.5, 1, 1.5, 2, 5),
  colsample_bytree = c(0.6, 0.8, 1.0),
  min_child_weight = c(1,5,10),
  subsample = c(0.6, 0.8, 1.0)
)
xgb_model_math <- train(formula_math, data = train_data, method = "xgbTree", trControl = ctrl, tuneGrid = xgb_grid)
xgb_model_por <- train(formula_por, data = train_data, method = "xgbTree", trControl = ctrl, tuneGrid = xgb_grid)
xgb_preds_math <- predict(xgb_model_math, test_data)
xgb_preds_por <- predict(xgb_model_por, test_data)
mspe_xgb_math <- mse(test_data$G3_math, xgb_preds_math)
mspe_xgb_por <- mse(test_data$G3_por, xgb_preds_por)

print(mspe_xgb_math)
print(mspe_xgb_por)


# Model 3: Random Forest
# Define hyperparameters
rf_mtry   <- c(2, 4, 6, 8, 10)
rf_ntrees <- c(100, 300, 500, 1000)

# Grid of parameters
param_grid <- expand.grid(mtry = rf_mtry, ntree = rf_ntrees)

# Set up results data.frames
rf_results_math <- param_grid
rf_results_math$CV_MSPE <- NA_real_

rf_results_por <- param_grid
rf_results_por$CV_MSPE <- NA_real_

# Define 5-fold CV
folds <- createFolds(train_data$G3_math, k = 5, returnTrain = TRUE)

# Loop over parameter grid
for (i in seq_len(nrow(param_grid))) {
  m  <- param_grid$mtry[i]
  nt <- param_grid$ntree[i]
  
  mspe_math_folds <- c()
  mspe_por_folds  <- c()
  
  for (fold in folds) {
    cv_train <- train_data[fold, ]
    cv_val   <- train_data[-fold, ]
    
    # Train model for math
    model_math <- randomForest(formula_math, data = cv_train, mtry = m, ntree = nt)
    preds_math <- predict(model_math, newdata = cv_val)
    mspe_math_folds <- c(mspe_math_folds, mse(cv_val$G3_math, preds_math))
    
    # Train model for portuguese
    model_por <- randomForest(formula_por, data = cv_train, mtry = m, ntree = nt)
    preds_por <- predict(model_por, newdata = cv_val)
    mspe_por_folds <- c(mspe_por_folds, mse(cv_val$G3_por, preds_por))
  }
  
  # Store average CV MSPE
  rf_results_math$CV_MSPE[i] <- mean(mspe_math_folds)
  rf_results_por$CV_MSPE[i]  <- mean(mspe_por_folds)
}

# Select best parameters
best_rf_math <- rf_results_math[which.min(rf_results_math$CV_MSPE), ]
best_rf_por  <- rf_results_por[which.min(rf_results_por$CV_MSPE), ]

# Train final models with best hyperparameters
final_model_math <- randomForest(formula_math, data = train_data, 
                                 mtry = best_rf_math$mtry, ntree = best_rf_math$ntree)
final_model_por <- randomForest(formula_por, data = train_data, 
                                mtry = best_rf_por$mtry, ntree = best_rf_por$ntree)

# Predict on test set
final_preds_math <- predict(final_model_math, newdata = test_data)
final_preds_por  <- predict(final_model_por, newdata = test_data)

# Compute final MSPE on test data
final_mspe_math <- mse(test_data$G3_math, final_preds_math)
final_mspe_por  <- mse(test_data$G3_por, final_preds_por)

# Output
print(best_rf_math)
cat("Final MSPE on test (math):", final_mspe_math, "\n")

print(best_rf_por)
cat("Final MSPE on test (portuguese):", final_mspe_por, "\n")


# Model 4: 1 hidden layer Feedforward Neural Network (nnet)
ctrl <- trainControl(method = "cv", number = 5)

nnet_grid <- expand.grid(size = c(5, 10, 15, 20, 40),
                        decay = c(0.001, 0.01, 0.1)
)
                        
nnet_model_math <- train(formula_math, data = train_data, method = "nnet", linout = TRUE, trace = FALSE,
                         trControl = ctrl, tuneGrid = nnet_grid)
nnet_model_por <- train(formula_por, data = train_data, method = "nnet", linout = TRUE, trace = FALSE,
                        trControl = ctrl, tuneGrid = nnet_grid)
nnet_preds_math <- predict(nnet_model_math, test_data)
nnet_preds_por <- predict(nnet_model_por, test_data)
mspe_nnet_math <- mse(test_data$G3_math, nnet_preds_math)
mspe_nnet_por <- mse(test_data$G3_por, nnet_preds_por)
print(mspe_nnet_math)
print(mspe_nnet_por)
