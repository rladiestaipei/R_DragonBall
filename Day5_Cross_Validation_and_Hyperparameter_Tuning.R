##---------------- 設定環境 ----------------
#setwd(dir) #設定working directory的存放位置
# MAC : setwd("/Users/rladiestaipei/R_DragonBall/") 
# Windows : setwd("C://Users/rladiestaipei/Desktop/R_DragonBall/")  

#安裝套件(僅需執行一次)
#install.packages(c("caTools", "caret", "dplyr", 'xgboost'),
#                 dependencies = TRUE)

#load packages
library(caTools)
library(caret)
library(dplyr)
library(xgboost)


### 3.2 讀取特徵工程後的資料

dataset <- read.csv("train_new.csv")

# select features you want to put in models
# 這邊請根據前面幾天所學的去放入你認為重要的變數(如下只是範例)
dataset <- dataset %>% dplyr::select(SalePrice_log, X1stFlrSF, TotalBsmtSF, 
                                     YearBuilt, LotArea, Neighborhood, GarageCars, GarageArea, GrLivArea_stand, 
                                     MasVnrArea_stand, LotFrontage_log, is_Fireplace, TotalBathrooms, TotalSF_stand)

# transfer all feature to numeric
cat_index <- which(sapply(dataset, class) == "factor")
dataset[cat_index] <- lapply(dataset[cat_index], as.numeric)

# Splitting the dataset into the Training set and Validation set
set.seed(1)
split <- sample.split(dataset$SalePrice_log, SplitRatio = 0.8)
training_set <- subset(dataset, split == TRUE)
val_set <- subset(dataset, split == FALSE)


# put testing & training data into two seperates Dmatrixs objects
tr_x <- as.matrix(training_set)
tr_y <- training_set$SalePrice_log
val_x <- as.matrix(val_set)
val_y <- val_set$SalePrice_log

dtrain <- xgb.DMatrix(data = tr_x, label = tr_y) 
dval <- xgb.DMatrix(data = val_x, label = val_y)


### 3.3 用XGBoost的預設參數來訓練一個基準(baseline)模型


#default parameters
default_params <- expand.grid(
  nrounds = 100,
  max_depth = 6,
  eta = 0.3,
  gamma = 0,
  colsample_bytree = 1,
  min_child_weight = 1,
  subsample = 1
)

train_control <- caret::trainControl(
  method = "none",
  verboseIter = FALSE, # no training log
  allowParallel = TRUE # FALSE for reproducible results 
)

xgb_base <- caret::train(
  x = tr_x,
  y = tr_y,
  trControl = train_control,
  tuneGrid = default_params,
  method = "xgbTree",
  verbose = TRUE
)

xgb_base_rmse <- ModelMetrics::rmse(val_y, predict(xgb_base, newdata = val_x))
xgb_base_rmse
# 0.004612619


### 3.4 XGBoost 的調參策略

  
## Step 1. Number of iterations and learning rate

## 使用library(caret)來調參
# 這裡示範用grid search的方式來調參
grid <- expand.grid(
  nrounds = seq(from = 200, to = 1000, by = 50),
  eta = c(0.025, 0.05, 0.1, 0.3),
  max_depth = c(2, 3, 4, 5, 6),
  gamma = 0, 
  colsample_bytree = 1,
  min_child_weight = 1,
  subsample = 1
)

control <- caret::trainControl(
  method = "cv",
  number = 3, # cross validation with n(n=3) folds
  verboseIter = FALSE,
  allowParallel = FALSE # FALSE for reproducible results 
)

xgb_tune <- caret::train(
  x = tr_x,
  y = tr_y,
  trControl = control,
  tuneGrid = grid,
  method = "xgbTree",
  verbose = TRUE
)

# 查看tune好的最佳參數和結果
xgb_tune$bestTune


## Step 2. Maximum depth and minimum child weight

grid2 <- expand.grid(
  nrounds = seq(from = 200, to = 1000, by = 50),
  eta = 0.025, # 或者直接動態設定為 xgb_tune$bestTune$eta
  max_depth =  c(5, 6, 7), # 或者直接動態設定為 ifelse(xgb_tune$bestTune$max_depth == 2,
  # c(xgb_tune$bestTune$max_depth:4),
  # xgb_tune$bestTune$max_depth - 1:xgb_tune$bestTune$max_depth + 1),
  gamma = 0, 
  colsample_bytree = 1,
  min_child_weight = c(1, 2, 3),
  subsample = 1
)

xgb_tune2 <- caret::train(
  x = tr_x,
  y = tr_y,
  trControl = control,
  tuneGrid = grid2,
  method = "xgbTree",
  verbose = TRUE
)
# 查看tune好的最佳參數和結果
xgb_tune2$bestTune



## Step 3. Column and row sampling

grid3 <- expand.grid(
  nrounds = seq(from = 200, to = 1000, by = 50),
  eta = 0.025, # 或者直接動態設定為 xgb_tune$bestTune$eta
  max_depth =  6,  # 或者直接動態設定為 xgb_tune2$bestTune$max_depth
  gamma = 0, 
  colsample_bytree = c(0.4, 0.6, 0.8, 1.0),
  min_child_weight = 1, # 或者直接動態設定為 xgb_tune2$bestTune$min_child_weight
  subsample = c(0.5, 0.65, 0.8, 0.95, 1.0)
)

xgb_tune3 <- caret::train(
  x = tr_x,
  y = tr_y,
  trControl = control,
  tuneGrid = grid3,
  method = "xgbTree",
  verbose = TRUE
)
# 查看tune好的最佳參數和結果
xgb_tune3$bestTune


## Step 4. Gamma

grid4 <- expand.grid(
  nrounds = seq(from = 200, to = 1000, by = 50),
  eta = 0.025, # 或者直接動態設定為 xgb_tune$bestTune$eta
  max_depth =  6,  # 或者直接動態設定為 xgb_tune2$bestTune$max_depth
  gamma = c(0, 0.05, 0.1, 0.5, 0.7, 0.9, 1.0),
  colsample_bytree = 1, # 或者直接動態設定為 xgb_tune3$bestTune$colsample_bytree
  min_child_weight = 1, # 或者直接動態設定為 xgb_tune2$bestTune$min_child_weight
  subsample = 0.5 # 或者直接動態設定為 xgb_tune3$bestTune$subsample
)

xgb_tune4 <- caret::train(
  x = tr_x,
  y = tr_y,
  trControl = control,
  tuneGrid = grid4,
  method = "xgbTree",
  verbose = TRUE
)
# 查看tune好的最佳參數和結果
xgb_tune4$bestTune



## Step 5. Reducing the learning rate and increase nrounds

grid5 <- expand.grid(
  nrounds = seq(from = 500, to = 10000, by = 100),
  eta = c(0.01, 0.015, 0.025, 0.05, 0.1), 
  max_depth =  6,  # 或者直接動態設定為 xgb_tune2$bestTune$max_depth
  gamma = 0, # 或者直接動態設定為 xgb_tune4$bestTune$gamma
  colsample_bytree = 1, # 或者直接動態設定為 xgb_tune3$bestTune$colsample_bytree
  min_child_weight = 1, # 或者直接動態設定為 xgb_tune2$bestTune$min_child_weight
  subsample = 0.5 # 或者直接動態設定為 xgb_tune3$bestTune$subsample
)

xgb_tune5 <- caret::train(
  x = tr_x,
  y = tr_y,
  trControl = control,
  tuneGrid = grid5,
  method = "xgbTree",
  verbose = TRUE
)
# 查看tune好的最佳參數和結果
xgb_tune5$bestTune


## Step 6. Fit model with final parameters

## 獲得最後的參數
final_grid <- expand.grid(
  nrounds = xgb_tune5$bestTune$nrounds,
  eta = xgb_tune5$bestTune$eta,
  max_depth = xgb_tune5$bestTune$max_depth,
  gamma = xgb_tune5$bestTune$gamma,
  colsample_bytree = xgb_tune5$bestTune$colsample_bytree,
  min_child_weight = xgb_tune5$bestTune$min_child_weight,
  subsample = xgb_tune5$bestTune$subsample
)
final_grid

# 用tune好的最佳參數套用到模型
xgb_model <- caret::train(
  x = tr_x,
  y = tr_y,
  trControl = train_control,
  tuneGrid = final_grid,
  method = "xgbTree",
  verbose = TRUE
)


## Step 7. Compare perfomrance on validation set with baseline model

# 查看訓練出來的模型在驗證集中的表現
xgb_tuned_rmse <- ModelMetrics::rmse(val_y, predict(xgb_model, newdata = val_x))

xgb_tuned_rmse # tune過的模型在驗證集上的RMSE
xgb_base_rmse # 用預設參數的模型在驗證集上的RMSE

# RMSE 提升程度
xgb_base_rmse - xgb_tuned_rmse
## 和baseline模型相比會發現，經過調參後 RMSE 從 0.0046 降低到了 0.0011！！！

  
## 本日小挑戰
  
## 嘗試回答機器學習中如何處理 “Bias-Variance Tradeoff”的問題？
