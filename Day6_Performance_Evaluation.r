##---------------- 設定環境 ----------------
##---------------- 設定環境 ----------------
#setwd(dir) #設定working directory的存放位置
# MAC : setwd("/Users/rladiestaipei/R_DragonBall/") 
# Windows : setwd("C://Users/rladiestaipei/Desktop/R_DragonBall/")  

#安裝套件(僅需執行一次)
#install.packages(c("caTools", "caret", "dplyr", 'xgboost',"Metrics"),dependencies = TRUE)

#load packages
library(caTools)
library(caret)
library(dplyr)
library(xgboost)
library(Metrics)

##------------------------------------------------
## Part 2 : Model Evaluation in R
##------------------------------------------------
##讀取特徵工程後的資料
dataset <- read.csv("train_new.csv")

##分別跑模型, 此處以Day 4學到的Linear Regression & XGBoost 演算法為例
# select features you want to put in models
# 這邊請根據前面幾天所學的去放入你認為重要的變數(如下只是範例)
dataset <- dataset %>% dplyr::select(SalePrice_log, X1stFlrSF, TotalBsmtSF, 
                                     YearBuilt, LotArea, Neighborhood, GarageCars, GarageArea, GrLivArea_stand, 
                                     MasVnrArea_stand, LotFrontage_log, is_Fireplace, TotalBathrooms, TotalSF_stand)

# Splitting the dataset into the Training set and Validation set
# library(caTools)
set.seed(1)
split <- sample.split(dataset$SalePrice_log, SplitRatio = 0.8)
training_set <- subset(dataset, split == TRUE)
val_set <- subset(dataset, split == FALSE)

##Linear Regression
# Fitting Multiple Linear Regression to the Training set
Reg <- lm(formula = SalePrice_log ~ ., data = training_set)


##XGBoost
# library(xgboost)
# 因XGBoost是吃matrix的格式，故須將所有資料皆轉換為數值型，存入矩陣中

# transfer all feature to numeric
training_set_new <- training_set %>% dplyr::select(-SalePrice_log)
val_set_new <- val_set %>% dplyr::select(-SalePrice_log)
cat_index <- which(sapply(training_set_new, class) == "factor")
training_set_new[cat_index] <- lapply(training_set_new[cat_index], as.numeric)
val_set_new[cat_index] <- lapply(val_set_new[cat_index], as.numeric)

# put testing & training data into two seperates Dmatrixs objects
labels <- training_set$SalePrice_log
dtrain <- xgb.DMatrix(data = as.matrix(training_set_new),label = labels) 
dval <- xgb.DMatrix(data = as.matrix(val_set_new))

# set parameters
param <-list(objective = "reg:linear",
             booster = "gbtree",
             eta = 0.01, #default = 0.3
             gamma=0,
             max_depth=3, #default=6
             min_child_weight=4, #default=1
             subsample=1,
             colsample_bytree=1
)

# Fitting XGBoost to the Training set
set.seed(1)
xgb_base <- xgb.train(params = param, data = dtrain, nrounds = 3000
                       #watchlist = list(train = dtrain, val = dval),
                       #print_every_n = 50, early_stopping_rounds = 300
)


## Performance Evaluation

# RMSE
# Day4 & Day5有分別用2個不同的套件計算過RMSE (Day4的Metrics & Day5套件caret中的ModelMetrics)

# 先存取用validation data跑好的預測結果, 再用Day4的Metrics分別計算Linear Regression & XGBoost 演算法的RMSE
# 1. Predicting the Validation set results
# Linear Regression
pred_reg <- predict(Reg, newdata = val_set)
# XGBoost
pred_xgb_base <- predict(xgb_base, dval)

# 2. RMSE-Metrics Package
# Linear Regression
rmse(val_set$SalePrice_log, pred_reg)
# XGBoost
rmse(val_set$SalePrice_log, pred_xgb_base)

# 還原公式, 計算MSE & RMSE, 以XGBoost預測結果為例
mse_cal <- mean((pred_xgb_base - val_set$SalePrice_log)**2)
print(mse_cal)

rmse_cal <- sqrt(mse_cal)
print(rmse_cal)

# 3. 計算MAE
# XGBoost - Metrics Package
mae(val_set$SalePrice_log, pred_xgb_base)

# 計算結果相同, 0.1442952 = 0.1442952

##------------------------------------------------
## Part 3 : 選擇模型產出上傳 Kaggle 結果檔
##------------------------------------------------
# 本次Kaggle競賽採用的衡量基準為testset log(SalePrice)預測值與真實值的RMSE,越小越好
# 選取Validation set RMSE最小的模型
# Steps:
# 1. 套用最好的模型跑testset
# 2. 模型的預測標的Sale_Price做過log轉換,若最後上傳時要用exp轉換回原始的Sale Price

# 還原log的範例
x <- 87
x_log <- log(x)
exp(x_log)

##------------------------------------------------
## 本日小挑戰
##------------------------------------------------
#A. 找出你最好的模型，上傳你的testset預測結果到Kaggle平台上吧! 並記錄下Kaggle幫你算出來的實際RMSE是多少
#B. 如何得出模型的R-Squared
