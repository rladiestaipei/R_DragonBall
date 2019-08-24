##---------------- 設定環境 ----------------
setwd("/Users/rladiestaipei/R_DragonBall/")

library(shiny)
library(caTools)
library(e1071)
library(rpart)
library(randomForest)
library(xgboost)
library(dplyr)
library(Metrics)

##---------------- Load Data ----------------
dataset <- read.csv("train_new.csv")

# select features you want to put in models
# 這邊請根據前面幾天所學的去放入你認為重要的變數(如下只是範例)
dataset <- dataset %>% dplyr::select(SalePrice_log, X1stFlrSF, TotalBsmtSF, YearBuilt,
                                     LotArea, Neighborhood, GarageCars, GarageArea, GrLivArea_stand, 
                                     MasVnrArea_stand, LotFrontage_log, is_Fireplace, TotalBathrooms, 
                                     TotalSF_stand)

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
