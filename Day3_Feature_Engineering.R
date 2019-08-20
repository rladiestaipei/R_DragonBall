##---------------- 設定環境 ----------------
#setwd(dir) #設定working directory的存放位置
# MAC : setwd("/Users/rladiestaipei/R_DragonBall/") 
# Windows : setwd("C://Users/rladiestaipei/Desktop/R_DragonBall/")  

#安裝套件(僅需執行一次)
#install.packages(c("tidyverse","purrr","tidyr","plyr"), dependencies = TRUE)

#load packages
library(tidyverse)
library(purrr)
library(tidyr)
library(plyr)
options(dplyr.print_max=1e9)


##------------------------------------------------
## Part 1 : 特徵工程為何重要
##------------------------------------------------
train0 <- read.csv("train.csv", stringsAsFactors = FALSE)

# 分割 Numeric and Character 欄位
num_features <- names(which(sapply(train0, is.numeric)))
cat_features <- names(which(sapply(train0, is.character)))
train_numeric <- train0[, names(train0) %in% num_features]
train_categoric <- train0[, names(train0) %in% cat_features]

train0$OverallCond <- as.factor(train0$OverallCond)
train0$OverallQual <- as.factor(train0$OverallQual)
train0$MSSubClass <- as.factor(train0$MSSubClass)

missing_values <- sapply(train0, function(x) sum(is.na(x)))

null_count <- data.frame(Column =names(missing_values), 
                         Count = missing_values, 
                         Proportion = missing_values/nrow(train0)) %>%
  filter(Count > 0) %>%
  arrange(-Count)

#刪掉缺失超過 80％ 的欄位
train0 <- train0 %>% 
  select(-c(as.vector(null_count$Column[null_count$Proportion>0.8])))


#車庫相關欄位 (GarageCars, GarageArea)
#地下室相關欄位 (BsmtFullBath, BsmtHalfBath, BsmtFinSF1, BsmtFinSF2, BsmtUnfSF, TotalBsmtSF)
#外觀相關欄位 (MasVnrArea)
train0 <- train0 %>%
  replace_na(list(GarageCars = 0, GarageArea = 0, BsmtFullBath = 0, 
                  BsmtHalfBath = 0, BsmtFinSF1 = 0, BsmtFinSF2 = 0, 
                  BsmtUnfSF = 0, TotalBsmtSF = 0, MasVnrArea = 0))

# LotFrontage(前面街道長度) 使用中位數取代
train0$LotFrontage[is.na(train0$LotFrontage)] <- median(train0$LotFrontage, na.rm = T)

# GarageYrBlt(車庫年份) 使用 YearBuilt(房子建造年份) 取代
train0$GarageYrBlt[is.na(train0$GarageYrBlt)] <- train0$YearBuilt[is.na(train0$GarageYrBlt)]


#車庫相關欄位 (GarageFinish , GarageQual, GarageCond, GarageType)
#地下室相關欄位 (BsmtCond, BsmtExposure, BsmtQual, BsmtFinType2, BsmtFinType1)
#其他欄位 (MasVnrType, FireplaceQu)
train0 <- train0 %>%
  replace_na(list(GarageFinish = "None", GarageQual = "None", 
                  GarageCond = "None", GarageType = "None",
                  BsmtExposure = "None", BsmtQual = "None",
                  BsmtCond = "None", BsmtFinType1 = "None",
                  BsmtFinType2 = "None", MasVnrType = "None", FireplaceQu="None"))

# Electrical(電力系統) 使用眾數取代
#建立 Mode Function
Mode <- function(x) {
  u_x <- unique(x)
  u_x[which.max(tabulate(match(x, u_x)))]
}
train0$Electrical[is.na(train0$Electrical)] <- Mode(train0$Electrical)


#將面積相關的欄位 (GrLivArea, TotalSF, MasVnrArea) 做標準化
train0$GrLivArea_stand <- scale(train0$GrLivArea)
train0$MasVnrArea_stand <- scale(train0$MasVnrArea)


#將面積相關的欄位 (LotFrontage, SalePrice) 做 log transformation
train0$LotFrontage_log <- log(train0$LotFrontage)
train0$SalePrice_log <- log(train0$SalePrice)

#新增一個欄位，判斷房子是否有壁爐
train0$is_Fireplace <- ifelse(train0$Fireplaces>0, "1", "0")


#將地下室等級(BsmtCond)轉換成數值 
#Ex=Excellent,Gd=Good,TA=Typical,Fa=Fair,Po=Poor,None=No Basement 
Qualities <- c('None' = 0, 'Po' = 1, 'Fa' = 2, 'TA' = 3, 'Gd' = 4, 'Ex' = 5)       
train0$BsmtCond <- as.integer( revalue(train0$BsmtCond, Qualities) )


#建立一個總共 bathroom 數量
train0$TotalBathrooms <- train0$FullBath+(train0$HalfBath*0.5)+train0$BsmtFullBath+(train0$BsmtHalfBath*0.5)


##------------------------------------------------
## 本日小挑戰
##------------------------------------------------
#請利用 `TotalBsmtSF` 、 `X1stFlrSF` 、 `X2ndFlrSF` 
#1.建立出一個總房屋面積 `TotalSF`
#2.將 `TotalSF` 做標準化轉換

