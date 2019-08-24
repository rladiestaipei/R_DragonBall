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

Mode <- function(x) {
  u_x <- unique(x)
  u_x[which.max(tabulate(match(x, u_x)))]
}
##------------------------------------------------
## Prepare Training Data
##------------------------------------------------
train0 <- read.csv("train.csv", stringsAsFactors = FALSE)

missing_values <- sapply(train0, function(x) sum(is.na(x)))
null_count <- data.frame(Column =names(missing_values), 
                         Count = missing_values, 
                         Proportion = missing_values/nrow(train0)) %>%
  filter(Count > 0) %>%
  arrange(-Count)
#刪掉缺失超過 80％ 的欄位
train0 <- train0 %>% 
  select(-c(as.vector(null_count$Column[null_count$Proportion>0.8])))

train0 <- train0 %>%
  replace_na(list(GarageCars = 0, GarageArea = 0, BsmtFullBath = 0, 
                  BsmtHalfBath = 0, BsmtFinSF1 = 0, BsmtFinSF2 = 0, 
                  BsmtUnfSF = 0, TotalBsmtSF = 0, MasVnrArea = 0))
train0$LotFrontage[is.na(train0$LotFrontage)] <- median(train0$LotFrontage, na.rm = T)
train0$GarageYrBlt[is.na(train0$GarageYrBlt)] <- train0$YearBuilt[is.na(train0$GarageYrBlt)]
train0 <- train0 %>%
  replace_na(list(GarageFinish = "None", GarageQual = "None", 
                  GarageCond = "None", GarageType = "None",
                  BsmtExposure = "None", BsmtQual = "None",
                  BsmtCond = "None", BsmtFinType1 = "None",
                  BsmtFinType2 = "None", MasVnrType = "None", FireplaceQu="None"))
train0$Electrical[is.na(train0$Electrical)] <- Mode(train0$Electrical)
train0$GrLivArea_stand <- scale(train0$GrLivArea)
train0$MasVnrArea_stand <- scale(train0$MasVnrArea)
train0$LotFrontage_log <- log(train0$LotFrontage)
train0$SalePrice_log <- log(train0$SalePrice)
train0$is_Fireplace <- ifelse(train0$Fireplaces>0, "1", "0")
Qualities <- c('None' = 0, 'Po' = 1, 'Fa' = 2, 'TA' = 3, 'Gd' = 4, 'Ex' = 5)       
train0$BsmtCond <- as.integer( revalue(train0$BsmtCond, Qualities) )
train0$TotalBathrooms <- train0$FullBath+(train0$HalfBath*0.5)+train0$BsmtFullBath+(train0$BsmtHalfBath*0.5)
train0$TotalSF <- train0$TotalBsmtSF + train0$X1stFlrSF + train0$X2ndFlrSF 
train0$TotalSF_stand <- scale(train0$TotalSF)

train0 <- train0 %>%
  select(-c(GrLivArea,MasVnrArea,LotFrontage,SalePrice,TotalSF)) %>%
  as.data.frame(.)
  
##------------------------------------------------
## Prepare Testing Data
##------------------------------------------------
test0 <- read.csv("test.csv", stringsAsFactors = FALSE)

test0 <- test0 %>% 
  select(-c(as.vector(null_count$Column[null_count$Proportion>0.8])))

test0 <- test0 %>%
  replace_na(list(GarageCars = 0, GarageArea = 0, BsmtFullBath = 0, 
                  BsmtHalfBath = 0, BsmtFinSF1 = 0, BsmtFinSF2 = 0, 
                  BsmtUnfSF = 0, TotalBsmtSF = 0, MasVnrArea = 0))
test0$LotFrontage[is.na(test0$LotFrontage)] <- median(test0$LotFrontage, na.rm = T)
test0$GarageYrBlt[is.na(test0$GarageYrBlt)] <- test0$YearBuilt[is.na(test0$GarageYrBlt)]
test0 <- test0 %>%
  replace_na(list(GarageFinish = "None", GarageQual = "None", 
                  GarageCond = "None", GarageType = "None",
                  BsmtExposure = "None", BsmtQual = "None",
                  BsmtCond = "None", BsmtFinType1 = "None",
                  BsmtFinType2 = "None", MasVnrType = "None", FireplaceQu="None"))
test0$Electrical[is.na(test0$Electrical)] <- Mode(test0$Electrical)
test0$GrLivArea_stand <- scale(test0$GrLivArea)
test0$MasVnrArea_stand <- scale(test0$MasVnrArea)
test0$LotFrontage_log <- log(test0$LotFrontage)
test0$is_Fireplace <- ifelse(test0$Fireplaces>0, "1", "0")
Qualities <- c('None' = 0, 'Po' = 1, 'Fa' = 2, 'TA' = 3, 'Gd' = 4)       
test0$BsmtCond <- as.integer( revalue(test0$BsmtCond, Qualities) )
test0$TotalBathrooms <- test0$FullBath+(test0$HalfBath*0.5)+test0$BsmtFullBath+(test0$BsmtHalfBath*0.5)
test0$TotalSF <- test0$TotalBsmtSF + test0$X1stFlrSF + test0$X2ndFlrSF 
test0$TotalSF_stand <- scale(test0$TotalSF)

test0 <- test0 %>%
  select(-c(GrLivArea,MasVnrArea,LotFrontage,TotalSF)) %>%
  as.data.frame(.)


##------------------------------------------------
## Output Data
##------------------------------------------------
write.csv(train0, file = "train_new.csv", row.names = FALSE)
write.csv(test0, file = "test_new.csv", row.names = FALSE)

#Delete Column : PoolQC, MiscFeature, Alley, Fence, GrLivArea, MasVnrArea, LotFrontage, SalePrice
#Add Column : GrLivArea_stand, MasVnrArea_stand, LotFrontage_log, SalePrice_log, is_Fireplace, TotalBathrooms, TotalSF_stand