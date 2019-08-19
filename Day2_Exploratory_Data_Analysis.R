##---------------- 設定環境 ----------------
#setwd(dir) #設定working directory的存放位置
# MAC : setwd("/Users/rladiestaipei/R_DragonBall/") 
# Windows : setwd("C://Users/rladiestaipei/Desktop/R_DragonBall/")  

#安裝套件(僅需執行一次)
#install.packages(c("tidyverse", "ploty", "zoo", "lubridate", "rmarkdown","data.table", "DT", "kableExtra"), dependencies = TRUE)

#load packages
library(tidyverse)
library(ggplot2)
library(dplyr)
library(magrittr)
library(corrplot)
library(gridExtra)
library(plotly)
options(dplyr.print_max=1e9)

##------------------------------------------------
## Part 1 : load data and split
##------------------------------------------------
# 讀取資料
train0 <- read.csv("train.csv", stringsAsFactors = FALSE)
test0 <- read.csv("test.csv", stringsAsFactors = FALSE)

# 分割 numeric and character 欄位
num_features <- names(which(sapply(train0, is.numeric)))
cat_features <- names(which(sapply(train0, is.character)))
train_numeric <- train0[, names(train0) %in% num_features]
train_categoric <- train0[, names(train0) %in% cat_features]

print(num_features)
print(cat_features)


##------------------------------------------------
## Part 2 : 檢視 missing value
##------------------------------------------------
# 先看num_features的36個欄位
# MSSubClass(住宅類型)、OverallQual(材料與完成度評比)、OverallCond(綜合狀況評比) 很明確屬於factor，先轉換。
train0$OverallCond <- as.factor(train0$OverallCond)
train0$OverallQual <- as.factor(train0$OverallQual)
train0$MSSubClass <- as.factor(train0$MSSubClass)

# 查看每個欄位的缺失值和比例
missing_values <- sapply(train0, function(x) sum(is.na(x)))
null_count <- data.frame(Count = missing_values, Proportion = missing_values/nrow(train0))
null_count_gteZero <- null_count[null_count$Count > 0, ]
null_count_gteZero[order(-null_count_gteZero$Count),]

# 刪除所有出現NA的欄位
train_non_null <- train0 %>% 
  select(-c(rownames(null_count_gteZero), OverallCond, OverallQual, MSSubClass))


##------------------------------------------------
## Part 3 : Various plots with SalePrice
##------------------------------------------------
# 先剔除出現NA的欄位，而且是數字的欄位
match_num_features <- paste(num_features, collapse = "|")
train_non_null_df <- select(train_non_null, matches(match_num_features))

theme_set(theme_bw())  # pre-set the bw theme.
# 篩選SF(面積)的欄位 
train_SF <- select(train_non_null, matches("SF|SalePrice"))
# 各種面積與SalePrice的關係
train_SF %>%
  # keep(is.numeric) %>% 
  gather(-SalePrice, key = "var", value = "value") %>%
  ggplot(aes(x = value, y = SalePrice)) +   # Plot the values
  facet_wrap(~ var, scales = "free") +   # In separate panels
  geom_point() 

# 篩選SF(面積)的欄位 
train_Time <- select(train_non_null, matches("Yr|Year|Mo|year|yr|SalePrice"))
train_Time %>%
  gather(-SalePrice, key = "var", value = "value") %>%
  ggplot(aes(x = value, y = SalePrice)) +   # Plot the values
  facet_wrap(~ var, scales = "free") +   # In separate panels
  geom_point() 

# 篩選Area(區域空間)的欄位 
train_Area <- select(train_non_null, matches("Area|SalePrice"))
train_Area %>%
  gather(-SalePrice, key = "var", value = "value") %>%
  ggplot(aes(x = value, y = SalePrice)) +   # Plot the values
  facet_wrap(~ var, scales = "free") +   # In separate panels
  geom_point() 

# 先剔除出現NA的欄位，而且是數字的欄位
match_cat_features <- paste(cat_features, collapse = "|")
train_non_null_df <- select(train_non_null, matches(match_num_features))

# 篩選房屋外觀的欄位 
train_outside <- select(train_non_null, matches("Roof|MSSubClass|LotShape|Exterior|SalePrice"))
# 各種房屋外觀與SalePrice的關係
train_outside %>%
  gather(-SalePrice, key = "var", value = "value") %>%
  ggplot(aes(x = value, y = SalePrice)) +   # Plot the values
  facet_wrap(~ var, scales = "free") +   # In separate panels
  geom_boxplot(na.rm = T) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# 篩選房屋內部的欄位 
train_inside <- select(train_non_null, matches("BldgType|Utilities|House|Bsmt|TotRmsAbvGrd|Fireplace|SalePrice"))
train_inside %>%
  gather(-SalePrice, key = "var", value = "value") %>%
  ggplot(aes(x = value, y = SalePrice)) +   # Plot the values
  facet_wrap(~ var, scales = "free") +   # In separate panels
  geom_boxplot(na.rm = T) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# 篩選Area(區域空間)的欄位 
train_other <- select(train_non_null, matches("Electrical|Neighborhood|Street|Garage|MSZoning|SalePrice"))
train_other %>%
  gather(-SalePrice, key = "var", value = "value") %>%
  ggplot(aes(x = value, y = SalePrice)) +   # Plot the values
  facet_wrap(~ var, scales = "free") +   # In separate panels
  geom_boxplot(na.rm = T) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

correlations <- cor(train_non_null_df, use = "complete.obs")
cor_bar <- data.frame("cor" = correlations[,"SalePrice"])
cor_bar$item <- row.names(cor_bar)
cor_bar <- cor_bar[order(-cor_bar$cor),][-1,]
cor_bar$item <- factor(cor_bar$item, levels=cor_bar$item)
ggplot(cor_bar) + 
  geom_bar(stat='identity', aes(x = item, y = cor), width=.5)  +
  labs(title= "Correlations Bars") + 
  coord_flip()


##------------------------------------------------
## Part 5 : 檢視 SalePrice 分布
##------------------------------------------------
ggplot(data = train0[!is.na(train0$SalePrice),], aes(x=SalePrice)) +
  geom_histogram(fill="blue", binwidth = 10000) +
  scale_x_continuous(breaks= seq(0, 800000, by=100000))
summary(train0[!is.na(train0$SalePrice),]$SalePrice)

train0$SalePrice <- log(train0$SalePrice)
ggplot(train0, aes(x=SalePrice)) + 
  geom_histogram(fill="blue", binwidth = .05)
summary(train0[!is.na(train0$SalePrice),]$SalePrice)


##------------------------------------------------
## 本日小挑戰
##------------------------------------------------
#請挑選training data中1-3個你覺得重要的欄位，或好幾個性質類似的欄位，進行轉換，拆解，或合併（自由發揮）。


