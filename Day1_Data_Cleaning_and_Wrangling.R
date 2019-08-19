##---------------- 設定環境 ----------------
#setwd(dir) #設定working directory的存放位置
# MAC : setwd("/Users/rladiestaipei/R_DragonBall/") 
# Windows : setwd("C://Users/rladiestaipei/Desktop/R_DragonBall/") 

#安裝套件(僅需執行一次)
#install.packages(c("tidyverse", "ploty", "zoo", "lubridate", "rmarkdown","data.table", "DT", "kableExtra"), dependencies = TRUE)

#load packages
library(DT)
library(zoo)
library(plotly)
library(lubridate)
library(rmarkdown)
library(data.table)
library(tidyverse)
library(kableExtra)
options(dplyr.print_max=1e9)

##------------------------------------------------
## Part 1: Read and load data
##------------------------------------------------
#資料需放在此workspace中，若不是的話需要指定路徑
train1<-read.csv("train.csv")  
train2<-fread("train.csv")
#str(train1)        #檢視所有欄位
str(train1[,1:10])  #避免佔用教材過大篇幅，僅顯示前十個欄位
#str(train2)
str(train2[,1:10])


##------------------------------------------------
## Part 2:檢視data結構
##------------------------------------------------
#讀入raw data 
train0<-read.csv("train.csv", stringsAsFactors = FALSE)
test0<-read.csv("test.csv", stringsAsFactors = FALSE)
dim(train0)  #training data共有1460個房子的資料，81個欄位
dim(test0)   #testing data共有1460個房子的資料，80個欄位
#training 與 testing 資料差別在training data多了房價 (SalesPrice)，這是testing data在建模完畢後最終要去預測的欄位
colnames(train0)[!colnames(train0) %in% colnames(test0)] 


##------------------------------------------------
## Part 3: 初階資料清理及轉換
##------------------------------------------------
dat<-train0%>%select(10:15)
head(dat)

dat<-train0%>%select(MSZoning, Utilities, HouseStyle, Heating, YearBuilt, SalePrice)
head(dat)

#1. 欄位含某個字串:選取欄位名稱含有Lot，Bsmt開頭，或是以Condition結尾
dat<-train0%>%select(contains("Lot"), starts_with("Bsmt"), ends_with("Condition"))
head(dat)

#2. 欄位符合某種pattern
dat<-train0%>%select(matches("Yr|Year|year|yr"))  #跟年份有關的欄位
head(dat)

#3. 刪除欄位
dat<-train0%>%select(-PoolArea, -Fence, -matches("Bsmt|Lot|Garage"))  #PoolArea, Fence, 含有Bsmt或Lot或Garage的欄位都刪除
colnames(dat)


plot_ly(train0, x=~SalePrice, type="histogram")

range(train0$SalePrice)  #所有資料的房價範圍
dat<-train0%>%filter(SalePrice>=100000, SalePrice<=300000)
range(dat$SalePrice)  #選取後的房價範圍
#房價100000~300000佔所有資料的比例達84.3%
scales::percent(nrow(dat)/nrow(train0))

table(train0$SaleType)  #SaleType當中WD類型最多
#選SaleTeyp=="WD"的資料(WD:Warranty Deed - Conventional)
dat<-train0%>%filter(SaleType=="WD") 
table(dat$SaleType)

table(train0$YrSold, train0$SaleType)  #售出年份與SaleType的數量
#選2008年以前售出，而且SaleType為New(剛蓋好就賣出)的資料
dat<-train0%>%        
  filter(YrSold<2008, SaleType=="New")   
table(dat$YrSold, dat$SaleType)

dat<-train0%>%slice(1000:1001)  #第1000-1001筆資料
head(dat[,1:3])

#選出售價前五名
dat<-train0%>%top_n(5, SalePrice)    
#列出Id, 社區名稱並將售價由高至低排序
head(dat%>%select(Id, Neighborhood, SalePrice)%>%arrange(-SalePrice))

dat<-train0%>%
  group_by(Neighborhood)%>%
  summarise(low=min(SalePrice),
            high=max(SalePrice),
            average=mean(SalePrice),
            sd=sd(SalePrice))%>%
  arrange(-average)

dat<-train0%>%
  group_by(Neighborhood, Street)%>%
  summarise(low=min(SalePrice),
            high=max(SalePrice),
            average=mean(SalePrice),
            sd=sd(SalePrice))


dat<-train0%>%
  group_by(YearBuilt, MasVnrType)%>%
  summarise(average=mean(SalePrice))%>%
  arrange(-average)

plot_ly(dat, x = ~YearBuilt, y = ~average, text = ~MasVnrType, type = 'scatter', mode = 'markers', size = ~average, color = ~MasVnrType,
        #Choosing the range of the bubbles' sizes:
        sizes = c(10, 80),
        marker = list(opacity = 0.5, sizemode = 'diameter')) %>%
  layout(title = 'Estate Sale Price by Neighborhood',
         xaxis = list(showgrid = FALSE),
         yaxis = list(showgrid = FALSE),
         showlegend = TRUE)

dat<-train0%>%   
  select(BsmtQual)%>%
  rename(BsmtHght=BsmtQual)   
head(dat)

colnames(train0%>%select(contains("Bsmt"))) 

#以BsmtFullBath, BsmtHalfBath為例，可以將兩個欄位合併成BsmtBath
Bsmt<-train0%>%                    
  select(matches("Bsmt.*Bath"))%>%
  mutate(BsmtBath=case_when(BsmtFullBath>0|BsmtHalfBath>0~1,  #只要有Bath，無論種類都標示為1，否則為0
                            TRUE~0))
head(Bsmt%>%arrange(-BsmtBath))

dat<-train0%>%                         
  group_by(OverallQual)%>%
  mutate(average_SalePrice=mean(SalePrice))%>%    
  select(Id, OverallQual, SalePrice)
dat[1:10,] 

dcr0<-read.delim("data_description.txt", header = FALSE, stringsAsFactors = FALSE)
datatable(dcr0)

dcr<-dcr0%>%
  #先切開欄位名稱跟該欄位的數值種類(by tab or space)，切完後取前面那段
  mutate(feature=sapply(strsplit(V1, '\t|[[:space:]]'), "[", 1))%>%  
  filter(!is.na(feature))%>%           #為了下一步的fill, 先把空白的欄位用NA取代
  mutate_all(na_if, "")%>%             #把feature這個欄位的NA用前一個非NA的值取代
  fill(feature, .direction="down")%>% 
  rename(value=V1, description=V2)%>%  #改成比較直覺的名稱
  select(feature, value, description)

datatable(dcr, options = list(pageLength=20))


##------------------------------------------------
## Part 4: 進階資料清理及轉換
##------------------------------------------------
#BsmtFinType1
fintype1<-train0%>%             #計算每個Id在某個BsmtFinType1出現的頻率
  group_by(Id, BsmtFinType1)%>%
  summarise(count=n())%>%
  spread(BsmtFinType1, count, fill=0)  #fill=0的意思是該Id沒有某種Type時，用0取代，預設值為NA
head(fintype1)

#BsmtFinType2
fintype2<-train0%>%
  group_by(Id, BsmtFinType2)%>%
  summarise(count=n())%>%
  spread(BsmtFinType2, count, fill=0)
head(fintype2)

#合併以及加總
bsmtfintype<-
  #bind_rows與rbind的不同是前者會自行比對相同的欄位去合併，後者必須欄位序相同才行
  bind_rows(fintype1, fintype2) %>%            
  group_by(Id) %>%
  summarise_all(funs(sum(., na.rm = TRUE)))%>% 
  rename_all(function(x) paste0("BsmtFinType_", x)) #標記"BsmtFinType"當作prefix

#併回train data
train<-train0%>%
  left_join(bsmtfintype, by=c("Id"="BsmtFinType_Id"))

datatable(train%>%select(Id, contains("BsmtFinType")))


##------------------------------------------------
## 本日小挑戰
##------------------------------------------------
#請挑選training data中1-3個你覺得重要的欄位，或好幾個性質類似的欄位，進行轉換，拆解，或合併（自由發揮）。


