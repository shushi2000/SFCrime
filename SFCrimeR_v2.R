setwd("C:/Users/Segovia/Box Sync/pyCodes/Kaggle/SFCrime")
rm(list=ls())
library(ggplot2)
library(ggmap)
library(dplyr)
library(gridExtra)
library(caret)
library(e1071)
library(dbscan)
library(MASS)
library(ggExtra)
library(LiblineaR)
library(readr)

data_train <- read_csv('train.csv')
data_test <- read_csv('test.csv')

make_vars_date <- function (crime_df){
  crime_df$Years = strftime(strptime(crime_df$Dates,
                                     "%Y-%m-%d %H:%M:%S"), '%Y')
  crime_df$Month = strftime(strptime(crime_df$Dates,
                                     "%Y-%m-%d %H:%M:%S"), '%m')
  crime_df$DayOfMonth = strftime(strptime(crime_df$Dates,
                                     "%Y-%m-%d %H:%M:%S"), '%d')
  crime_df$Hour = strftime(strptime(crime_df$Dates,
                                     "%Y-%m-%d %H:%M:%S"), '%H')
  crime_df$YearsMo = paste(crime_df$Years, crime_df$Month, sep='-')
  
  
  crime_df$DayOfWeek <- factor(crime_df$DayOfWeek, 
                               levels = c('Monday', 'Tuesday','Wednesday',
                                          'Thursday','Friday',
                                          'Saturday','Sunday'),
                               ordered = TRUE)
  crime_df$weekday = 'Weekday'
  crime_df$weekday[crime_df$DayOfWeek == 'Saturday' |
                     crime_df$DayOfWeek == 'Sunday'] = 'Weekend'
  crime_df$weekday[crime_df$DayOfWeek == 'Monday' |
                     crime_df$DayOfWeek == 'Friday'] = 'SemiWeekend'
  addr_spl = strsplit(as.character(crime_df$Address), '/')
  crime_df$AddressType = 'Non-Intersection'
  ind_inxn = vector()
  ind_inxn = sapply(1:dim(crime_df)[1],
                    function(x) length(addr_spl[[x]] == 2))
  crime_df$AddressType[ ind_inxn] = 'Intersection'
  
  return (crime_df)
  
}

data_train_new = make_vars_date(data_train)
data_test_new = make_vars_date(data_test)
data_train_new_ss = sample_n(data_train, 10000)

months_name = c('Jan', 'Feb', 'Mar','Apr','May',
                'Jun', 'Jul', 'Aug', 'Sep', 'Oct',
                'Nov', 'Dec')

## Feature Engineering
make_factors <- function (df){
  df$Years = paste('Yr', df$Years, sep='.')
  df$Years = factor(df$Years)
  y <- as.data.frame(model.matrix(~df$Years - 1))
  names(y) <- levels(df$Years)
  
  
  df$Hour = paste('Hr', df$Hour, sep = '.')
  df$Hour = factor(df$Hour)
  h <- as.data.frame(model.matrix(~df$Hour - 1))
  names(h) <- levels(df$Hour)
  
  dow <- as.data.frame(model.matrix(~df$DayOfWeek -1))
  names(dow) <- levels(df$DayOfWeek)
  
  df$Month = paste('Mon', df$Month, sep='.')
  df$Month = factor(df$Month)
  m <- as.data.frame(model.matrix(~df$Month -1))
  names(m) <- levels(df$Month)
  head(m)
  
  district <- as.data.frame(model.matrix(~df$PdDistrict - 1))
  names(district) <- levels(df$PdDistrict)
  
  df$pY = paste(df$PdDistrict, df$Years, sep='.')
  df$pY = factor(df$pY)
  pY <- as.data.frame(model.matrix(~df$pY - 1))
  names(pY) <- levels(df$pY)
  
  train <- data.frame(y, dow, h, district, m, pY)
  return (train)
    
}

MultiLogLoss <- function(act, pred){
  eps = 1e-15
  nr < nrow(pred)
  pred = matrix(sapply(pred, function(x) max(eps, x)), nrow = nr)
  pred = matrix(sapply(pred, function(x) max(1-eps,x)), nrow = nr)
  ll = sum(act*log(pred) + (1-act)*log(1-pred))
  ll = ll* -1/(nrow(act))
  return (ll)
}



# Model -------------------------------------------------------------------

set.seed(22)

target <- data_train$Category
train = make_factors(data_train_new)
test = make_factors(data_test_new)
# ss <- sample(1:dim(train)[1], 10000, replace = F)
# train2 = train[ss,]
# target2 = target[ss]
rm(list=c('data_test_new', 'data_test', 'data_train', 'data_train_new',
          'data_train_new','data_train_new_ss'))
model <- LiblineaR (train, target, type = 7, verbose = FALSE)
prediction <- predict(model, test, proba = TRUE)
submit <- data.frame(prediction$probabilities)
cn <- colnames(submit)
cn2 <- sapply(cn, FUN=function(x) {paste(strsplit(x, split='[.]')[[1]], collapse = ' ')})
colnames(submit) <- cn2
submit <- submit[, order(names(submit))]

id <- data.frame(Id = data_test$Id)
submit2 <- cbind(id, submit)
write.csv(submit2,'submit_01.csv', row.names = F)
