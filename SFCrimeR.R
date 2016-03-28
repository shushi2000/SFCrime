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
map <- get_map(location = 'sanfrancisco', zoom = 12, source = 'osm')

ggmap(map, extent = 'device')+
  geom_point(data = sample_n(data_train,10000), aes(x=X, y=Y),
             alpha = 1/10, color='red')+
  scale_colour_brewer(type='qual')


map_crime <- function(crime_df, crime){
  filtered <- filter(crime_df, Cateogry %in% crime)
  plot <- ggmap(map, extent = 'device')+
    geom_point(data = filtered,
               aes(x=X, y=Y, color = Category),
               alpha = 0.1)
  return (plot)
}

plot_crime_day <- function(crime_df, crime, wday){
  filtered <- filter(crime_df, Category %in% crime)
  filtered <- summarise(group_by(filtered, Month, Years),
                        count = n())
  print (filtered)
  plot <- ggplot(data = filtered,
                 aes(x=Month, y = count, color = Years))
  return (plot)
}

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


map_contours <- function (data_trunc, alp){
  p1 = ggmap(map, extent = 'device')+
    geom_point(data = data_trunc, aes(x=X, y=Y), alpha = apl)+
    stat_density2d(aes(x=X, y=Y,
                       fill = ..level.., alpha = ..level..),
                   size = 0.1, data = data_trunc, n = 100,
                   geom = 'polygon')+
    theme(legend.position = 'None')
  return(p1)
}


plot_marginals <- function(data_trunc){
  p2 = ggplot(data=data_trunc, aes(x=X, y=Y, alpha = 0.1))+
    geom_point()
  p2 = ggMarginal(p2+theme_gray(), type = 'histogram',
                  fill = 'steelblue', col = 'darkblue')
  return (p2)
}


data_train_new = make_vars_date(data_train)
data_test_new = make_vars_date(data_test)
data_train_new_ss = sample_n(data_train, 10000)

data_plot = data_train %>% 
  group_by(Category) %>%
  summarise(count=n()) %>%
  transform(Category = reorder(Category, -count))

ggplot(data_plot)+
  geom_bar(aes(x=Category, y = count, color = Category, fill = Category),
           stat = 'identity')+
  coord_flip()+
  theme(legend.position = 'none')+
  ggtitle('Number of crimes in individual category')+
  xlab('Number of crimes')+
  ylab('Category of crime')

data_plot = data_plot[with(data_plot, order(-count)),]
top_crimes = data_plot
print ('Top 10 crimes')
head(top_crimes,10)


df = data.frame()
sum = 0
for (i in 1:dim(top_crimes)[1]){
  sum = sum + top_crimes[i,2]
  top_crimes$CumCr[i] = sum/sum(top_crimes$count)
}

per_20 = top_crimes$CumCr[20]*100
print (paste('Percentage of crimes in top 20 categories = ',
             as.character(per_20)))


data_plot = data_train_new %>%
  subset(Category %in% top_10) %>%
  group_by(Years, Category, Month) %>%
  summarise(count=n())

data_plot$Category = factor(data_plot$Category, top_10)

ggplot(data = data_plot, aes(x=Years, y= count, fill = Category))+
  geom_boxplot()+
  facet_wrap(~Category, ncol = 5)+
  theme(legend.position = 'None',
        axis.text.x = element_text(angle = 90, hjust = 1))+
  xlab('Year')+
  ylab('Number of crime incidents')+
  ggtitle('Variations in crime by year')

months_name = c('Jan', 'Feb', 'Mar','Apr','May',
                'Jun', 'Jul', 'Aug', 'Sep', 'Oct',
                'Nov', 'Dec')

data_plot = data_train_new %>% 
  group_by(DayOfWeek, Years, Month, YearsMo) %>%
  summarise(count = n())

ggplot(data_plot,aes(x=DayOfWeek, y=count, fill = DayOfWeek))+
  geom_boxplot()+
  theme(legend.position = 'None')+
  xlab('Day of Week')+
  ylab('Number of crime incidents')+
  coord_cartesian(ylim = c(300,1200))


## Feature Engineering
make_factors <- function (df){
  df$Years = paste('Yr', df$Years, sep='.')
  df$Years = factor(df$Years)
  y <- as.data.frame(model.matrix((~df$Years - 1)))
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


set.seed(22)
target <- data_train$Category
train = make_factors(data_train_new)
gc()
model <- LiblineaR (train, target, type = 7, verbose = FALSE)
