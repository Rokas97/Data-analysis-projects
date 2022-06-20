## ----setup, include=FALSE---------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ----results = 'hide', message = FALSE--------------------------------------------------------------------------------
library(Hmisc)
library(corrplot)
library(plyr)
library(dplyr)
library(scales)
library(viridis)
library(lubridate)
library(knitr)
library(caTools)
library(leaps)
library(cvms)  #fro confusion matrix
library(earth) #for mars model
library(caret) # for mars tuning and cross-validation
library(vip)    # for variable importance plots(vips)
library(rpart)  #for decision trees
library(gam) #for GAMs
library(gbm) #for gbm
library(Metrics) #for gbm
library(xgboost)
library(rstatix)  #for outliers identification



## ----reading csv file-------------------------------------------------------------------------------------------------
seoul_bike <- read.csv("raw_seoul_bike_sharing.csv")
str(seoul_bike)
summary(seoul_bike)



## ----remvoing NA values-----------------------------------------------------------------------------------------------

sapply(seoul_bike, function(x) sum(is.na(x)))
clean_bike <- na.omit(seoul_bike)
str(clean_bike)


## ----lower cases------------------------------------------------------------------------------------------------------
names(clean_bike) <- tolower(names(clean_bike))
names(clean_bike)


## ----cleaning---------------------------------------------------------------------------------------------------------
clean_bike$date <- dmy(clean_bike$date)
clean_bike$holiday <- ifelse(clean_bike$holiday == 'No Holiday' ,0, 1)
#clean_bike$seasons <- recode(clean_bike$seasons, 'Winter'= 1, 'Spring'= 2, 'Summer'= 3, 'Autumn'= 4)
clean_bike$seasons <- as.factor(clean_bike$seasons)
class(clean_bike$seasons)

clean_bike$day <- weekdays(as.Date(clean_bike$date))
clean_bike$day <- as.factor(clean_bike$day)



## ----tables for analysis----------------------------------------------------------------------------------------------

table(clean_bike$functioning_day)

pie(table(clean_bike$holiday), labels=c("No","Yes"))
table(clean_bike$holiday)

pie(table(clean_bike$seasons), labels=c("Winter","Spring","Summer","Autumn"))
table(clean_bike$seasons)

pie(table(clean_bike$day),labels=c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))
table(clean_bike$day)

clean_bike <- clean_bike %>% select(-functioning_day)


## ----correlation , out.width='100%', fig.align = 'left', fig.cap= "Figure 1.1 This is Caption"------------------------
corrplot(cor(clean_bike[c(2:11)]),type = "upper",col=COL2("RdBu",100))
clean_bike <- clean_bike %>% select(-dew_point_temperature)


## ----tables, message = FALSE------------------------------------------------------------------------------------------


rent_by_year <- clean_bike %>%
  mutate(year= year(clean_bike$date), month=month(clean_bike$date)) %>%
  group_by(year, month) %>%
  summarise(total = sum(rented_bike_count), average_bike_rent_count = mean(rented_bike_count), average_temperature = mean(temperature))
kable(rent_by_year)


## ---- out.width='100%', fig.align = 'left', fig.cap= "Figure 1.1 This is Caption", warning= FALSE---------------------

ggplot(clean_bike, aes( x=date, y =rented_bike_count, color= temperature)) + geom_point() +
  facet_wrap(~seasons, scales = "free_x") +
ylab('Count of Rented Bikes')  +
xlab('Date') +
ggtitle('Seasonal Bike Rent Dependency in Seoul')  + 
 scale_fill_viridis(direction = -1)+
  scale_colour_viridis_c(option = "plasma")


highlight <- clean_bike %>% filter(holiday==1)

  ggplot(clean_bike, aes( x=date, y =rented_bike_count, color= temperature)) + geom_point(alpha=0.3) +
 geom_point(data= highlight , aes(x=date, y =rented_bike_count), color ='black', size=4) +
ylab('Count of Rented Bikes')  +
xlab('Date') +
ggtitle('Seasonal Bike Rent Dependency in Seoul with highlighted holidays')  + 
    scale_fill_viridis(direction = -1)+
  scale_colour_viridis_c(option = "plasma")



## ----hour-a-----------------------------------------------------------------------------------------------------------
table(clean_bike$hour)

ggplot(clean_bike, aes(x= hour, y = rented_bike_count)) + geom_col() +  scale_x_continuous(breaks=seq(0,23,1)) + ggtitle("Bike rent count dependancy on hour")


## ----hour-b-----------------------------------------------------------------------------------------------------------

ggplot(clean_bike, aes(x= hour, y = rented_bike_count)) + geom_boxplot(aes(group = hour)) +  scale_x_continuous(breaks=seq(0,23,1)) +
  coord_flip() + ggtitle("Bike rent count dependancy on hour")

outliers_hour <- clean_bike %>%
  group_by(hour) %>%
  identify_outliers(rented_bike_count)
outliers_hour

clean_bike <- clean_bike %>% anti_join(outliers_hour, by = c( "hour", "rented_bike_count"))



## ----hour-c-----------------------------------------------------------------------------------------------------------
ggplot(clean_bike, aes(x= hour, y = rented_bike_count, fill = day)) + geom_col() +
scale_x_continuous(breaks=seq(0,23,2))+ facet_wrap(~day) + theme(legend.position='none' ) + ggtitle("Daily bike rent count dependancy on hour")


## ----temp-------------------------------------------------------------------------------------------------------------
hist(clean_bike$temperature  ,xlab= "Temperature", main= "Temperature distribution", xaxp = c(-20,40,24) )

summary(clean_bike$temperature)

ggplot(clean_bike, aes(temperature, rented_bike_count)) + geom_point() + ylim(0,4000) +geom_smooth()






## ----hum--------------------------------------------------------------------------------------------------------------

hist(clean_bike$humidity  ,xlab= "Humidity", main= "Humidity distribution" )
abline(v = mean(clean_bike$humidity),                       
       col = "red",
       lwd = 3)
hum <- clean_bike %>%
  mutate(higher_humidity= humidity > median(humidity))
 hum$higher_humidity<- as.factor(ifelse(hum$higher == TRUE, "High", "Low"))

 
  hum1_low <- hum   %>%
   group_by(seasons, higher_humidity)%>%
   summarize(mean=mean(rented_bike_count),count = n())
 
 
ggplot(hum, aes(humidity, rented_bike_count, color = higher_humidity)) + geom_point() + facet_wrap(~seasons)

ggplot(hum, aes(higher_humidity, rented_bike_count, fill = higher_humidity)) + geom_bar( stat = "summary", fun = "mean") + facet_wrap(~seasons)






## ----wind-------------------------------------------------------------------------------------------------------------

hist(clean_bike$wind_speed  ,xlab= "Wind speed", main= "Wind speed distribution" )
ggplot(clean_bike, aes(wind_speed, rented_bike_count)) + geom_point() + geom_smooth()

summary(clean_bike$wind_speed )


## ----vis--------------------------------------------------------------------------------------------------------------
hist(clean_bike$visibility ,xlab= "Visibility", main= "Visibility distribution" )


ggplot(clean_bike, aes(visibility, rented_bike_count)) + geom_point() + geom_smooth()




## ----solar------------------------------------------------------------------------------------------------------------
hist(clean$solar_radiation ,xlab= "Solar radiation", main= "Solar radiation distribution" )

ggplot(solar, aes(solar_radiation, rented_bike_count, color= seasons)) + geom_point() + facet_wrap(~seasons) + 
  theme(legend.position = "none") + ggtitle("Seasonal bike rent count dependency on solar radiation")





## ----rain-------------------------------------------------------------------------------------------------------------
hist(clean_bike$rainfall ,xlab= "Rainfall", main= "Rainfall distribution" )

rain <- clean_bike %>% mutate(is_raining = rainfall > 0)

rain$is_raining <- as.factor(ifelse(rain$is_raining == TRUE,"Yes", "No" ))
rain %>% summarize(raining_ratio = mean(is_raining== "Yes"))
rain_yes_mean <- rain %>% filter(is_raining == "Yes") %>% summarize(avg_bike_count = mean(rented_bike_count))
rain_no_mean <- rain %>% filter(is_raining == "No") %>% summarize(avg_bike_count = mean(rented_bike_count))
rain_yes_mean
rain_no_mean


ggplot(rain, aes(rainfall, rented_bike_count, color= is_raining)) + geom_point() + ggtitle("Rented bike count dependency on rainfall") + 
  theme(legend.position = "none") + facet_wrap(~seasons)
## ----snowfall-------------------------------------------------------------------------------------------------------------
hist(clean_bike$snowfall ,xlab= "Snowfall", main= "Snowfall distribution" )

snow <- clean_bike %>% mutate(is_snowing = snowfall > 0)

snow$is_snowing <- as.factor(ifelse(snow$is_snowing == TRUE,"Yes", "No" )) 
snow %>% summarize(snowing_ratio = mean(is_snowing== "Yes"))
snow_no_mean
ggplot(snow, aes(snowfall, rented_bike_count, color= is_snowing)) + geom_point() + ggtitle("Rented bike count dependency on snowfall") + 
  theme(legend.position = "none") + facet_wrap(~seasons)

## ----data splitting---------------------------------------------------------------------------------------------------
split = sort(sample(nrow(clean_bike), nrow(clean_bike)*.7))
train<-clean_bike[split,]
test<-clean_bike[-split,]
dim(train)
dim(test)
glimpse(train)



## ----mars, eval = FALSE-----------------------------------------------------------------------------------------------
## 
## marsm <- earth(rented_bike_count ~ ., data = train, degree = 1)
## 
## 
## marsm
## summary(marsm)
## plot(marsm, which = 1)
## axis(1, at = 1:20)
## 
## knitr::knit_exit()
## 
## 


## ----mars2, eval = FALSE----------------------------------------------------------------------------------------------
## hyper_grid <- expand.grid(
##   degree = 1:3,
##   nprune = seq(2, 30, length.out = 10) %>% floor()
##   )
## 
## tuned_mars <- train(
##   x = subset(train, select = -rented_bike_count),
##   y = train$rented_bike_count,
##   method = "earth",
##   metric = "RMSE",
##   trControl = trainControl(method = "cv", number = 10),
##   tuneGrid = hyper_grid
## )
## tuned_mars$bestTune
## 
## ggplot(tuned_mars)+
##   scale_x_continuous(breaks = seq(0, 30, by = 5))
## 
## 
## p1 <- vip(tuned_mars, num_features = 20, bar = FALSE, value = "gcv") + ggtitle("GCV")
## p2 <- vip(tuned_mars, num_features = 20, bar = FALSE, value = "rss") + ggtitle("RSS")
## 
## gridExtra::grid.arrange(p1, p2, ncol = 2)
## 
## 


## ----decision tree----------------------------------------------------------------------------------------------------
treem <- rpart(rented_bike_count ~ ., 
             method = "anova", data = train)
treem

plot(treem, uniform = TRUE,
          main = "... 
                 Tree using Regression")
text(treem, use.n = TRUE, cex = .7)

# Step 1 - create the evaluation metrics function


eval_results <- function(true, predicted, df) {

  SSE <- sum((predicted - true)^2)

  SST <- sum((true - mean(true))^2)

  R_square <- 1 - SSE / SST

  RMSE = sqrt(SSE/nrow(df))

  

# Model performance metrics

  data.frame(

    RMSE = RMSE,

    Rsquare = R_square

  )

  

}


# Step 2 - predicting and evaluating the model on train data


predictions_train_cart = predict(treem, data = train)

eval_results(train$rented_bike_count, predictions_train_cart, train)


# Step 3 - predicting and evaluating the model on test data


predictions_test_cart = predict(treem, newdata = test)

eval_results(test$rented_bike_count, predictions_test_cart, test)



## ----GAMs-------------------------------------------------------------------------------------------------------------
GAMsm <- gam(rented_bike_count ~ .,data = train)
summary(GAMsm)





## ----GBM--------------------------------------------------------------------------------------------------------------
model_gbm <- gbm(formula = rented_bike_count ~.,
                data = train[,c(-1,-13)],
                distribution = "gaussian",
               cv.folds = 5,
               interaction.depth = 2,
                shrinkage = .01,
                n.minobsinnode = 10,
                n.trees = 5000,
               n.cores = NULL, # will use all cores by defaul
               verbose = FALSE)
 

print(model_gbm)

perf_gbm1 = gbm.perf(model_gbm, method = "cv")
print(perf_gbm1)


bike_prediction_1 <- stats::predict(
                          object = model_gbm, 
                          newdata = test,
                          n.trees = perf_gbm1)

rmse_fit1 <- Metrics::rmse(actual = test$rented_bike_count, 
                           predicted = bike_prediction_1)


print(rmse_fit1)

min_MSE <- which.min(model_gbm$cv.error)

sqrt(model_gbm$cv.error[min_MSE])


gbm.perf( model_gbm, method = "cv")




## ----XGBoost, eval = FALSE--------------------------------------------------------------------------------------------
## 
## 
## X_train = data.matrix(train[,-2])                  # independent variables for train
## y_train = train[,2]                                # dependent variables for train
## 
## X_test = data.matrix(test[,-2])                    # independent variables for test
## y_test = test[,2]                                   # dependent variables for test
## 
## # convert the train and test data into xgboost matrix type.
## xgboost_train = xgb.DMatrix(data=X_train, label=y_train)
## xgboost_test = xgb.DMatrix(data=X_test, label=y_test)
## 
## 
## xgm <- xgboost(data = xgboost_train,                    # the data
##                  max.depth=3,                # max depth
##                scale_pos_weight = 1,
##                early_stopping_rounds = 3,
##                mode="regression",
##                  nrounds=5000)                              # max number of boosting iterations
## 
## summary(xgm)
## xgm
## 
## #pred_test <- predict(xgm, xgboost_test)
## 
## #pred_test
## 
## 


## ---------------------------------------------------------------------------------------------------------------------



