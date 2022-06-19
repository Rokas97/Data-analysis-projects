
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
str_clean


#
rent_by_year <- clean_bike %>%
  mutate(year= year(clean_bike$date), month=month(clean_bike$date)) %>%
  group_by(year, month) %>%
  summarise(total = sum(rented_bike_count), average_bike_rent_count = mean(rented_bike_count), average_temperature = mean(temperature))
kable(rent_by_year)




 holiday<-clean_bike %>%
   filter(holiday == 1) %>%
  group_by(day, seasons) %>%
   summarise( avg = mean(rented_bike_count)) %>%
  ggplot( aes(day,avg)) + geom_col() + facet_W
holiday
  
  
  no_holiday <-clean_bike %>%
    filter(holiday == 0) %>%
    select(day, rented_bike_count) %>%
    group_by(day) %>%
    summarise( avg = mean(rented_bike_count)) %>%

#hour
table(clean_bike$hour)

ggplot(clean_bike, aes(x= hour, y = rented_bike_count)) + geom_col() +  scale_x_continuous(breaks=seq(0,23,1)) +
 ggtitle ( "Bike rent count dependancy on hour")


#outliers
ggplot(clean_bike, aes(x= hour, y = rented_bike_count)) + geom_boxplot(aes(group = hour)) +  scale_x_continuous(breaks=seq(0,23,1)) +
  coord_flip()


df1 <- clean_bike %>%
  group_by(hour) %>%
  identify_outliers(rented_bike_count)

clean_bike %>% anti_join(df1, by = c( "hour", "rented_bike_count"))

#hour different

ggplot(clean_bike, aes(x= hour, y = rented_bike_count, fill = day)) + geom_col() +
scale_x_continuous(breaks=seq(0,23,2))+ facet_wrap(~day) + theme(legend.position='none' ) 


holiday <- clean_bike %>% filter (holiday == 1)
ggplot(clean_bike, aes(x= hour, y = max(rented_bike_count), fill = seasons)) + geom_col() +
  facet_wrap(~holiday)


## ----temp-------------------------------------------------------------------------------------------------------------
hist(clean_bike$temperature  ,xlab= "Temperature", main= "Temperature distribution", xaxp = c(-20,40,24) )
  

summary(clean_bike$temperature)

ggplot(clean_bike, aes(temperature, rented_bike_count)) + geom_point() + ylim(0,4000) +geom_smooth()
ggplot(clean_bike, aes(hour, temperature)) + geom_boxplot(aes(group=hour)) +facet_wrap(~seasons)





ggplot(clean_bike, aes(temperature, rented_bike_count, color=humidity)) + geom_point() + ylim(0,4000) +geom_smooth()




#hum
hum_autumn<- hum%>% filter(seasons == "Autumn") 
hum_summer<- hum%>% filter(seasons == "Summer") 
hum_spring<- hum%>% filter(seasons == "Spring") 
hum_aWinter<- hum%>% filter(seasons == "Winter") 

mean(clean_bike$humidity)
median(clean_bike$humidity)


hist(clean_bike$humidity  ,xlab= "Humidity", main= "Humidity distribution" )
abline(v = mean(clean_bike$humidity),                       # Add line for mean
       col = "red",
       lwd = 3)

hum <- clean_bike %>%
  mutate(higher_humidity= humidity > median(humidity))
 hum$higher_humidity<- as.factor(ifelse(hum$higher == TRUE, "High", "Low"))

 

 
 hum1_cc <- hum   %>%
   group_by(seasons, higher_humidity)%>%
   summarize(mean=median(rented_bike_count),count = n())
 
 
ggplot(hum, aes(humidity, rented_bike_count, color = higher_humidity)) + geom_point() + facet_wrap(~seasons)

ggplot(hum, aes(higher_humidity, rented_bike_count, fill = higher_humidity)) + geom_bar( stat = "summary", fun = "mean") + facet_wrap(~seasons)

#wind_speed
hist(clean_bike$wind_speed  ,xlab= "Wind speed", main= "Wind speed distribution" )
ggplot(clean_bike, aes(wind_speed, rented_bike_count)) + geom_point() + geom_smooth()

summary(clean_bike$wind_speed )


#visibility
hist(clean_bike$visibility ,xlab= "Visibility", main= "Visibility distribution" )


ggplot(clean_bike, aes(visibility, rented_bike_count)) + geom_point() + geom_smooth

sum(clean_bike$visibility > 1800)
sum(clean_bike$visibility <1800)


#solar_radiation



#Seasonal

## ---- out.width='100%', fig.align = 'left', fig.cap= "Figure 1.1 This is Caption", warning= FALSE---------------------

#knitr::knit_exit()

highlight <- clean_bike %>% filter(holiday==1)

ggplot(clean_bike, aes( x=date, y =rented_bike_count, color= temperature)) + geom_point(alpha=0.3) +
  facet_wrap(~seasons, scales = "free_x") +
ylab('Count of Rented Bikes')  +
xlab('Date') +
ggtitle('Seasonal Bike Rent Dependency in Seoul')  + 
 scale_fill_viridis(direction = -1)+
  scale_colour_viridis_c(option = "plasma")


ggplot(clean_bike, aes( x=date, y =rented_bike_count, color= temperature)) + geom_point(alpha=0.3) +
  geom_point(data= highlight , aes(x=date, y =rented_bike_count), color ='black', size=4) +
  ylab('Count of Rented Bikes')  +
  xlab('Date') +
  ggtitle('Seasonal Bike Rent Dependency in Seoul')  + 
  scale_fill_viridis(direction = -1)+
  scale_colour_viridis_c(option = "plasma")


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

# model performance
perf_gbm1 = gbm.perf(model_gbm, method = "cv")
print(perf_gbm1)


bike_prediction_1 <- stats::predict(
                           # the model from above
                          object = model_gbm, 
                          # the testing data
                          newdata = test,
                          # this is the number we calculated above
                          n.trees = perf_gbm1)

rmse_fit1 <- Metrics::rmse(actual = test$rented_bike_count, 
                           predicted = bike_prediction_1)


print(rmse_fit1)

min_MSE <- which.min(model_gbm$cv.error)

# get MSE and compute RMSE
sqrt(model_gbm$cv.error[min_MSE])
## [1] 23112.1

# plot loss function as a result of n trees added to the ensemble
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



