library(Hmisc)
library(corrplot)
library(plyr)
library(dplyr)
library(scales)
library(RColorBrewer) #??????
library(viridis)
library(lubridate)
library(caTools)
library(leaps)
library(data.table)
 setwd("D:/Desktop/bigdata/20 paskaita(proj)/proje/seoul") #panaikinti poto

bike<-data.table::fread("raw_seoul_bike_sharing.csv")
str(bike)
seoul_bike <- read.csv("raw_seoul_bike_sharing.csv")
str(seoul_bike)

#Counting NA values for each column and removing rows with na values
sapply(seoul_bike, function(x) sum(is.na(x)))
clean_bike <- na.omit(seoul_bike)

#renaming all column names to lower cases for comfortability
names(clean_bike)<- tolower(names(clean_bike))
names(clean_bike)

#converting dates from char to Date format and the to numeric for correlation calculations
clean_bike$date<- dmy(clean_bike$date)
#clean_bike$date <- as.numeric(clean_bike$date)


ggplot(clean_bike, aes(temperature,rented_bike_count)) + geom_boxplot(aes(group=hour))



#changing all categorical variables to numerical values
clean_bike$holiday<- ifelse(clean_bike$holiday=='No Holiday',0, 1)
table(clean_bike$holiday)
clean_bike$functioning_day<- ifelse(clean_bike$functioning_day=='Yes',1, 0)
table(clean_bike$functioning_day)
 clean_bike$seasons<-recode(clean_bike$seasons, 'Winter'=1, 'Spring'=2, 'Summer'=3, 'Autumn'=4)
table(clean_bike$seasons)
clean_bike$day <- weekdays(as.Date(clean_bike$date))
Sys.setlocale("LC_TIME", "C")
clean_bike$day <- recode(clean_bike$day, 'Monday'=1, 'Tuesday'=2, 'Wednesday'=3, 'Thursday'=4, 'Friday'=5, 'Saturday'=6, 'Sunday' = 7)
table(clean_bike$day)
#Calculating correlation for all variables and plotting correlation plot
cor(clean_bike[c(2:15)])
corrplot(cor(clean_bike[2:15]),type = 'upper',col=COL2('RdBu',100))
setDT(clean_bike)[, (date_col) := lapply(.(date), anytime::anydate), .SDcols = date_cols]



corrplot(cor(clean_bike[2:13]),type = "upper",col=COL2("RdBu",100))


ggplot(clean_bike, aes(y=rented_bike_count, x=temperature, color = seasons)) + geom_point()
ggplot(clean_bike, aes(y=rented_bike_count, x=humidity, color = seasons)) + geom_point()
ggplot(clean_bike, aes(y=rented_bike_count, x=functioning_day )) + geom_bar(stat='identity')

rent_by_year <- clean_bike %>%
  mutate(year= year(clean_bike$date), month=month(clean_bike$date)) %>%
  group_by(year, month) %>%
  summarise(total = sum(rented_bike_count), average_bike_rent_count = mean(rented_bike_count), average_temperature = mean(temperature))

rent_by_hour <- clean_bike %>%
  select( hour, rented_bike_count) %>%
  group_by( hour) %>%
  summarise(total = sum(rented_bike_count))

rent_by_hour_and_month <- clean_bike %>%
  mutate(month=  month(clean_bike$date))%>%
  
  select( hour, rented_bike_count, month) %>%
  group_by(month,  hour ) %>%
  summarise(total = sum(rented_bike_count))
  
  
bike_rent_on_holidays <- clean_bike %>%
  mutate(day=  day(clean_bike$date))%>%
  group_by(day) %>%
  filter(holiday == 1 ) %>%
 
  summarise(avg=mean(rented_bike_count))

bike_rent_on_non_holidays <- clean_bike %>%
  filter(holiday == 0 ) %>%
  summarise(avg=mean(rented_bike_count))


palette <- brewer.pal(5, "RdYlRd")[(2:4)]

ggplot(clean_bike, aes( x=seasons,y=rented_bike_count, color = seasons)) + geom_col() +
         
scale_x_discrete(limits = 1:4,labels=c("Winter","Spring","Summer","Autumn")) +
  theme(axis.text.x = element_text(angle = 45)) +  
scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6)) + 
ylab('Count of Rented Bikes')  +
xlab('Seasons') +
ggtitle('Bike Rent Dependency on Seasons in Seoul') + 
  scale_fill_viridis(direction = -1)+
  scale_colour_viridis_c(option = "plasma")+
  theme(legend.position = "none")


rent_by_year_and_month <- clean_bike %>%
  mutate(year= year(clean_bike$date), month=month(clean_bike$date)) %>%
  group_by(year, month) %>%
  summarise(total = sum(rented_bike_count), average_bike_rent_count = mean(rented_bike_count), average_temperature = mean(temperature))

ggplot(rent_by_year, aes(x=average_temperature, y= total, color = month)) + geom_col() + coord_flip()
#scale_color_gradientn(  colors = palette) 
 
#figure it out how to count 1 in holiday, dependance on temperature,season,  hour, HUMIDITY,
#average amount of bikes per day during holidays and non-holidays
#functioning days?
#hour analysis, on hour rented the most bikes?
#

split = sort(sample(nrow(clean_bike), nrow(clean_bike)*.7))
train<-clean_bike[split,]
test<-clean_bike[-split,]
train[,3:4]


model_lm1 <- lm(formula = rented_bike_count ~ . , data = train)
Best_Subset <-
  regsubsets(rented_bike_count~.,
             data =train,
             nbest = 1,      # 1 best model for each number of predictors
             nvmax = NULL,    # NULL for no limit on number of variables
             force.in = NULL, force.out = NULL,
             method = "exhaustive")
which.max(summary_best_subset$adjr2)
summary_best_subset$which[13,]




url<- "https://cran.r-project.org/src/contrib/Archive/randomForest/randomForest_4.6-14.tar.gz"

install.packages(url, repos=NULL, type="source") 

