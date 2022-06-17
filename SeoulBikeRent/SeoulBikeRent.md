Seoul bike rent analysis (still in progress)
================
Rokas

Last compiled on 17 June, 2022

<style>
body {
  color: #blue;
  font-family: Calibri;
  background-color: #lightblue;
}
pre {
  color: #708090;
  background-color: #F8F8FF;
}
</style>

# Dataset Attributes

Identifier

-   Id - The Unique Integer Id for every entry in data set

Other variables (columns)

-   Date - dd/mm/yyyy (String)
-   Hour - Hour of the day (int)
-   Temperature - Temperature(°C)
-   Humidity - Humidity (%)
-   Wind_Speed - Wind Speed (m/s)
-   Visibility - Visibility ( 10m range)
-   Dew_Point - Dew Point Temperature(°C)
-   Solar_Radiation - Solar Radiation (MJ/m^2)
-   Rainfall - Rainfall (mm)
-   Snowfall - Snowfall (mm)

Categorical Variables (columns)

-   Season - Winter, Spring, Summer and Autumn
-   IsHoliday - Yes/No stands for Holiday/No Holiday for Public Holidays
    from the South Korean Public Holidays List
-   IsFunctioningDay - Yes/No stands for Yes/No to Whether the Hour in
    the entry is a functioning hour (work hour) of the day or not.

Target variable (column)

-   Bikes_Rented - Number of Bikes Rented in the Hour of the Day with
    Specified parameters. Dataset Attributes

Identifiers

    Id - The Unique Integer Id for every entry in both sets

Other columns

    Date - dd/mm/yyyy String for Date
    Hour - int, Hour of the day
    Temperature - Temperature in Degree Celsius
    Humidity - % of Humidity
    Wind_Speed - Wind Speed in m/s
    Visibility - Visibility in 10m range
    Dew_Point - Dew Point Temperature in Degree Celsius
    Solar_Radiation - Solar Radiation in MJ/m^2
    Rainfall - Rainfall in mm
    Snowfall - Snowfall in mm

Categorical Columns

    Season - Winter, Spring, Summer & Autumn
    IsHoliday - 1/0 for Holiday/No Holiday for Public Holidays from the South Korean Public Holidays List
    IsFunctioningDay - 1/0 for Yes/No to Whether the Hour in the entry is a functioning hour (work hour) of the day or not.

Target Column

    Bikes_Rented - Number of Bikes Rented in the Hour of the Day with Specified parameters.

## Libraries

``` r
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
```

## Exploratory analysis

Reading a file as a data.table and checking its structure and summary

``` r
seoul_bike <- read.csv("raw_seoul_bike_sharing.csv")
str(seoul_bike)
```

    ## 'data.frame':    8760 obs. of  14 variables:
    ##  $ Date                 : chr  "01/12/2017" "01/12/2017" "01/12/2017" "01/12/2017" ...
    ##  $ RENTED_BIKE_COUNT    : int  254 204 173 107 78 100 181 460 930 490 ...
    ##  $ Hour                 : int  0 1 2 3 4 5 6 7 8 9 ...
    ##  $ TEMPERATURE          : num  -5.2 -5.5 -6 -6.2 -6 -6.4 -6.6 -7.4 -7.6 -6.5 ...
    ##  $ HUMIDITY             : int  37 38 39 40 36 37 35 38 37 27 ...
    ##  $ WIND_SPEED           : num  2.2 0.8 1 0.9 2.3 1.5 1.3 0.9 1.1 0.5 ...
    ##  $ Visibility           : int  2000 2000 2000 2000 2000 2000 2000 2000 2000 1928 ...
    ##  $ DEW_POINT_TEMPERATURE: num  -17.6 -17.6 -17.7 -17.6 -18.6 -18.7 -19.5 -19.3 -19.8 -22.4 ...
    ##  $ SOLAR_RADIATION      : num  0 0 0 0 0 0 0 0 0.01 0.23 ...
    ##  $ RAINFALL             : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ Snowfall             : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ SEASONS              : chr  "Winter" "Winter" "Winter" "Winter" ...
    ##  $ HOLIDAY              : chr  "No Holiday" "No Holiday" "No Holiday" "No Holiday" ...
    ##  $ FUNCTIONING_DAY      : chr  "Yes" "Yes" "Yes" "Yes" ...

``` r
summary(seoul_bike)
```

    ##      Date           RENTED_BIKE_COUNT      Hour        TEMPERATURE    
    ##  Length:8760        Min.   :   2.0    Min.   : 0.00   Min.   :-17.80  
    ##  Class :character   1st Qu.: 214.0    1st Qu.: 5.75   1st Qu.:  3.40  
    ##  Mode  :character   Median : 542.0    Median :11.50   Median : 13.70  
    ##                     Mean   : 729.2    Mean   :11.50   Mean   : 12.87  
    ##                     3rd Qu.:1084.0    3rd Qu.:17.25   3rd Qu.: 22.50  
    ##                     Max.   :3556.0    Max.   :23.00   Max.   : 39.40  
    ##                     NA's   :295                       NA's   :11      
    ##     HUMIDITY       WIND_SPEED      Visibility   DEW_POINT_TEMPERATURE
    ##  Min.   : 0.00   Min.   :0.000   Min.   :  27   Min.   :-30.600      
    ##  1st Qu.:42.00   1st Qu.:0.900   1st Qu.: 940   1st Qu.: -4.700      
    ##  Median :57.00   Median :1.500   Median :1698   Median :  5.100      
    ##  Mean   :58.23   Mean   :1.725   Mean   :1437   Mean   :  4.074      
    ##  3rd Qu.:74.00   3rd Qu.:2.300   3rd Qu.:2000   3rd Qu.: 14.800      
    ##  Max.   :98.00   Max.   :7.400   Max.   :2000   Max.   : 27.200      
    ##                                                                      
    ##  SOLAR_RADIATION     RAINFALL          Snowfall         SEASONS         
    ##  Min.   :0.0000   Min.   : 0.0000   Min.   :0.00000   Length:8760       
    ##  1st Qu.:0.0000   1st Qu.: 0.0000   1st Qu.:0.00000   Class :character  
    ##  Median :0.0100   Median : 0.0000   Median :0.00000   Mode  :character  
    ##  Mean   :0.5691   Mean   : 0.1487   Mean   :0.07507                     
    ##  3rd Qu.:0.9300   3rd Qu.: 0.0000   3rd Qu.:0.00000                     
    ##  Max.   :3.5200   Max.   :35.0000   Max.   :8.80000                     
    ##                                                                         
    ##    HOLIDAY          FUNCTIONING_DAY   
    ##  Length:8760        Length:8760       
    ##  Class :character   Class :character  
    ##  Mode  :character   Mode  :character  
    ##                                       
    ##                                       
    ##                                       
    ## 

Data frame consists of `14` variables or columns and `8760` observations
or rows. Let’s look for `NA` values first and remove them by saving it
in the new data frame. There are `295` NA values of
`RENTED_BIKE_COlUMN`, and `11` `NA` values of `TEMPERATURE`. We can
remove them

``` r
sapply(seoul_bike, function(x) sum(is.na(x)))
```

    ##                  Date     RENTED_BIKE_COUNT                  Hour 
    ##                     0                   295                     0 
    ##           TEMPERATURE              HUMIDITY            WIND_SPEED 
    ##                    11                     0                     0 
    ##            Visibility DEW_POINT_TEMPERATURE       SOLAR_RADIATION 
    ##                     0                     0                     0 
    ##              RAINFALL              Snowfall               SEASONS 
    ##                     0                     0                     0 
    ##               HOLIDAY       FUNCTIONING_DAY 
    ##                     0                     0

``` r
clean_bike <- na.omit(seoul_bike)
str(clean_bike)
```

    ## 'data.frame':    8454 obs. of  14 variables:
    ##  $ Date                 : chr  "01/12/2017" "01/12/2017" "01/12/2017" "01/12/2017" ...
    ##  $ RENTED_BIKE_COUNT    : int  254 204 173 107 78 100 181 460 930 490 ...
    ##  $ Hour                 : int  0 1 2 3 4 5 6 7 8 9 ...
    ##  $ TEMPERATURE          : num  -5.2 -5.5 -6 -6.2 -6 -6.4 -6.6 -7.4 -7.6 -6.5 ...
    ##  $ HUMIDITY             : int  37 38 39 40 36 37 35 38 37 27 ...
    ##  $ WIND_SPEED           : num  2.2 0.8 1 0.9 2.3 1.5 1.3 0.9 1.1 0.5 ...
    ##  $ Visibility           : int  2000 2000 2000 2000 2000 2000 2000 2000 2000 1928 ...
    ##  $ DEW_POINT_TEMPERATURE: num  -17.6 -17.6 -17.7 -17.6 -18.6 -18.7 -19.5 -19.3 -19.8 -22.4 ...
    ##  $ SOLAR_RADIATION      : num  0 0 0 0 0 0 0 0 0.01 0.23 ...
    ##  $ RAINFALL             : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ Snowfall             : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ SEASONS              : chr  "Winter" "Winter" "Winter" "Winter" ...
    ##  $ HOLIDAY              : chr  "No Holiday" "No Holiday" "No Holiday" "No Holiday" ...
    ##  $ FUNCTIONING_DAY      : chr  "Yes" "Yes" "Yes" "Yes" ...
    ##  - attr(*, "na.action")= 'omit' Named int [1:306] 3145 3146 3147 3148 3149 3150 3151 3152 3153 3154 ...
    ##   ..- attr(*, "names")= chr [1:306] "3145" "3146" "3147" "3148" ...

All the names of `14` variables are converted to lower cases for
convenience.

``` r
names(clean_bike) <- tolower(names(clean_bike))
names(clean_bike)
```

    ##  [1] "date"                  "rented_bike_count"     "hour"                 
    ##  [4] "temperature"           "humidity"              "wind_speed"           
    ##  [7] "visibility"            "dew_point_temperature" "solar_radiation"      
    ## [10] "rainfall"              "snowfall"              "seasons"              
    ## [13] "holiday"               "functioning_day"

By using a `lubridate` package date is converted from char format to
date format with `dmy` function.(day/month/year). Also categorical
variable
`holiday' is changed to numerical values for calculations and`seasons’
names are changed to factors. New variable `day` added to `clean_bike`
data frame to name weekdays for analysis. Weekdays are converted to
factors as well.

``` r
clean_bike$date <- dmy(clean_bike$date)
clean_bike$holiday <- ifelse(clean_bike$holiday == 'No Holiday' ,0, 1)
#clean_bike$seasons <- recode(clean_bike$seasons, 'Winter'= 1, 'Spring'= 2, 'Summer'= 3, 'Autumn'= 4)
clean_bike$seasons <- as.factor(clean_bike$seasons)
class(clean_bike$seasons)
```

    ## [1] "factor"

``` r
clean_bike$day <- weekdays(as.Date(clean_bike$date))
clean_bike$day <- as.factor(clean_bike$day)
```

By using `table` function it is possible now to count all the values. It
is clear now that ‘functioning day’ has only 1 value. It means that
bikes are rented in Seoul any time of the year and any time of the day.
So we can remove this variable from the data frame ‘clean_bike’.

``` r
table(clean_bike$functioning_day)
```

    ## 
    ##  Yes 
    ## 8454

``` r
pie(table(clean_bike$holiday), labels=c("No","Yes"))
```

![](SeoulBikeRent_files/figure-gfm/tables%20for%20analysis-1.png)<!-- -->

``` r
table(clean_bike$holiday)
```

    ## 
    ##    0    1 
    ## 8046  408

``` r
pie(table(clean_bike$seasons), labels=c("Winter","Spring","Summer","Autumn"))
```

![](SeoulBikeRent_files/figure-gfm/tables%20for%20analysis-2.png)<!-- -->

``` r
table(clean_bike$seasons)
```

    ## 
    ## Autumn Spring Summer Winter 
    ##   1937   2160   2197   2160

``` r
pie(table(clean_bike$day),labels=c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))
```

![](SeoulBikeRent_files/figure-gfm/tables%20for%20analysis-3.png)<!-- -->

``` r
table(clean_bike$day)
```

    ## 
    ##    Friday    Monday  Saturday    Sunday  Thursday   Tuesday Wednesday 
    ##      1224      1248      1215      1223      1197      1150      1197

``` r
clean_bike <- clean_bike %>% select(-functioning_day)
```

Now I can calculate correlation and look for variables that correlate
the most with `rented_bike_count`. In other words, we are looking for
the most paramount factors for bike rent. Also ‘dew_point_temperature’
is removed because it highly correlates with temperature and for
predictive analysis it won’t be useful. One ‘temperature’ variable is
enough for analysis. It can be seen that

``` r
corrplot(cor(clean_bike[c(2:11)]),type = "upper",col=COL2("RdBu",100))
```

<img src="SeoulBikeRent_files/figure-gfm/correlation -1.png" title="Figure 1.1 This is Caption" alt="Figure 1.1 This is Caption" width="100%" style="display: block; margin: auto auto auto 0;" />

``` r
clean_bike <- clean_bike %>% select(-dew_point_temperature)
```

With the help of `grouping`, `average_bike_rent_coount` and
’average_temperature`is calculated per month and year since there is one month of 2017. The most bikes were rented during June`n=1237`and September`n=1079`and temperatures 'T1=23.1' and`T2=21.7\`
accordingly.

``` r
rent_by_year <- clean_bike %>%
  mutate(year= year(clean_bike$date), month=month(clean_bike$date)) %>%
  group_by(year, month) %>%
  summarise(total = sum(rented_bike_count), average_bike_rent_count = mean(rented_bike_count), average_temperature = mean(temperature))
kable(rent_by_year)
```

| year | month |  total | average_bike_rent_count | average_temperature |
|-----:|------:|-------:|------------------------:|--------------------:|
| 2017 |    12 | 185330 |                249.0995 |           -1.928763 |
| 2018 |     1 | 150006 |                201.6210 |           -3.943145 |
| 2018 |     2 | 151833 |                225.9420 |           -1.664732 |
| 2018 |     3 | 380594 |                511.5511 |            8.044409 |
| 2018 |     4 | 524227 |                753.1997 |           12.950776 |
| 2018 |     5 | 707088 |                982.0667 |           18.233417 |
| 2018 |     6 | 883541 |               1237.4524 |           23.116106 |
| 2018 |     7 | 732059 |                989.2689 |           27.775676 |
| 2018 |     8 | 650610 |                875.6528 |           28.740646 |
| 2018 |     9 | 673612 |               1079.5064 |           21.748718 |
| 2018 |    10 | 650675 |                978.4586 |           12.675188 |
| 2018 |    11 | 465715 |                718.6960 |            7.364506 |

Furthermore, seasonal bike rental analysis is carried out by using
`date` and `seasons` variables in addition. It is evident that bike rent
is highly dependent on the temperature. The lowest rent numbers are
generated during the winter and highest in the summer. Also other was
made with highlighted `holiday` points. There is no visual difference of
bike rent during the holidays, so we can ignore it, during the analysis.

# Seasonal

``` r
ggplot(clean_bike, aes( x=date, y =rented_bike_count, color= temperature)) + geom_point() +
  facet_wrap(~seasons, scales = "free_x") +
ylab('Count of Rented Bikes')  +
xlab('Date') +
ggtitle('Seasonal Bike Rent Dependency in Seoul')  + 
 scale_fill_viridis(direction = -1)+
  scale_colour_viridis_c(option = "plasma")
```

<img src="SeoulBikeRent_files/figure-gfm/unnamed-chunk-2-1.png" title="Figure 1.1 This is Caption" alt="Figure 1.1 This is Caption" width="100%" style="display: block; margin: auto auto auto 0;" />

``` r
highlight <- clean_bike %>% filter(holiday==1)

  ggplot(clean_bike, aes( x=date, y =rented_bike_count, color= temperature)) + geom_point(alpha=0.3) +
 geom_point(data= highlight , aes(x=date, y =rented_bike_count), color ='black', size=4) +
ylab('Count of Rented Bikes')  +
xlab('Date') +
ggtitle('Seasonal Bike Rent Dependency in Seoul with highlighted holidays')  + 
    scale_fill_viridis(direction = -1)+
  scale_colour_viridis_c(option = "plasma")
```

<img src="SeoulBikeRent_files/figure-gfm/unnamed-chunk-2-2.png" title="Figure 1.1 This is Caption" alt="Figure 1.1 This is Caption" width="100%" style="display: block; margin: auto auto auto 0;" />

# Hour

After Using `table` function on `hour` variable it is clear that each
hour has the same amount of records. The bar plots shows hourly rented
bike dependency. The least amount bikes is rented 4 in the morning.
There is a spiking in a graph during 8 in the morning. It is probably
due to people going to work. The highest number of bikes rented is at
the 18th hour. Around that time people finish working in Korea, so they
are going home after work.

``` r
table(clean_bike$hour)
```

    ## 
    ##   0   1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16  17  18  19 
    ## 352 352 352 352 351 352 351 353 353 352 352 353 353 352 352 353 353 351 352 352 
    ##  20  21  22  23 
    ## 353 353 353 352

``` r
ggplot(clean_bike, aes(x= hour, y = rented_bike_count)) + geom_col() +  scale_x_continuous(breaks=seq(0,23,1)) + ggtitle("Bike rent count dependancy on hour")
```

![](SeoulBikeRent_files/figure-gfm/hour-a-1.png)<!-- -->

Let’s look for an outliers for each hour with the help of `boxplot`,
`dplyr` package and `indtify_outliers` function, which looks for
outliers with the help of first and third `quantiles` and `IQR`. 2
outlier points ar visible from the boxplot. After printing all the
records with these outliers, we can see that there is nothing special
about them, so they are removed using `anti-join`

``` r
ggplot(clean_bike, aes(x= hour, y = rented_bike_count)) + geom_boxplot(aes(group = hour)) +  scale_x_continuous(breaks=seq(0,23,1)) +
  coord_flip() + ggtitle("Bike rent count dependancy on hour")
```

![](SeoulBikeRent_files/figure-gfm/hour-b-1.png)<!-- -->

``` r
outliers_hour <- clean_bike %>%
  group_by(hour) %>%
  identify_outliers(rented_bike_count)
outliers_hour
```

    ## # A tibble: 2 x 15
    ##    hour date       rented_bike_count temperature humidity wind_speed visibility
    ##   <int> <date>                 <int>       <dbl>    <int>      <dbl>      <int>
    ## 1     2 2018-06-24              1254        20.9       87        1.8        222
    ## 2     4 2018-06-13               421        18.7       74        0.9       1933
    ## # ... with 8 more variables: solar_radiation <dbl>, rainfall <dbl>,
    ## #   snowfall <dbl>, seasons <fct>, holiday <dbl>, day <fct>, is.outlier <lgl>,
    ## #   is.extreme <lgl>

``` r
clean_bike <- clean_bike %>% anti_join(outliers_hour, by = c( "hour", "rented_bike_count"))
```

Very obvious pattern is observed when analyzing hourly bike rent by the
day. During weekends graphs are much smoother(ne peaks, like on
weekdays) observed due to the fact that majority of people don’t work on
Saturday and Sunday.

``` r
ggplot(clean_bike, aes(x= hour, y = rented_bike_count, fill = day)) + geom_col() +
scale_x_continuous(breaks=seq(0,23,2))+ facet_wrap(~day) + theme(legend.position='none' ) + ggtitle("Daily bike rent count dependancy on hour")
```

![](SeoulBikeRent_files/figure-gfm/hour-c-1.png)<!-- -->

Moving on, there is obviuos correlation between temperature and bike
rent \# Temperature

``` r
hist(clean_bike$temperature  ,xlab= "Temperature", main= "Temperature distribution", xaxp = c(-20,40,24) )
```

![](SeoulBikeRent_files/figure-gfm/temp-1.png)<!-- -->

``` r
summary(clean_bike$temperature)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##  -17.80    3.00   13.40   12.75   22.60   39.40

``` r
ggplot(clean_bike, aes(temperature, rented_bike_count)) + geom_point() + ylim(0,4000)
```

![](SeoulBikeRent_files/figure-gfm/temp-2.png)<!-- -->

``` r
ggplot(clean_bike, aes(hour, temperature)) + geom_boxplot() 
```

    ## Warning: Continuous x aesthetic -- did you forget aes(group=...)?

![](SeoulBikeRent_files/figure-gfm/temp-3.png)<!-- -->

## Modeling analysis

Data is split into training and test data sets with 70/30 % ratio

``` r
split = sort(sample(nrow(clean_bike), nrow(clean_bike)*.7))
train<-clean_bike[split,]
test<-clean_bike[-split,]
dim(train)
```

    ## [1] 5916   13

``` r
dim(test)
```

    ## [1] 2536   13

``` r
glimpse(train)
```

    ## Rows: 5,916
    ## Columns: 13
    ## $ date              <date> 2017-12-01, 2017-12-01, 2017-12-01, 2017-12-01, 201~
    ## $ rented_bike_count <int> 254, 204, 107, 78, 100, 181, 460, 490, 339, 360, 449~
    ## $ hour              <int> 0, 1, 3, 4, 5, 6, 7, 9, 10, 11, 12, 13, 16, 17, 19, ~
    ## $ temperature       <dbl> -5.2, -5.5, -6.2, -6.0, -6.4, -6.6, -7.4, -6.5, -3.5~
    ## $ humidity          <int> 37, 38, 40, 36, 37, 35, 38, 27, 24, 21, 23, 25, 54, ~
    ## $ wind_speed        <dbl> 2.2, 0.8, 0.9, 2.3, 1.5, 1.3, 0.9, 0.5, 1.2, 1.3, 1.~
    ## $ visibility        <int> 2000, 2000, 2000, 2000, 2000, 2000, 2000, 1928, 1996~
    ## $ solar_radiation   <dbl> 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.23, 0.65~
    ## $ rainfall          <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0~
    ## $ snowfall          <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0~
    ## $ seasons           <fct> Winter, Winter, Winter, Winter, Winter, Winter, Wint~
    ## $ holiday           <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0~
    ## $ day               <fct> Friday, Friday, Friday, Friday, Friday, Friday, Frid~

# Mars

MARS

``` r
marsm <- earth(rented_bike_count ~ ., data = train, degree = 1)


marsm
summary(marsm)
plot(marsm, which = 1)
axis(1, at = 1:20)

knitr::knit_exit()
```

``` r
hyper_grid <- expand.grid(
  degree = 1:3, 
  nprune = seq(2, 30, length.out = 10) %>% floor()
  )

tuned_mars <- train(
  x = subset(train, select = -rented_bike_count),
  y = train$rented_bike_count,
  method = "earth",
  metric = "RMSE",
  trControl = trainControl(method = "cv", number = 10),
  tuneGrid = hyper_grid
)
tuned_mars$bestTune

ggplot(tuned_mars)+
  scale_x_continuous(breaks = seq(0, 30, by = 5))


p1 <- vip(tuned_mars, num_features = 20, bar = FALSE, value = "gcv") + ggtitle("GCV")
p2 <- vip(tuned_mars, num_features = 20, bar = FALSE, value = "rss") + ggtitle("RSS")

gridExtra::grid.arrange(p1, p2, ncol = 2)
```

# decisions tree

``` r
treem <- rpart(rented_bike_count ~ ., 
             method = "anova", data = train)
treem
```

    ## n= 5916 
    ## 
    ## node), split, n, deviance, yval
    ##       * denotes terminal node
    ## 
    ##  1) root 5916 2442319000  726.2262  
    ##    2) temperature< 11.95 2765  325314500  360.5631  
    ##      4) date< 17595.5 1588   35963090  225.0926 *
    ##      5) date>=17595.5 1177  220888000  543.3390  
    ##       10) hour< 6.5 485   14034650  250.8165 *
    ##       11) hour>=6.5 692  136265300  748.3584  
    ##         22) humidity>=79.5 137   17846710  350.8467 *
    ##         23) humidity< 79.5 555   91426730  846.4829 *
    ##    3) temperature>=11.95 3151 1422880000 1047.0950  
    ##      6) hour< 14.5 1853  413780400  738.2607  
    ##       12) solar_radiation< 0.205 892  101407700  451.9114 *
    ##       13) solar_radiation>=0.205 961  171343500 1004.0500  
    ##         26) hour>=8.5 817   95087750  944.6340 *
    ##         27) hour< 8.5 144   57007650 1341.1530  
    ##           54) day=Saturday,Sunday 45    1135800  625.0889 *
    ##           55) day=Friday,Monday,Thursday,Tuesday,Wednesday 99   22310200 1666.6360 *
    ##      7) hour>=14.5 1298  580056600 1487.9820  
    ##       14) humidity>=83.5 136   28344820  385.6838 *
    ##       15) humidity< 83.5 1162  367123000 1616.9940  
    ##         30) hour< 16.5 281   50604570 1296.3560 *
    ##         31) hour>=16.5 881  278414800 1719.2630  
    ##           62) solar_radiation< 0.025 475   89115720 1480.4380 *
    ##           63) solar_radiation>=0.025 406  130508900 1998.6770 *

``` r
plot(treem, uniform = TRUE,
          main = "... 
                 Tree using Regression")
text(treem, use.n = TRUE, cex = .7)
```

![](SeoulBikeRent_files/figure-gfm/decision%20tree-1.png)<!-- -->

``` r
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
```

    ##       RMSE   Rsquare
    ## 1 338.4795 0.7224823

``` r
# Step 3 - predicting and evaluating the model on test data


predictions_test_cart = predict(treem, newdata = test)

eval_results(test$rented_bike_count, predictions_test_cart, test)
```

    ##       RMSE   Rsquare
    ## 1 348.2712 0.7019746

# gam model

``` r
GAMsm <- gam(rented_bike_count ~ .,data = train)
summary(GAMsm)
```

    ## 
    ## Call: gam(formula = rented_bike_count ~ ., data = train)
    ## Deviance Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1123.57  -275.51   -55.04   207.51  1991.35 
    ## 
    ## (Dispersion Parameter for gaussian family taken to be 183764.2)
    ## 
    ##     Null Deviance: 2442318597 on 5915 degrees of freedom
    ## Residual Deviance: 1083473838 on 5896 degrees of freedom
    ## AIC: 88521.1 
    ## 
    ## Number of Local Scoring Iterations: 2 
    ## 
    ## Anova for Parametric Effects
    ##                   Df     Sum Sq   Mean Sq   F value    Pr(>F)    
    ## date               1  378079494 378079494 2057.4163 < 2.2e-16 ***
    ## hour               1  439583320 439583320 2392.1051 < 2.2e-16 ***
    ## temperature        1  306712074 306712074 1669.0522 < 2.2e-16 ***
    ## humidity           1  133419960 133419960  726.0388 < 2.2e-16 ***
    ## wind_speed         1     141264    141264    0.7687 0.3806486    
    ## visibility         1    2703399   2703399   14.7112 0.0001266 ***
    ## solar_radiation    1   13100663  13100663   71.2906 < 2.2e-16 ***
    ## rainfall           1   31476907  31476907  171.2896 < 2.2e-16 ***
    ## snowfall           1     201408    201408    1.0960 0.2951858    
    ## seasons            3   40159360  13386453   72.8458 < 2.2e-16 ***
    ## holiday            1    4148358   4148358   22.5744 2.070e-06 ***
    ## day                6    9118553   1519759    8.2702 6.115e-09 ***
    ## Residuals       5896 1083473838    183764                        
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# GBM

``` r
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
```

    ## gbm(formula = rented_bike_count ~ ., distribution = "gaussian", 
    ##     data = train[, c(-1, -13)], n.trees = 5000, interaction.depth = 2, 
    ##     n.minobsinnode = 10, shrinkage = 0.01, cv.folds = 5, verbose = FALSE, 
    ##     n.cores = NULL)
    ## A gradient boosted model with gaussian loss function.
    ## 5000 iterations were performed.
    ## The best cross-validation iteration was 4998.
    ## There were 10 predictors of which 10 had non-zero influence.

``` r
# model performance
perf_gbm1 = gbm.perf(model_gbm, method = "cv")
print(perf_gbm1)
```

    ## [1] 4998

``` r
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
```

    ## [1] 253.8354

``` r
min_MSE <- which.min(model_gbm$cv.error)

# get MSE and compute RMSE
sqrt(model_gbm$cv.error[min_MSE])
```

    ## [1] 250.9272

``` r
## [1] 23112.1

# plot loss function as a result of n trees added to the ensemble
gbm.perf( model_gbm, method = "cv")
```

![](SeoulBikeRent_files/figure-gfm/GBM-1.png)<!-- -->

    ## [1] 4998

# XGboost

``` r
X_train = data.matrix(train[,-2])                  # independent variables for train
y_train = train[,2]                                # dependent variables for train
  
X_test = data.matrix(test[,-2])                    # independent variables for test
y_test = test[,2]                                   # dependent variables for test

# convert the train and test data into xgboost matrix type.
xgboost_train = xgb.DMatrix(data=X_train, label=y_train)
xgboost_test = xgb.DMatrix(data=X_test, label=y_test)


xgm <- xgboost(data = xgboost_train,                    # the data   
                 max.depth=3,                # max depth 
               scale_pos_weight = 1, 
               early_stopping_rounds = 3,
               mode="regression",
                 nrounds=5000)                              # max number of boosting iterations

summary(xgm)
xgm

#pred_test <- predict(xgm, xgboost_test)

#pred_test
```

# Time-series forecast
