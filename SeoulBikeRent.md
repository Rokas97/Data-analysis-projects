Seoul bike rent analysis (still in progress)
================
Rokas

Last compiled on 14 June, 2022

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
date format with `dmy` function.(day/month/year). Also all categorical
character variables are changed to numerical values for calculations.
New variable `day` added to `clean_bike` data frame to name weekdays for
analysis. Weekdays are converted to numbers as well.

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
```

By using `table` function it is possible now to count all the values. It
is clea now that ‘functioning day’ has only 1 value. It means that bikes
are rented in Seoul any time of the year. So we can remove this variable
from the data frame ‘clean_bike’.

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

# Temperature

``` r
hist(clean_bike$temperature  ,xlab= "Temperature", main= "Temperature distribution" )
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

# Hour

``` r
knitr::knit_exit()
ggplot(clean_bike, aes( x=date, y =rented_bike_count, color= temperature)) + geom_point() +
  facet_wrap(~seasons, scales = "free_x") +
ylab('Count of Rented Bikes')  +
xlab('Date') +
ggtitle('Seasonal Bike Rent Dependency in Seoul')  + 
 scale_fill_viridis(direction = -1)+
  scale_colour_viridis_c(option = "plasma")
```

<img src="SeoulBikeRent_files/figure-gfm/unnamed-chunk-2-1.png" title="Figure 1.1 This is Caption" alt="Figure 1.1 This is Caption" width="100%" style="display: block; margin: auto auto auto 0;" />
