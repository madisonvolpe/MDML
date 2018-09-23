
```{r}
knitr::opts_chunk$set(echo = TRUE)
```

```{r}
library(tidyverse)
library(lubridate)
```

# 2

## A) Download 2013 NYC Taxi Data and Read Csvs

```{r}
trips <- read_csv("trip_data_8.csv")
fares <- read_csv("trip_fare_8.csv")
```

## B) Create new datasets based on 08/15/2018

```{r}
#filtering on 08/15/2018
trips <- filter(trips,as.Date(trips$pickup_datetime) == "2013-08-15")
fares <- filter(fares,as.Date(fares$pickup_datetime) == "2013-08-15")

#writing csvs
write_csv(trips, "trips_hw_1.csv")
write_csv(fares, "fares_hw_1.csv")
``` 

```{r}
#read in csvs 
trips <- read_csv("trips_hw_1.csv")
fares <- read_csv("fares_hw_1.csv") 
```

## C) Clean both the ‘trips’ and ‘fares’ tibbles using your best judgment. Document your cleaning steps and assumptions.

## Trips 

### Examining trip_distance in trips tibble

```{r}
summary(trips$trip_distance)

trips[trips$trip_distance > 50, ]

trips <- trips %>%
  filter(trip_distance >0 & trip_distance < 50) %>%
  arrange(trip_distance)
```

```{r}
summary(trips$trip_distance)
```

### Examining passenger_count

```{r}
summary(trips$passenger_count)

trips <- trips %>%
  filter(passenger_count > 0) 

summary(trips$passenger_count)
```

### Examining trip_time_in_secs in trips tibble

```{r}
summary(trips$trip_time_in_secs)

Differences <- trips %>%
  transmute(medallion, hack_license, trip_time_in_secs, trip_time_in_secs_2 = as.duration(dropoff_datetime - pickup_datetime))

all(Differences$trip_time_in_secs == as.integer(Differences$trip_time_in_secs_2))
```

```{r}
trips <- mutate(trips, trip_time_in_secs_2 = as.duration(dropoff_datetime - pickup_datetime), diff = abs(dseconds(trip_time_in_secs) - trip_time_in_secs_2))

trips <- arrange(trips, desc(diff)) 
head(trips$diff,20)

#we filter to only have trips that differ in these two variables by less than 1 min
trips <- filter(trips, diff < dminutes(1))
head(trips$diff,20) 
range(trips$diff) #difference ranging from 0 to 57 seconds! 

# drop irrelevant columns
trips <- select(trips, -trip_time_in_secs_2, -diff) 

summary(trips$trip_time_in_secs)
```

```{r}
#filter to remove negative trip times that are less than 60 seconds (1 min)
trips <- trips %>%
  filter(trip_time_in_secs >= 60 )

trips%>%
 arrange(desc(trip_time_in_secs))

summary(trips$trip_time_in_secs)
```

## Fares

### Examining payment_type 

```{r}
#examining the different values for payment type 
unique(fares$payment_type)

#filter only on cash (CSH) and and credit (CRD) payment types 
fares <- fares %>%
  filter(payment_type %in% c("CSH", "CRD"))

unique(fares$payment_type)
```


### Examining fare_amount 

```{r}
#summary on fare_amount 
summary(fares$fare_amount)

#examining outliers negative fare_amount values and fare_amount values greater than 500
fares[fares$fare_amount < 0,]
fares[fares$fare_amount > 500,]

#filter for fare amounts greater than 0 
fares <- fares %>%
  filter(fare_amount >= 2.50)

#reobserving fare_amount
summary(fares$fare_amount)
```

### Examining other charges and amounts

```{r}
summary(fares$surcharge)
summary(fares$mta_tax)
summary(fares$tip_amount)
summary(fares$tolls_amount)
summary(fares$total_amount)
```

```{r}
fares[fares$total_amount>500,]
```

## D) [10 pts] Join the ‘trips’ and ‘fares’ tibbles together into a tibble called ‘taxi_data.’ This joined tibble should have one row for each trip, and should have complete fare information (you may have to deal with duplicates and missing values that arise from the join).

### Initial Cleaning 

```{r}
trips$UID <- paste(trips$medallion, trips$hack_license, trips$pickup_datetime, sep = "_")
fares$UID <- paste(fares$medallion, fares$hack_license, fares$pickup_datetime, sep = "_")

trips %>% count(UID)%>%filter(n>1) #works for trips 
length(unique(trips$UID)) == nrow(trips) #also see if true 

fares %>% count(UID)%>%filter(n>1) #does not work for trips 
length(unique(fares$UID)) == nrow(fares) #we can see that there are duplicates! 

trips <- distinct(trips)
fares <- distinct(fares)
```

```{r}
fares$pickup_datetime <- as.POSIXct(fares$pickup_datetime)
taxi_data <- trips %>% left_join(fares)
taxi_data %>% count(UID)%>%filter(n>1) #works for trips 
``` 

```{r}
# let's use anti join to see if there are rows in trips that don't have corresponding fares associated
trips %>% anti_join(fares)
```

```{r}
# now let's get rid of those "duplicate" rows in the fares tibble by keeping the fare with highest total amount
fares <- fares %>% arrange(desc(total_amount)) %>% distinct(medallion, hack_license, pickup_datetime, .keep_all = TRUE)
```

```{r}
taxi_data <- inner_join(trips,fares)

nrow(taxi_data) + nrow(trips %>% anti_join(fares)) == nrow(trips) # we now see that the number of rows resulting from the inner joing plus the nrow  from the anti join does equal the nrow of trips 
```

```{r}
length(unique(taxi_data$UID)) == nrow(taxi_data) #one row for each trip test 
sum(is.na(taxi_data[,16:22])) #complete fare information for each trip test. 
```

## E) For each taxicab, identified by the unique ‘medallion’ field, compute: i) total_trips: the total number of trips begun on 8/15/2018, ii) total_passengers: the total number of passengers carried on 8/15/2018, iii) total_time_with_passengers: the total amount of time spent carrying passengers on 8/15/2018, iv) total_distance: the total distance traveled on 8/15/2018, and v) total_earnings: the total amount of money earned on 8/15/2018. The columns of your final output tibble should be: [medallion, total_trips, total_passengers, total_time_with_passengers, total_distance, total_earnings].

```{r}
taxi_data %>%
  group_by(medallion)%>%
  transmute(total_trips = n(), total_passengers = sum(passenger_count), total_time_with_passengers = sum(trip_time_in_secs), total_distance = sum(trip_distance), total_earnings = sum(total_amount))
```
## F)

```{r}
#get hours as its own column 
taxi_data$hour <- hour(taxi_data$pickup_datetime)
length(unique(taxi_data$hour)) #check to see if there are 24 unique hours! 
```

```{r}
taxi_data %>%
  group_by(hack_license,hour)%>%
  summarise(total_passengers_picked_up = sum(passenger_count), trips_started = n())

ATEST <- taxi_data[taxi_data$hack_license == 2013000001,]#check
ATEST2<- ATEST[ATEST$hour==22,] #this code was a check we can delete it after - but it shows that our output is right, there were 3 trips started for this ID at hr 22, likewise the total passenger count is really 24! 
```

##G) 

```{r}
taxi_data %>%
  group_by(hack_license, hour)%>%
  summarise(total_time_with_passengers = sum(trip_time_in_secs), miles_with_passengers = sum(trip_distance), earnings = sum(fare_amount, tip_amount))

# a start might have to switch 
```



