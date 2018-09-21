```{r}
library(tidyverse)
library(lubridate)
```
# 2

## A: Download 2013 NYC Taxi Data and Read Csvs

```{r}
trips <- read_csv("trip_data_8.csv")
fares <- read_csv("trip_fare_8.csv")
```

## B: Create new datasets based on 08/15/2018

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

## C: Clean both the ‘trips’ and ‘fares’ tibbles using your best judgment. Document your cleaning steps and assumptions.

## Trips 

### Examining trip_distance in trips tibble

```{r}
summary(trips$trip_distance)

trips[trips$trip_distance > 50, ]

trips <- trips %>%
  filter(trip_distance >0 & trip_distance < 50) %>%
  arrange(trip_distance)
```

The documentation for the dataset describes trip distance as trip distance measured by the taximeter in miles. The summary command reveals that the range of trip_distance in the dataset is from 0 miles to 5043318 miles. When we subset the trips tibble on trips that have a trip_distance greater than 50, we see that there are only 49 observations out of 473,544 that have trip_distance greater than 50 miles. Therefore, we made the decision to restrict observations in trips to trip_distances greater than 0, but less than 50 miles. 

```{r}
summary(trips$trip_distance)
```

We now see that the range of our trip_distance variable in the trips tibble is from .01 miles to 48 miles, which seems more reasonable. 

### Examining passenger_count

```{r}
summary(trips$passenger_count)

trips <- trips %>%
  filter(passenger_count > 0) 

summary(trips$passenger_count)
```

According to NYC.gov, the legal amount of passengers for taxis in NYC is between four or five depending on the size of the vehicle. Also small children are allowed to sit on laps in the back seat, therefore a max of six passengers is justifiable. 

### Examining trip_time_in_secs in trips tibble

```{r}
summary(trips$trip_time_in_secs)

Differences <- trips %>%
  transmute(medallion, hack_license, trip_time_in_secs, trip_time_in_secs_2 = as.duration(dropoff_datetime - pickup_datetime))

all(Differences$trip_time_in_secs == as.integer(Differences$trip_time_in_secs_2))
```

The documentation for the dataset describes the trip_time_in_secs field as the trip time measured in seconds by the taximetere. However, it also states that sometimes this variable is recorded in minutes.Therefore, they note that this variable is unreliable. They state that a more reliable measure of trip_time is obtained when y subtracting the pickup_datetime from the dropoff_datetime.

The initial summary of trip_time_in_secs shows that the range for this value is from -10 to 22080. We then compute the more "accurate" measure of trip_time by subtracting pickup_datetime from dropoff_datetime, we name this variable trip_time_in_secs_2. We used transmute to create a new tibble "Differences" that shows the original trip_time_in_secs, native to the dataset, and trip_time_in_secs_2, the value we calculated in order to compare how these two measures differ. We used the all command to check if the two columns trip_time_in_secs and trip_time_in_secs2 were the same for all values. The FALSE informs us that they are not. 

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

As a result, in the trips dataframe we add the trip_time_in_secs2 variable, which is computed by subtracting pickup_datetime from dropoff_datetime. We then create another new variable diff, which takes the difference from the trip_time_in_secs variable, native to the dataset and sometimes wrong, and trip_time_in_secs2 variable, constructed as a more valid measure of trip time. We then filter to include only the trips that differ between these two measures by less than one min. 

After doing this, the summary of trip_time_in_secs still reveals unlikely numbers, such as -10 second trip time.The max value of 22080 seconds or 6.1 hours seems likely. Finally, the mean of 13.8 mins does seem likely. 

```{r}
#filter to remove negative trip times that are less than 60 seconds (1 min)
trips <- trips %>%
  filter(trip_time_in_secs >= 60 )

trips%>%
 arrange(desc(trip_time_in_secs))

summary(trips$trip_time_in_secs)
```

Ultimately, we decided to keep trips that only have trip_time_in_secs greater than or equal to 60 seconds (1 minute). This was checked by running summary on trip_time_in_secs to reveal the min value of 60. 

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

We decided to examine payment_type and we limited payment_type to only cash and credit payments. 

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

We see that fare_amount, which represents the meter fare in USD, has a range of -460.00 dollars to 955.00 dollars.  We further examine that there are 126 observations with negative fare_amount by observation we can see that there total_amount is also negative. Furthermore, we observed that there are 4 observations with fare_amount greater than 500 dollars. We decided to drop any fare_amount less than 2.50 because 2.50 is the minimum initial charge listed by NYC Taxi & Limousine Commission, but we keep the fare_amount values over 500 dollars.

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
Test <- fares %>% count(UID)%>%filter(n>1) #does not work for trips 

trips <- distinct(trips)
fares <- distinct(fares)
```

```{r}

Test <- fares %>%
  count(UID)%>%
  filter(n>1)

#does not work for trips
ATEST <- right_join(trips, fares)
```
