---
title: "Reproducible Research - Course Project 1"
output: html_document
---



This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

## Loading and preprocessing the data

Loading the data and removing missing observations

```r
data <- read.csv(file="activity.csv",sep=",", header=TRUE )
head(data)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

Transforming the date variable to a Date format

```r
library(lubridate)
data$date <- as.Date(parse_date_time(data$date, "ymd"))
```

## What is the mean total number of steps taken per day?

Removing missing values from the dataset


```r
data_omit <- na.omit(data)
```

Calculating the total number of steps taken each day


```r
total_steps <- aggregate(list(steps=data_omit$steps), by=list(day=data_omit$date), sum)
```

Making a histogram to show the distribution of the total number of steps taken each day.


```r
hist(total_steps$steps, xlab="", main="Number of steps taken each day")
```

![plot of chunk histogram](figure/histogram-1.png)

Calculating the mean and median of the total number of steps taken per day


```r
summary(total_steps$steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    8841   10765   10766   13294   21194
```


## What is the average daily activity pattern?

Calculating the average number of steps per 5-minute interval.


```r
data_intervals <- aggregate(list(steps=data_omit$steps), by=list(interval=data_omit$interval), mean)
```

Making a time series plot of the 5-minute interval and the average number of steps taken.


```r
with(data_intervals, plot(interval, steps, type="l", main="Average number of steps per 5-minute interval", xlab="Interval", 
                          ylab="Number of steps"))
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png)

Calculating which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps. 


```r
data_intervals[which.max(data_intervals$steps),]
```

```
##     interval    steps
## 104      835 206.1698
```

## Imputing missing values

There are a number of days/intervals where there are missing values. These values have te be dealed with because they introduce bias into some calculations or summaries of the data.

Calculating the total number of rows with NAs


```r
sum(is.na(data))
```

```
## [1] 2304
```

Imputing the missing data with mean values


```r
library(mice)
impute <- mice(data, m=5, maxit=10, method = "mean", seed=500)
data_imputed <- complete(impute,1)
```

Calculating the total number of steps taken each day, and making a histogram of it.


```r
total_steps <- aggregate(list(steps=data_imputed$steps), by=list(day=data_imputed$date), sum)
hist(total_steps$steps, main="Total number of steps taken each day", xlab="")
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-1.png)

Calculating the mean and median number of steps taken per day.


```r
summary(total_steps$steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    9819   10766   10766   12811   21194
```

Note that these values are different than the values in the first part of the assignment, when the missing values of the data were not imputed. 

## Are there differences in activity patterns between weekdays and weekends?

Calculating which day of the week the date is and then creating a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whethere a given data is a weekday or a weekend day


```r
library(dplyr)
data <- mutate(data_imputed, day=weekdays(date))
data$day[data$day=="maandag" | data$day=="dinsdag" | data$day=="woensdag" | data$day=="donderdag" |data$day=="vrijdag"] <- "weekday"
data$day[data$day=="zaterdag" | data$day=="zondag"] <- "weekend"
```

Calculating the average number of steps taken,averaged across all weekdays or weekend days, for all 5-minute intervals.


```r
interval_mean <- aggregate(list(steps=data$steps), by=list(interval=data$interval, day=data$day), mean)
```

Plotting the time series of the 5-minute interval and the average number of steps taken, averaged across all weekdays or weekenddays.


```r
library(ggplot2)
with(interval_mean, qplot(interval, steps, facets = .~day, geom="line", ylab="Steps", xlab="Interval" ,
                               main="Average number of steps per 5-min interval"))
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10-1.png)
