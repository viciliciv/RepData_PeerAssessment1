---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data
library(dplyr)
library(ggplot2)
library(knitr)
activity<-read.csv('activity.csv')


## What is mean total number of steps taken per day?
1.Total steps per day:
```{r steps per day, echo=TRUE}
one<-activity%>%
    select(steps,date)%>%
    group_by(date)%>%
    summarise(total_steps = sum(steps,na.rm = TRUE))
head(one)
```
2. Total steps per day histogram.
```{r pressure, echo=TRUE}
hist(one$total_steps, main = 'Total Steps Per Day',xlab = 'Total Steps', ylab = 'Frequency', breaks = 50)
```
3. Mean and median steps per day.
```{r mean_median, echo=TRUE}
##Mean and median per day
total_steps_mean<-round(mean(one$total_steps, na.rm = TRUE),2)
print(paste('Mean steps is', total_steps_mean))
total_steps_median<-median(one$total_steps, na.rm = TRUE)
print(paste('Median steps is ', total_steps_median))
```
  - Mean steps is 9354.23
  - Median steps is  10395

## What is the average daily activity pattern?

1. Interval time series plot
```{r ts_plot, echo=TRUE}
avg_steps_per_interval<-activity%>%
    select(steps,interval)%>%
    group_by(interval)%>%
    summarise(average_steps = mean(steps,na.rm = TRUE))
plot(avg_steps_per_interval$interval, avg_steps_per_interval$average_steps, type = 'l', 
     main = 'Time Series Plot', xlab = 'Interval', ylab = 'Average Steps')
```
2. The interval with the highest average.
```{r max_interval, echo=TRUE}
max<-avg_steps_per_interval[avg_steps_per_interval$average_steps == max(avg_steps_per_interval$average_steps),]
print(paste('The interval with the highest average is ',max$interval))
```
  - The interval with the highest average is  835

## Imputing missing values

1.Calculate and report the total number of missing values in the dataset.

``` {r n_missing_data, echo=TRUE}
sum(is.na(activity$steps))
```

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
    - Impute mean of interval to intervals that are missing data.
    
3. Create a new dataset that is equal to the original dataset but with the missing data filled in.
``` {r new_data, echo=TRUE}
avg_steps_per_interval_fix<-avg_steps_per_interval%>%
    select(interval, average_steps)

activity_imputed_steps<-merge(activity, avg_steps_per_interval_fix, by = 'interval', all.x = TRUE)
activity_imputed_steps<-activity_imputed_steps%>%
    mutate(imputed_steps = if_else(is.na(steps), average_steps, as.numeric(steps)))

activity_imputed_steps_day<-activity_imputed_steps%>%
    select(steps,imputed_steps, date)%>%
    group_by(date)%>%
    summarise(total_steps = sum(steps,na.rm = TRUE), total_imputed_steps = sum(imputed_steps,na.rm = TRUE))%>%
    ungroup()%>%
    select(date, total_imputed_steps)

head(activity_imputed_steps_day)
```
4. Histogram
``` {r new_hist, echo=TRUE}
hist(activity_imputed_steps_day$total_imputed_steps, main = 'Histogram - Total Steps per Day', xlab = 'Total Imputed Steps per Day', breaks = 50)
```
  - After imputing the mean, the average total steps increased from 9354.23 to 10766 and the average total steps increased from 10395 to 10766. There is a slight difference, less than 10%, from the original mean, which shows that imputing the average doesn't skew the median/average far from the orginal data. 

## Are there differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

```{r new_data_day, echo=TRUE}
four<-activity%>%
    mutate(date = as.Date(date))%>%
    mutate(daytype = 
               case_when(
                   weekdays(date) %in% c('Sunday','Saturday') ~ 'weekend',
                   TRUE ~ as.character('weekday')))
```

2.Make a panel plot containing a time series plot using the weekday/weekend data.

```{r day_plot, echo=TRUE}
four_average<-four%>%
    select(daytype, interval, steps)%>%
    group_by(daytype, interval)%>%
    summarise(average_steps = mean(steps, na.rm = TRUE))
qplot(interval, average_steps, data=four_average, geom='line', facets=daytype~., ylab = 'Average Steps')

```
  - The subject is more active between 1000 and 1700 on the weekends compared to the weekdays. Less activity in the weekday during this time maybe due to the subject having a desk job.
