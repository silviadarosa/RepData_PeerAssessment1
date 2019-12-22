---
title: "Reproducible Research: Peer Assessment 1"
author: "tayzee"
date: "21/12/2019"
output: html_document
keep_md: true
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```



## Loading and preprocessing the data
Load, transform and see
```{r}

activity<-read.csv("activity.csv",header = TRUE)
activity[,2]<-as.Date(activity$date)
str(activity)
```
## What is mean total number of steps taken per day?
```{r}
steps_1<-with(activity,tapply(steps,date,sum,na.rm=TRUE))
print(mean_steps<-mean(steps_1))
```
## What is the average daily activity pattern?
Let’s first make a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis). Then see which 5-minute interval contains the maximum number of steps
```{r}
avg_steps<-with(activity,tapply(steps,interval,mean,na.rm=TRUE))
intervals<-unique(activity$interval)
new<-data.frame(cbind(avg_steps,intervals))
plot(new$intervals,new$avg_steps,type = "l",xlab = "Intervals",
     ylab = "Average Steps",main = "Average Steps per Interval")
index<-which.max(new$avg_steps)
max<-new[index,2]
print(max)
```


## Imputing missing values
The taken approach is to calculate the average of average steps per day across all dates in the data set (ignoring NA values) and use the resulting value in place of NAs.
A new dataset with the missing values filled in is created.

```{r}
sum(is.na(activity$steps))
index<-which(is.na(activity$steps))
l<-length(index)
steps_avg<-with(activity,tapply(steps,date,mean,na.rm=TRUE))
na<-mean(steps_avg,na.rm = TRUE)
for (i in 1:l) {
        activity[index[i],1]<-na
}
```

## Are there differences in activity patterns between weekdays and weekends?
We need the package dplyr. Also, we will need to create a new variable in the dataset named “day” that shows the day of the week in terms of weekday or weekend. Then it can be plotted.
```{r}
library(dplyr)

activity$date <- as.Date(activity$date)
activity$weekday <- weekdays(activity$date)
activity$day_type <- ifelse(activity$weekday=="Saturday" |
                                           activity$weekday=="Sunday","Weekend","Weekday")
activity$day_type <- factor(activity$day_type)
day_types_data <- aggregate(steps ~ interval + day_type, data=activity, mean)

library(ggplot2)
ggplot(day_types_data, aes(interval, steps)) + 
        geom_line() + 
        facet_grid(day_type ~ .) +
        xlab("5-minute intervals") + 
        ylab("Avarage number of steps taken") +
        ggtitle("Weekdays and weekends activity patterns")

```
A new variable indicating if the date is weekday or weekend needs to be created. The, plotting the number of steps for all five minutes intervals it can be concluded that the number of steps is higher on average on weekdays.
