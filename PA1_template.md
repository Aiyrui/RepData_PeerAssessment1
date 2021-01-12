---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

```r
data <- read.csv(unzip("activity.zip"), sep = ",")
```


## What is mean total number of steps taken per day?

```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(ggplot2)

df <- data %>% group_by(date) %>% 
  summarise(total.steps = sum(steps, na.rm = TRUE))
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
png("plot1.png", width = 480, height = 480)
qplot(df$total.steps, binwidth = 500, 
      xlab = "Total Steps Each Day")
dev.off()
```

```
## png 
##   2
```

Mean number of steps taken each day

```r
mean(df$total.steps)
```

```
## [1] 9354.23
```

Median number of steps taken each day

```r
median(df$total.steps)
```

```
## [1] 10395
```



## What is the average daily activity pattern?

```r
library(ggplot2)

daily.pattern <- aggregate(steps ~ interval, data, mean, na.rm = TRUE)

plot2 <- ggplot(daily.pattern, aes(interval, steps)) + 
  geom_line() + 
  xlab("5 Minute Interval") + 
  ylab("Average Steps Taken")

png("plot2.png", width = 480, height = 480)
print(plot2)
dev.off()
```

```
## png 
##   2
```

5 minute interval with the maximum of steps in daily activity

```r
daily.pattern[which.max(daily.pattern$steps),]
```

```
##     interval    steps
## 104      835 206.1698
```


## Imputing missing values
Calculate total number of missing values in dataset

```r
## Total NA's in dataset
sum(is.na(data))
```

```
## [1] 2304
```

Here, missing values are replaced by the mean of that 5 minute interval.

```r
indice <- which(is.na(data))
steps.noNA <- data

for(i in indice){
  if(is.na(data$steps[i])){
    steps.noNA$steps[i] <- daily.pattern$steps[steps.noNA$interval[i] == daily.pattern$interval]
  }
}
```

Plot histogram with dataset with the missing values filled in.

```r
library(ggplot2)
total.noNA <- steps.noNA %>% group_by(date) %>% summarise(total = sum(steps))
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
png("plot3.png", width = 480, height = 480)
qplot(total.noNA$total, binwidth = 500, xlab = "Total Steps Each Day")
dev.off()
```

```
## png 
##   2
```

New mean of steps taken with missing values filled in

```r
mean(total.noNA$total)
```

```
## [1] 10766.19
```

New median of steps taken with missing values filled in

```r
median(total.noNA$total)
```

```
## [1] 10766.19
```

The mean and median values are seen to be higher than the original estimates. By imputing the missing data instead of removing them completely, as done in the original estimates, the total daily steps are recalculated to include days where values were missing.

## Are there differences in activity patterns between weekdays and weekends?
Categorize dates into factor variable.

```r
library(dplyr)
library(lubridate)
```

```
## 
## Attaching package: 'lubridate'
```

```
## The following objects are masked from 'package:base':
## 
##     date, intersect, setdiff, union
```

```r
steps.noNA$date <- as_date(steps.noNA$date)
weekend <- c("Saturday", "Sunday")
steps.noNA$day <- if_else(weekdays(steps.noNA$date) %in% weekend, "Weekend", "Weekday")
steps.noNA$day <- factor(steps.noNA$day)
```

Plot weekday vs weekend panel plot.

```r
library(ggplot2)
avg.steps <- aggregate(steps ~ interval + day, steps.noNA, mean)
plot4 <- ggplot(avg.steps, aes(interval,steps)) + 
  facet_grid(day ~ .) +
  geom_line(aes(color = day)) +
  xlab("5 Minute Interval") +
  ylab("Average Steps Taken")

png("plot4.png", width = 480, height = 480)
print(plot4)
dev.off()
```

```
## png 
##   2
```
