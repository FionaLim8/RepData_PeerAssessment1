---
title: "Reproducible Research Week 2 Assignment"
author: "Fiona"
date: "10/11/2020"
output: 
  html_document: 
    keep_md: yes
---

## Load and Convert
This is the code to load the csv file and convert it


```r
setwd("C:/Users/Fiona/Documents/R/5. Reproducible Research/Week 2 Assignment/repdata_data_activity")
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
library("lattice")
data <- read.csv("activity.csv")
data <- mutate(data, date = as.Date(date))
```


Calculate total and average steps for each day

```r
StepsDay <- with(data, tapply(steps, date, sum, na.rm=TRUE))
```
# Plot
Plot histogram of total steps per day
Calculate mean and median


```r
hist(StepsDay, xlab = "No. of Steps", main = "Total Steps per day")
```

![](PA1_template_files/figure-html/StepsDay-1.png)<!-- -->

```r
mean(StepsDay)
```

```
## [1] 9354.23
```

```r
median(StepsDay)
```

```
## [1] 10395
```

# Average Daily Activity Pattern
- Time series plot of 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
- Indicate which 5-minute interval contains maximum average steps

```r
StepsAvg <- with(data, tapply(steps, interval, mean, na.rm=TRUE))
plot(names(StepsAvg), StepsAvg, type = "l", xlab = "Interval", ylab = "Average Steps")
```

![](PA1_template_files/figure-html/StepsAvg-1.png)<!-- -->

```r
which.max(StepsAvg)
```

```
## 835 
## 104
```

# Imputing missing values


```r
sum(is.na(data))
```

```
## [1] 2304
```

```r
dat.clean <- na.omit(subset(data))
dat.na <- data[is.na(data$steps),]
StepsAvgDF <- as.data.frame.table(StepsAvg)
names(StepsAvgDF)[1] <- "interval"
names(StepsAvgDF)[2] <- "Avg_steps"
dat.merge <- merge(dat.na, StepsAvgDF)
dat.merge <- dat.merge[,c(4,3,1)]
names(dat.merge)[1] <- "steps"
dat.all <- rbind(dat.clean, dat.merge)
```

Create histogram with new data, and calculate mean and median

```r
NewStepsDay <- with(dat.all, tapply(steps, date, sum, na.rm=TRUE))
hist(NewStepsDay, xlab = "No. of Steps", main = "Total Steps per day (new)")
```

![](PA1_template_files/figure-html/New_data_Hist-1.png)<!-- -->

```r
mean(NewStepsDay)
```

```
## [1] 10766.19
```

```r
median(NewStepsDay)
```

```
## [1] 10766.19
```
In the new dataset, the histogram appears to be a bell curve, with the highest point at 10766.19 steps. Hence, the mean and the median are the same.

# Differences in activity patterns between weekdays and weekends
- Create factor variable to indicate weekday or weekend
- Panel plot of average number of steps by weekday and weekend.


```r
dat.all$day <- ifelse(weekdays(dat.all$date) %in% c("Saturday", "Sunday"), "weekend", "weekday")
dat.wkd <- subset(dat.all, day %in% c("weekday"))
dat.wkend <- subset(dat.all, day %in% c("weekend"))
avg.wkd <- with(dat.wkd, tapply(steps, interval, mean))
avg.wkend <- with(dat.wkend, tapply(steps, interval, mean))
avg.wkd <- as.data.frame.table(avg.wkd)
avg.wkd$day <- "weekday"
avg.wkend <- as.data.frame.table(avg.wkend)
avg.wkend$day <- "weekend"
avg.combine <- rbind(avg.wkd, avg.wkend)
names(avg.combine)[1] <- "Interval"
names(avg.combine)[2] <- "Average_Steps"

xyplot(Average_Steps ~ Interval | day, 
       group = day, data = avg.combine,
       type = "l", layout = c(1,2), ylab = "Number of Steps")
```

![](PA1_template_files/figure-html/Day-1.png)<!-- -->

