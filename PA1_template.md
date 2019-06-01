---
title: "Reproducible Research: Peer Assessment 1"
author: "Shenay"
date: "June 2, 2019"
output: 
  html_document: 
    keep_md: yes
---



## Loading and preprocessing the data

* Load the data
* Process/transform the data if necessary into a format suitable for analysis


```r
setwd("C:/Users/sheng/Coursera")
if (!file.exists("./data")) {dir.create("./data")}
fileURL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(fileURL, destfile = "./data/activity.zip", mode = "wb")

unzip("./data/activity.zip")
data <- read.csv("activity.csv")

str(data)  
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
summary(data) 
```

```
##      steps                date          interval     
##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
##  Median :  0.00   2012-10-03:  288   Median :1177.5  
##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
##  3rd Qu.: 12.00   2012-10-05:  288   3rd Qu.:1766.2  
##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
##  NA's   :2304     (Other)   :15840
```

```r
data$date <- as.Date(data$date, format = "%Y-%m-%d")
```

## What is mean total number of steps taken per day?

* Calculate the total number of steps taken per day
* Make a histogram of the total number of steps taken per day
* Calculate and report the mean and median of total number of steps taken per day


```r
png(file = "plot1.png", width = 480, height = 480)

total_day <- aggregate(x = data$steps, by = list(data$date), FUN = sum, na.rm = TRUE)
colnames(total_day) <- c("date", "steps")
summary(total_day$steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##       0    6778   10395    9354   12811   21194
```

```r
hist(total_day$steps, main = "Histogram of Total Number of Steps Taken Per Day",
     xlab = "Steps", border = "royalblue4", col = "royalblue", las = 1)
abline(v = mean(total_day$steps), lwd = 2)

dev.off()
```

```
## png 
##   2
```


## What is the average daily activity pattern?

* Make a time series plot of the 5-minute interval and the average number of steps taken, averaged across all days
* Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
png(file = "plot2.png", width = 480, height = 480)

avg_day <- aggregate(x = data$steps, by = list(data$interval), FUN = mean, na.rm = TRUE)
colnames(avg_day) <- c("interval", "avg.steps")
summary(avg_day$avg.steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   0.000   2.486  34.113  37.383  52.835 206.170
```

```r
ggplot(avg_day, aes(x = interval)) +
        geom_line(aes(y = avg.steps)) +
        geom_hline(yintercept = mean(avg_day$avg.steps), col = "blue") +
        geom_hline(yintercept = max(avg_day$avg.steps), col = "green") +
        labs(title = "Time Series Plot of Average Number of Steps Taken (Per 5-Min Interval)",
             x = "Intervals", y = "Steps") +
        annotate("label", x = max(avg_day$avg.steps), y = mean(avg_day$avg.steps), label = "Mean") +
        annotate("label", x = max(avg_day$avg.steps), y = max(avg_day$avg.steps), label = "Maximum") +
        theme_classic()

dev.off()
```

```
## png 
##   2
```

## Imputing missing values

* Calculate and report the total number of missing values in the dataset
* Devise a strategy for filling in all of the missing values in the dataset
* Create a new dataset that is equal to the original dataset but with the missing data filled in
* Make a histogram of the total number of steps taken each day. Calculate and report the mean and median total number of steps taken per day


```r
png(file = "plot3.png", width = 480, height = 480)

sum(is.na(data$steps), na.rm = TRUE)
```

```
## [1] 2304
```

```r
data$steps[which(is.na(data$steps))] <- mean(data$steps, na.rm = TRUE)

na_index <- data.frame(data[which(is.na(data$steps)), ])
na_index$steps <- replace(na_index$steps, na_index$interval %in% avg_day$interval, avg_day$avg.steps)

data_lessna <- data.frame(data[complete.cases(data), ])
new_data <- rbind(na_index, data_lessna)

newtotal_day <- aggregate(x = new_data$steps, by = list(new_data$date), FUN = sum, na.rm = TRUE)
colnames(newtotal_day) <- c("date", "steps")

summary(newtotal_day$steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    9819   10766   10766   12811   21194
```

```r
hist(newtotal_day$steps, main = "Histogram of Total Number of Steps Taken Per Day",
     xlab = "Steps", border = "lightskyblue4", col = "lightskyblue", las = 1)

dev.off()
```

```
## png 
##   2
```


## Are there differences in activity patterns between weekdays and weekends?

* Create a new factor variable in the dataset with two levels – “weekday” and “weekend” 
* Make a panel plot containing a time series plot of the 5-minute interval and the average number of steps taken, averaged across all weekday days or weekend days


```r
png(file = "plot4.png", width = 480, height = 480)

wkday <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")

new_data$weekday <- factor((weekdays(new_data$date) %in% wkday),
                         levels = c(TRUE, FALSE),
                         labels = c("weekday", "weekend"))

newavg_day <- aggregate(steps ~ interval + weekday, data = new_data, FUN = mean)
colnames(newavg_day) <- c("interval", "weekday", "avg.steps")

ggplot(newavg_day, aes(x = interval)) +
        geom_line(aes(y = avg.steps)) +
        facet_wrap(~ weekday, ncol = 1) +
        labs(title = "Time Series Plot of Average Number of Steps Taken",
             x = "Interval", y = "Steps") +
        theme_classic()

dev.off()
```

```
## png 
##   2
```

