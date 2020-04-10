---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

```r
activity <- read.csv('activity.csv')
library(ggplot2)
library(dplyr)
library(knitr)
```

## What is mean total number of steps taken per day?


```r
hist_act <- activity %>% group_by(date) %>% summarize(Daily_Steps = sum(steps, na.rm = TRUE))
hist_act$date <- as.Date(hist_act$date, format = '%Y-%m-%d')
g <- ggplot(hist_act, aes(x = Daily_Steps)) +geom_histogram(bins = 10)
summary_act <- hist_act %>% summarize(mean_steps = mean(Daily_Steps, na.rm = TRUE), median_steps = median(Daily_Steps, na.rm = TRUE))
```

1. Calculate the total number of steps taken per day

date          Daily_Steps
-----------  ------------
2012-10-01              0
2012-10-02            126
2012-10-03          11352
2012-10-04          12116
2012-10-05          13294
2012-10-06          15420
2012-10-07          11015
2012-10-08              0
2012-10-09          12811
2012-10-10           9900
2012-10-11          10304
2012-10-12          17382
2012-10-13          12426
2012-10-14          15098
2012-10-15          10139
2012-10-16          15084
2012-10-17          13452
2012-10-18          10056
2012-10-19          11829
2012-10-20          10395
2012-10-21           8821
2012-10-22          13460
2012-10-23           8918
2012-10-24           8355
2012-10-25           2492
2012-10-26           6778
2012-10-27          10119
2012-10-28          11458
2012-10-29           5018
2012-10-30           9819
2012-10-31          15414
2012-11-01              0
2012-11-02          10600
2012-11-03          10571
2012-11-04              0
2012-11-05          10439
2012-11-06           8334
2012-11-07          12883
2012-11-08           3219
2012-11-09              0
2012-11-10              0
2012-11-11          12608
2012-11-12          10765
2012-11-13           7336
2012-11-14              0
2012-11-15             41
2012-11-16           5441
2012-11-17          14339
2012-11-18          15110
2012-11-19           8841
2012-11-20           4472
2012-11-21          12787
2012-11-22          20427
2012-11-23          21194
2012-11-24          14478
2012-11-25          11834
2012-11-26          11162
2012-11-27          13646
2012-11-28          10183
2012-11-29           7047
2012-11-30              0

2. Histogram of the total number of steps taken each day
![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

3. Calculate and report the mean and median of the total number of steps taken per day

 mean_steps   median_steps
-----------  -------------
    9354.23          10395



## What is the average daily activity pattern?

```r
int_act<-activity %>% group_by(interval) %>% summarize(Avg_Daily_Pattern = mean(steps, na.rm = TRUE))
daily <- ggplot(int_act, aes(x=interval, y = Avg_Daily_Pattern)) + geom_line()
```

1. Time series plot of the 5-minute interval and avg number of steps taken across all days
![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

2. 5-min interval, on average across all days in the data, contains the maximum number of steps?
\hfill\break
[1] 835


## Imputing missing values

```r
total_missing <- sum(is.na(activity))
impute_activity <- activity
for (i in 1:nrow(impute_activity)){
  
  if (is.na(activity[i,]$steps)){
    
    impute_activity[i,]$steps <- int_act[which(int_act$interval == impute_activity[i,]$interval),]$Avg_Daily_Pattern
    
  }
  
}

impute_activity_hist <- impute_activity %>% group_by(date) %>% summarize(Daily_Steps = sum(steps, na.rm = TRUE))
impute_activity_hist$date <- as.Date(impute_activity_hist$date, format = '%Y-%m-%d')
imp_g<-ggplot(impute_activity_hist, aes(x=Daily_Steps)) +geom_histogram(bins = 15)

summary_impute_act <- impute_activity %>% group_by(date) %>% summarize(mean_steps = mean(steps, na.rm = TRUE), median_steps = median(steps, na.rm = TRUE))
```

1. Calculate the total number of missing values in the data set
\hfill\break
[1] 2304

2/3. Impute missing values in the data set. Create a new data set.
\hfill\break

```r
impute_activity <- activity
for (i in 1:nrow(impute_activity)){
  
  if (is.na(activity[i,]$steps)){
    
    impute_activity[i,]$steps <- int_act[which(int_act$interval == impute_activity[i,]$interval),]$Avg_Daily_Pattern
    
  }
  
}
```


4. Histogram of the total number of steps taken each day: I have found the values to differ from the first part of the assignment. The data is less skewed in my opinion.
\hfill\break


![](PA1_template_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

date          mean_steps   median_steps
-----------  -----------  -------------
2012-10-01    37.3825996       34.11321
2012-10-02     0.4375000        0.00000
2012-10-03    39.4166667        0.00000
2012-10-04    42.0694444        0.00000
2012-10-05    46.1597222        0.00000
2012-10-06    53.5416667        0.00000
2012-10-07    38.2465278        0.00000
2012-10-08    37.3825996       34.11321
2012-10-09    44.4826389        0.00000
2012-10-10    34.3750000        0.00000
2012-10-11    35.7777778        0.00000
2012-10-12    60.3541667        0.00000
2012-10-13    43.1458333        0.00000
2012-10-14    52.4236111        0.00000
2012-10-15    35.2048611        0.00000
2012-10-16    52.3750000        0.00000
2012-10-17    46.7083333        0.00000
2012-10-18    34.9166667        0.00000
2012-10-19    41.0729167        0.00000
2012-10-20    36.0937500        0.00000
2012-10-21    30.6284722        0.00000
2012-10-22    46.7361111        0.00000
2012-10-23    30.9652778        0.00000
2012-10-24    29.0104167        0.00000
2012-10-25     8.6527778        0.00000
2012-10-26    23.5347222        0.00000
2012-10-27    35.1354167        0.00000
2012-10-28    39.7847222        0.00000
2012-10-29    17.4236111        0.00000
2012-10-30    34.0937500        0.00000
2012-10-31    53.5208333        0.00000
2012-11-01    37.3825996       34.11321
2012-11-02    36.8055556        0.00000
2012-11-03    36.7048611        0.00000
2012-11-04    37.3825996       34.11321
2012-11-05    36.2465278        0.00000
2012-11-06    28.9375000        0.00000
2012-11-07    44.7326389        0.00000
2012-11-08    11.1770833        0.00000
2012-11-09    37.3825996       34.11321
2012-11-10    37.3825996       34.11321
2012-11-11    43.7777778        0.00000
2012-11-12    37.3784722        0.00000
2012-11-13    25.4722222        0.00000
2012-11-14    37.3825996       34.11321
2012-11-15     0.1423611        0.00000
2012-11-16    18.8923611        0.00000
2012-11-17    49.7881944        0.00000
2012-11-18    52.4652778        0.00000
2012-11-19    30.6979167        0.00000
2012-11-20    15.5277778        0.00000
2012-11-21    44.3993056        0.00000
2012-11-22    70.9270833        0.00000
2012-11-23    73.5902778        0.00000
2012-11-24    50.2708333        0.00000
2012-11-25    41.0902778        0.00000
2012-11-26    38.7569444        0.00000
2012-11-27    47.3819444        0.00000
2012-11-28    35.3576389        0.00000
2012-11-29    24.4687500        0.00000
2012-11-30    37.3825996       34.11321




## Are there differences in activity patterns between weekdays and weekends?

```r
impute_activity$day_type <- ifelse(weekdays(as.Date(impute_activity$date, format = '%Y-%m-%d')) == 'Saturday' | 
                                     weekdays(as.Date(impute_activity$date, format = '%Y-%m-%d')) == 'Sunday', 'weekend', 'weekday')


int_impute_act <- impute_activity %>% group_by(interval, day_type) %>% summarize(Avg_Daily_Pattern = mean(steps, na.rm = TRUE))


wk_act <- ggplot(int_impute_act, aes(x=interval, y = Avg_Daily_Pattern)) + geom_line() + facet_grid(as.factor(int_impute_act$day_type))
```


1. New factor variable with two levels for weekday and weekend

```r
impute_activity$day_type <- ifelse(weekdays(as.Date(impute_activity$date, format = '%Y-%m-%d')) == 'Saturday' | 
                                     weekdays(as.Date(impute_activity$date, format = '%Y-%m-%d')) == 'Sunday', 'weekend', 'weekday')
```

2. Panel plot of the 5 minute interval (x-axis)
![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

