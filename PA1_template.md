# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
data <- read.csv("activity.csv")
```

## What is mean total number of steps taken per day?
### Make a histogram of the total number of steps taken each day

```r
countSteps <- aggregate(steps ~ date, data, sum,na.rm = TRUE)
hist(countSteps$steps, 25,xaxt = 'n', main = 'Histogram of total step counts per day',xlab = 'Step Count')
axis(side=1, at=seq(0,25000, 5000), labels=seq(0,25000,5000))
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 

### Calculate and report the mean and median total number of steps taken per day What is the average daily activity pattern?

```r
step_mean <- mean(countSteps$steps)
step_median <- median(countSteps$steps)
```

```r
step_mean
```

```
## [1] 10766
```

```r
step_median
```

```
## [1] 10765
```

## What is the average daily activity pattern?
### Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
intervalStepAvg <- aggregate(steps ~ interval, data, mean, na.rm = TRUE)
plot(intervalStepAvg$interval,intervalStepAvg$steps, type = 'l', xaxt = 'n',
     main = 'Average number of steps every 5 minute interval',
     xlab = '5 minute interval', ylab = 'steps')
axis(side=1, at=seq(0,2500, 100), labels=seq(0,2500,100))
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5.png) 
### Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
sortintervalStepAvg <- intervalStepAvg[order(intervalStepAvg$steps,decreasing = TRUE),] 
maxSteps <- sortintervalStepAvg$interval[1]
```

```r
maxSteps
```

```
## [1] 835
```
## Imputing missing values
### Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
naTotal <- length(which(is.na(data[1])==T))
```

```r
naTotal
```

```
## [1] 2304
```
### The strategy used for filling in all of the missing values in the dataset is to use the mean for that 5-minute interval.
### Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
datacopy <- data
for (i in 1:length(datacopy$steps)) {
      if(is.na(datacopy$steps[i]) == TRUE)
      {
            b <- datacopy$interval[i]
            c <- 1 + b/5
            datacopy$steps[i] <- intervalStepAvg$steps[c]
      }
}
```
### Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
countStepsnew <- aggregate(steps ~ date, data, sum,na.rm = TRUE)
hist(countStepsnew$steps, 25,xaxt = 'n', main = 'Histogram of total step counts per day',xlab = 'Step Count')
axis(side=1, at=seq(0,25000, 5000), labels=seq(0,25000,5000))
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11.png) 

```r
step_mean_new <- mean(countStepsnew$steps)
step_median_new <- median(countStepsnew$steps)
```

```r
step_mean_new
```

```
## [1] 10766
```

```r
step_median_new
```

```
## [1] 10765
```
## Are there differences in activity patterns between weekdays and weekends?
### Create a new factor variable in the dataset with two levels, weekday and weekend indicating whether a given date is a weekday or weekend day.

```r
datacopy$day <- weekdays(as.Date(datacopy$date))

datacopy <- as.data.frame(sapply(datacopy,gsub,pattern="Monday",replacement="Weekday"))
datacopy <- as.data.frame(sapply(datacopy,gsub,pattern="Tuesday",replacement="Weekday"))
datacopy <- as.data.frame(sapply(datacopy,gsub,pattern="Wednesday",replacement="Weekday"))
datacopy <- as.data.frame(sapply(datacopy,gsub,pattern="Thursday",replacement="Weekday"))
datacopy <- as.data.frame(sapply(datacopy,gsub,pattern="Friday",replacement="Weekday"))
datacopy <- as.data.frame(sapply(datacopy,gsub,pattern="Saturday",replacement="Weekend"))
datacopy <- as.data.frame(sapply(datacopy,gsub,pattern="Sunday",replacement="Weekend"))

n <- split(datacopy,datacopy$day)
```
### Make a panel plot containing a time series plot (i.e. type l) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). The plot should look something like the following, which was creating using simulated data:

```r
p <- n$Weekday
q <- n$Weekend

p$steps <- as.numeric(as.character(p$steps))
p$interval <- as.numeric(as.character(p$interval))

df1 <- aggregate(steps ~ interval, p, mean, na.rm = TRUE)
plot(df1$interval,df1$steps, type = 'l', xaxt = 'n',
     main = 'Average number of steps every 5 minute interval-weekday',
     xlab = '5 minute interval', ylab = 'steps')
axis(side=1, at=seq(0,2500, 100), labels=seq(0,2500,100))
```

![plot of chunk unnamed-chunk-15](figure/unnamed-chunk-15.png) 

```r
q$steps <- as.numeric(as.character(q$steps))
q$interval <- as.numeric(as.character(q$interval))

df2 <- aggregate(steps ~ interval, q, mean, na.rm = TRUE)
plot(df2$interval,df2$steps, type = 'l', xaxt = 'n',
     main = 'Average number of steps every 5 minute interval-weekend',
     xlab = '5 minute interval', ylab = 'steps')
axis(side=1, at=seq(0,2500, 100), labels=seq(0,2500,100))
```

![plot of chunk unnamed-chunk-16](figure/unnamed-chunk-16.png) 
