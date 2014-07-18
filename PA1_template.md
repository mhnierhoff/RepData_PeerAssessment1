# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

1. Load the data (i.e. read.csv()) 

```r
## Loading the personal movement data set
unzip("activity.zip")
activity <- read.csv("activity.csv", header = TRUE)
activity$date <- as.Date(activity$date, format = "%Y-%m-%d")
```

2. Process/transform the data (if necessary) into a format suitable for your analysis

```r
## Set German to English
Sys.setlocale(category = "LC_TIME", locale = "C")
```

____

## What is mean total number of steps taken per day?

1. Make a histogram of the total number of steps taken each day

```r
## Aggregating the sum of the total number of steps
dailyTotal <- aggregate(steps ~ date, data = activity, sum)
```


```r
## Plotting a histogram of the total daily steps
hist(dailyTotal$steps,
     xlab = "Steps",
     main = "Total Daily Steps",
     col = "darkblue",
     breaks = 10)
```

![plot of chunk unnamed-chunk-4](./PA1_template_files/figure-html/unnamed-chunk-4.png) 

2. Calculate and report the mean and median total number of steps taken per day

```r
## Calculate the mean and the median of the total number of steps
meanTotal <- mean(dailyTotal$steps)
medianTotal <- median(dailyTotal$steps)
```

The **mean** is 10766 and the **median** is 10765 for the total number of steps per day.

____

## What is the average daily activity pattern?

1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
## Aggregating the average daily activity pattern
averagePattern <- aggregate(steps ~ interval, data = activity, mean)
```


```r
## Creating a time series plot for the daily activity pattern
plot(averagePattern,
     type = "l",
     xlab = "5-Minute Intervals", 
     ylab = "Average Steps, Averaged Across All Days",
     main = "Average Daily Activity Pattern",
     col = "darkblue")
```

![plot of chunk unnamed-chunk-7](./PA1_template_files/figure-html/unnamed-chunk-7.png) 

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
## Finding the 5-minute interval with the maximum number of steps
maxInterval <- averagePattern[which.max(averagePattern$steps),"interval"]
```
The 5-minute interval, on average across all days in the dataset, that contains the maximum number of steps is 835.

____

## Imputing missing values

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
## Calculating the number of missing values
missingValues <- sum(is.na(activity))
```
The total number of missing values in the dataset is 2304. 

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

The **missing values** are getting replaced with the *mean* number of steps in that interval.

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
## Looping the new dataset with mean numbers instead of missing data
activityNew <- activity
for (i in 1:length(activity$steps)) {
        if (is.na(activity$steps[i])) {
            activityNew$steps[i] <- mean(activity$steps[activity$interval 
                                      == activity$interval[i]], na.rm=TRUE)
        }
}
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
## Aggregating the new sum of the total number of steps
dailyTotalNew <- aggregate(steps ~ date, data = activityNew, sum)
```


```r
## Plotting a histogram of the newly aggregated total daily steps
hist(dailyTotalNew$steps,
     xlab = "Steps",
     main = "Total Daily Steps\n(with imputed missing values)",
     col = "darkblue",
     breaks = 10)
```

![plot of chunk unnamed-chunk-12](./PA1_template_files/figure-html/unnamed-chunk-12.png) 


```r
## Calculate the mean and the median of the total number of steps
meanTotalNew <- mean(dailyTotalNew$steps)
medianTotalNew <- median(dailyTotalNew$steps)
```

The *new* **mean** is 10766 and the *new* **median** is 10766 for the total number of steps per day.
Therefore the impact of imputing missing data is really minimal. The *new* **median** is now one step higher than before and is equal to the *new* **mean**, which stays the same like before.

____

## Are there differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
## Creating the cator variable with the levels weekdays and weekend
weekend <- c("Saturday","Sunday")
activityNew$daytype <- as.factor(sapply(as.Date(activityNew$date), 
                       function(x) ifelse(weekdays(x) %in% weekend, 
                                          "weekend","weekday")))
```

2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 


```r
## Splitting the data set into weekdays and weekend
library(plyr)
averagePatternNew <- ddply(activityNew,
                           .(interval, daytype), summarize, steps = mean(steps))
```


```r
## Plotting a time series chart for the weekdays and weekend
library(lattice)
xyplot(steps ~ interval | daytype, 
       data = averagePatternNew, 
       layout = c(1, 2), 
       type = "l", 
       xlab = "5-Minute Intervals", 
       ylab = "Number Of Steps",
       main = "Activity Patterns For Weekends And Weekdays",
       col = "darkblue")
```

![plot of chunk unnamed-chunk-16](./PA1_template_files/figure-html/unnamed-chunk-16.png) 
