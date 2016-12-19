# Reproducible Research: Peer Assessment Project 1

## Download, unzip and load the data
```{r download_unzip_load_process_data}
src_file <- "http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
tgt_file <- "/Users/Sachin/coursera/reproducible_research_project1/activity.zip"
if(!file.exists(tgt_file)) {
   download.file(src_file, tgt_file)
   unzip(tgt_file)
}

data <- read.csv("/Users/Sachin/coursera/reproducible_research_project1/activity.csv")
```

## What is mean total number of steps taken per day?
Sum steps by day, create Histogram, and calculate mean and median.
```{r} 
steps_by_day <- aggregate(steps ~ date, data, sum)
hist(steps_by_day$steps, main = paste("Total Steps Each Day"), col="blue", xlab="Number of Steps")
rmean <- mean(steps_by_day$steps)
rmedian <- median(steps_by_day$steps)
```

The `mean` is `r rmean` and the `median` is `r rmedian`.

![plot of chunk unnamed-chunk-2](https://github.com/dmaurath/datasciencecoursera/blob/master/Reproducible%20Research%20Projects/Peer%20Assessment%201/Reproducible_Research_Project_1_Steps_files/figure-html/unnamed-chunk-2.png) 

```r
rmean <- mean(steps_by_day$steps)
rmedian <- median(steps_by_day$steps)
```

The `mean` is 1.0766 &times; 10<sup>4</sup> and the `median` is 10765.

## What is the average daily activity pattern?

* Calculate average steps for each interval for all days. 
* Plot the Average Number Steps per Day by Interval. 
* Find interval with most average steps. 

```r
steps_by_interval <- aggregate(steps ~ interval, data, mean)

plot(steps_by_interval$interval,steps_by_interval$steps, type="l", xlab="Interval", ylab="Number of Steps",main="Average Number of Steps per Day by Interval")
```

![plot of chunk unnamed-chunk-3](https://github.com/dmaurath/datasciencecoursera/blob/master/Reproducible%20Research%20Projects/Peer%20Assessment%201/Reproducible_Research_Project_1_Steps_files/figure-html/unnamed-chunk-3.png) 

```r
max_interval <- steps_by_interval[which.max(steps_by_interval$steps),1]
```

The 5-minute interval, on average across all the days in the data set, containing the maximum number of steps is 835.

## Impute missing values. Compare imputed to non-imputed data.
Missing data needed to be imputed. Only a simple imputation approach was required for this assignment. 
Missing values were imputed by inserting the average for each interval. Thus, if interval 10 was missing on 10-02-2012, the average for that interval for all days (0.1320755), replaced the NA. 

```r
incomplete <- sum(!complete.cases(data))
imputed_data <- transform(data, steps = ifelse(is.na(data$steps), steps_by_interval$steps[match(data$interval, steps_by_interval$interval)], data$steps))
```

Zeroes were imputed for 10-01-2012 because it was the first day and would have been over 9,000 steps higher than the following day, which had only 126 steps. NAs then were assumed to be zeros to fit the rising trend of the data. 

```r
imputed_data[as.character(imputed_data$date) == "2012-10-01", 1] <- 0
```

Recount total steps by day and create Histogram. 

```r
steps_by_day_i <- aggregate(steps ~ date, imputed_data, sum)
hist(steps_by_day_i$steps, main = paste("Total Steps Each Day"), col="blue", xlab="Number of Steps")

#Create Histogram to show difference. 
hist(steps_by_day$steps, main = paste("Total Steps Each Day"), col="red", xlab="Number of Steps", add=T)
legend("topright", c("Imputed", "Non-imputed"), col=c("blue", "red"), lwd=10)
```

![plot of chunk unnamed-chunk-6](https://github.com/dmaurath/datasciencecoursera/blob/master/Reproducible%20Research%20Projects/Peer%20Assessment%201/Reproducible_Research_Project_1_Steps_files/figure-html/unnamed-chunk-6.png) 

Calculate new mean and median for imputed data. 

```r
rmean.i <- mean(steps_by_day_i$steps)
rmedian.i <- median(steps_by_day_i$steps)
```

Calculate difference between imputed and non-imputed data.

```r
mean_diff <- rmean.i - rmean
med_diff <- rmedian.i - rmedian
```

Calculate total difference.

```r
total_diff <- sum(steps_by_day_i$steps) - sum(steps_by_day$steps)
```
* The imputed data mean is 1.059 &times; 10<sup>4</sup>
* The imputed data median is 1.0766 &times; 10<sup>4</sup>
* The difference between the non-imputed mean and imputed mean is -176.4949
* The difference between the non-imputed mean and imputed mean is 1.1887
* The difference between total number of steps between imputed and non-imputed data is 7.5363 &times; 10<sup>4</sup>. Thus, there were 7.5363 &times; 10<sup>4</sup> more steps in the imputed data.


## Are there differences in activity patterns between weekdays and weekends?
Created a plot to compare and contrast number of steps between the week and weekend. There is a higher peak earlier on weekdays, and more overall activity on weekends.  

```r
weekdays <- c("Monday", "Tuesday", "Wednesday", "Thursday", 
              "Friday")
imputed_data$dow = as.factor(ifelse(is.element(weekdays(as.Date(imputed_data$date)),weekdays), "Weekday", "Weekend"))

steps_by_interval_i <- aggregate(steps ~ interval + dow, imputed_data, mean)

library(lattice)

xyplot(steps_by_interval_i$steps ~ steps_by_interval_i$interval|steps_by_interval_i$dow, main="Average Steps per Day by Interval",xlab="Interval", ylab="Steps",layout=c(1,2), type="l")
```
![plot of chunk unnamed-chunk-10](https://github.com/dmaurath/datasciencecoursera/blob/master/Reproducible%20Research%20Projects/Peer%20Assessment%201/Reproducible_Research_Project_1_Steps_files/figure-html/unnamed-chunk-10.png) 

