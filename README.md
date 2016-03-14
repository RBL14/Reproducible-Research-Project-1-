---
#**Title: Reproducable Research: Course Project 1** 

**Author: Ranjit Balijepalli**
---

#**Introduction**

It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the “quantified self” movement – a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

The data for this assignment can be downloaded from the link below:

Dataset: Activity monitoring data [52K] 
Download Link: http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip

#**The variables included in this dataset are:**

- steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)
- date: The date on which the measurement was taken in YYYY-MM-DD format
- interval: Identifier for the 5-minute interval in which measurement was taken
- The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.

#**Loading and preprocessing the data**
```{r}
if(!file.exists("getdata-projectfiles-UCI HAR Dataset.zip")) {
  temp <- tempfile()
  download.file("http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip",temp)
  unzip(temp)
  unlink(temp)
}

data <- read.csv("activity.csv")
```


#**What is mean total number of steps taken per day?**
The mean and median of the total number of steps taken per day


```{r}
steps_by_day <- aggregate(steps ~ date, data, sum)
hist(steps_by_day$steps, main = paste("Total Steps Per Day"), col="orange", xlab="Number of Steps")
```

![Img1](https://github.com/RBL14/Reproducible-Research-Project-1-/blob/master/Project%201_files/img1.png)


```{r}
rmean <- mean(steps_by_day$steps)
rmedian <- median(steps_by_day$steps)

```

The Mean = 10766.19 and The medina = 10765


#**What is the average daily activity pattern?**
- Calculate average steps for each interval for all days.
- Plot the Average Number Steps per Day by Interval.
- Find interval with most average steps.


```{r}
steps_by_interval <- aggregate(steps ~ interval, data, mean)

plot(steps_by_interval$interval,steps_by_interval$steps, type="l", xlab="Interval", ylab="No. of Steps",main="Avg. No. of Steps per Day by Interval" )

```

![Img2](https://github.com/RBL14/Reproducible-Research-Project-1-/Project 1_files/img2.png)


```{r}
max_interval <- steps_by_interval[which.max(steps_by_interval$steps),1]

```

5-minute interval, contains the maximum number of steps = 835


#**Impute missing values. Compare imputed to non-imputed data.**

Missing values were imputed with by inserting the average for each interval. for instance, if interval 10 was missing on 10-02-2012, the average for that interval for all days (0.1320755), replaced the NA.

```{r}
incomplete <- sum(!complete.cases(data))
imputed_data <- transform(data, steps = ifelse(is.na(data$steps), steps_by_interval$steps[match(data$interval, steps_by_interval$interval)], data$steps))
```

The first day (10-01-2012) was imputed with 0 as it would will be over 9,000 steps higher than the following day, which had only 126 steps. 

```{r}
imputed_data[as.character(imputed_data$date) == "2012-10-01", 1] <- 0
```

Recount total steps by day and create Histogram.

```{r}
steps_by_day_i <- aggregate(steps ~ date, imputed_data, sum)
hist(steps_by_day_i$steps, main = paste("Total Steps Each Day"), col="red", xlab="Number of Steps")
 
hist(steps_by_day$steps, main = paste("Total Steps Each Day"), col="orange", xlab="Number of Steps", add=T)
legend("topright", c("Imputed", "Non-imputed"), col=c("red", "orange"), lwd=10)
```

Create Histogram to show difference.


![Img3](https://github.com/RBL14/Reproducible-Research-Project-1-/Project 1_files/img3.png)

Calculate new mean and median for imputed data.

```{r}
rmean.i <- mean(steps_by_day_i$steps)
rmedian.i <- median(steps_by_day_i$steps)
```

Calculate difference between imputed and non-imputed data.

```{r}
mean_diff <- rmean.i - rmean
med_diff <- rmedian.i - rmedian
```

Calculate total difference.

```{r}
total_diff <- sum(steps_by_day_i$steps) - sum(steps_by_day$steps)
```

*The imputed data mean is 1.059 × 104
*The imputed data median is 1.0766 × 104
*The difference between the non-imputed mean and imputed mean is -176.4949
*The difference between the non-imputed mean and imputed mean is 1.1887
*The difference between total number of steps between imputed and non-imputed data is 7.5363 × 104. Thus, there were 7.5363 × 104 more steps in the imputed data.


#**Are there differences in activity patterns between weekdays and weekends?**

Created a plot to compare and contrast number of steps between the week and weekend. There is a higher peak earlier on weekdays, and more overall activity on weekends.

```{r}
weekdays <- c("Monday", "Tuesday", "Wednesday", "Thursday", 
              "Friday" )
imputed_data$dow = as.factor(ifelse(is.element(weekdays(as.Date(imputed_data$date)),weekdays), "Weekday", "Weekend"))

steps_by_interval_i <- aggregate(steps ~ interval + dow, imputed_data, mean)

library(lattice)

xyplot(steps_by_interval_i$steps ~ steps_by_interval_i$interval|steps_by_interval_i$dow, main="Avg. Steps per Day by Interval",xlab="Interval", ylab="Steps",layout=c(1,2), type="l") 

```


![Img4](https://github.com/RBL14/Reproducible-Research-Project-1-/Project 1_files/img4.png)