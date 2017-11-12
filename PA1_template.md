Reproducible Research Course
============================

Assignment 1
------------

### Made by Mauricio Paletta

1. Loading and preprocessing the data
=====================================

Data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

-   Dataset: Activity monitoring data \[52K\]

The variables are:

-   **steps**: Number of steps taking in a 5-minute interval (missing values are coded as NA)

-   **date**: The date on which the measurement was taken in YYYY-MM-DD format

-   **interval**: Identifier for the 5-minute interval in which measurement was taken

``` r
# Code for reading in the dataset and/or processing the data
#
activity <- read.csv("activity.csv")
activity$date <- as.Date(strptime(activity$date, "%Y-%m-%d"))
summary(activity)
```

    ##      steps             date               interval     
    ##  Min.   :  0.00   Min.   :2012-10-01   Min.   :   0.0  
    ##  1st Qu.:  0.00   1st Qu.:2012-10-16   1st Qu.: 588.8  
    ##  Median :  0.00   Median :2012-10-31   Median :1177.5  
    ##  Mean   : 37.38   Mean   :2012-10-31   Mean   :1177.5  
    ##  3rd Qu.: 12.00   3rd Qu.:2012-11-15   3rd Qu.:1766.2  
    ##  Max.   :806.00   Max.   :2012-11-30   Max.   :2355.0  
    ##  NA's   :2304

``` r
head(activity)
```

    ##   steps       date interval
    ## 1    NA 2012-10-01        0
    ## 2    NA 2012-10-01        5
    ## 3    NA 2012-10-01       10
    ## 4    NA 2012-10-01       15
    ## 5    NA 2012-10-01       20
    ## 6    NA 2012-10-01       25

2. What is mean total number of steps taken per day?
====================================================

Total number of steps taken per day
-----------------------------------

``` r
StepsByDay <- with(activity, tapply(steps, date, sum))
```

Histogram of the total number of steps taken each day
-----------------------------------------------------

``` r
hist(StepsByDay, 
     col = "green", 
     main = "Plot 1. Histogram of the total number of steps taken each day", 
     xlab = "Total steps taken")
rug(StepsByDay)

StepsByDayMeanOri <- mean(StepsByDay, na.rm = TRUE)
StepsByDayMedianOri <- median(StepsByDay, na.rm = TRUE)

abline(v = StepsByDayMeanOri, col = "blue", lwd = 1, lty = 2)
abline(v = StepsByDayMedianOri, col = "red", lwd = 1, lty = 3)

legend("topright", lty = c(2, 3), col = c("blue", "red"), 
       legend = c("Mean", "Median"))
```

![](PA1_template_files/figure-markdown_github/unnamed-chunk-4-1.png)

Mean and median of the total number of steps taken per day
----------------------------------------------------------

-   **Mean**: 10766.19

-   **Median**: 10765

3. What is the average daily activity pattern?
==============================================

Average number of steps taken by the 5-minute interval
------------------------------------------------------

``` r
# There are 288 5-minute intervals per day
#
intAverages <- aggregate(steps ~ interval, data = activity, mean)

# Change the intervals from 0 to 1435
#
aux <- c()
for (i in intAverages$interval) { aux <- c(aux, i - (i %/% 100 * 40)) }
intAverages$interval <- aux

dim(intAverages)
```

    ## [1] 288   2

Series plot of the 5-minute interval and the average number of steps taken, averaged across all days
----------------------------------------------------------------------------------------------------

``` r
with(intAverages, plot(interval, steps, 
                      type = "l", lwd = 2, col = "green",
                      xlab = "5-minute interval",
                      ylab = "Average of steps",
                      col.lab = "blue"))
title("Plot 2. Average of steps taken across all days by 5-minute interval")

StepsByIntMax <- with(intAverages, intAverages[which(steps == max(steps)), "interval"])
abline(v = StepsByIntMax, col = "blue", lwd = 1)
```

![](PA1_template_files/figure-markdown_github/unnamed-chunk-6-1.png)

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
-------------------------------------------------------------------------------------------------------------

**As it can be seen in Plot 2, the 5-minute interval which contains the maximum number of steps (on average across all the days) is 515 which corresponds to 8:35**

4. Imputing missing values
==========================

Calculate and report the total number of missing values in the dataset
----------------------------------------------------------------------

``` r
paste("Total number of missing values in the dataset: ", 
      nrow(activity[!complete.cases(activity),]))
```

    ## [1] "Total number of missing values in the dataset:  2304"

Devise a strategy for filling in all of the missing values in the dataset
-------------------------------------------------------------------------

### OPTION 1: using Hmisc for imputing missing data

``` r
if (!require("Hmisc")) install.packages("Hmisc")
```

    ## Loading required package: Hmisc

    ## Loading required package: lattice

    ## Loading required package: survival

    ## Loading required package: Formula

    ## Loading required package: ggplot2

    ## 
    ## Attaching package: 'Hmisc'

    ## The following objects are masked from 'package:base':
    ## 
    ##     format.pval, round.POSIXt, trunc.POSIXt, units

``` r
require("Hmisc")

# New dataset equal to the original dataset 
# A new column is added with the imputed steps column
#
activity.imputed1 <- activity
activity.imputed1$steps.imputed <- with(activity.imputed1, impute(steps, mean))

summary(activity.imputed1)
```

    ## 
    ##  2304 values imputed to 37.3826

    ##      steps             date               interval      steps.imputed   
    ##  Min.   :  0.00   Min.   :2012-10-01   Min.   :   0.0   Min.   :  0.00  
    ##  1st Qu.:  0.00   1st Qu.:2012-10-16   1st Qu.: 588.8   1st Qu.:  0.00  
    ##  Median :  0.00   Median :2012-10-31   Median :1177.5   Median :  0.00  
    ##  Mean   : 37.38   Mean   :2012-10-31   Mean   :1177.5   Mean   : 37.38  
    ##  3rd Qu.: 12.00   3rd Qu.:2012-11-15   3rd Qu.:1766.2   3rd Qu.: 37.38  
    ##  Max.   :806.00   Max.   :2012-11-30   Max.   :2355.0   Max.   :806.00  
    ##  NA's   :2304

### OPTION 2: Assigning the average for the corresponding 5-minute interval

``` r
if (!require("foreach")) install.packages("foreach")
```

    ## Loading required package: foreach

``` r
require("foreach")

activity.imputed2 <- activity

# For each steps == NA
#
invisible(foreach(idx = which(is.na(activity$steps))) %do% {
    # Take the corresponding interval
    #
    int <- activity[idx, "interval"]
    int <- int - (int %/% 100 * 40)

    # Change the steps with the average of the corresponding interval
    #
    activity.imputed2[idx, "steps"] <- 
        intAverages[which(intAverages$interval == int), "steps"]
})

summary(activity.imputed2)
```

    ##      steps             date               interval     
    ##  Min.   :  0.00   Min.   :2012-10-01   Min.   :   0.0  
    ##  1st Qu.:  0.00   1st Qu.:2012-10-16   1st Qu.: 588.8  
    ##  Median :  0.00   Median :2012-10-31   Median :1177.5  
    ##  Mean   : 37.38   Mean   :2012-10-31   Mean   :1177.5  
    ##  3rd Qu.: 27.00   3rd Qu.:2012-11-15   3rd Qu.:1766.2  
    ##  Max.   :806.00   Max.   :2012-11-30   Max.   :2355.0

Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day
---------------------------------------------------------------------------------------------------------------------------------------------

### For OPTION 1

``` r
StepsByDay <- with(activity.imputed1, tapply(steps.imputed, date, sum))
hist(StepsByDay, 
     col = "green", 
     main = "Plot 3. Histogram of the total number of steps taken each day", 
     xlab = "Total steps taken")
mtext("Missing values imputed by using Hmisc", side = 3)
rug(StepsByDay)

StepsByDayMeanImp1 <- mean(StepsByDay)
StepsByDayMedianImp1 <- median(StepsByDay)

abline(v = StepsByDayMeanImp1, col = "blue", lwd = 1, lty = 2)
abline(v = StepsByDayMedianImp1, col = "red", lwd = 1, lty = 3)

legend("topright", lty = c(2, 3), col = c("blue", "red"), 
       legend = c("Mean", "Median"))
```

![](PA1_template_files/figure-markdown_github/unnamed-chunk-10-1.png)

``` r
paste("Mean of the total number of steps taken each day: ", 
      round(StepsByDayMeanImp1, digits = 2))
```

    ## [1] "Mean of the total number of steps taken each day:  10766.19"

``` r
paste("Median of the total number of steps taken each day: ", 
      round(StepsByDayMedianImp1, digits = 2))
```

    ## [1] "Median of the total number of steps taken each day:  10766.19"

### For OPTION 2

``` r
StepsByDay <- with(activity.imputed2, tapply(steps, date, sum))
hist(StepsByDay, 
     col = "green", 
     main = "Plot 4. Histogram of the total number of steps taken each day", 
     xlab = "Total steps taken")
mtext("Missing values imputed with average of 5-minute interval", side = 3)
rug(StepsByDay)

StepsByDayMeanImp2 <- mean(StepsByDay)
StepsByDayMedianImp2 <- median(StepsByDay)

abline(v = StepsByDayMeanImp2, col = "blue", lwd = 1, lty = 2)
abline(v = StepsByDayMedianImp2, col = "red", lwd = 1, lty = 3)

legend("topright", lty = c(2, 3), col = c("blue", "red"), 
       legend = c("Mean", "Median"))
```

![](PA1_template_files/figure-markdown_github/unnamed-chunk-11-1.png)

``` r
paste("Mean of the total number of steps taken each day: ", 
      round(StepsByDayMeanImp2, digits = 2))
```

    ## [1] "Mean of the total number of steps taken each day:  10766.19"

``` r
paste("Median of the total number of steps taken each day: ", 
      round(StepsByDayMedianImp2, digits = 2))
```

    ## [1] "Median of the total number of steps taken each day:  10766.19"

Do these values differ from the estimates from the first part of the assignment?
--------------------------------------------------------------------------------

### For OPTION 1

Mean:

-   Without imputing missing values: 10766.19

-   Imputing missing values: 10766.19

-   Difference: 0

Median:

-   Without imputing missing values: 10765

-   Imputing missing values: 10766.19

-   Difference: -1.19

### For OPTION 2

Mean:

-   Without imputing missing values: 10766.19

-   Imputing missing values: 10766.19

-   Difference: 0

Median:

-   Without imputing missing values: 10765

-   Imputing missing values: 10766.19

-   Difference: -1.19

What is the impact of imputing missing data on the estimates of the total daily number of steps?
------------------------------------------------------------------------------------------------

**There is not much difference between the histograms using the original data and imputing missing values even with the two methods used.**

5. Are there differences in activity patterns between weekdays and weekends?
============================================================================

Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day
-------------------------------------------------------------------------------------------------------------------------------------------------

``` r
if (!require("lubridate")) install.packages("lubridate")
```

    ## Loading required package: lubridate

    ## 
    ## Attaching package: 'lubridate'

    ## The following object is masked from 'package:base':
    ## 
    ##     date

``` r
require("lubridate")

# Addind the new variable in both of the imputed datasets 
#
activity.imputed1$daytype <- apply(activity.imputed1, 1, function(act) {
    as.factor(ifelse(wday(act[2]) %in% 2:6, "weekday", "weekend"))    
})

head(activity.imputed1)
```

    ##   steps       date interval steps.imputed daytype
    ## 1    NA 2012-10-01        0       37.3826 weekday
    ## 2    NA 2012-10-01        5       37.3826 weekday
    ## 3    NA 2012-10-01       10       37.3826 weekday
    ## 4    NA 2012-10-01       15       37.3826 weekday
    ## 5    NA 2012-10-01       20       37.3826 weekday
    ## 6    NA 2012-10-01       25       37.3826 weekday

``` r
activity.imputed2$daytype <- apply(activity.imputed2, 1, function(act) {
    as.factor(ifelse(wday(act[2]) %in% 2:6, "weekday", "weekend"))    
})

head(activity.imputed2)
```

    ##       steps       date interval daytype
    ## 1 1.7169811 2012-10-01        0 weekday
    ## 2 0.3396226 2012-10-01        5 weekday
    ## 3 0.1320755 2012-10-01       10 weekday
    ## 4 0.1509434 2012-10-01       15 weekday
    ## 5 0.0754717 2012-10-01       20 weekday
    ## 6 2.0943396 2012-10-01       25 weekday

Panel plot containing a time series of the 5-minute interval and the average number of steps taken, averaged across all weekday days or weekend days
----------------------------------------------------------------------------------------------------------------------------------------------------

### OPTION 1:

``` r
# ggplot2 package is used for this plot
#
if (!require("ggplot2")) install.packages("ggplot2")
require("ggplot2")

averages <- aggregate(steps ~ interval + daytype, data = activity.imputed1, mean)
ggplot(data = averages, aes(x = interval, y = steps)) + 
    geom_line(color = "darkgreen", alpha = 1/2) + 
    facet_grid(daytype ~ .) +
    labs(title = "Plot 5. 5-minute interval vs. average number of steps taken",          subtitle = "Missing values imputed by using Hmisc") +
    labs(x = "Interval", y = "Number of steps")
```

![](PA1_template_files/figure-markdown_github/unnamed-chunk-13-1.png)

### OPTION 2:

``` r
averages <- aggregate(steps ~ interval + daytype, data = activity.imputed2, mean)
ggplot(data = averages, aes(x = interval, y = steps)) + 
    geom_line(color = "darkgreen", alpha = 1/2) + 
    facet_grid(daytype ~ .) +
    labs(title = "Plot 6. 5-minute interval vs. average number of steps taken",
         subtitle = "Missing values imputed with average of 5-minute interval") +
    labs(x = "Interval", y = "Number of steps")
```

![](PA1_template_files/figure-markdown_github/unnamed-chunk-14-1.png)

**As it can be seen in Plot 5 and Plot 6, there are differences in activity patterns between weekdays and weekends. In weekends the behavior is more flat throughout the day, instead, on weekdays, more activity is observed at the end of the morning.**
