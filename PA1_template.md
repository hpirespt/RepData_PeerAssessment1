---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
author: "Horácio Pires"
---


## Loading and preprocessing the data

```r
 #1.
 dataactivity<-read.csv("activity.csv")
```


## What is mean total number of steps taken per day?

```r
#1.
#Rearrange and clean data
sumdays<-tapply(dataactivity$steps, dataactivity$date, sum)
sumdaysdf<-data.frame(days=names(sumdays), totsteps=as.vector(sumdays))
sumdaysdf<-sumdaysdf[complete.cases(sumdaysdf),]
#2.
#Print Histogram with mean
hist(sumdaysdf$totsteps, main = "Histogram of Total Steps per day", xlab = "Total Setps per day")
abline(v=mean(sumdaysdf$totsteps), col = "red")
```

![](PA1_template_files/figure-html/mean total steps-1.png)<!-- -->

### Mean and Median

```r
#3.
#Mean
mean(sumdaysdf$totsteps)
```

```
## [1] 10766.19
```

```r
#3.
#Median
median(sumdaysdf$totsteps)
```

```
## [1] 10765
```

## What is the average daily activity pattern?

```r
#1.
#Create the mean array by interval and create a dataframe out of it
meanint<-tapply(dataactivity$steps, dataactivity$interval, mean, na.rm = TRUE)
meanintdf<-data.frame(Interval = as.numeric(names(meanint)), MeanSteps = as.vector(meanint))

#Plot results as a line graph
plot(meanintdf$Interval, meanintdf$MeanSteps, type='l', col=2, main="Average number of steps by Interval", xlab="Time Intervals", ylab="Average number of steps")
```

![](PA1_template_files/figure-html/avg daily pattern-1.png)<!-- -->

```r
#2.
# Identify the interval index which has the highest average steps
intervalrow <- which.max(meanintdf$MeanSteps)
```

```r
#Interval
meanintdf[intervalrow,1]
```

```
## [1] 835
```

```r
#Mean nº of Steps
meanintdf[intervalrow,2]
```

```
## [1] 206.1698
```

## Imputing missing values

```r
#1.
#Total of missing Values
TotalNA<-sum(!complete.cases(dataactivity))
#2./3.
#Loop all the original dataset, and if finds an NA value gets the mean value for that interval from MeanInt and replace  
#the original NA for the new value
copydataact<-dataactivity

for (i in 1:nrow(copydataact)) {
     if(is.na(copydataact$steps[i])) {
         val <- meanintdf$MeanSteps[which(meanintdf$Interval == copydataact$interval[i])]
         copydataact$steps[i] <- val 
     }
}

#Apply sum of all the steps by day
sumdays_complete<-tapply(copydataact$steps, copydataact$date, sum)
sumdaysdf_complete<-data.frame(days=names(sumdays_complete), totsteps=as.vector(sumdays_complete))

#4. Print Histogram
hist(sumdaysdf_complete$totsteps, main = "Histogram of total number of steps per day with Input Values Replacing NA", xlab = "Steps per day"    )
abline(v=mean(sumdaysdf_complete$totsteps), col = "red")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->
### Mean and Median

```r
#4.
#Mean
mean(sumdaysdf_complete$totsteps)
```

```
## [1] 10766.19
```

```r
#4.
#Median
median(sumdaysdf_complete$totsteps)
```

```
## [1] 10766.19
```

## Are there differences in activity patterns between weekdays and weekends?

The first important step is to create a function to split the Workdays from Weekends. The weekday function is good but only says whcih day of 
the week are we in, so it's still necessary to split the if it's a workday from weekend. 


```r
week_day <- function(date) {
    wd <- weekdays(as.Date(date, '%Y-%m-%d'))
    if  (!(wd == 'Saturday' || wd == 'Sunday')) {
        x <- 'Weekday'
    } else {
        x <- 'Weekend'
    }
    x
}
```
Now we are ready to Sapply this fucntion to our new dataset

```r
#1.
# Apply the new week_day function to the date column in the new dataset, creating a new
#factor column with the information if it's weekday or weekend
copydataact$typeofday <- as.factor(sapply(copydataact$date, week_day))

#2.
#Create the mean array by interval and create a dataframe out of it
meanintdf_complete <- aggregate(steps ~ interval+typeofday, copydataact, mean)

#load ggplot2
library(ggplot2)

#Use ggplot2 to create a graph
p <- ggplot(meanintdf_complete, aes(interval, steps)) +
    geom_line(stat = "identity", aes(colour = typeofday)) +
    facet_grid(typeofday ~ .) + #create one chart on top and the other below, .~typeofday creates side by side
    labs(x="Interval", y=expression("Nº of Steps")) +
    ggtitle("Nº of steps Per Interval by type of day")
print(p)
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

