---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---
### By Joe M

Note: Code chunks are set to show by default using: knitr::opts_chunk$set(echo = TRUE)


## Check enviroment to make sure this is reproducable
Check to make sure the system is set up to look at the files the same way mine were when creating this document

```r
## Code from Sean Murphy on this post: https://stackoverflow.com/questions/9341635/check-for-installed-packages-before-running-install-packages

packages <- c("lubridate","ggplot2","tidyverse")                                  ## LIst of packages needed to run this coed
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {              ## Check if its installed
  install.packages(setdiff(packages, rownames(installed.packages())))             ## If it is not, install it  
  
  ## Load the necessary libraries
  library(lubridate)                                                              ## Time manipulations
  library(ggplot2)                                                                ## Allow GG Plot functions
}
```

Next check the file structure

Note: If this code is ran again, Since getWD is used here, it is expected that this code is being ran in the working directory.

```r
## Variables for this code 
wd <- getwd()                                                                     ## Set Working location 
class_ZIP <- paste(wd, "activity.zip", sep = '/')                                 ## Create Zip File string
class_URL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip" # Class zip location
class_CSV <- paste(wd, "activity.csv", sep = '/')                                 ## Create CSV File string

## Zip check - get if not there
if (!file.exists(class_ZIP)) {download.file(class_URL, class_ZIP)}                ## If file does not exist, retrieve it

## CSV file check - Unzip crom class zip if not there
if (!file.exists(class_CSV)) {unzip(class_ZIP, overwrite = TRUE)}                 ## If file does not exist, retrieve it
```

## Loading and preprocessing the data

Pull the data into a data variable for manipulation

```r
csv_AD <- read.csv('activity.csv')                                                ## pull data from file for use 
```

## With the data in a usable format by R, we can get some data on it 
### Before using it, lets look at the data fast to see what is looks like

```r
str(csv_AD)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : chr  "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

Note the existing data, NAs that will need to be dealt with, and the date does not have time (as expected) 
the time interval will need to be incorporated...


```r
summary(csv_AD)
```

```
##      steps            date              interval     
##  Min.   :  0.00   Length:17568       Min.   :   0.0  
##  1st Qu.:  0.00   Class :character   1st Qu.: 588.8  
##  Median :  0.00   Mode  :character   Median :1177.5  
##  Mean   : 37.38                      Mean   :1177.5  
##  3rd Qu.: 12.00                      3rd Qu.:1766.2  
##  Max.   :806.00                      Max.   :2355.0  
##  NA's   :2304
```
OK, notice Date is a string (character), and the time is maxing out at 2355. 
Knowing this we should be able to perform the tasks for this assignment

First lets merge date and time for easier plotting/using

```r
#csv_AD$interval <- strptime(gsub("([0-9]{1,2})([0-9]{2})", "\\1:\\2", csv_AD$interval), format='%H:%M')
```

## Some basic questions can be answered

### What is mean total number of steps taken per day?


```r
stepsBD <- tapply(csv_AD$steps, csv_AD$date, sum, na.rm=TRUE)                    # Get Steps - Use Tapply as its over the set

plot(stepsBD, xlab='Count of Occurances', ylab='Steps Per Day', main = "Count of steps per day") ## Set plot to show data
```

![](PA1_template_files/figure-html/get_AvgStepsADay-1.png)<!-- -->

```r
## For last part of the question
meaStepsDay <- mean(stepsBD)                                                     ## Get the overall mean                          
medStepsDay <- median(stepsBD)                                                   ## Get the overall median
```

The last part of this question is to get the **mean** (9354.2295082) and the **Median** (10395).


## What is the average daily activity pattern?


```r
ast <- aggregate(x=list(meanSteps=csv_AD$steps), by=list(interval=csv_AD$interval), FUN=mean, na.rm=TRUE) ## NAs managed here
#### type = "l"
plot(data=ast, x=ast$interval, y=ast$meanSteps, type = "l", axes=FALSE, ann=FALSE)
```

```
## Warning in plot.window(...): "data" is not a graphical parameter
```

```
## Warning in plot.xy(xy, type, ...): "data" is not a graphical parameter
```

```r
    title(xlab="5Min Interval", ylab="Ave # Steps") 
   box()
```

![](PA1_template_files/figure-html/get_AvgDailyPattern-1.png)<!-- -->

```r
## also grab the info for the final part of this question
mS <- which.max(ast$meanSteps)                                                    ## Max steps in set
ms_Time <-  gsub("([0-9]{1,2})([0-9]{2})", "\\1:\\2", ast[mS,'interval'])         ## Find when it happened
```

The **most steps** (104) occurred at **8:35**.



## Imputing missing values
This section will look at the NAs we saw above. 


```r
num_NAs <- length(which(is.na(csv_AD$steps)))                                     ## Pull number of missing values

csv_HAD <- csv_AD                                                                 ## Place existing data into a holder 
mn_Steps = mean(csv_AD$steps, na.rm = TRUE)                                       ## Pull the mean number of steps  
csv_AD$steps[is.na(csv_AD$steps)] <- mn_Steps                                     ## Replace null with the mean steps

num_NAsA <- length(which(is.na(csv_AD$steps)))                                    ## Pull number of missing values again to compare

## This was done to reuse the above block to look at he steps and get the Median and Mean
```



```r
stepsBD <- tapply(csv_AD$steps, csv_AD$date, sum, na.rm=TRUE)                    # Get Steps - Use Tapply as its over the set

plot(stepsBD, xlab='Count of Occurances', ylab='Steps Per Day', main = "Count of steps per day") ## Set plot to show data
```

![](PA1_template_files/figure-html/get_AvgStepsADay-1.png)<!-- -->

```r
## For last part of the question
meaStepsDay <- mean(stepsBD)                                                     ## Get the overall mean                          
medStepsDay <- median(stepsBD)                                                   ## Get the overall median
```

Originally, there were 2304 occurances of NA, after the update it changed to 0.  
The last part of this question is to get the **mean** (1.0766189\times 10^{4}) and the **Median** (1.0766189\times 10^{4}).

## **Lastly**, are there differences in activity patterns between weekdays and weekends?

```r
library(ggplot2)                                                                 ## Have to call library in the block to work
csv_AD$dateType <-  ifelse(as.POSIXlt(csv_AD$date)$wday %in% c(0,6), 'weekend', 'weekday')   ## New variable

ggplot(csv_AD, aes(interval, steps)) + geom_line() + facet_grid(dateType ~ .) 
```

![](PA1_template_files/figure-html/set_WeekDayEnd-1.png)<!-- -->

```r
##  xlab("5Min Interval") + ylab("Ave # Steps")
```


