TASK 1 - What is mean total number of steps taken per day?
----------------------------------------------------------

SUBTASK 1.1 - Calculate the total number of steps taken per day

``` r
dt <- read.csv(file="activity.csv", header=TRUE, sep=",")

dt$date <- as.Date(as.character(dt$date), "%Y-%m-%d")

agg<- aggregate(steps~date, data=dt, sum)
```

SUBTASK 1.2 - If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day

``` r
## nice plot wiht the total steps per day, but we can't use it as we need a histogram
##g <- ggplot(data=agg, aes(factor(date), steps)) + geom_bar(stat="identity") +
##     theme(axis.text.x  = element_text(angle=90)) +
##     xlab("Date")  
## histogram with total number of steps per day
g <- hist(agg$steps, main="Histogram of total number of steps per day", 
     xlab="Total number of steps in a day")
```

![](PA1_template_files/figure-markdown_github/subtask%201.2-1.png)

SUBTASK 1.3 - Calculate and report the mean and median of the total number of steps taken per day

``` r
s <- summary(agg)
print(paste("Total number of steps taken per day, ", s[9]))
```

    ## [1] "Total number of steps taken per day,  Median :10765  "

``` r
print(paste("Total number of steps taken per day, ", s[10]))
```

    ## [1] "Total number of steps taken per day,  Mean   :10766  "

TASK 2 - IWhat is the average daily activity pattern?
-----------------------------------------------------

SUBTASK 2.1 - Make a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

``` r
agg<- aggregate(steps~interval, data=dt, mean)
g <- ggplot(agg, 
            aes(x=interval,
                y=steps)) +
            geom_line() +
            ggtitle("Average number of steps taken across all days at 5-minute interval") +
            theme(plot.title = element_text(face="bold.italic"))
print(g)
```

![](PA1_template_files/figure-markdown_github/subtask%202.1-1.png)

SUBTASK 2.2 - Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

``` r
agg <- aggregate(steps~interval, mean, data = dt)
print(paste("The 5-minute interval, on average across all the days in the dataset, that contains the maximum number of steps: ", subset(agg, agg$steps == max(agg$steps))$interval))
```

    ## [1] "The 5-minute interval, on average across all the days in the dataset, that contains the maximum number of steps:  835"

TASK 3 - Imputing missing values
--------------------------------

SUBTASK 3.1 - Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with 𝙽𝙰s)

``` r
print(paste("The total number of missing values in the dataset:", nrow(dt[!complete.cases(dt),])))
```

    ## [1] "The total number of missing values in the dataset: 2304"

SUBTASK 3.2 - Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

``` r
##Since all values for every day that contains NA's are NA's we can't use the mean/median for that day to fill the NA's
##so we will use the 5-minute average to fill all the NA's
agg3 <- dt[complete.cases(dt),]
agg3 <- aggregate(steps~interval, data=agg3, mean)
##t distinct dates into a variable
uniqueDT <- unique(dt$date)
##loop through the distinct dates
for (i in 1:length(uniqueDT))
{
    ##get ALL rows for the date selected
    sub = subset(dt, dt$date == uniqueDT[i])
    ##check if any NA's exist for a certain date
    if(anyNA(sub))
    {
        ##check if ALL values for this date are NA's
        if(nrow(sub[complete.cases(sub),]) == 0)
        {
            print(paste("All values are NA's for this date >" ,uniqueDT[i]))    
        }
    }
}
```

    ## [1] "All values are NA's for this date > 2012-10-01"
    ## [1] "All values are NA's for this date > 2012-10-08"
    ## [1] "All values are NA's for this date > 2012-11-01"
    ## [1] "All values are NA's for this date > 2012-11-04"
    ## [1] "All values are NA's for this date > 2012-11-09"
    ## [1] "All values are NA's for this date > 2012-11-10"
    ## [1] "All values are NA's for this date > 2012-11-14"
    ## [1] "All values are NA's for this date > 2012-11-30"

``` r
names(agg3) <- c("agginterval", "aggsteps")
##append the mean at 5-min interval to original data frame and create new data frame
both <- cbind(dt, agg3)
myf <- function(setps, aggsteps) { 
        ifelse(is.na(setps), aggsteps, setps) 
        }
##create new column by taking non NA's from either steps(if not NA) or 5-min mean
for (row in 1:nrow(both)) { 
    both$newvar[row] <- myf(both$steps[row], both$aggsteps[row]) 
}
##create new data frame by taking 3 columns from data frame above
newdt <- both[c("newvar", "date", "interval")]
##rename first column
names(newdt)[1]<-"steps"
```

SUBTASK 3.3 - Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

``` r
agg4<- aggregate(steps~date, data=newdt, sum)
## histogram with total number of steps per day
g <- hist(agg4$steps, main="Histogram of total number of steps per day", 
          xlab="Total number of steps in a day")
```

![](PA1_template_files/figure-markdown_github/subtask%203.3-1.png)

``` r
agg4 <- aggregate(steps~date, data=newdt, sum)
s <- summary(agg4)
print(paste("Total number of steps taken per day, ", s[9]))
```

    ## [1] "Total number of steps taken per day,  Median :10766  "

``` r
print(paste("Total number of steps taken per day, ", s[10]))
```

    ## [1] "Total number of steps taken per day,  Mean   :10766  "

``` r
print("New Median is like the Mean")
```

    ## [1] "New Median is like the Mean"

TASK 4 - Are there differences in activity patterns between weekdays and weekends?
----------------------------------------------------------------------------------

SUBTASK 4.1 - Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

``` r
newdt$weekday <- as.factor(ifelse(weekdays(newdt$date) %in% c("Saturday", "Sunday"), "weekend", "weekday"))
```

SUBTASK 4.2 - Make a panel plot containing a time series plot (i.e type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

``` r
newdtagg <- aggregate(steps~interval+weekday, data=newdt, mean)

g <- ggplot( data = newdtagg,
             aes(x=interval, y=steps, 
                   fill=weekday, color=weekday))  + 
            geom_line(alpha=0.3) +
            geom_smooth() + 
            facet_wrap( ~ weekday, ncol=1) +
            theme(legend.position="none") + 
            labs(x="Interval", y="Number of steps")

print(g)
```

    ## `geom_smooth()` using method = 'loess'

![](PA1_template_files/figure-markdown_github/subtask%204.2-1.png)
