## ----loaddata------------------------------------------------------------
unzip(zipfile="activity.zip")
dataset <- read.csv("activity.csv")


## ------------------------------------------------------------------------
library(ggplot2)
total.steps <- tapply(dataset$steps, dataset$date, FUN=sum, na.rm=TRUE)
qplot(total.steps, binwidth=1000, xlab="Total number of steps taken each day")
mean(total.steps, na.rm=TRUE)
median(total.steps, na.rm=TRUE)


## ------------------------------------------------------------------------
library(ggplot2)
averages <- aggregate(x=list(steps=dataset$steps), by=list(interval=dataset$interval),
                      FUN=mean, na.rm=TRUE)
ggplot(data=averages, aes(x=interval, y=steps)) +
    geom_line() +
    xlab("5-minute interval") +
    ylab("average number of steps taken")


## ------------------------------------------------------------------------
averages[which.max(averages$steps),]


## ----how_many_missing----------------------------------------------------
missing <- is.na(dataset$steps)
# How many missing
table(missing)


## ------------------------------------------------------------------------
# Replace each missing value with the mean value of its 5-minute interval
fill.value <- function(steps, interval) {
    filled <- NA
    if (!is.na(steps))
        filled <- c(steps)
    else
        filled <- (averages[averages$interval==interval, "steps"])
    return(filled)
}
filled.dataset <- dataset
filled.dataset$steps <- mapply(fill.value, filled.dataset$steps, filled.dataset$interval)


## ------------------------------------------------------------------------
total.steps <- tapply(filled.dataset$steps, filled.dataset$date, FUN=sum)
qplot(total.steps, binwidth=1000, xlab="Total number of steps taken each day")
mean(total.steps)
median(total.steps)


## ------------------------------------------------------------------------
weekday.or.weekend <- function(date) {
    day <- weekdays(date)
    if (day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
        return("weekday")
    else if (day %in% c("Saturday", "Sunday"))
        return("weekend")
    else
        stop("invalid date")
}
filled.dataset$date <- as.Date(filled.dataset$date)
filled.dataset$day <- sapply(filled.dataset$date, FUN=weekday.or.weekend)


## ------------------------------------------------------------------------
averages <- aggregate(steps ~ interval + day, data=filled.dataset, mean)
ggplot(averages, aes(interval, steps)) + geom_line() + facet_grid(day ~ .) +
    xlab("5-minute interval") + ylab("Number of steps")


