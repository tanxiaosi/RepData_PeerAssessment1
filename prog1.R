# Unzip file and read in data

file <- "./activity.csv"
if (!file.exists(file)) {
  unzip("./activity.zip", exdir="./")
}
activitydata <- read.csv("./activity.csv", colClasses = c("numeric","character","numeric"))
activitydata$date <- as.Date(activitydata$date, "%Y-%m-%d")

library(dplyr)
stepsperday <- aggregate(steps ~ date, data = activitydata, sum, na.rm=TRUE)
meanperday <- mean(stepsperday$steps)
medianperday <- median(stepsperday$steps)

hist(stepsperday$steps, main = "Frequency of Steps Taken Per Day", 
     xlab = "Daily Step Count Range",
     ylab = "Number of Days Step Count Range is Reached", breaks = 15,
     xlim = c(0, 25000), col = "blue")

activitydata$interval[activitydata$date == "2012-10-01"]
activitydata <- mutate(activitydata, 
            interval1 = ((activitydata$interval %% 100)/5 + 1) + activitydata$interval %/% 100 * 12)

timeseries <- aggregate(steps ~ interval1, data = activitydata, mean, na.action = na.omit)

plot(timeseries$interval1, timeseries$steps, type = "l", xlab = "5-minute Interval (288/Day)", 
        ylab = "Daily Average (steps)", 
        main = "Average Number of Steps During 5-minute Time Intervals In a 24-hour Day", 
        col = "blue", xlim = c(0,300))

timeseries$interval1[timeseries$steps == max(timeseries$steps)]
gettimep <- function(x) {
  h1 <- (x*5) %/% 60
  h2 <- ((x+1)*5) %/% 60
  min1 <- (x*5) %% 60
  min2 <- ((x+1)*5) %% 60
  time <- paste(c(h1,h2), c(min1,min2), sep = ":")
  return(time)
}

NAcount <- sum(is.na(activitydata$steps))
library(reshape2)
a <- dcast(activitydata, interval ~ date, value.var="steps", fill= timeseries$steps)
a2 <- reshape(a, direction = "long", varying=list(names(r)[2:length(names(r))]),
              v.names=c("steps"), timevar="date", idvar=c("interval"),
              times=names(r)[2:length(names(r))], new.row.names=1:dim(activitydata)[1])
a2$date <- as.Date(a2$date, "%Y-%m-%d")

stepsperday2 <- aggregate(steps ~ date, data = a2, sum, na.rm=TRUE)
meanperday2 <- mean(stepsperday2$steps)
medianperday2 <- median(stepsperday2$steps)

hist(stepsperday2$steps, main = "Frequency of Steps Taken Per Day", 
     xlab = "Daily Step Count Range",
     ylab = "Number of Days Step Count Range is Reached", breaks = 15,
     xlim = c(0, 25000), col = "blue")

isweekend <- function(x){
  dw <- weekdays(x)
  th <- character(0)
  for(i in 1:length(dw)){
    if(dw[i] %in% c("Saturday","Sunday")){
      th[i] = "weekend"
    }
    else{
      th[i] = "weekday"
    }
  }
  return(th)
}

a2 <- mutate(a2, interval1 = ((activitydata$interval %% 100)/5 + 1) + activitydata$interval %/% 100 * 12)
a2 <- mutate(a2, day = as.factor(isweekend(a2$date)))
stepsweekday <- aggregate(steps ~ interval1 + day, data = a2, mean)

splot <- xyplot(steps ~ interval1 | day, stepsweekday, type = "l", layout = c(1, 2), 
                xlab = "5-minute Interval (288 per Day)", ylab = "Daily Average (steps)")
update(splot,
       main="Comparison of Average Number of Steps During 5-minute\nTime Intervals In a 24-hour Day\nfor Weekend Days Versus Weekdays")
