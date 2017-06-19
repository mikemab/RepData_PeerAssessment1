library(httr)

if(!file.exists("activity.zip")){
     temp = "activity.zip"
     GET("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip", write_disk(temp, overwrite=FALSE)) } else if (!file.exists("activity.csv")){
     unzip("activity.zip")     
     }

activityData <- read.csv("activity.csv", header = TRUE, na = "NA", stringsAsFactors = TRUE)

activityData$date <- as.Date(activityData$date)


summary(activityData)

## Steps by day

library(reshape2)


stepsbyday <- tapply(activityData$steps, activityData$date, sum, na.rm=TRUE)
hist(stepsbyday, main="Histogram of total number of steps taken each day", xlab = "Step Count", ylab = "# of days", breaks = seq(0,25000, by = 2500))

mean(stepsbyday, na.rm = TRUE)
median(stepsbyday, na.rm = TRUE)

#data frame with steps by day indexed by date
dfstepbyday <- cbind(unique(activityData$date), stepsbyday)

dfstepbyday[,1]

stepsbyday

meanStepsbyInterval <- tapply(activityData$steps, activityData$interval, mean, na.rm=TRUE)
plot(unique(activityData$interval), meanStepsbyInterval, type="l", col="blue", main="Average number of steps by Interval", xlab="Interval",ylab = "Average number of steps")

#data frame creation

dfInterval <- cbind(unique(activityData$interval), meanStepsbyInterval)
dfInterval <- as.data.frame(dfInterval)
names(dfInterval) <- c("interval","meanSteps")

maxdfInterval <- (c(dfInterval$interval[which.max(dfInterval$meanSteps)],dfInterval$meanSteps[which.max(dfInterval$meanSteps)]))
names(maxdfInterval) <- c("Interval#", "MeanSteps")
maxdfInterval

length(unique(activityData$interval))
length(meanStepsbyInterval)


#Imputing NA values

sum(!complete.cases(activityData))

#Strategy use the Median values

medianStepsbyInterval <- tapply(activityData$steps, activityData$interval, median)

dfInterval_median <- cbind(unique(activityData$interval), medianStepsbyInterval)
dfInterval_median <- as.data.frame(dfInterval_median)
head(dfInterval_median)

#dfInterval_median$medianStepsbyInterval[match(activityData$interval, dfInterval_median$V1)]


act_impute <- transform(activityData, steps = ifelse(is.na(activityData$steps), yes = dfInterval_median$medianStepsbyInterval[match(activityData$interval, dfInterval_median$V1)], no = activityData$steps))

stepsbyday_impute <- tapply(act_impute$steps, act_impute$date, sum)
hist(stepsbyday_impute, main="Histogram of total number of steps taken each day", xlab = "Step Count", ylab = "# of days", breaks = seq(0,25000, by = 2500), col="blue")

mean(stepsbyday_impute, na.rm = TRUE)
median(stepsbyday_impute, na.rm = TRUE)

summary(stepsbyday_impute)

weekdays(activityData$date)

weekendIndicator <- function(x){
     if (weekdays(x) == "Saturday" | weekdays(x) == "Sunday") {
          y = "Weekend" } 
     else {
          y = "Weekday"
     }
     y
}


activityData$dayType <- sapply(activityData$date, weekendIndicator)
meanstepsbyintervaldaytype <- aggregate(steps~interval+dayType, activityData, mean)

head(meanstepsbyintervaldaytype)

library(ggplot2)
plot <- ggplot(meanstepsbyintervaldaytype, aes(x = interval, y = steps, color = dayType)) + geom_line() + facet_wrap(~dayType, ncol=1, nrow = 2) + labs(title = "Average Daily Steps by Weekend/Weekday", x = "Interval (5 minutes each)", y = "Step count in interval")
print(plot)


meanStepsbyIntervaltest 
library(ggplot2)


meanStepsbyIntervalday <- tapply(activityData$steps, list(activityData$interval, activityData$dayType), mean, na.rm=TRUE)