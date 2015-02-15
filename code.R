#Import the packages

library("ggplot2");
library(plyr);
# Loading and preprocessing the data

rawdata<-read.csv("activity.csv",header=T)

summary1<-ddply(rawdata,c("date"),summarise
               ,totalSteps=sum(steps,na.rm=TRUE)
)
# What is mean total number of steps taken per day?


#Make a histogram of the total number of steps taken each day
hist(summary1$totalSteps)

#Calculate and report the mean and median total number of steps taken per day
mean(summary1$totalSteps)
median(summary1$totalSteps)

# 2 What is the average daily activity pattern?

# Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

summary2<-ddply(rawdata,c("interval"),summarise
                ,avg=mean(steps,na.rm=TRUE))

ggplot(summary2,aes(x=interval,y=avg)) + geom_line()

# Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

summary2[which.max(summary2[,2]),]

# 3 Imputing missing values

#Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

count(is.na(rawdata$steps))

#Create a new dataset that is equal to the original dataset but with the missing data filled in.

summary3<-rawdata

#the fixed data is the average for that 5 minute interval across all days
summary3$fixed<-ifelse(is.na(rawdata$steps),summary2$avg,rawdata$steps)

#Make a histogram of the total number of steps taken each day
summary3_tot<-ddply(summary3,c("date"),summarise
                ,totalSteps=sum(fixed)
)

hist(summary3_tot$totalSteps)

#Calculate and report the mean and median total number of steps taken per day
mean(summary3_tot$totalSteps)
median(summary3_tot$totalSteps)


#Are there differences in activity patterns between weekdays and weekends?


