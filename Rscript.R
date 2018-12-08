# Coursera - Data Science Specializaion
# Reproducible Research Course Project W02
# Script to load and process data

library('ggplot2')
library("dplyr")

# Import data
data <- read.table('./activity/activity.csv',header=T,sep=',')

# Calculate the total number of steps per day
byday <- aggregate(steps~date,data=data,FUN=sum)
hist(byday$steps,breaks=20)

# Mean and median value
mean(byday$steps)
median(byday$steps)

# Calculate the average number of steps per interval
by5minint <- aggregate(steps~interval,data=data,FUN=mean)

# Plot average time serie
with(by5minint,plot(interval,steps))

# Get the interval where the maximum number of steps is observed
maxsteps <- max(by5minint$steps, na.rm = FALSE)
by5minint$interval[which(by5minint$steps==maxsteps)]

# Number of NA in the data
sum(is.na(data$steps))

# Replace NA value per the average number of steps for each interval
data$steps_5minavg <- rep(by5minint$steps,61)
data$steps_woNA <- ifelse(is.na(data$steps),data$steps_5minavg,data$steps)

# Calculate the average number of steps per interval
byday_woNA <- aggregate(steps_woNA~date,data=data,FUN=sum)
hist(byday_woNA$steps_woNA,breaks=20)

# Mean and median value after removing the NA
mean(byday_woNA$steps_woNA)
median(byday_woNA$steps_woNA)

# Get days and discriminate between weekend and weekdays
data$day <- weekdays(as.Date(data$date))
data$week_ <- as.factor(ifelse(
                    data$day == 'Saturday' | data$day == 'Sunday',
                    'weekend',
                    'weekday'))

# Calculate the average number of steps per interval and for weekend/days
by5minint <- aggregate(steps~interval+week_,data=data,FUN=mean)

# Plot the data
p <- qplot(interval,steps,data=by5minint,color=week_)
p + geom_point(size = 3) + ggtitle('Average number of steps vs time for weekdays and weekend days')


