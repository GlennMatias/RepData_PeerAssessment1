#load library

library("dplyr")
library("ggplot2")
library("lattice")

# Extract dataset
unzip(zipfile = "./repdata_data_activity.zip")
# Load the dataset

# Loading and preprocessing the data
#-------------------
# 1. Load the data (i.e. \color{red}{\verb|read.csv()|}read.csv())
#

dataset = read.table("activity.csv", sep = ",", skip=1)
# Perform occular inspection on the dataset if it has been imported correctly
# View(dataset)
#Set dataset column names
colnames(dataset) = c("steps","date","interval")

#-----------------
# 2. Process/transform the data (if necessary) into a format suitable for your analysis
#

datasetGroupedByDaySums = dataset %>% group_by(date) %>% summarise(Sum = sum(steps, na.rm=FALSE))
#Plot the histogram
hist(datasetGroupedByDaySums$Sum, xlab="Steps per day", ylab="Number of Days", main="Total steps per day")

#---------------------------
# 3. Calculate and report the mean and median of the total number of steps taken per day
#

meanOfStepsPerDay <- mean(datasetGroupedByDaySums$Sum, na.rm = TRUE)
medianOfStepsPerDay <- median(datasetGroupedByDaySums$Sum, na.rm = TRUE)

View(meanOfStepsPerDay)
View(medianOfStepsPerDay)

#-------------------------
#What is the average daily activity pattern?

#------------------------
#1. Make a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
datasetGroupedByIntervalMeans = dataset %>%  filter(!is.na(steps)) %>% group_by(interval) %>% summarise(Mean = mean(steps, na.rm=FALSE))
    
View(datasetGroupedByIntervalMeans)
ggplot(datasetGroupedByIntervalMeans, aes(x=interval, y=Mean)) +
    geom_line(color = "blue")
#------------------------
#2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
View(head(arrange(datasetGroupedByIntervalMeans,desc(datasetGroupedByIntervalMeans$Mean)),1))


# Imputing missing values
# Note that there are a number of days/intervals where there are missing values (coded as \color{red}{\verb|NA|}NA). The presence of missing days may introduce bias into some calculations or summaries of the data.
# ----------------
# 1. Calculate and report the total number of missing values 
# in the dataset (i.e. the total number of rows with NAs)
#
sum(is.na(dataset$steps))

# -----------------
# 2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
#
# I will use the average of the whole dataset so that the plugged in values wont heavily impact the whole dataset 

# -------------------------
# 3. Create a new dataset that is equal to the original dataset but with the missing data filled in.
#
filledInNADataset = dataset
filledInNADataset$steps[is.na(filledInNADataset$steps)] <- mean(filledInNADataset$steps, na.rm=TRUE)

# --------------------
# 4. Make a histogram of the total number of 
# steps taken each day and Calculate and report 
# the mean and median total number of steps taken 
# per day. Do these values differ from the estimates 
# from the first part of the assignment? 
# What is the impact of imputing missing data on
# the estimates of the total daily number of steps?
datasetGroupedByDaySums = dataset %>% group_by(date) %>% summarise(Sum = sum(steps, na.rm=FALSE))
hist(datasetGroupedByDaySums$Sum, xlab="Steps per day", ylab="Number of Days", main="Total steps per day")

mean(datasetGroupedByDaySums$Sum, na.rm = TRUE)
median(datasetGroupedByDaySums$Sum, na.rm = TRUE)

datasetWithoutNAGroupedByDaySums = filledInNADataset %>% group_by(date) %>% summarise(Sum = sum(steps, na.rm=FALSE))
hist(datasetWithoutNAGroupedByDaySums$Sum, xlab="Steps per day", ylab="Number of Days", main="Total steps per day")

mean(datasetWithoutNAGroupedByDaySums$Sum)
median(datasetWithoutNAGroupedByDaySums$Sum)


#
# Are there differences in activity patterns between weekdays and weekends?
# For this part the function may be of some help here. Use the dataset with the filled-in missing values for this part.

#----------------------
# 1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.
#

filledInNADataset$weekdayname = weekdays(as.POSIXlt(dataset$date))

filledInNADataset$weekdayname[filledInNADataset$weekdayname=="Saturday" | filledInNADataset$weekdayname=="Sunday"] = "weekend"
filledInNADataset$weekdayname[filledInNADataset$weekdayname!="weekend"] = "weekday"

View(test)

#-----------------------
# 2. Make a panel plot containing a time series plot (i.e. \color{red}{\verb|type = "l"|}type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.
#

plotNewDataframe <- aggregate(steps ~ weekdayname+interval, data=filledInNADataset, FUN=mean)

xyplot(steps ~ interval | factor(weekdayname),
       layout = c(1, 2),
       xlab="Interval",
       ylab="Number of steps",
       type="l",
       lty=1,
       data=plotNewDataframe)





