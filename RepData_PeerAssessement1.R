
# Load and process

DataSet <- read.csv("activity.csv")

head(DataSet)

# Calculate the mean

# Subset, removing NAs
NoNASet <- DataSet[ which (is.na(DataSet$steps) == FALSE), ]

# Get dates and intervals from the set for use later
Dates <- as.Date(unique(NoNASet$date))
Intervals <- unique(NoNASet$interval) # use DataSet to avoid non-missing NA err

# Setup two frames for use to show by date and by interval
StepsByDayFrame <- data.frame(stringsAsFactors = FALSE, DATE = Dates)
StepsByIntervalFrame <- data.frame(stringsAsFactors = FALSE, INTERVAL = Intervals)

# Function to get steps and lapply to get a list
GetStepsByDate <- function(n) {
    sum(NoNASet[ which (as.Date(NoNASet$date) == n), 1])
}
GetStepsByInterval <- function(i) {
    sum(NoNASet [ which (NoNASet$interval == i), 1])
}

stepsvec <- sapply(StepsByDayFrame$DATE, GetStepsByDate)
intervalvec <- sapply(StepsByIntervalFrame$INTERVAL, GetStepsByInterval)

# check the frames for errors
StepsByDayFrame <- cbind(StepsByDayFrame, stepsvec, stringsAsFactors = FALSE)
head(StepsByDayFrame)

StepsByIntervalFrame <- cbind(StepsByIntervalFrame, intervalvec, 
                              stringsAsFactors = FALSE)
head(StepsByIntervalFrame)

# Plot the day and steps on a histogram
plot( x = StepsByDayFrame$DATE, y = StepsByDayFrame$stepsvec, type = "h",
      xlab = "Date", ylab = "Sum of Steps")

# print mean and median steps taken daily
MeanSteps <- mean(StepsByDayFrame$stepsvec)
MedianSteps <- median(StepsByDayFrame$stepsvec)
print(MeanSteps)
print(MedianSteps)

# plot interval activity
plot ( x = StepsByIntervalFrame$INTERVAL, y = StepsByIntervalFrame$intervalvec,
       type = "l",
       xlab = "Interval", ylab = "Sum of Steps")

# get and print max
MaxSteps <- max(StepsByIntervalFrame$intervalvec)
print(MaxSteps)

# create a new set of data from DataSet and replace NA values
NewSet <- DataSet

# create a dataframe to hold interval mean values
IntervalMeanFrame <- data.frame(stringsAsFactors = FALSE,
                                  INTERVAL = unique(NewSet$interval))

# calculate a mean for each interval
IntervalMean <- function(n) {
    mean(NewSet[ which(NewSet$interval == n),1], na.rm = TRUE)
}
intervalmeansvec <- sapply(IntervalMeanFrame$INTERVAL, IntervalMean)
IntervalMeanFrame <- cbind(IntervalMeanFrame, intervalmeansvec, 
                           stringsAsFactors = FALSE)

# replace all the NAs with the mean for that interval
for (BlankInterval in unique(NewSet[which(is.na(NewSet$steps) == TRUE),3])) {
    NewSet[which(is.na(NewSet$steps) == TRUE & NewSet$interval == BlankInterval),1] <-
        IntervalMeanFrame[which(IntervalMeanFrame$INTERVAL == BlankInterval),2]
}

# repeat plots and mean/median for comparison
NewStepsByDayFrame <- data.frame(stringsAsFactors = FALSE, DATE = as.Date(unique(NewSet$date)))
GetNewStepsByDate <- function(n) {
    sum(NewSet[ which (as.Date(NewSet$date) == n), 1])
}
newstepsvec <- sapply(NewStepsByDayFrame$DATE, GetNewStepsByDate)
NewStepsByDayFrame <- cbind(NewStepsByDayFrame, newstepsvec, stringsAsFactors = FALSE)

# Plot the day and steps on a histogram
plot( x = NewStepsByDayFrame$DATE, y = NewStepsByDayFrame$newstepsvec, type = "h",
      xlab = "Date", ylab = "Sum of Steps")

# print mean and median steps taken daily
NewMeanSteps <- mean(NewSet$steps)
NewMedianSteps <- median(NewSet$steps)
print(NewMeanSteps)
print(NewMedianSteps)

# check for weekend vs weekday activity in NewData
WeekendVsDay <- function(d) {
    v <- switch(weekdays(d), Saturday = "weekend", Sunday = "weekend", "weekday")
    return(v)
}
WeekendVsDayVec <- sapply(as.Date(NewSet$date), WeekendVsDay)
NewSet <- cbind(NewSet, weekday=WeekendVsDayVec)

WeekendNewSetData <- NewSet[ which (NewSet$weekday == "weekend"), ]
WeekdayNewSetData <- NewSet[ which (NewSet$weekday == "weekday"), ]

# get averages for each and put into a frame for plotting
sbiSet <- data.frame(stringsAsFactors = FALSE, 
                         INTERVAL = as.integer(Intervals))
WeekendIntervalMean <- function(n) {
    val <- mean(WeekendNewSetData[ which(WeekendNewSetData$interval == n),1], na.rm = TRUE)
    if (val < 0.5) { return(0) }
    else { return(round(val, digits = 0)) }
}
weekendVec <- sapply(sbiSet$INTERVAL, WeekendIntervalMean)
sbiSet <- cbind(sbiSet, WeekendMean=weekendVec, 
                           stringsAsFactors = FALSE)

WeekdayIntervalMean <- function(n) {
    val <- mean(WeekdayNewSetData[ which(WeekdayNewSetData$interval == n),1], na.rm = TRUE)
    if (val < 0.5) { return(0) }
    else { return(round(val, digits = 0)) }
}
weekdayVec <- sapply(sbiSet$INTERVAL, WeekdayIntervalMean)
sbiSet <- cbind(sbiSet, WeekdayMean=weekdayVec, 
                    stringsAsFactors = FALSE)

par(mfrow = c(2,1))
plot(sbiSet$INTERVAL, sbiSet$WeekdayMean, type="l",
     xlab = "Interval", ylab = "Weekday Mean")
plot(sbiSet$INTERVAL, sbiSet$WeekendMean, type="l",
     xlab = "Interval", ylab = "Weekend Mean")
