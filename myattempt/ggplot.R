#Creating plot to visualize the trends of borrowing time
setwd("/Users/apple/Documents/study/R/BikeSharingDemand/myattempt")
train <- read.csv("train.csv")
train$datetime <- strptime(train$datetime, format = "%Y-%m-%d%H:%M:%S")
train$weekday <- weekdays(train$datetime)
train$hour <- train$datetime$hour
#save average counts for each day/time in data frame
day_hour_counts$hour <- as.data.frame(aggregate(train[,"count"],list(train$weekday, train$hour)),mean)
?aggregate()
