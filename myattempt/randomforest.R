# First attempt using random forest package
setwd("/Users/apple/Documents/study/R/BikeSharingDemand/myattempt")
train <- read.csv("train.csv")
test <- read.csv ("test.csv")
# First plot all the variables of a data set against each other
pairs(train)
plot(train$count)
# Create a function to perform same operations on the train and test data
featureEngineer <- function(df){

# Factorize the data
names <- c("season", "holiday", "workingday", "weather")
df[,names]<-lapply(df[,names],factor)
# Extract the day of the week
df$datetime <- as.character(df$datetime)
df$datetime <- strptime(df$datetime, format = "%Y-%m-%d%T", tz = "EST")
#parse the hour 
df$hour <- as.integer(substr(df$datetime,12,13))
df$hour <- as.factor(df$hour)
#get the weekday for each date using weekdays function
df$weekday <- as.factor(weekdays(df$datetime))
df$weekday <- factor(df$weekday,
                     levels
                     =
                       c("Monday",
                         "Tuesday",
                         "Wednesday",
                         "Thursday",
                         "Friday",
                         "Saturday",
                         "Sunday"))
#the count also increases as time goes on
#add year as a factor into our model
df$year <- as.integer(substr(df$datetime,1,4))
df$year <- as.factor(df$year)

# the count also vary among different time
# to be done next time
return (df)
}
train <- featureEngineer(train)
test <- featureEngineer(test)
#from frequency graph, we noticed that the count of people borrowing bikes would 
# varied among workday and weekends, create weekend variable
#train$weekend[(train$day == "Sunday")|(train$day == "Saturday")] <- "1"
#df$weekend[(df$day != "Sunday") & (df$day != "Saturday")] <- "0"
# convert to factor
#df$weekend <- as.factor(df$weekend)
#apply the function

#import necessary packages
install.packages("randomForest")
library('randomForest')

# variable
myNtree = 500
myMtry = 5
myImportance = TRUE
set.seed(415)

# check what variables matter the most
testTrain <- subset(train, select = c(datetime, count, registered))
testFit <- randomForest(casual~.,data = testTrain, nTree = myNtree, mtry = myMtry, importance = myImportance)

casualFit <- randomForest(casual ~ hour + year + humidity + temp + atemp + workingday + weekday, data=train, ntree=myNtree, mtry=myMtry, importance=myImportance)
test$casual <- predict(casualFit, test)
registeredFit <- randomForest(registered ~ hour + year + season + weather + workingday + humidity + weekday + atemp, data=train, ntree=myNtree, mtry=myMtry, importance=myImportance)
test$registered <- predict(registeredFit, test)

test$count <- round(test$casual + test$registered, 0)
plot(test$count)
submit <- data.frame(datetime = test$datetime, count = test$count)
write.csv(submit, file= "prediction_randomforest.csv", row.names = FALSE)

