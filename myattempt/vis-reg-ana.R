#Visualization part for evaluating the data
install.packages('dplyr')
library(dplyr)
library(ggplot2)
setwd("/Users/apple/Documents/study/R/BikeSharingDemand/myattempt")
train = read.csv("train.csv")
test = read.csv("test.csv")
attach(train)
# http://www.cookbook-r.com/Manipulating_data/Summarizing_data/
# We have numerical variables: temp, atemp, humidity and windspeed

train$datetime = strptime(train$datetime, format = "%Y-%m-%d%H:%M:%S")
train$weekday = weekdays(train$datetime)
train$hour = train$datetime$hour
# We have categorical variables: season(1/2/3/4), holiday(0/1), workingday(0/1),weather(1/2/3) and now we add hour and weekday
season_count = aggregate(train["count"], by=train[c("season")], FUN=sum)
holiday_count = aggregate(train["count"], by=train[c("holiday")], FUN=sum)
workingday_count = aggregate(train["count"], by=train[c("workingday")], FUN=sum)
weather_count = aggregate(train["count"], by=train[c("weather")], FUN=sum)
hour_count = aggregate(train["count"], by=train[c("hour")], FUN=sum)
weekday_count = aggregate(train["count"], by = train[c("weekday")], FUN = sum)


par("mar")
par(mar=c(0.5,1,1,1))
par(mfrow=c(3,2))
s_c_plot =barplot(season_count$count,names.arg = season_count$season,main = "count VS season",xlab = "season", ylab = "count")
h_c_plot =barplot(holiday_count$count,names.arg = holiday_count$holiday,main = "count VS holiday",xlab = "holiday", ylab = "count")
wk_c_plot = barplot(weekday_count$count,names.arg = weekday_count$weekday,main = "count VS weekday",xlab = "weekday", ylab = "count")
we_c_plot = barplot(weather_count$count,names.arg = weather_count$weather,main = "count VS weather",xlab = "weather", ylab = "count")
h_c_plot = barplot(hour_count$count,names.arg = hour_count$hour,main = "count VS hour",xlab = "hour", ylab = "count")
work_c_plot = barplot(workingday_count$count,names.arg = workingday_count$workingday,main = "count VS workingday",xlab = "workingday", ylab = "count")

# part one for model selection
# first try some parameters

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
                       =c("Monday",
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





subtrain = featureEngineer(train)
attach(subtrain)
head(subtrain)

# Turn the categorical variables into numeric
 subtrain$season = as.numeric(subtrain$season)
 subtrain$holiday = as.numeric(subtrain$holiday)
 subtrain$workingday = as.numeric(subtrain$workingday)
 subtrain$weekday = as.numeric(subtrain$weekday)

#Fit1: linear model,with response variable as registered
fit1 = lm(registered~season+holiday+workingday+weather+temp+atemp+humidity+windspeed+weekday+hour+year,data = subtrain)
summ1 = summary(fit1)

# Checking Cook's distance for abnormal data or observation
diagnose = ls.diag(fit1)
print(diagnose$cook)
plot(diagnose$cook)
print(diagnose$dfits)
plot(diagnose$dfits)

# Testing the linear model by residual plot
#  season holiday workingday weather temp  atemp humidity windspeed (casual registered count) weekday hour
names(summ1)
pred1 = predict(fit1)  # predicted value from fit1 regression model
res1 = resid(fit1)     # residuals
sigma1 = summ1$sigma
# residual plots

#plot 1 for all variables
qqnorm(res1,main = "normal QQ plot of residuals,registered as response variable")
plot(pred1,res1,xlab = "predicted value",ylab = "residuals")
abline(h = 2*sigma1);abline(h = -2*sigma1)

# plot2 for hour
plot(hour,res1,xlab = "predicted hour",ylab = "residuals",main = "fit1 with registered as response variable");abline(h = 2*sigma1);abline(h = -2*sigma1)

# plot3 for weekday
plot(weekday,res1,xlab = "weekday",ylab = "residuals",main = "fit1 with registered as response variable");abline(h = 2*sigma1);abline(h = -2*sigma1)

# plot4 for windspeed
plot(windspeed,res1,xlab = "windspeed",ylab = "residuals",main = "fit1 with registered as response variable");abline(h = 2*sigma1);abline(h = -2*sigma1)

# plot5 for humidity
plot(humidity,res1,xlab = "humidity",ylab = "residuals",main = "fit1 with registered as response variable");abline(h = 2*sigma1);abline(h = -2*sigma1)

#plot6 for atemp
plot(atemp,res1,xlab = "atemp",ylab = "residuals",main = "fit1 with registered as response variable");abline(h = 2*sigma1);abline(h = -2*sigma1)

#plot7 for temp
plot(temp,res1,xlab = "temp",ylab = "residuals",main = "fit1 with registered as response variable");abline(h = 2*sigma1);abline(h = -2*sigma1)

#plot8 for weather
plot(weather,res1,xlab = "weather",ylab = "residuals",main = "fit1 with registered as response variable");abline(h = 2*sigma1);abline(h = -2*sigma1)

#plot9 for workingday
plot(workingday,res1,xlab = "workingday",ylab = "residuals",main = "fit1 with registered as response variable");abline(h = 2*sigma1);abline(h = -2*sigma1)

#plot10 for holiday
plot(holiday,res1,xlab = "holiday",ylab = "residuals",main = "fit1 with registered as response variable");abline(h = 2*sigma1);abline(h = -2*sigma1)

#plot11 for holiday
plot(season,res1,xlab = "season",ylab = "residuals",main = "fit1 with registered as response variable");abline(h = 2*sigma1);abline(h = -2*sigma1)





#Fit2: linear model,with response variable as casual
fit2 = lm(casual~season+holiday+workingday+weather+temp+atemp+humidity+windspeed+weekday+hour+year,data = subtrain)
summ2 = summary(fit2)

# Checking Cook's distance for abnormal data or observation
diagnose = ls.diag(fit2)
print(diagnose$cook)
plot(diagnose$cook)
print(diagnose$dfits)
plot(diagnose$dfits)

# Testing the linear model by residual plot
#  season holiday workingday weather temp  atemp humidity windspeed (casual registered count) weekday hour
names(summ2)
pred2 = predict(fit2)  # predicted value from fit1 regression model
res2 = resid(fit2)     # residuals
sigma2 = summ2$sigma
# residual plots

#plot 1 for all variables
qqnorm(res2,main = "normal QQ plot of residuals,causal as response variable")
plot(pred2,res2,xlab = "predicted value",ylab = "residuals",main = "fit2 with casual as response variable");abline(h = 2*sigma2);abline(h = -2*sigma2)

# plot2 for hour
plot(hour,res2,xlab = "predicted hour",ylab = "residuals",main = "fit2 with casual as response variable");abline(h = 2*sigma2);abline(h = -2*sigma2)

# plot3 for weekday
plot(weekday,res2,xlab = "weekday",ylab = "residuals",main = "fit2 with casual as response variable");abline(h = 2*sigma2);abline(h = -2*sigma2)

# plot4 for windspeed
plot(windspeed,res2,xlab = "windspeed",ylab = "residuals",main = "fit2 with casual as response variable");abline(h = 2*sigma2);abline(h = -2*sigma2)

# plot5 for humidity
plot(humidity,res2,xlab = "humidity",ylab = "residuals",main = "fit2 with casual as response variable");abline(h = 2*sigma2);abline(h = -2*sigma2)

#plot6 for atemp
plot(atemp,res2,xlab = "atemp",ylab = "residuals",main = "fit2 with casual as response variable");abline(h = 2*sigma2);abline(h = -2*sigma2)

#plot7 for temp
plot(temp,res2,xlab = "temp",ylab = "residuals",main = "fit2 with casual as response variable");abline(h = 2*sigma2);abline(h = -2*sigma2)

#plot8 for weather
plot(weather,res2,xlab = "weather",ylab = "residuals",main = "fit2 with casual as response variable");abline(h = 2*sigma2);abline(h = -2*sigma2)

#plot9 for workingday
plot(workingday,res2,xlab = "workingday",ylab = "residuals",main = "fit2 with casual as response variable");abline(h = 2*sigma2);abline(h = -2*sigma2)

#plot10 for holiday
plot(holiday,res2,xlab = "holiday",ylab = "residuals",main = "fit2 with casual as response variable");abline(h = 2*sigma2);abline(h = -2*sigma2)

#plot11 for holiday
plot(season,res2,xlab = "season",ylab = "residuals",main = "fit2 with casual as response variable");abline(h = 2*sigma2);abline(h = -2*sigma2)


# Fit4 with improvement on the explanatory variables
subtrain$logtemp = log(subtrain$temp)
subtrain$sqrttemp = sqrt(subtrain$temp)
subtrain$sqrtemp = (subtrain$temp)*(subtrain$temp)

fit4 = lm(casual~season+holiday+workingday+weather+sqrtemp+atemp+humidity+windspeed+weekday+hour+year,data = subtrain)
summ4 = summary(fit4)
res4 = summ4$residuals
sigma4 = summ4$sigma

#plot7 for temp
plot(subtrain$temp,res4,xlab = "temp",ylab = "residuals",main = "fit4 with casual as response variable");abline(h = 2*sigma4);abline(h = -2*sigma4)
summ3 = summary(fit3)

detach(train)

# 
subtest = featureEngineer(test)
head(subtest)



# Turn the categorical variables into numeric
subtest$season = as.numeric(subtest$season)
subtest$holiday = as.numeric(subtest$holiday)
subtest$workingday = as.numeric(subtest$workingday)
subtest$weekday = as.numeric(subtest$weekday)


test$registered = predict(fit1,newdata = subtest)
