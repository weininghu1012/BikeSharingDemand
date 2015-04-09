#Visualization part for evaluating the data

library(dplyr)

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
par(mar=c(3.9,3.9,2,2))
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
                       levels=c("Monday","Tuesday","Wednesday", "Thursday","Friday","Saturday","Sunday"))
  #the count also increases as time goes on
  #add year as a factor into our model
  df$year <- as.integer(substr(df$datetime,1,4))
  df$year <- as.factor(df$year)
  # the count also vary among different time
  # to be done next time
  return (df)
}



# Use the function featureEngineer to get the subset of data

subtrain = featureEngineer(train)
attach(subtrain)
head(subtrain)

# Turn the categorical variables into numeric
 subtrain$season = as.numeric(subtrain$season)
 subtrain$holiday = as.numeric(subtrain$holiday)
 subtrain$workingday = as.numeric(subtrain$workingday)
 subtrain$weekday = as.numeric(subtrain$weekday)


# ======================================================
# ======================================================
# Variable selection and creation of explanatory variable
#install.packages('leaps')
library(leaps)

cat("\nexhaustive\n")
out.exh=regsubsets(registered~atemp+weekday+hour+year+holiday+humidity+season+temp+weather+windspeed+workingday,data=subtrain,nbest=1,nvmax=40)
summ.exh=summary(out.exh)
names(summ.exh)
print(summ.exh$outmat)
c =print(summ.exh$cp)
a = print(summ.exh$adjr)
#Good model should with  small cp and large adjr, we choose the one with variables that we put in fit1.glm
minc = min(c)
cindex = which(c==minc)
maxa = max(a)
aindex = which(a==maxa)
# The best model according to CP has 32 variables while the best model according to adjr has 34 variables, we choose the one with 32 variables since the adr are pretty close
# The variables are: atemp+hour+I(year==2012)+humidity+season+weather+windspeed+workingday
# cp=33.16922, adjr=0.6840206
# model selection by exhaustive search


# fit1.glm: generalized linear model with 32 variables, response variable as registered
fit1.glm= glm(registered~atemp+hour+I(year==2012)+humidity+season+weather+windspeed+workingday,family="poisson")
#AIC: 348374

#fit1.lm: linear model with 32 variables, response variable as registered
fit1.lm = lm(registered~atemp+hour+I(year==2012)+humidity+season+weather+windspeed+workingday)
# Residual standard error: 84.53 on 10851 degrees of freedom
# Multiple R-squared:  0.6878,  Adjusted R-squared:  0.6868 
# F-statistic: 703.1 on 34 and 10851 DF,  p-value: < 2.2e-16
summ1.glm = summary(fit1.glm)
summ1.lm = summary(fit1.lm)


par("mar")
par(mar = c(4,4,1,1))
# Checking Cook's distance for abnormal data or observation
diagnose.glm = ls.diag(fit1.glm)
print(diagnose.glm$cook)
plot(diagnose.glm$cook)
print(diagnose.glm$dfits)
plot(diagnose.glm$dfits)


# Remove the data with large Cook's distance, we use the criteria to remove certain data that has
# Cook's distance >= (4/n), with n is the number of data

n = length(diagnose.glm$cook)
indexvector = c()

for (i in 1:n){
  
  if (!is.na(diagnose.glm$cook[i])){
    if (diagnose.glm$cook[i]>(4/n)){
    
    indexvector = c(indexvector,i) }
}
}
# The new subtrain is the one with abnorm data subtracted
subtrain = subtrain[-indexvector,]

# Testing the linear model fit1.lm by residual plot

names(summ1.lm)
pred1.lm = predict(fit1.lm, data = subtrain)  # predicted value from fit1.lm regression model
res1.lm = resid(fit1.lm, data = subtrain)     # residuals
sigma1.lm = summ1.lm$sigma

pred1.glm = predict(fit1.glm, data = subtrain)  # predicted value from fit1.lm regression model
res1.glm = resid(fit1.glm, data = subtrain)     # residuals
sigma1.glm = summ1.glm$sigma


# residual plots
par(mfrow = c(1,1))
#plot 1 for all variables
qqnorm(res1.lm,main = "normal QQ plot of residuals,registered as response variable")
plot(pred1.lm,res1.lm,xlab = "predicted value",ylab = "residuals");abline(h = 2*sigma1.lm);abline(h = -2*sigma1.lm)


#The variables we shall check: atemp+hour+I(year==2012)+humidity+season+weather+windspeed+workingday
# Residual plots for quantitative variables
# par(mfrow = c(2,2)) 

# plot for atemp
plot(atemp,res1.lm,xlab = "atemp",ylab = "residuals",main = "fit1.lm with registered as response variable");abline(h = 2*sigma1.lm);abline(h = -2*sigma1.lm)

# plot for hour
plot(hour,res1.lm,xlab = "predicted hour",ylab = "residuals",main = "fit1.lm with registered as response variable");abline(h = 2*sigma1.lm);abline(h = -2*sigma1.lm)

# plot for humidity
plot(humidity,res1.lm,xlab = "humidity",ylab = "residuals",main = "fit1.lm with registered as response variable");abline(h = 2*sigma1.lm);abline(h = -2*sigma1.lm)

#plot for season
plot(season,res1.lm,xlab = "season",ylab = "residuals",main = "fit1.lm with registered as response variable");abline(h = 2*sigma1.lm);abline(h = -2*sigma1.lm)

#plot for weather
plot(weather,res1.lm,xlab = "weather",ylab = "residuals",main = "fit1.lm with registered as response variable");abline(h = 2*sigma1.lm);abline(h = -2*sigma1.lm)

# plot for windspeed
plot(windspeed,res1.lm,xlab = "windspeed",ylab = "residuals",main = "fit1.lm with registered as response variable");abline(h = 2*sigma1.lm);abline(h = -2*sigma1.lm)

#plot for workingday
plot(workingday,res1.lm,xlab = "workingday",ylab = "residuals",main = "fit1.lm with registered as response variable");abline(h = 2*sigma1.lm);abline(h = -2*sigma1.lm)

#plot for holiday
plot(holiday,res1,xlab = "holiday",ylab = "residuals",main = "fit1 with registered as response variable");abline(h = 2*sigma1);abline(h = -2*sigma1)

# Adjust the linear model glm.lm and nonlinear model fit1.glm with residual plot,mainly time period
subtrain$numHour = as.numeric(subtrain$hour)
subtrain$hourcat6 = cut(subtrain$numHour,breaks = c(-Inf, 6, 10, 16, 20,Inf), labels = c(1:5))
out.exh=regsubsets(registered~atemp+weekday+hourcat6+year+holiday+humidity+season+temp+weather+windspeed+workingday,data=subtrain,nbest=1,nvmax=40)
summ.exh=summary(out.exh)
names(summ.exh)
print(summ.exh$outmat)
c = print(summ.exh$cp)
a = print(summ.exh$adjr)
minc = min(c)
maxa = max(a)
cindex = which(c == minc)
aindex = which(a == maxa)
# The minimum is with 11 variables
fit1.lm = lm(registered~atemp+hourcat6+I(year==2012)+humidity+season+I(weather==2)+I(weather==3)+workingday,data=subtrain)
# Residual standard error: 98.54 on 10552 degrees of freedom
# Multiple R-squared:  0.5775,  Adjusted R-squared:  0.577 
# F-statistic:  1311 on 11 and 10552 DF,  p-value: < 2.2e-16


fit1.glm = glm(registered~atemp+hourcat6+I(year==2012)+humidity+season+I(weather==2)+I(weather==3)+workingday,data=subtrain,family=poisson)
summ1.glm = summary(fit1.glm)  
#AIC: 477430

attach(subtrain)
#Make transformations to the variables according to residual plot
fit1.lm = lm(registered~I(atemp^2)+atemp+hourcat6+I(year==2012)+humidity+I(log(humidity+1))+season+I(weather==2)+I(weather==3)+workingday,data=subtrain)

# Residual standard error: 98.39 on 10550 degrees of freedom
# Multiple R-squared:  0.5788,  Adjusted R-squared:  0.5783 
# F-statistic:  1115 on 13 and 10550 DF,  p-value: < 2.2e-16

# We can see that the adjusted-r does not improve much(0.577,0.5783)
# We plan to transform the response variable

fit1.lm = lm(I(log(registered+1))~I(atemp^2)+atemp+hourcat6+I(year==2012)+humidity+I(log(humidity+1))+season+I(weather==2)+I(weather==3)+workingday,data=subtrain)
# Residual standard error: 0.6861 on 10550 degrees of freedom
# Multiple R-squared:  0.7644,  Adjusted R-squared:  0.7641 
# F-statistic:  2633 on 13 and 10550 DF,  p-value: < 2.2e-16

fit1.lm = lm(I(log(registered+1))~atemp+hourcat6+I(year==2012)+humidity+season+weather+windspeed+workingday,data=subtrain)
# Residual standard error: 0.6934 on 10552 degrees of freedom
# Multiple R-squared:  0.7592,  Adjusted R-squared:  0.759 
# F-statistic:  3025 on 11 and 10552 DF,  p-value: < 2.2e-16

fit1.lm = lm(I(log(registered+1))~atemp+hour+I(year==2012)+humidity+season+weather+windspeed+workingday,data=subtrain)
# Residual standard error: 0.5482 on 10531 degrees of freedom
# Multiple R-squared:  0.8498,  Adjusted R-squared:  0.8493 
# F-statistic:  1862 on 32 and 10531 DF,  p-value: < 2.2e-16

pred1.lm = predict(fit1.lm, data = subtrain)  # predicted value from fit1.lm regression model
res1.lm = resid(fit1.lm, data = subtrain)     # residuals
sigma1.lm = summ1.lm$sigma
summ1.lm = summary(fit1.lm)


# residual plots
par(mfrow = c(1,1))
#plot 1 for all variables
qqnorm(res1.lm,main = "normal QQ plot of residuals,registered as response variable")
plot(pred1.lm,res1.lm,xlab = "predicted value",ylab = "residuals");abline(h = 2*sigma1.lm);abline(h = -2*sigma1.lm)


#The variables we shall check: atemp+hour+I(year==2012)+humidity+season+weather+windspeed+workingday

# plot for atemp
plot(atemp,res1.lm,xlab = "atemp",ylab = "residuals",main = "fit1.lm with registered as response variable");abline(h = 2*sigma1.lm);abline(h = -2*sigma1.lm)

# plot for hour
plot(hour,res1.lm,xlab = "predicted hour",ylab = "residuals",main = "fit1.lm with registered as response variable");abline(h = 2*sigma1.lm);abline(h = -2*sigma1.lm)

# plot for humidity
plot(humidity,res1.lm,xlab = "humidity",ylab = "residuals",main = "fit1.lm with registered as response variable");abline(h = 2*sigma1.lm);abline(h = -2*sigma1.lm)

#plot for season
plot(season,res1.lm,xlab = "season",ylab = "residuals",main = "fit1.lm with registered as response variable");abline(h = 2*sigma1.lm);abline(h = -2*sigma1.lm)

#plot for weather
plot(weather,res1.lm,xlab = "weather",ylab = "residuals",main = "fit1.lm with registered as response variable");abline(h = 2*sigma1.lm);abline(h = -2*sigma1.lm)

# plot for windspeed
plot(windspeed,res1.lm,xlab = "windspeed",ylab = "residuals",main = "fit1.lm with registered as response variable");abline(h = 2*sigma1.lm);abline(h = -2*sigma1.lm)

#plot for workingday
plot(workingday,res1.lm,xlab = "workingday",ylab = "residuals",main = "fit1.lm with registered as response variable");abline(h = 2*sigma1.lm);abline(h = -2*sigma1.lm)





# FIT2:
# variable selection and creation of explanatory variable

# The process for model selection
cat("\nexhaustive\n")
out.casual.exh=regsubsets(casual~atemp+weekday+hour+year+holiday+humidity+season+temp+weather+windspeed+workingday,data=subtrain,nbest=1)
summ.casual.exh=summary(out.casual.exh)
names(summ.casual.exh)
print(summ.casual.exh$outmat)
print(summ.casual.exh$cp)
print(summ.casual.exh$adjr)

cat("\nbackward\n")
out.casual.back=regsubsets(casual~atemp+weekday+hour+year+holiday+humidity+season+temp+weather+windspeed+workingday,data=subtrain,method="backward")
summ.casual.back=summary(out.casual.back)
print(summ.casual.back$outmat)
cat("Cp and adjr\n")
print(summ.casual.back$cp)
print(summ.casual.back$adjr)

cat("\nforward\n")
out.casual.forw=regsubsets(casual~atemp+weekday+hour+year+holiday+humidity+season+temp+weather+windspeed+workingday,data=subtrain,method="forward")
summ.casual.forw=summary(out.casual.forw)
print(summ.casual.forw$outmat)
cat("Cp and adjr\n")
print(summ.casual.forw$cp)
print(summ.casual.forw$adjr)

cat("\nseqrep\n")
out.casual.sequ=regsubsets(casual~atemp+weekday+hour+year+holiday+humidity+season+temp+weather+windspeed+workingday,data=subtrain,method="seqrep")
summ.casual.sequ=summary(out.casual.sequ)
print(summ.casual.sequ$outmat)
cat("Cp and adjr\n")
print(summ..casual.sequ$cp)
print(summ.casual.sequ$adjr)

# generally want small cp and large adjr
# model selection by exhaustive search
fit2.exh = lm(casual~atemp+I(hour==13)+I(hour==14)+I(hour==15)+I(hour==16)+I(hour==17)+humidity+workingday
# cp=2373.350, adjr=0.4859510
              
# model selection by backward search
fit2.backw= lm(casual~I(hour==13)+I(hour==14)+I(hour==15)+I(hour==16)+I(hour==17)+humidity+temp+workingday)
#cp = 2385.866, adjr=0.4854651
              
# model selection by forward search
fit2.forw= lm(casual~I(hour==13)+I(hour==14)+I(hour==15)+I(hour==16)+I(hour==17)+humidity+temp+workingday)
# cp = 2385.866, adjr=0.4854651
              
# model selection by sequential replacement (same result as exhaustive search)
              
summary(fit2.exh)
summary(fit2.backw)
summary(fit2.forw)
              
              ## in conclusion, the exhaustive search is the best fit since it has the smallest cp and largest adjr.
              
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

#plot  for all variables
qqnorm(res2,main = "normal QQ plot of residuals,causal as response variable")
plot(pred2,res2,xlab = "predicted value",ylab = "residuals",main = "fit2 with casual as response variable");abline(h = 2*sigma2);abline(h = -2*sigma2)


# Residual plots for quantitative variables
par(mfrow = c(1,1)) 

# plot4 for windspeed
plot(windspeed,res2,xlab = "windspeed",ylab = "residuals",main = "fit2 with casual as response variable");abline(h = 2*sigma2);abline(h = -2*sigma2)

# plot5 for humidity
plot(humidity,res2,xlab = "humidity",ylab = "residuals",main = "fit2 with casual as response variable");abline(h = 2*sigma2);abline(h = -2*sigma2)

#plot6 for atemp
plot(atemp,res2,xlab = "atemp",ylab = "residuals",main = "fit2 with casual as response variable");abline(h = 2*sigma2);abline(h = -2*sigma2)

#plot7 for temp
plot(temp,res2,xlab = "temp",ylab = "residuals",main = "fit2 with casual as response variable");abline(h = 2*sigma2);abline(h = -2*sigma2)


#Residual plots for categorical variables
par = mfrow = (c(2,3))

# plot for hour
plot(hour,res2,xlab = "predicted hour",ylab = "residuals",main = "fit2 with casual as response variable");abline(h = 2*sigma2);abline(h = -2*sigma2)

# plot for weekday
plot(weekday,res2,xlab = "weekday",ylab = "residuals",main = "fit2 with casual as response variable");abline(h = 2*sigma2);abline(h = -2*sigma2)

#plot for weather
plot(weather,res2,xlab = "weather",ylab = "residuals",main = "fit2 with casual as response variable");abline(h = 2*sigma2);abline(h = -2*sigma2)

#plot for workingday
plot(workingday,res2,xlab = "workingday",ylab = "residuals",main = "fit2 with casual as response variable");abline(h = 2*sigma2);abline(h = -2*sigma2)

#plot for holiday
plot(holiday,res2,xlab = "holiday",ylab = "residuals",main = "fit2 with casual as response variable");abline(h = 2*sigma2);abline(h = -2*sigma2)

#plot for holiday
plot(season,res2,xlab = "season",ylab = "residuals",main = "fit2 with casual as response variable");abline(h = 2*sigma2);abline(h = -2*sigma2)




# Fit4 with improvement on the explanatory variables
subtrain$squarewindspeed = I((subtrain$windspeed)^2)
subtrain$logtemp = I(log(subtrain$temp))
subtrain$sqrttemp = sqrt(subtrain$temp)
subtrain$sqrtemp = (subtrain$temp)*(subtrain$temp)
attach(subtrain)

fit4 = lm(I(casual^2)~season+holiday+workingday+weather+temp+atemp+humidity+windspeed+I(windspeed^2)+weekday+hour+year,data = subtrain)
summ4 = summary(fit4)
pred4 = predict(fit4)
res4 = summ4$residuals
sigma4 = summ4$sigma

#
plot(pred4,res4,xlab = "predicted value",ylab = "residuals",main = "fit4 with casual as response variable");abline(h = 2*sigma4);abline(h = -2*sigma4)
#plot7 for temp
plot(windspeed,res4,xlab = "windspeed",ylab = "residuals",main = "fit4 with casual as response variable");abline(h = 2*sigma4);abline(h = -2*sigma4)
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
