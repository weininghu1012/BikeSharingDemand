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
out.casual.exh=regsubsets(casual~atemp+weekday+hour+year+holiday+humidity+season+temp+weather+windspeed+workingday,data=subtrain,nbest=1,nvmax = 40)
summ.casual.exh=summary(out.casual.exh)
names(summ.casual.exh)
print(summ.casual.exh$outmat)
c = print(summ.casual.exh$cp)
a = print(summ.casual.exh$adjr)
minc = min(c)
maxa = max(a)
cindex = which(c == minc)
aindex = which(a == maxa)

# The best model is with 32 variables

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
print(summ.casual.sequ$cp)
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
              
#Fit2: linear model,with response variable as casual, 31 variables
summ.casual.exh$outmat[31,]
subtrain = subtrain[,-(hour == 1)]
subtrain = subtrain[,-(hour == 2)]
fit2.lm = lm(casual~atemp+weekday+hour+I(year == 2012)+holiday+humidity+season+temp+I(weather == 2)+ I(weather == 3)+windspeed+workingday,data = subtrain)
# Residual standard error: 32.2 on 10529 degrees of freedom
# Multiple R-squared:  0.5857,  Adjusted R-squared:  0.5843 
# F-statistic: 437.8 on 34 and 10529 DF,  p-value: < 2.2e-16

fit2.lm = lm(I(log(casual+1))~atemp+weekday+hour+I(year == 2012)+holiday+humidity+season+temp+I(weather == 2)+ I(weather == 3)+windspeed+workingday,data = subtrain)
# Residual standard error: 0.6434 on 10529 degrees of freedom
# Multiple R-squared:  0.8173,  Adjusted R-squared:  0.8167 
# F-statistic:  1386 on 34 and 10529 DF,  p-value: < 2.2e-16

fit2.lm = lm(I(log(casual+1))~atemp+weekday+hour+I(year == 2012)+holiday+humidity+season+temp+weather+windspeed+workingday,data = subtrain)
# Residual standard error: 0.628 on 10844 degrees of freedom
# Multiple R-squared:  0.8233,  Adjusted R-squared:  0.8226 
# F-statistic:  1232 on 41 and 10844 DF,  p-value: < 2.2e-16

# We could check with the adjr tha the best model is  simply with original explanatory variables


# Testing the linear model by residual plot
#  season holiday workingday weather temp  atemp humidity windspeed (casual registered count) weekday hour
summ2 = summary(fit2.lm)
names(summ2)
pred2 = predict(fit2.lm)  # predicted value from fit1 regression model
res2 = resid(fit2.lm)     # residuals
sigma2 = summ2$sigma
# residual plots
par(mfrow = c(1,1)) 
par(mar = c(3,3,1,1))
#plot  for all variables
#atemp+weekday+hour+I(year == 2012)+holiday+humidity+season+temp+weather+windspeed+workingday
qqnorm(res2,main = "normal QQ plot of residuals,causal as response variable")
plot(pred2,res2,xlab = "predicted value",ylab = "residuals",main = "fit2.lm with casual as response variable");abline(h = 2*sigma2.lm);abline(h = -2*sigma2.lm)

#plot for atemp
plot(atemp,res2,xlab = "atemp",ylab = "residuals",main = "fit2 with casual as response variable");abline(h = 2*sigma2);abline(h = -2*sigma2)

# plot for weekday
plot(weekday,res2,xlab = "weekday",ylab = "residuals",main = "fit2 with casual as response variable");abline(h = 2*sigma2);abline(h = -2*sigma2)

# plot for hour
plot(hour,res2,xlab = "predicted hour",ylab = "residuals",main = "fit2 with casual as response variable");abline(h = 2*sigma2);abline(h = -2*sigma2)

#plot for holiday
plot(holiday,res2,xlab = "holiday",ylab = "residuals",main = "fit2 with casual as response variable");abline(h = 2*sigma2);abline(h = -2*sigma2)

# plot for humidity
plot(humidity,res2,xlab = "humidity",ylab = "residuals",main = "fit2 with casual as response variable");abline(h = 2*sigma2);abline(h = -2*sigma2)

#plot for temp
plot(temp,res2,xlab = "temp",ylab = "residuals",main = "fit2 with casual as response variable");abline(h = 2*sigma2);abline(h = -2*sigma2)

#plot for weather
plot(weather,res2,xlab = "weather",ylab = "residuals",main = "fit2 with casual as response variable");abline(h = 2*sigma2);abline(h = -2*sigma2)

# plot for windspeed
plot(windspeed,res2,xlab = "windspeed",ylab = "residuals",main = "fit2 with casual as response variable");abline(h = 2*sigma2);abline(h = -2*sigma2)

#plot for workingday
plot(workingday,res2,xlab = "workingday",ylab = "residuals",main = "fit2 with casual as response variable");abline(h = 2*sigma2);abline(h = -2*sigma2)

train = read.csv("train.csv")
subtrain = featureEngineer(train)

#CROSS VALIDATION 
set.seed(456)
n = nrow(subtrain)
iperm=sample(n,n)
subtrain.train = subtrain[iperm[1:10000],]
subtrain.holdout = subtrain[iperm[10001:n],]

#Linear model with response variable logged
fitMostTrain = lm(I(log(registered+1))~atemp+hour+I(year==2012)+humidity+season+weather+windspeed+workingday, data=subtrain.train)
predMostHold = predict(fitMostTrain,new = subtrain.holdout)
rmseMostHold = sqrt(mean((log(subtrain.holdout$registered+1)-predMostHold)^2))
rmseMostHold
#[1] 0.6085023, quite small

# Linear model 
fitMostTrain = lm(registered~atemp+hour+I(year==2012)+humidity+season+I(weather==2)+I(weather==3)+workingday, data=subtrain.train)
predMostHold = predict(fitMostTrain,new = subtrain.holdout)
rmseMostHold = sqrt(mean((subtrain.holdout$registered-predMostHold)^2))
rmseMostHold
# [1] 83.41994
# LARGE!

#fitMostTrain=fit1.atemp.sqrt
#[1] 97.12997 #using fit in pink text
fitMostTrain.pois = glm(registered~atemp+hour+I(year==2012)+humidity+season+I(weather==2)+I(weather==3)+workingday, data=subtrain.train, family = poisson)
predMostHold.pois = predict(fitMostTrain.pois, new = subtrain.holdout)
rmseMostHold.pois = sqrt(mean((subtrain.holdout$registered - predMostHold.pois)^2))
rmseMostHold.pois
# [1] 220.1969
# LARGER!



fitMostTrain.atemp2 = lm(registered~atemp+I(atemp^2)+hour+I(year==2012)+humidity+season+I(weather==2)+I(weather==3)+workingday, data=subtrain.train)
predMostHold.atemp2 = predict(fitMostTrain.atemp2, new = subtrain.holdout)
rmseMostHold.atemp2 = sqrt(mean((subtrain.holdout$registered - predMostHold.atemp2)^2))
rmseMostHold.atemp2
# [1] 83.41499

fitMostTrain.pois.atemp2 = glm(registered~atemp+I(atemp^2)+hour+I(year==2012)+humidity+season+I(weather==2)+I(weather==3)+workingday, data=subtrain.train,family=poisson)
predMostHold.pois.atemp2 = predict(fitMostTrain.pois.atemp2, new = subtrain.holdout)
rmseMostHold.pois.atemp2 = sqrt(mean((subtrain.holdout$registered - predMostHold.pois.atemp2)^2))
rmseMostHold.pois.atemp2
# [1] 220.1953
#fit1.humidity=glm(registered~I(atemp^2)+atemp+hourcat6+I(year==2012)+I(log(humidity+1))#+humidity+season+I(weather==2)+I(weather==3)+workingday,data=subtrain,family=poisson)

fitMostTrain.humidity = lm(registered~I(atemp^2)+atemp+hour+I(year==2012)+I(log(humidity+1))+humidity+season+I(weather==2)+I(weather==3)+workingday,data=subtrain.train)
predMostHold.humidity = predict(fitMostTrain.humidity, new = subtrain.holdout)
rmseMostHold.humidity = sqrt(mean((subtrain.holdout$registered - predMostHold.humidity)^2))
rmseMostHold.humidity
# [1] 83.26914

fitMostTrain.pois.humidity = glm(registered~I(atemp^2)+atemp+hour+I(year==2012)+I(log(humidity+1))+humidity+season+I(weather==2)+I(weather==3)+workingday,data=subtrain.train,family=poisson)
predMostHold.pois.humidity = predict(fitMostTrain.pois.humidity, new = subtrain.holdout)
rmseMostHold.pois.humidity = sqrt(mean((subtrain.holdout$registered - predMostHold.pois.humidity)^2))
rmseMostHold.pois.humidity
# [1]220.1953

fitMostTrain.log = lm(I(log(registered+1))~atemp+hour+I(year==2012)+humidity+season+weather+windspeed+workingday, data=subtrain.train)
predMostHold.log = predict(fitMostTrain,new = subtrain.holdout)
predMostHold = exp(predMostHold.log)-1
rmseMostHold.log = sqrt(mean((subtrain.holdout$registered-predMostHold)^2))
rmseMostHold.log
# [1] 216.5318

#since variance of prediction in GLM varies based on covariates, we cannot directly compare a linear model with the poisson model

# just rmse cannot verify best fit

fit1.lm = lm(I(log(registered+1))~atemp+hour+I(year==2012)+humidity+season+weather+windspeed+workingday,data=subtrain)
fit2.lm = lm(I(log(casual+1))~atemp+weekday+hour+I(year == 2012)+holiday+humidity+season+temp+weather+windspeed+workingday,data = subtrain)
test = read.csv("test.csv")
test= featureEngineer(test)
head(test)
test$datetime = strptime(test$datetime, format = "%Y-%m-%d%H:%M:%S")
test$weekday = weekdays(test$datetime)
test$hour = test$datetime$hour
test$registered = predict(fit1.lm,test)

# Turn the categorical variables into numeric
test$season = as.numeric(test$season)
test$holiday = as.numeric(test$holiday)
test$workingday = as.numeric(test$workingday)
test$weekday = as.numeric(test$weekday)
test$hour = factor(test$hour)
test$registered = exp(predict(fit1.lm,new = test))-1

#test$casual = exp(predict(fit2.lm,new = test))-1

test$count = round(test$registered)+abs(rnorm(6493,mean = 7.5,sd=5))
submit = data.frame(datetime = test$datetime, count = test$registered)
write.csv(submit, file= "prediction_linear.csv", row.names = FALSE)



