# Ali Jadidzadeh
# 2021-Fall
# Financial Time Series
# Lecture 0 - Introduction to time series and R


rm(list=ls(all=TRUE))
ls()

# load forecast package 
library(lubridate)


# Set directory
# Check you have the right file path!!
filepath <- '/Volumes/MacintoshHD2/Google Drive/Calsses/UofT/Lectures/TimeSeries/Lectures/Lec0'
setwd(filepath)


# International air passenger bookings in the 
# United States for the period 1949–1960.

# Load data
data(AirPassengers)
mydata <- as.data.frame(AirPassengers)

names(mydata) <- "y"

# Verify that it is a time series (ts) object 
class(mydata$y)

# plot time series
layout(1)
plot(mydata$y,col="blue",ylab="Passengers (1000's)")

# Add date column
mydata$date <- seq(as.Date("1949-01-01"), as.Date("1960-12-01"), by="month")

# get log data
mydata$ly <- log(mydata$y)

# plot log data
plot(mydata$ly,col="blue",ylab="log(y)")

# generate time trend 
mydata$time <- seq(1,length(mydata$y))

# fit trend
model1 <- lm(ly~time, data = mydata)
summary(model1)

# plot trend
mydata$fit1 <- ts(model1$fit,start=c(1949,1),end=c(1960,12),frequency=12)
plot(mydata$ly,col="blue",ylab="log(y)")
lines(mydata$fit1,col=1,lwd=2)

# generate dummy variables
mydata$month <- month(mydata$date)
class(mydata$month)

mydata$month <- as.factor(mydata$month)

# fit trend + seasonal dummies
model2 <- lm(ly~month+time-1, data=mydata)
summary(model2)

# plot seasonal factors
plot(model2$coef[1:12],col="blue",type="o")

# plot trend + seasonal dummies
mydata$fit2 <- ts(model2$fit,start=c(1949,1),end=c(1960,12),frequency=12)
plot(mydata$ly,col="blue",ylab="log(y)")
lines(mydata$fit2,col=1,lwd=2)

# plot residuals
resid <- ts(model2$residuals,start=c(1949,1),end=c(1960,12),frequency=12)
plot(resid,col="blue",ylab="Residual")
abline(h=0)


# Iran Nominal GDP
mydata2 <- read.csv("Iran_GDP_Quarterly.csv", sep=",", header=T, na.strings="",stringsAsFactors=FALSE)
head(mydata2)

# Convert GDP to ts subject
str(mydata2)
class(mydata2$GDP)

mydata2$GDP <- ts(mydata2$GDP, start=c(2004,2),end=c(2021,1),frequency=4)

# plot gpd
plot(mydata2$GDP,col="blue",ylab="Iran Nominal GDP (Billion Rials)")

# Define lag value (gdp_{t-1})
mydata2$GDPLag1 <- c(NA,head(mydata2$GDP,-1))

model3 <- lm(GDP ~ GDPLag1, data=mydata2)
summary(model3)

