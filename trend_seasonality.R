# Example code to demonstrate de-trending and de-seasoning of a time series
# followed by a simple prediction model
#
# Date: February 11, 2015
#
# Author: Naumaan Nayyar

# Clear workspace
rm(list=ls())

# Import required libraries
library(TTR)
library(zoo)

# Import time series data into workspace
data(AirPassengers)

# Plot time series of data
pdf("plots/time_series.pdf")
plot(AirPassengers, xlab="Time", ylab="Passengers (1000s)", main="Passengers between 1949 and 1960")
dev.off()

# Plot sample autocorrelation function of data
pdf("plots/acf.pdf")
acf(AirPassengers, lag.max=75, xlab="Lag (years)", ylab="Sample autocorrelation function", main="Autocorrelation function")
dev.off()

# Plot box plots to study monthly and yearly deviation
matrixAirPassengers <- matrix(AirPassengers, nrow=12)
pdf("plots/box_plots_monthly.pdf")
boxplot(t(matrixAirPassengers), xlab="Months", ylab="Passengers (1000s)", main="Box plots by month")  # months
dev.off()
pdf("plots/box_plots_yearly.pdf")
boxplot(matrixAirPassengers, xlab="Years", ylab="Passengers (1000s)", main="Box plots by year")  # years
dev.off()

# Remove trend and seasonality of time series
trendAirPassengers <- rollapply(AirPassengers, 13, function(x) c(0.5, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0.5) %*% x / 12)  # determine moving average trend
pdf("plots/trend_moving_average.pdf")
plot(trendAirPassengers, ylab="12 Month Moving Average", main="12 Month Moving Average")
dev.off()
detrendedAirPassengers <- AirPassengers - trendAirPassengers  # de-trended series
season <- NULL
for (i in 1:12) {
	season[i] <- mean(subset(detrendedAirPassengers,cycle(detrendedAirPassengers)==i))
}
season <- season - mean(season)
season <- rep(season, 12)
seasonAirPassengers <- ts(season, start = 1949, freq = 12)  # determine seasonality in time series
statAirPassengers <- detrendedAirPassengers - seasonAirPassengers  # de-trended and de-seasoned series
pdf("plots/time_series_without_trend_seasonality.pdf")
plot(statAirPassengers, ylab="Passengers (1000s)", main="De-trended and de-seasoned series")
dev.off()

# Re-do analysis with log transformation applied to data
AirPassengersActual <- AirPassengers
AirPassengers <- log(AirPassengers)
pdf("plots/log_time_series.pdf")
plot(AirPassengers, xlab="Time", ylab="logarithm of Passengers in 1000s", main="Passengers between 1949 and 1960")
dev.off()
pdf("plots/log_acf.pdf")
acf(AirPassengers, lag.max=75, xlab="Lag (years)", ylab="Sample autocorrelation function", main="Autocorrelation function")
dev.off()

matrixAirPassengers <- matrix(AirPassengers, nrow=12)
pdf("plots/log_box_plots_monthly.pdf")
boxplot(t(matrixAirPassengers), xlab="Months", ylab="logarithm of Passengers in 1000s", main="Box plots by month") #months
dev.off()
pdf("plots/log_box_plots_yearly.pdf")
boxplot(matrixAirPassengers, xlab="Years", ylab="logarithm of Passengers in 1000s", main="Box plots by year")    #years
dev.off()

trendAirPassengers <- rollapply(AirPassengers, 13, function(x) c(0.5, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0.5) %*% x / 12)
pdf("plots/log_trend_moving_average.pdf")
plot(trendAirPassengers, ylab="logarithm of Passengers in 1000s", main="12 Month Moving Average")
dev.off()
detrendedAirPassengers <- AirPassengers - trendAirPassengers
season <- NULL
for (i in 1:12) {
	season[i] <- mean(subset(detrendedAirPassengers,cycle(detrendedAirPassengers)==i))
}
season <- season - mean(season)
season <- rep(season, 12)
seasonAirPassengers <- ts(season, start = 1949, freq = 12)
statAirPassengers <- detrendedAirPassengers - seasonAirPassengers
pdf("plots/log_time_series_without_trend_seasonality.pdf")
plot(statAirPassengers, ylab="logarithm of Passengers in 1000s", main="De-seasoned and de-trended series")
dev.off()

# Build a regression model for the log-transformed time series with a second-degree polynomial and seasonality
# modeled by sin and cos.
vecAirPassengers <- as.numeric(AirPassengers)
vecTimeAirPassengers <- as.numeric(time(AirPassengers))
meanTimeAirPassengers <- mean(vecTimeAirPassengers)
sdTimeAirPassengers <- sd(vecTimeAirPassengers)
tauT <- (vecTimeAirPassengers - meanTimeAirPassengers)/sdTimeAirPassengers
mydata <- data.frame(vecTimeAirPassengers, tauT)
finalmodel <- lm(vecAirPassengers ~ tauT + I(tauT^2) + sin(2*pi*vecTimeAirPassengers) + cos(2*pi*vecTimeAirPassengers) + sin(2*pi*2*vecTimeAirPassengers) + cos(2*pi*2*vecTimeAirPassengers) +sin(2*pi*3*vecTimeAirPassengers) + cos(2*pi*3*vecTimeAirPassengers) +sin(2*pi*4*vecTimeAirPassengers) + cos(2*pi*4*vecTimeAirPassengers) +sin(2*pi*5*vecTimeAirPassengers) + cos(2*pi*5*vecTimeAirPassengers) +sin(2*pi*6*vecTimeAirPassengers) + cos(2*pi*6*vecTimeAirPassengers))
finalmodelTS <- ts(fitted(finalmodel), start = 1949, freq = 12)
pdf("plots/model_accuracy.pdf")
plot(AirPassengersActual,col='red', ylab="Passengers (1000s)", main="Actual passengers and Fitted model")
lines(exp(finalmodelTS), col='blue')
legend(x="topleft",legend=c("Actual data", "Fitted model"), lty=c(1,1),col=c("red","blue"),lwd=c(2.5,2.5))
dev.off()

# Predict time series for a future time period.
vecNewTime <- seq(1961, 1970, by=1/12)
vecTimeAirPassengers <- vecNewTime
newTauT <- (vecTimeAirPassengers - meanTimeAirPassengers)/sdTimeAirPassengers
tauT <- newTauT
mynewdata <- data.frame(vecTimeAirPassengers,tauT)
vecPredicted <- predict(finalmodel,newdata=mynewdata)
predictedTS <- ts(vecPredicted, start = 1961, freq = 12)
pdf("plots/model_accuracy_prediction.pdf")
plot(AirPassengersActual, col='red', xlim=c(1948,1972), ylim=c(0,1000), main="Actual and predicted passengers")
lines(exp(predictedTS), col='blue')
legend(x="topleft", legend=c("Actual data", "Predicted data"), lty=c(1,1), col=c("red","blue"), lwd=c(2.5,2.5))
dev.off()

# Restore dataset
AirPassengers <- AirPassengersActual
