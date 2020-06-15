#install.packages("forecast") #-- do this only once 
#install.packages("fpp") #-- do this only once 
library(forecast)
library(fpp)

# load csv data for US and WW users
WWUsers<-read.csv(file.choose(), header=TRUE, sep=",")
USUsers<-read.csv(file.choose(), header=TRUE, sep=",")

#create Time Series for both
WWUsers_ts <- ts(WWUsers$People, start = c(2010,4), frequency=12) 
USUsers_ts <- ts(USUsers$People, start = c(2010,4), frequency=12) 

#plot of 2 time series
par(mfrow=c(1,2))
plot(WWUsers_ts)
plot(USUsers_ts)

#plot decompositions into error/noise, trend and seasonality
fit <- decompose(WWUsers_ts, type="additive") #decompose using "classical" method, additive form
plot(fit)

fitWW <- stl(WWUsers_ts, t.window=12, s.window="periodic") #decompose using STL (Season and trend using Loess)
plot(fitWW)

fitUS <- stl(USUsers_ts, t.window=12, s.window="periodic") #decompose using STL (Season and trend using Loess)
plot(fitUS)


# Create exponential smoothing models: 
WWUsers_MMZD <- ets(WWUsers_ts, model="MMZ", damped =TRUE)
WWUsers_MMZ <- ets(WWUsers_ts, model="MMZ", damped =FALSE)
WWUsers_AAZD <- ets(WWUsers_ts, model="AAZ", damped=TRUE)
WWUsers_AAZ <- ets(WWUsers_ts, model="AAZ", damped=FALSE)

USUsers_MMZD <- ets(USUsers_ts, model="MMZ", damped =TRUE)
USUsers_MMZ <- ets(WWUsers_ts, model="MMZ", damped = FALSE)
USUsers_AAZD <- ets(USUsers_ts, model="AAZ", damped = TRUE)
USUsers_AAZ <- ets(USUsers_ts, model="AAZ", damped =FALSE)


# Create their predictions for 115 months (10 years) into the future with quintile confidence intervals

WWUsers_MMZD_pred <- forecast(WWUsers_MMZD, h=115, level=c(0.8, 0.95))
WWUsers_MMZ_pred <- forecast(WWUsers_MMZ, h=115, level=c(0.8, 0.95))
WWUsers_AAZD_pred <- forecast(WWUsers_AAZD, h=115, level=c(0.8, 0.95))
WWUsers_AAZ_pred <- forecast(WWUsers_AAZ, h=115, level=c(0.8, 0.95))

USUsers_MMZD_pred <- forecast(USUsers_MMZD, h=115, level=c(0.8, 0.95))
USUsers_MMZ_pred <- forecast(USUsers_MMZ, h=115, level=c(0.8, 0.95))
USUsers_AAZD_pred <- forecast(USUsers_AAZD, h=115, level=c(0.8, 0.95))
USUsers_AAZ_pred <- forecast(USUsers_AAZ, h=115, level=c(0.8, 0.95))




# WW plotting
par(mfrow=c(4,1))
plot(WWUsers_MMZD_pred, xlab="Year", ylab="Predicted Users Number -MMZ - dam")
plot(WWUsers_MMZ_pred, xlab="Year", ylab="Predicted Users Number -MMZ - no dam")
plot(WWUsers_AAZD_pred, xlab="Year", ylab="Predicted Users Number -AAZ - dam")
plot(WWUsers_AAZ_pred, xlab="Year", ylab="Predicted Users Number -AAZ - no dam")


#US Plotting
par(mfrow=c(4,1)) 
plot(USUsers_MMZD_pred, xlab="Year", ylab="Predicted Users Number -MMZ - dam")
plot(USUsers_MMZ_pred, xlab="Year", ylab="Predicted Users Number -MMZ - no dam")
plot(USUsers_AAZ_pred, xlab="Year", ylab="Predicted Users Number -AAZ - dam")
plot(USUsers_AAZD_pred, xlab="Year", ylab="Predicted Users Number -AAZ - no dam")

WWUsers_MMZ
WWUsers_MMZD
WWUsers_AAZ
WWUsers_AAZD


USUsers_MMZ
USUsers_MMZD
USUsers_AAZ
USUsers_AAZD

### Comparing models -- Time series Cross Validation (Rolling Horizon Holdout)
f_AAZD  <- function(y, h) forecast(ets(y, model="AAZ", damped =TRUE), h = h)
errors_AAZD <- tsCV(WWUsers_ts, f_AAZD, h=1, window=NULL)
errors_USAAZD <- tsCV(USUsers_ts, f_AAZD, h=1, window=NULL)

f_AAZ  <- function(y, h) forecast(ets(y, model="AAZ", damped =FALSE), h = h)
errors_AAZ <- tsCV(WWUsers_ts, f_AAZ, h=1, window=NULL)
errors_USAAZ <- tsCV(USUsers_ts, f_AAZ, h=1, window=NULL)

f_MMZD  <- function(y, h) forecast(ets(y, model="MMZ", damped =TRUE), h = h)
errors_MMZD <- tsCV(WWUsers_ts, f_MMZD, h=1, window=NULL)
errors_USMMZD <- tsCV(USUsers_ts, f_MMZD, h=1, window=NULL)

f_MMZ  <- function(y, h) forecast(ets(y, model="MMZ", damped =FALSE), h = h)
errors_MMZ <- tsCV(WWUsers_ts, f_MMZ, h=1, window=NULL)
errors_USMMZ <- tsCV(USUsers_ts, f_MMZ, h=1, window=NULL)

par(mfrow=c(1,1))
plot(errors_AAZD, ylab = "tscv errors")
abline(0,0)
lines(errors_AAZ, col ="red")
lines(errors_MMZD, col="blue")
legend("left", legend=c("CV_error_AAZD", "CV_error_AAZ","CV_error_MMZD","CV_error_tbats"), col=c("black", "red", "blue", "green"), lty=1:4)

par(mfrow=c(1,1))
plot(errors_USAAZD, ylab = "US data tscv errors")
abline(0,0)
lines(errors_USAAZ, col ="red")
lines(errors_USMMZD, col="blue")
legend("left", legend=c("CV_error_US_AAZD", "CV_error_US_AAZ","CV_error_US_MMZD","CV_error_US_tbats"), col=c("black", "red", "blue", "green"), lty=1:4)


mean(abs(errors_AAZD/WWUsers_ts), na.rm=TRUE)*100
mean(abs(errors_AAZ/WWUsers_ts), na.rm=TRUE)*100
mean(abs(errors_MMZD/WWUsers_ts), na.rm=TRUE)*100
mean(abs(errors_MMZ/WWUsers_ts), na.rm=TRUE)*100

mean(abs(errors_USAAZD/USUsers_ts), na.rm=TRUE)*100
mean(abs(errors_USAAZ/USUsers_ts), na.rm=TRUE)*100
mean(abs(errors_USMMZD/USUsers_ts), na.rm=TRUE)*100
mean(abs(errors_USMMZ/USUsers_ts), na.rm=TRUE)*100

finalD <-cbind(WW_Users = WWUsers_AAZD_pred$mean, US_Users = USUsers_AAZD_pred$mean, WW_Users_Low = WWUsers_AAZD_pred$lower, US_Users_Low = USUsers_AAZD_pred$lower, WW_Users_High = WWUsers_AAZD_pred$upper, US_Users_High = USUsers_AAZD_pred$upper)
# Export the results out
write.csv(finalD, file = "Predicted User Numbers_v02.csv") # export the selected model's predictions into a CSV file
