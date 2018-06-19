library(zoo)
library(tseries)
library(fBasics)
library(forecast)

#Transformations
# Make the Squar Root
sqrt = sqrt(price)
sq = price^2

#Make Time-Series Object
sqrt_ts = zoo(sqrt, as.Date(as.character(bitc$date), format = "%m/%d/%Y"))
<<<<<<< HEAD
plot(sqrt_ts, xlab="", ylab="", main="Transformation: SQRT")

sq_ts = zoo(sq, as.Date(as.character(bitc$date), format = "%m/%d/%Y"))
plot(sq_ts, xlab="", ylab="", main="Transformation: SQURE")
=======
plot(sqrt_ts)

#Make the Square
sq_price = price * price
sq_price_ts = zoo(sq_price, as.Date(as.character(bitc$date), format = "%m/%d/%Y"))
plot(sq_price_ts)

#diff transformation
price_diff = diff(price)
price_diff_ts = zoo(price_diff, as.Date(as.character(bitc$date), format = "%m/%d/%Y"))
plot(price_diff_ts)

>>>>>>> 55a0e69f711ab7307d0d0d3b4c6e8f90c542bd84
#Augmented Dickey-Fuller Test for stationarity
adf.test(sqrt_ts, alternative = "stationary")



pricets = zoo(price, as.Date(as.character(bitc$date), format = "%m/%d/%Y"))
dif = diff(pricets)
plot(dif, xlab="", ylab="", main="Transformation: DIFFERENCING")

par(mfcol=c(2,2))
#Histogram
hist(sqrt_ts, xlab = "Price", prob = TRUE, main = "Histogram")
xfit = seq(min(sqrt_ts), max(sqrt_ts), length = 1793)
yfit = dnorm(xfit, mean = mean(sqrt_ts), sd = sd(sqrt_ts))
lines(xfit, yfit, col = "blue")



# Q-Q Plot
qqnorm(sqrt_ts)
qqline(sqrt_ts, col =2)

# Normality Test
normalTest(sqrt_ts, method = c("jb"))

# ACF Plot
acf_values = acf(sqrt_ts, plot = T, lag = 100)
acf_values

# PACF
pacf(sqrt_ts, plot = T, lag = 100)

# Ljung Box Test
Box.test(sqrt_ts, lag = 30, type = "Ljung")


#auto Arima model
auto.arima(price_ret_zoo, max.p = 30, max.q = 30, ic = "aic")

#AR model
AR=arima(sqrt_ts, order = c(20,0,0), method="ML")



