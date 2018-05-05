library(tseries)
library(fBasics)
par(mfcol=c(1,1))

# Buliding Return Model
bitc = read.csv("Data\\bitc.csv", header = T, stringsAsFactors = F)
bitc_prices = bitc["close"]
n = nrow(bitc_prices)
price_ret = ((bitc_prices[2:n, 1] - bitc_prices[1:(n-1), 1])/bitc_prices[1:(n-1), 1])
price_ret


#Create Time-Series object and plot based on zoo function
price_ret_zoo = zoo::zoo(price_ret, as.Date(as.character(bitc$date), format = "%m/%d/%Y"))
par(mfcol=c(1,1))
plot(price_ret_zoo, xlab="", ylab="", main="Transformation: RETURN")

#Augmented Dickey-Fuller Test for stationarity

adf.test(price_ret_zoo, alternative = "stationary")

#Histogram
hist(price_ret_zoo, xlab = "Price", prob = TRUE, main = "Histogram")
xfit = seq(min(price_ret_zoo), max(price_ret_zoo), length = 1793)
yfit = dnorm(xfit, mean = mean(price_ret_zoo), sd = sd(price_ret_zoo))
lines(xfit, yfit, col = "blue")

# Q-Q Plot
qqnorm(price_ret_zoo)
qqline(price_ret_zoo, col =2)

# Normality Test
normalTest(price_ret_zoo, method = c("jb"))

# ACF Plot
acf_values = acf(price_ret_zoo, plot = T, lag = 20)
acf_values

# PACF
pacf(price_ret_zoo, plot = T, lag = 20)

# Ljung Box Test
Box.test(price_ret_zoo, lag = 30, type = "Ljung")



############## LOG RETURN #################

lnReturn = log(price_ret)
logReturn = log10(price_ret)
plot(logReturn, xlab="", ylab="", main="Transformation: LOG RETURN")
plot(price_ret_zoo)

price_lnReturn_zoo = zoo::zoo(zoo::coredata(lnReturn), as.Date(as.character(bitc$date), format = "%m/%d/%Y"))
plot(coredata(price_lnReturn_zoo))
plot(price_lnReturn_zoo, xlab="", ylab="", main="Transformation: RETURN")
price_logReturn_zoo = zoo::zoo(zoo::coredata(logReturn), as.Date(as.character(bitc$date), format = "%m/%d/%Y"))
plot(coredata(price_lnReturn_zoo))



## ACF Plot
acf_values = acf(price_ret_zoo, plot = T, lag = 20)
acf_values

#pacf
pacf(price_ret_zoo, plot = T, lag = 20)

############### MAKING TEH MODEL ###############

#auto Arima model
auto_arima = auto.arima(price_ret_zoo, max.p = 30, max.q = 30, ic = "aic")

#AR model
AR=arima(price_ret_zoo, order = c(31,0,0))
AR

############ EVALUATION ###########

# %80 - %20
train.data = price_ret_zoo[1:1434, ]
test.data = price_ret_zoo[1435:1793, ]

plot(forecast(auto_arima, 12), include=36)
plot(forecast(AR, 12), include=36)

auto_arima = auto.arima(train.data, max.p = 30, max.q = 30, ic = "aic")
accuracy(forecast(auto_arima, 50), test.data)


#----------------------------------
library(fpp)
e = tsCV(price_ret_zoo, AR, h = 2)
f = tsCV(price_ret_zoo, auto_arima, h = 2)

library(ggfortify)
training = window(price_ret_zoo, end = c(2016-12-31))
test = window(price_ret_zoo, start = 2017-01-01)
fc = naive(training, h = 10)
autoplot(fc) + autolayer(test, series = "Test data")
