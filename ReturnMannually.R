library(tseries)
# Buliding Return Model
bitc = read.csv("Data\\bitc.csv", header = T, stringsAsFactors = F)
bitc_prices <- bitc["close", drop = FALSE]
n < nrow(bitc_prices)
price_ret <- ((bitc_prices[2:n, 1] - bitc_prices[1:(n-1), 1])/bitc_prices[1:(n-1), 1])
price_ret


#Create Time-Series object and plot based on zoo function
price_ret_zoo = zoo::zoo(price_ret, as.Date(as.character(bitc$date), format = "%m/%d/%Y"))
par(mfcol=c(1,1))
plot(price_ret_zoo)

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
acf_values = acf(price_ret_zoo, plot = T, lag = 50)
acf_values

# PACF
pacf(price_ret_zoo, plot = T, lag = 50)

# Ljung Box Test
Box.test(price_ret_zoo, lag = 30, type = "Ljung")


############## LOG RETURN #################

lnReturn = log(price_ret)
logReturn = log10(price_ret)
plot(logReturn)
plot(price_ret_zoo)

price_lnReturn_zoo = zoo::zoo(zoo::coredata(lnReturn), as.Date(as.character(bitc$date), format = "%m/%d/%Y"))
plot(zoo::coredata(price_lnReturn_zoo))
plot(price_lnReturn_zoo)
price_logReturn_zoo = zoo::zoo(zoo::coredata(logReturn), as.Date(as.character(bitc$date), format = "%m/%d/%Y"))
plot(zoo::coredata(price_lnReturn_zoo))



## ACF Plot
acf_values = acf(price_ret_zoo, plot = T, lag = 50)
acf_values

#pacf
pacf(price_ret_zoo, plot = T, lag = 50)

#auto Arima model
auto.arima(price_ret_zoo, max.p = 30, max.q = 30, ic = "bic")

#AR model
AR=arima(price_ret_zoo, order = c(31,0,0))
AR



