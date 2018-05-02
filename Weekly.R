library(xts)
library(forecast)


w_sqrt = sqrt(bitc_prices)
price_sqrt_zoo = zoo::zoo(w_sqrt, as.Date(as.character(bitc$date), format = "%m/%d/%Y"))

n = nrow(bitc_prices)
w_ret = ((bitc_prices[2:n, 1] - bitc_prices[1:(n-1), 1])/bitc_prices[1:(n-1), 1]) 
w_price_ret_zoo = zoo::zoo(w_ret, as.Date(as.character(bitc$date), format = "%m/%d/%Y"))

wdata = as.xts(w_price_ret_zoo)


montlyData = apply.monthly(as.xts(wdata),FUN=mean)
weeklyData = apply.weekly(as.xts(wdata),FUN=mean)

#
plot(weeklyData)
par(mfcol=c(2,1))

# Histogram
hist(weeklyData, xlab = "Price", prob = TRUE, main = "Histogram")
xfit = seq(min(weeklyData), max(weeklyData), length = 1793)
yfit = dnorm(xfit, mean = mean(weeklyData), sd = sd(weeklyData))
lines(xfit, yfit, col = "blue")

# Q-Q Plot
qqnorm(weeklyData)
qqline(weeklyData, col =2)

# Normality Test
normalTest(weeklyData, method = c("jb"))

# ACF Plot
acf_values = acf(weeklyData, plot = T, lag = 30)
acf_values

# PACF
pacf(weeklyData, plot = T, lag = 30)

# Ljung Box Test
Box.test(weeklyData, lag = 30, type = "Ljung")

############### MAKING TEH MODEL ###############

# auto Arima model
auto_arima = auto.arima(weeklyData, max.p = 30, max.q = 30, ic = "aic")

# AR model
AR=arima(weeklyData, order = c(29,0,0))

# MR model
MR=arima(weeklyData, order = c(0,0,29))

# ARIMA model
ARMA=arima(weeklyData, order = c(29,0,29))


############ EVALUATION ###########

# %80 - %20
train.data = weeklyData[1:205, ]
test.data = weeklyData[206:257, ]

plot(forecast(auto_arima, 12), include=36)
plot(forecast(AR, 12), include=36)
plot(forecast(MR, 12), include=36)
plot(forecast(ARMA, 12), include=36)

accuracy(forecast(AR, 50), test.data)


