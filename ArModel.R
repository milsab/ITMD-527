# Buliding Weekly_Return Data

bitc <- read.csv("bitc.csv", header = T)
bitc_prices <- bitc["close"]
price_ret <- ((bitc_prices[2:n, 1] - bitc_prices[1:(n-1), 1])/bitc_prices[1:(n-1), 1])
price_ret_ts <- zoo(price_ret, as.Date(as.character(bitc$date), format = "%m/%d/%Y"))
wdata <- as.xts(price_ret_ts)
weeklyData <- apply.weekly(as.xts(price_ret_ts),FUN=mean)
weeklyData

# Stationarity Test
plot(weeklyData)
par(mfcol=c(2,1))
acf(weeklyData, lag =15)
pacf(weeklyData, lag=15)

#building AR Model

AR_Model = arima(weeklyData, order = c(1,0,0))
AR_Model

#MA Model
MA_Model = arima(weeklyData, order = c(0,0,2))
MA_Model

#ARMA Model
ARMA_Model = arima(weeklyData, order = c(1,0,2))
ARMA_Model
