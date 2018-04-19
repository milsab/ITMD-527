# Buliding Return Model
bitc = read.csv("bitc.csv", header = T, stringsAsFactors = F)
bitc_prices <- bitc["close", drop = FALSE]
n < nrow(bitc_prices)
price_ret <- ((bitc_prices[2:n, 1] - bitc_prices[1:(n-1), 1])/bitc_prices[1:(n-1), 1])
price_ret


#Create Time-Series object and plot based on zoo function
price_ret_zoo = zoo(price_ret, as.Date(as.character(bitc$date), format = "%m/%d/%Y"))
par(mfcol=c(2,2))
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

## ACF Plot
acf_values = acf(price_ret_zoo, plot = T, lag = 50)
acf_values

#pacf
pacf(price_ret_zoo, plot = T, lag = 50)

#auto Arima model
auto.arima(price_ret_zoo)

#AR model
AR=arima(price_ret_zoo, order = c(5,0,0))
AR


