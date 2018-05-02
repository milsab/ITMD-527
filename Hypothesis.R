library(fBasics)
library(BSDA)

#Original Data
bitc

# Return Data
n = nrow(bitc)
bitc_ret_close = ((bitc$close[2:n, 1] - bitc$close[1:(n-1), 1])/bitc$close[1:(n-1), 1])


# Extract Data
open = bitc$open
high = bitc$high
low = bitc$low
close = bitc$close
n = NROW(close)
bitc_ret_close = ((close[2:n, 1] - close[1:(n-1), 1])/close[1:(n-1), 1])

volume = bitc$volume
market = bitc$market
close_ratio = bitc$close_ratio
spread = bitc$spread

#stats
basicStats(open)
basicStats(close)

# Boxplots
plot(open~high)
boxplot(close)
boxplot(high)
boxplot(low)
boxplot(volume)
boxplot(market)

#####################
arma_predicted = forecast(ARMA5, 8)
auto_arima_predicted = forecast(auto_arima5, 8)

d = arma_predicted - auto_arima_predicted

arma_predicted[1]

forecast(ARMA5, 8)

t.test(arma_predicted[4], auto_arima_predicted[4], paired = T, alternative = "two.sided", mu = 0, conf.level = 0.95)


a = c(0.007971269,0.007872780, 0.007167068, 0.005027349, 0.006547935, 0.005088557, 0.006391531, 0.005322293)
b = c(0.006944753, 0.006944753, 0.006944753, 0.006944753, 0.006944753, 0.006944753, 0.006944753, 0.006944753)

t.test(a, b, paired = T, alternative = "two.sided", mu = 0, conf.level = 0.95)
