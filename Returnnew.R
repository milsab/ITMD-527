# Buliding Return Model

bitc_prices <- bitc["close", drop = FALSE]
price_ret <- ((bitc_prices[2:n, 1] - bitc_prices[1:(n-1), 1])/bitc_prices[1:(n-1), 1])
price_ret
#Create Time-Series object and plot based on zoo function
price_ret_zoo = zoo(price_ret, as.Date(as.character(bitc$date), format = "%m/%d/%Y"))
plot(price_ret_zoo)

#Histogram
hist(price_ret_zoo, xlab = "Price", prob = TRUE, main = "Histogram")
xfit = seq(min(price_ret_zoo), max(price_ret_zoo))
yfit = dnorm(xfit, mean = mean(price_ret_zoo), sd = sd(price_ret_zoo))
lines(xfit, yfit, col = "blue")

# Q-Q Plot
par(mfcol=c(2,2))
qqnorm(price_ret_zoo)
qqline(price_ret_zoo, col =2)

# Normality Test
normalTest(price_ret_zoo, method = c("jb"))

## ACF Plot
acf_values = acf(price_ret_zoo, plot = T, lag = 20)
acf_values