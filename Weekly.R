library(xts)

wdata = as.xts(price_ret_zoo)

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

#auto Arima model
auto.arima(weeklyData, max.p = 30, max.q = 30, ic = "aic")

#AR model
AR=arima(weeklyData, order = c(31,0,0))
AR
