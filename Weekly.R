library(xts)
library(forecast)


w_sqrt = sqrt(bitc_prices)
price_sqrt_zoo = zoo::zoo(w_sqrt, as.Date(as.character(bitc$date), format = "%m/%d/%Y"))

n = nrow(bitc_prices)
w_ret = ((bitc_prices[2:n, 1] - bitc_prices[1:(n-1), 1])/bitc_prices[1:(n-1), 1]) 
w_price_ret_zoo = zoo::zoo(w_ret, as.Date(as.character(bitc$date), format = "%m/%d/%Y"))
w_price_zoo = zoo::zoo(bitc_prices, as.Date(as.character(bitc$date), format = "%m/%d/%Y"))

wdata = as.xts(w_price_ret_zoo)
wdata_nonst = as.xts(w_price_zoo)

montlyData = apply.monthly(as.xts(wdata),FUN=mean)
weeklyData = apply.weekly(as.xts(wdata),FUN=mean)
weeklyData_nonst = apply.weekly(as.xts(wdata_nonst),FUN=mean)

#
plot(weeklyData, xlab="", ylab="", main="Weekly ")
plot(weeklyData_nonst)
par(mfcol=c(2,1))
acf(weeklyData, lag=12)
pacf(weeklyData, lag=12)

## ACF Plot
acf_values = acf(weeklyData, plot = T, lag = 20)
acf_values

# PACF Plot
pacf(weeklyData, plot = T, lag = 20)

# Histogram
hist(weeklyData, xlab = "Price", prob = TRUE, main = "Histogram")
xfit = seq(min(weeklyData), max(weeklyData), length = 1793)
yfit = dnorm(xfit, mean = mean(weeklyData), sd = sd(weeklyData))
lines(xfit, yfit, col = "blue")

# Q-Q Plot
qqnorm(weeklyData)
qqline(weeklyData, col =2)

# Normality Test
library(fBasics)


# Ljung Box Test
Box.test(weeklyData, lag = 5, type = "Ljung")

############### MAKING TEH MODEL ###############

# auto Arima model
auto_arima = auto.arima(weeklyData, max.p = 12, max.q = 12, ic = "aic")

# auto arima on non-stationary data
auto_arima_nonst = auto.arima(weeklyData_nonst, max.p = 30, max.q = 30, ic = "aic")

# AR model
AR=arima(weeklyData, order = c(7,0,0))

# MR model
MA=arima(weeklyData, order = c(0,0,2))

# ARIMA model
ARMA=arima(weeklyData, order = c(7,0,2))

# ARMA_EACF model
ARMA_EACF = arima(weeklyData, order = c(1,0,2))

plot(forecast(AR, 8), include=36)
plot(forecast(MA, 8), include=36)
plot(forecast(ARMA, 8), include=36)
plot(forecast(ARMA_EACF, 8), include=36)
plot(forecast(auto_arima, 8), include=36)
plot(forecast(auto_arima_nonst, 8), include=36)

############ EVALUATION ###########

# %80 - %20
train.data = weeklyData[1:205, ]
test.data = weeklyData[206:257, ]

accuracy(forecast(AR, 50), test.data)

# %96 - %4
train.data = weeklyData[1:247, ]
test.data = weeklyData[248:257, ]
#####################################
#------------------------- Fold = 1 ---------------------------------
# 
train1 = weeklyData[1:217, ]
test1 = weeklyData[218:225, ]

# auto Arima model
auto_arima1 = auto.arima(train1, max.p = 30, max.q = 30, ic = "aic")

# auto arima on non-stationary data
#auto_arima_nonst = auto.arima(weeklyData_nonst, max.p = 30, max.q = 30, ic = "aic")

# AR model
AR1=arima(train1, order = c(7,0,0))

# MR model
MA1=arima(train1, order = c(0,0,2))

# ARIMA model
ARMA1=arima(train1, order = c(7,0,2))

accuracy(forecast(AR1, 8), test1)
accuracy(forecast(MA1, 8), test1)
accuracy(forecast(ARMA1, 8), test1)
accuracy(forecast(auto_arima1, 8), test1)

#----------------------------- Fold = 2 -----------------------------
# Fold = 2
train2 = weeklyData[9:225, ]
test2 = weeklyData[226:233, ]

# auto Arima model
auto_arima2 = auto.arima(train2, max.p = 30, max.q = 30, ic = "aic")

# auto arima on non-stationary data
#auto_arima_nonst = auto.arima(weeklyData_nonst, max.p = 30, max.q = 30, ic = "aic")

# AR model
AR2=arima(train2, order = c(7,0,0))

# MR model
MA2=arima(train2, order = c(0,0,2))

# ARIMA model
ARMA2=arima(train2, order = c(7,0,2))

accuracy(forecast(AR2, 8), test2)
accuracy(forecast(MA2, 8), test2)
accuracy(forecast(ARMA2, 8), test2)
accuracy(forecast(auto_arima2, 8), test2)

#----------------------------- Fold = 3 ---------------------------
# Fold = 3
train3 = weeklyData[17:233, ]
test3 = weeklyData[234:241, ]

# auto Arima model
auto_arima3 = auto.arima(train3, max.p = 30, max.q = 30, ic = "aic")

# auto arima on non-stationary data
#auto_arima_nonst = auto.arima(weeklyData_nonst, max.p = 30, max.q = 30, ic = "aic")

# AR model
AR3=arima(train3, order = c(7,0,0))

# MR model
MA3=arima(train3, order = c(0,0,2))

# ARIMA model
ARMA3=arima(train3, order = c(7,0,2))

# auto Arima model
auto_arima3 = auto.arima(train3, max.p = 30, max.q = 30, ic = "aic")

accuracy(forecast(AR3, 8), test3)
accuracy(forecast(MA3, 8), test3)
accuracy(forecast(ARMA3, 8), test3)
accuracy(forecast(auto_arima3, 8), test3)

#----------------------------- Fold = 4 -------------------------
# Fold = 4
train4 = weeklyData[25:241, ]
test4 = weeklyData[242:249, ]

# auto Arima model
auto_arima4 = auto.arima(train4, max.p = 30, max.q = 30, ic = "aic")

# auto arima on non-stationary data
#auto_arima_nonst = auto.arima(weeklyData_nonst, max.p = 30, max.q = 30, ic = "aic")

# AR model
AR4=arima(train4, order = c(7,0,0))

# MR model
MA4=arima(train4, order = c(0,0,2))

# ARIMA model
ARMA4=arima(train4, order = c(7,0,2))

accuracy(forecast(AR4, 8), test4)
accuracy(forecast(MA4, 8), test4)
accuracy(forecast(ARMA4, 8), test4)
accuracy(forecast(auto_arima4, 8), test4)

#----------------------------------------------------------
# Fold = 5
train5 = weeklyData[33:249, ]
test5 = weeklyData[250:257, ]

# auto Arima model
auto_arima5 = auto.arima(train5, max.p = 30, max.q = 30, ic = "aic")

# auto arima on non-stationary data
#auto_arima_nonst = auto.arima(weeklyData_nonst, max.p = 30, max.q = 30, ic = "aic")

# AR model
AR5=arima(train5, order = c(7,0,0))

# MR model
MA5=arima(train5, order = c(0,0,2))

# ARIMA model
ARMA5=arima(train5, order = c(7,0,2))

accuracy(forecast(AR5, 8), test5)
accuracy(forecast(MA5, 8), test5)
accuracy(forecast(ARMA5, 8), test5)
accuracy(forecast(auto_arima5, 8), test5)

#------------------ MEAN of MAE --------------------
# AR
ar_acc1 = accuracy(forecast(AR1, 8), test5)
ar_acc2 = accuracy(forecast(AR2, 8), test5)
ar_acc3 = accuracy(forecast(AR3, 8), test5)
ar_acc4 = accuracy(forecast(AR4, 8), test5)
ar_acc5 = accuracy(forecast(AR5, 8), test5)
ar_acc_mean = ( ar_acc1[, "MAE"] + ar_acc2[, "MAE"] + ar_acc3[, "MAE"] + ar_acc4[, "MAE"] + ar_acc5[, "MAE"] ) / 5

# MA
ma_acc1 = accuracy(forecast(MA1, 8), test5)
ma_acc2 = accuracy(forecast(MA2, 8), test5)
ma_acc3 = accuracy(forecast(MA3, 8), test5)
ma_acc4 = accuracy(forecast(MA4, 8), test5)
ma_acc5 = accuracy(forecast(MA5, 8), test5)
ma_acc_mean = ( ma_acc1[, "MAE"] + ma_acc2[, "MAE"] + ma_acc3[, "MAE"] + ma_acc4[, "MAE"] + ma_acc5[, "MAE"] ) / 5

# ARMA
arma_acc1 = accuracy(forecast(ARMA1, 8), test5)
arma_acc2 = accuracy(forecast(ARMA2, 8), test5)
arma_acc3 = accuracy(forecast(ARMA3, 8), test5)
arma_acc4 = accuracy(forecast(ARMA4, 8), test5)
arma_acc5 = accuracy(forecast(ARMA5, 8), test5)
#***
accuracy(predict(ARMA5, n.ahead = 8, se.fit = T))
#***
arma_acc_mean = ( arma_acc1[, "MAE"] + arma_acc2[, "MAE"] + arma_acc3[, "MAE"] + arma_acc4[, "MAE"] + arma_acc5[, "MAE"] ) / 5

# auto arima
auto_arima_acc1 = accuracy(forecast(auto_arima1, 8), test5)
auto_arima_acc2 = accuracy(forecast(auto_arima2, 8), test5)
auto_arima_acc3 = accuracy(forecast(auto_arima3, 8), test5)
auto_arima_acc4 = accuracy(forecast(auto_arima4, 8), test5)
auto_arima_acc5 = accuracy(forecast(auto_arima5, 8), test5)
auto_arima_acc_mean = ( auto_arima_acc1[, "MAE"] + auto_arima_acc2[, "MAE"] + auto_arima_acc3[, "MAE"] + auto_arima_acc4[, "MAE"] + auto_arima_acc5[, "MAE"] ) / 5

ar_acc_mean
ma_acc_mean
arma_acc_mean
auto_arima_acc_mean