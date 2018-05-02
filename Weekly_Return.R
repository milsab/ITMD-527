library(timeSeries)
library(xts)
library(forecast)
library(zoo)
library(fbasic)

# Buliding Weekly Data

bitc <- read.csv("bitc.csv", header = T)
bitc_prices <- bitc["close"]
price_ts <- zoo(bitc_prices, as.Date(as.character(bitc$date), format = "%m/%d/%Y"))
weeklyData_raw <- apply.weekly(as.xts(wdata_raw),FUN=mean)
weeklyData_raw

# Stationarity Test
plot(weeklyData_raw)
par(mfcol=c(2,1))
acf(weeklyData_raw, lag =15)
pacf(weeklyData_raw, lag=15)

# Histogram
hist(weeklyData_raw,xlab = "Price", prob = TRUE, main = "Histogram")
xfit = seq(min(weeklyData_raw), max(weeklyData_raw), length = 257)
yfit = dnorm(xfit, mean = mean(weeklyData_raw), sd = sd(weeklyData_raw))
lines(xfit, yfit, col = "blue")


#Arima model
Arima = auto.arima(coredata(weeklyData_raw))
Arima

#First Fold
#Prtitioning
weeklyData_raw_training_1 = weeklyData_raw[1:217]
weeklyData_raw_test_1 = weeklyData_raw[218:225]
weeklyData_raw_training_1
weeklyData_raw_test_1

#Arima Model
Arima_model_training_1 = auto.arima(coredata(weeklyData_raw_training_1))
Arima_model_training_1


#Prediction
pr_arima_1 = predict(Arima_model_training_1,n.ahead = 8, se.fit = TRUE)
pr_arima_1

#Evaluation
accuracy(forecast(Arima_model_training_1),weeklyData_raw_test_1)
forecast(Arima_model_training_1)

#second Fold
#Prtitioning
weeklyData_raw_training_2 = weeklyData_raw[9:225]
weeklyData_raw_test_2 = weeklyData_raw[226:233]
weeklyData_raw_training_2
weeklyData_raw_test_2

#Arima Model
Arima_model_training_2 = auto.arima(coredata(weeklyData_raw_training_2))
Arima_model_training_2

#Prediction
pr_arima_2 = predict(Arima_model_training_2,n.ahead = 8, se.fit = TRUE)
pr_arima_2

#Evaluation
accuracy(forecast(Arima_model_training_2),weeklyData_raw_test_2)
forecast(Arima_model_training_2)

#Third Fold
#Prtitioning
weeklyData_raw_training_3 = weeklyData_raw[17:233]
weeklyData_raw_test_3 = weeklyData_raw[234:241]
weeklyData_raw_training_3
weeklyData_raw_test_3

#Arima Model
Arima_model_training_3 = auto.arima(coredata(weeklyData_raw_training_3))
Arima_model_training_3

#Prediction
pr_arima_3 = predict(Arima_model_training_3, n.ahead = 8, se.fit = TRUE)
pr_arima_3

#Evaluation
accuracy(forecast(Arima_model_training_3),weeklyData_raw_test_3)

#Forth Fold
#Prtitioning
weeklyData_raw_training_4 = weeklyData_raw[25:241]
weeklyData_raw_test_4 = weeklyData_raw[242:249]
weeklyData_raw_training_4
weeklyData_raw_test_4

#Arima Model
Arima_model_training_4 = auto.arima(coredata(weeklyData_raw_training_4))
Arima_model_training_4

#Prediction
pr_arima_4 = predict(Arima_model_training_4, n.ahead = 8, se.fit = TRUE)
pr_arima_4

#Evaluation
accuracy(forecast(Arima_model_training_4),weeklyData_raw_test_4)

#Fifth Fold
#Prtitioning
weeklyData_raw_training_5 = weeklyData_raw[33:249]
weeklyData_raw_test_5 = weeklyData_raw[249:257]
weeklyData_raw_training_5
weeklyData_raw_test_5

#Arima Model
Arima_model_training_5 = auto.arima(coredata(weeklyData_raw_training_5))
Arima_model_training_5

#Prediction
pr_arima_5 = predict(Arima_model_training_5, n.ahead = 8, se.fit = TRUE)
pr_arima_5

#Evaluation
accuracy(forecast(Arima_model_training_5),weeklyData_raw_test_5)

MAE_mean = (34.96022 +  45.54701 + 53.41643 + 81.19333 + 149.5535) / 5
MAE_mean

