# Divide display to 4 sections
par(mfcol=c(2,2))

#
n = nrow(price)
price

#Make return values
simpleReturn = returns(price)
simpleReturn = 

# Make return value based on time-series object
returnts = returns(pricets)

#Make differencing
price_d = diff(pricets)

#Make Log return values
logReturn = log(zoo::coredata(simpleReturn))

#Structure of Data
str(simpleReturn)
str(logReturn)

#sample of Data
head(simpleReturn)
tail(simpleReturn)
head(logReturn)
tail(logReturn)

#Basic Statistic of data
basicStats(simpleReturn)
basicStats(logReturn)

#Create Time-Series object and plot based on zoo function
pricets_return_zoo = zoo::zoo(zoo::coredata(simpleReturn), as.Date(as.character(zoo::coredata(myd$date)), format = "%m/%d/%Y"))
plot(pricets_return_zoo)

pricets_logReturn_zoo = zoo::zoo(zoo::coredata(logReturn), as.Date(as.character(zoo::coredata(myd$date)), format = "%m/%d/%Y"))
plot(pricets_logReturn_zoo)

#Create Time-Series object and plot based on ts function
pricets_return_ts = ts(simpleReturn, start = c(2013, 8), freq = 1)
plot(pricets_return_ts)

pricets_logReturn_ts = ts(logReturn, start = c(2013, 8), freq = 1)
plot(pricets_logReturn_ts)

#Histogram
hist(zoo::coredata(returnts), xlab = "Price", prob = TRUE, main = "Histogram")
xfit = seq(min(pricets_return_ts, na.rm=TRUE), max(pricets_return_ts, na.rm=TRUE), length = 40)
yfit = dnorm(xfit, mean = mean(pricets_return_ts, na.rm=TRUE), sd = sd(pricets_return_ts, na.rm=TRUE))
lines(xfit, yfit, col = "blue", lwd = 2)

# Q-Q Plot
qqnorm(pricets_return_zoo)
qqline(pricets_return_zoo, col =2)

# Normality Test
normalTest(simpleReturn, method = c("jb"))

# ACF Plot
acf_values = acf(zoo::coredata(pricets_return_zoo), plot = T, lag = 20)
acf_values

# PACF Plot
pacf(zoo::coredata(pricets_return_zoo), lag = 20)

# Ljung Box Test
Box.test(pricets_return_zoo, lag = 30, type = "Ljung")

# Model
arima = auto.arima(pricets_return_zoo, max.p = 20, max.q = 20, ic = c("aic"))

# Prediction
predict(arima, n.head = 100, se.fit = T)
accuracy(forecast(arima), te)
