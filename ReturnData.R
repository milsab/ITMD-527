# Divide display to 4 sections
par(mfcol=c(2,2))

#Make return values
simpleReturn = returns(price)

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
hist(zoo::coredata(pricets_logReturn_ts), xlab = "Price", prob = TRUE, main = "Histogram")
xfit = seq(min(zoo::coredata(pricets_logReturn_ts)), max(zoo::coredata(pricets_logReturn_ts)))
yfit = dnorm(xfit, mean = mean(zoo::coredata(pricets_logReturn_ts)), sd = sd(zoo::coredata(pricets_logReturn_ts)))
lines(xfit, yfit, col = "blue", lwd = 2)

# Q-Q Plot
qqnorm(simpleReturn)
qqline(simpleReturn, col =2)

# Normality Test
normalTest(simpleReturn, method = c("jb"))

# ACF Plot
acf_values = acf(pricets_return_ts, plot = T, lag = 20)
acf_values

# PACF Plot
pacf(zoo::coredata(pricets_return_zoo), lag = 20)

# Ljung Box Test
Box.test(pricets_return_zoo, lag = 30, type = "Ljung")


