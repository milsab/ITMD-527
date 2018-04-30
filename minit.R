#Load libraries
basicStats(pricets)
library(fBasics)

#Load Data
#myd = read.table("Data\\bitc.csv", header = T, sep = ',')
myd = read.csv("Data\\bitc.csv", header = T, sep = ',')
price = myd$close
price = myd["close", drop = FALSE]


#Structure of Data
str(myd)

#sample of Data
head(myd)
tail(myd)

#Basic Statistic of data
basicStats(price)

#Create Time-Series object and plot based on zoo function
pricets_zoo = zoo::zoo(zoo::coredata(price), as.Date(as.character(zoo::coredata(myd$date)), format = "%m/%d/%Y"))
plot(pricets_zoo)

#Create Time-Series object and plot based on ts function
pricets_ts = ts(price, start = c(2013, 8), freq = 1)
plot(pricets_ts)

#Histogram
hist(zoo::coredata(pricets), xlab = "Price", prob = TRUE, main = "Histogram")
xfit = seq(min(zoo::coredata(pricets)), max(zoo::coredata(pricets)))
yfit = dnorm(xfit, mean = mean(zoo::coredata(pricets)), sd = sd(zoo::coredata(pricets)))
lines(xfit, yfit, col = "blue", lwd = 2)

# Q-Q Plot
qqnorm(pricets)
qqline(pricets, col =2)

qqnorm(pricets)
plot(pricets)
head(pricets)
min(pricets)
max(pricets)

auto.arima(pricets_zoo, max.p = 6, max.q = 6,  ic = "aic")
