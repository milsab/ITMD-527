install.packages("quantmod")
install.packages("rugarch")
library("rugarch")
library("quantmod")
fb = getSymbols("FB", auto.assign = F)
head(fb)
tail(fb)
chartSeries(fb)
chartSeries(pricets)
chartSeries(weeklyData)

# AIC = -5.5317
w11 = ugarchspec(variance.model = list(model="sGARCH", garchOrder=c(1,1)), mean.model = list(armaOrder=c(1,1)), distribution.model = "std")
wGarch11 = ugarchfit(spec = w11, data = weeklyData)

# AIC = -5.6043
w72 = ugarchspec(variance.model = list(model="sGARCH", garchOrder=c(1,1)), mean.model = list(armaOrder=c(7,2)), distribution.model = "std")
wGarch72 = ugarchfit(spec = w72, data = weeklyData)

# AIC = -5.5276
w12 = ugarchspec(variance.model = list(model="sGARCH", garchOrder=c(1,1)), mean.model = list(armaOrder=c(1,2)), distribution.model = "std")
wGarch12 = ugarchfit(spec = w12, data = weeklyData)

# AIC = -5.5436
w23 = ugarchspec(variance.model = list(model="sGARCH", garchOrder=c(1,1)), mean.model = list(armaOrder=c(2,3)), distribution.model = "std")
wGarch23 = ugarchfit(spec = w23, data = weeklyData)

# Predict
wPredict = ugarchboot(wGarch72, n.ahead = 20, method = ("Partial")[1])
plot(wPredict)


##########

gd = ugarchdistribution(wGarch72, n.sim = 50, recursive = TRUE, recursive.length = 600, recursive.window = 50, m.sim = 10, solver = 'hybrid')
show(gd)
