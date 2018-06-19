install.packages("quantmod")
install.packages("rugarch")
library("rugarch")
library("quantmod")
library("forecast")
library("fGarch")
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
wPredict = ugarchboot(wGarch72, n.ahead = 10, method = c("Partial", "FULL")[1])
plot(wPredict)

######## Test ##########

trainG = weeklyData[1:247, ]
testG = weeklyData[248:257, ]

w72t = ugarchspec(variance.model = list(model="sGARCH", garchOrder=c(1,1)), mean.model = list(armaOrder=c(7,2)), distribution.model = "std")
wGarch72t = ugarchfit(spec = w72t, data = trainG)
wPredict_t = predict(wGarch72t, n.ahead = 10, method = c("Partial", "FULL")[1])
plot(wPredict_t, which =2, window = 1 )

plot(testG, col='green')
lines(predicted, type = 'l', col = 'red')

plot(c(wPredict_t, which =2 ), testG, col = c("black", "red"))

plot(testG, trainG, col = c("red", "Black"))
##########

### Plot
gch = read.csv("Data\\garch.csv", header = T, stringsAsFactors = F)
observed = gch$Observed
predicted = gch$Predicted

#************************
plot(o$Observed, type='l', xlab='Date', ylab='returns', col='green', lwd=2, ylim=c(-.07,.07))
lines(o$Predicted, type='l', col='red', lwd=2)
legend('topright', c('Observed','Predicted'),lty=c(1,1),lwd=c(2.5,2.5),col=c('green','red'))
#*************************

gcht = zoo::zoo(gch, as.Date(as.character(gch$date), format = "%m/%d/%Y"))
o = gcht[,2-3]
ots = ts(observed, start = c(2018, 1), freq = 7)

observed = gcht$Observed
predicted = gcht$Predicted

forecast <- ugarchforecast(w72t, n.ahead = 10, data=wPredict_t)

##############
x <- runif(20,10,20)
y <- runif(20,30,50)
data<-weeklyData.frame(x,y)
generate.PDF <- function(data) {    
  pdf("p.pdf", width=4, height=2,onefile=T)
  plot1 <- plot(x,y)
  plot2 <- plot(y,x)
  plot3 <- plot(x,y*2)
  print(plot1)
  print(plot2)
  print(plot3)
  dev.off()
}
generate.PDF(data)
