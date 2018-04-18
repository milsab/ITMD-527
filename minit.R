basicStats(pricets)


myd = read.table("Data\\bitc.csv", header = T, sep = ',')
price = myd$close
pricets = zoo::zoo(zoo::coredata(price), as.Date(as.character(zoo::coredata(myd$date)), format = "%m/%d/%Y"))
library(fBasics)
qqnorm(pricets)
plot(pricets)
head(pricets)
min(pricets)
max(pricets)

#use ts function
pts = ts(price, start = c(2013, 8), freq = 1)
plot(pts)