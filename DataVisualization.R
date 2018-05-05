par(mfcol=c(1,1))
library(RColorBrewer)
plot(bitc$open, type='l', col=brewer.pal(3,"Set3"))
plot(bitc$close, bitc$open, type='l', col=brewer.pal(3,"Set2"))

boxplot(bitc$spread)
basicStats(close)
plot(close)
mean(close)
max(close)
min(close)
median(close)

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
result <- getmode(close)
print(result)

head(bitc$date)
