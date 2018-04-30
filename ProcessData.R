pureData = myd[,4:13]
pureData = pureData[,-2]
head(pureData)
volume = pureData[,7]

data = read.csv('Data\\bitc_modified.csv', header = T, sep = ',')
head(data)
tail(data)
