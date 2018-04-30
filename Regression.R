# Filter nominal Data to create Regresion Data
regData = myd[,6:13]
regData
plot(regData)

regData_volume = subset(regData, volume != 0)
plot(regData_volume)

# Shuffle data and split it
shuffle_data = regData_volume[sample(nrow(regData_volume)), ]
select.data = sample(1:nrow(shuffle_data), 0.8 * nrow(shuffle_data))
train.data = shuffle_data[select.data, ]
test.data = shuffle_data[-select.data, ]

# Extract Data
open = train.data$open
high = train.data$high
low = train.data$low
close = train.data$close
volume = train.data$volume
market = train.data$market
close_ratio = train.data$close_ratio
spread = train.data$spread

# Build Regression Model with Step Function
regressionModel = lm(high ~ open + low + close + volume + market + close_ratio + spread)
summary((regressionModel))

step(regressionModel, direction = "both", trace = T)


####### Reduced Data #########
regData_reduced = regData_volume[,1:4]
plot(regData_reduced)

# Shuffle data and split it
shuffle_data = regData_reduced[sample(nrow(regData_reduced)), ]
select.data = sample(1:nrow(shuffle_data), 0.8 * nrow(shuffle_data))
train.data = shuffle_data[select.data, ]
test.data = shuffle_data[-select.data, ]

# Extract Data
open = train.data$open
high = train.data$high
low = train.data$low
close = train.data$close

# Build Regression Model with Step Function
regressionModel_reduced = lm(high ~ open + low + close)
summary((regressionModel_reduced))
  
step(regressionModel_reduced, direction = "both", trace = T)


# Residual Analysis
res = rstandard(regressionModel_reduced)
plot(fitted(regressionModel_reduced), res, main = "Predicted vs Residuals plot")
abline(a = 0, b = 0, col='red')

qqnorm(res)
qqline(res, col = 2)
shapiro.test(res)

# Predict
p = predict.glm(regressionModel_reduced, test.data)
