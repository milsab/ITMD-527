library(fBasics)
library(BSDA)

#Original Data
bitc

# Return Data
n = nrow(bitc)
bitc_ret_close = ((bitc$close[2:n, 1] - bitc$close[1:(n-1), 1])/bitc$close[1:(n-1), 1])


# Extract Data
open = bitc$open
high = bitc$high
low = bitc$low
close = bitc$close
n = NROW(close)
bitc_ret_close = ((close[2:n, 1] - close[1:(n-1), 1])/close[1:(n-1), 1])

volume = bitc$volume
market = bitc$market
close_ratio = bitc$close_ratio
spread = bitc$spread

#stats
basicStats(open)
basicStats(close)

# Boxplots
plot(open~high)
boxplot(close)
boxplot(high)
boxplot(low)
boxplot(volume)
boxplot(market)

#####################
arma_predicted = forecast(ARMA5, 8)
auto_arima_predicted = forecast(auto_arima5, 8)

d = arma_predicted - auto_arima_predicted

arma_predicted[1]

forecast(ARMA5, 8)

t.test(arma_predicted[4], auto_arima_predicted[4], paired = T, alternative = "two.sided", mu = 0, conf.level = 0.95)

par(mfcol=c(1,1))
# ARMA model
a1= c(0.0055297493,-0.0009926293, 0.0087376483, -0.0032111638, 0.0065895855, -0.0014780105, 0.0054318684, 0.0006607124)
a2= c(0.016804286,0.008150385, 0.008493841, 0.005073255, 0.007538029, 0.006954962, 0.004983041, 0.004916994)
a3= c(0.004607031,0.005378416, 0.002231668, 0.003305823, 0.006445537, 0.001981536, 0.006080003, 0.001645905)
a4= c(0.015373849,0.006286787, 0.009771892, 0.007613390, 0.013520479, 0.006960146, 0.010234257, 0.003904376)
a5= c(0.007971269,0.007872780, 0.007167068, 0.005027349, 0.006547935, 0.005088557, 0.006391531, 0.005322293)
ARMA = c(a1, a2, a3, a4, a5)
# auto_arima model
b1 = c(0.0003168238, 0.0018991858, 0.0034504700, 0.0031641627, 0.0032170040, 0.0032072515, 0.0032090514, 0.0032087192)
b2 = c(0.018227285, 0.007443547, 0.006422137, 0.003553220, 0.004198827, 0.003388522, 0.003505816, 0.003651446)
b3 = c(0.007075567, 0.005361624, 0.004370866, 0.003926024, 0.003695245, 0.003585225, 0.003530150, 0.003503352)
b4 = c(0.014686901, 0.006319723, 0.011313847, 0.006810482, 0.012023574, 0.004263349, 0.007044679, 0.003442184)
b5 = c(0.006944753, 0.006944753, 0.006944753, 0.006944753, 0.006944753, 0.006944753, 0.006944753, 0.006944753)
ARIMA = c(b1, b2, b3, b4, b5)
par(mfrow = c(1, 2))
boxplot(ARMA)
boxplot(b)
t.test(a, b, paired = T, alternative = "two.sided", mu = 0, conf.level = 0.95)

# AR Model
d1= c(0.004561724,0.002189201, 0.006846708, 0.001946872, 0.003747521, 0.003280142, 0.003201011, 0.003722139)
d2= c(0.012249583,0.012591223, 0.003730165, 0.007104261, 0.007037685, 0.005265419, 0.005804249, 0.003959521)
d3= c(0.007558980,0.004707931, 0.002548812, 0.004709934, 0.004044445, 0.004348490, 0.003869586, 0.003553959)
d4= c(0.014190049,0.006718819, 0.008987999, 0.008935857, 0.010999399, 0.009612250, 0.007845698, 0.006722371)
d5= c(0.0026734173,0.0048803631, 0.0009575972, -0.0001488395, 0.0034424560, 0.0031107135, 0.0026958721, 0.0022736155)
AR = c(d1, d2, d3, d4, d5)

# MA Model
c1= c(0.0007426335,0.0016822765, 0.0032192368, 0.0032192368, 0.0032192368, 0.0032192368, 0.0032192368, 0.0032192368)
c2= c(0.012693903,0.009508461, 0.003434520, 0.003434520, 0.003434520, 0.003434520, 0.003434520, 0.003434520)
c3= c(0.007980740,0.004812736, 0.003461041, 0.003461041, 0.003461041, 0.003461041, 0.003461041, 0.003461041)
c4= c(0.010882050,0.006333968, 0.003983154, 0.003983154, 0.003983154, 0.003983154, 0.003983154, 0.003983154)
c5= c(0.001892668,0.002735688, 0.002436760, 0.002436760, 0.002436760, 0.002436760, 0.002436760, 0.002436760)
MA = c(c1, c2, c3, c4, c5)

# ARMA_EACF Model
e1= c(0.0003168238,0.0018991858, 0.0034504700, 0.0031641627, 0.03604642, 0.03603668, 0.03603849, 0.03603815)
e2= c(0.012699856,0.009571119, 0.003591369, 0.003442003, 0.003438272, 0.003438179, 0.003438177, 0.003438177)
e3= c(0.007982373,0.004813196, 0.003460457, 0.003460191, 0.003460191, 0.003460191, 0.003460191, 0.003460191)
e4= c(0.011245961,0.006626490, 0.004120369, 0.003996189, 0.003990036, 0.003989731, 0.003989715, 0.003989715)
e5= c(0.005737205,0.005549682, 0.005516794, 0.005484268, 0.005452099, 0.005420283, 0.005388816, 0.005357695)
ARMA_EACF = c(e1, e2, e3, e4, e5)

par(mfrow = c(1, 5))

boxplot(AR, main="AR(7)")
boxplot(MA, main="MA(2)")
boxplot(ARMA, main="ARMA(7,2)")
boxplot(ARIMA, main="ARMA(2,3)")
boxplot(ARMA_EACF, main="ARMA(1,2)")

t.test(ARMA, ARIMA, paired = T, alternative = "two.sided", mu = 0, conf.level = 0.95)
t.test(ARMA, AR, paired = T, alternative = "two.sided", mu = 0, conf.level = 0.95)
t.test(ARMA, MA, paired = T, alternative = "two.sided", mu = 0, conf.level = 0.95)
t.test(ARMA, ARMA_EACF, paired = T, alternative = "two.sided", mu = 0, conf.level = 0.95)
t.test(ARIMA, AR, paired = T, alternative = "two.sided", mu = 0, conf.level = 0.95)
t.test(ARIMA, MA, paired = T, alternative = "two.sided", mu = 0, conf.level = 0.95)
t.test(ARIMA, ARMA_EACF, paired = T, alternative = "two.sided", mu = 0, conf.level = 0.95)
t.test(AR, MA, paired = T, alternative = "two.sided", mu = 0, conf.level = 0.95)
t.test(AR, ARMA_EACF, paired = T, alternative = "two.sided", mu = 0, conf.level = 0.95)
t.test(MA, ARMA_EACF, paired = T, alternative = "two.sided", mu = 0, conf.level = 0.95)

