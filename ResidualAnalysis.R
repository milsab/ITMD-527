#AR
plot(AR$residuals, type = 'l')
qqnorm(AR$residuals)
qqline(AR$residuals, col=2)
Box.test(AR$residuals, lag=20, type='Ljung')
acf(AR$residuals, plot = T, lag = 20)

#MA
plot(MA$residuals, type = 'l')
qqnorm(MA$residuals)
qqline(MA$residuals, col=2)
Box.test(MA$residuals, lag=20, type='Ljung')
acf(MA$residuals, plot = T, lag = 20)

#ARMA(7,2)
plot(ARMA$residuals, type = 'l')
qqnorm(ARMA$residuals)
qqline(ARMA$residuals, col=2)
Box.test(ARMA$residuals, lag=20, type='Ljung')
acf(ARMA$residuals, plot = T, lag = 20)

#ARMA(2,3)
plot(auto_arima$residuals, type = 'l')
qqnorm(auto_arima$residuals)
qqline(auto_arima$residuals, col=2)
Box.test(auto_arima$residuals, lag=20, type='Ljung')
acf(auto_arima$residuals, plot = T, lag = 20)

#ARMA(1,2)
plot(ARMA_EACF$residuals, type = 'l')
qqnorm(ARMA_EACF$residuals)
qqline(ARMA_EACF$residuals, col=2)
Box.test(ARMA_EACF$residuals, lag=20, type='Ljung')
acf(auto_arima$residuals, plot = T, lag = 20)
