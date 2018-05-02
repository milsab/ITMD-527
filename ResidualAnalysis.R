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

#ARMA
plot(ARMA$residuals, type = 'l')
qqnorm(ARMA$residuals)
qqline(ARMA$residuals, col=2)
Box.test(ARMA$residuals, lag=20, type='Ljung')
acf(ARMA$residuals, plot = T, lag = 20)

#auto arima
plot(auto_arima$residuals, type = 'l')
qqnorm(auto_arima$residuals)
qqline(auto_arima$residuals, col=2)
Box.test(auto_arima$residuals, lag=20, type='Ljung')
acf(auto_arima$residuals, plot = T, lag = 20)
