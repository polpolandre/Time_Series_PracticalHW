library(tidyverse)
library(forecast)
library(tseries)

gnp <- read.csv("GNP.csv", row.names = "DATE")
# format index to be a date
tail(rownames(gnp), n = 1)
head(rownames(gnp), n = 1)
GNP <- ts(gnp$GNP, start = c(1947, 1), end = c(2021, 3), frequency = 4)

#make both series the same length
GNP <- window(GNP, start = c(1983,1))
GNP<-log(GNP)
autoplot(GNP)


#removing outliers
GNP <- tsclean(GNP)
autoplot(GNP)

arima <- auto.arima(GNP)
summary(arima)

#summary(a1)
res <- arima$residuals
autoplot(res)
checkresiduals(res)
Box.test(res, fitdf = 7, lag=10, type = "Ljung")

forecast_arima <- forecast(arima, h = 8)
plot(forecast_arima)

fore_rw <- rwf(GNP, h = 8)
fore_rwd <- rwf(GNP, drift = TRUE, h = 8)

accuracy(forecast_arima)
accuracy(fore_rw)
accuracy(fore_rwd)

fore_arima_112 <- function(y, h) {
  model <- Arima(y, order = c(1, 2, 3), seasonal = c(1, 0, 2))
  forecast <- forecast(model, h)
  return(forecast)
}
errors_cv <- na.remove(tsCV(GNP, fore_arima_112, h = 1))

sqrt(mean(errors_cv ^ 2))
