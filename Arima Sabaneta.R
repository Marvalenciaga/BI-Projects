install.packages("tseries")
installed.packages(lib.loc = NULL)
install.packages("astsa")
install.packages("forecast")
install.packages("tidyverse")
install.packages("lubridate")
install.packages("foreign")
install.packages("quantmod")
install.packages("TTR")
install.packages("xts")
install.packages("zoo")

library(tseries)
library(astsa)
library(forecast)
library(tidyverse)
library(lubridate)
library(foreign)
library(quantmod)
library(TTR)
library(xts)
library(zoo)

Toneladas_Sabaneta.ts=ts(Toneladas_Sabaneta, start = c(2016,4), frequency = 12)
Toneladas_Sabaneta.ts
plot(Toneladas_Sabaneta.ts)
serielog=log(Toneladas_Sabaneta.ts)
serielog
plot(serielog)

adf.test(serielog, alternative = "stationary")

seriedif=diff(Toneladas_Sabaneta.ts)
seriedif
plot(seriedif)

adf.test(seriedif)

plot(seriedif, type="o", lty="dashed", col="blue", main="Serie de Tiempo Toneladas Sabaneta")
par(mfrow=c(2,1), mar=c(4,4,4,1)+.1)
acf(seriedif)
pacf(seriedif)
acf(ts(seriedif, frequency = 1))
pacf(ts(seriedif, frequency = 1))
modelo1=arima(Toneladas_Sabaneta.ts, order=c(1,2,1))
modelo1
tsdiag(modelo1)
Box.test(residuals(modelo1), type = "Ljung-Box")
error=residuals(modelo1)
plot(error)

pronostico= forecast::forecast(modelo1, h = 10)
pronostico
plot(pronostico)

nnetar(Toneladas_Sabaneta.ts)
fit = nnetar(Toneladas_Sabaneta.ts)
fcast =forecast(fit)
plot(fcast)
