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

Toneladas_Copacabana.ts=ts(Toneladas_Copacabana, start = c(2016,4), frequency = 12)
Toneladas_Copacabana.ts
plot(Toneladas_Copacabana.ts)
serielog=log(Toneladas_Copacabana.ts)
serielog
plot(serielog)

adf.test(serielog, alternative = "stationary")

seriedif=diff(Toneladas_Bello.ts)
seriedif
plot(seriedif)

adf.test(serielog)

plot(serielog, type="o", lty="dashed", col="blue", main="Serie de Tiempo Toneladas Copacabana")
par(mfrow=c(2,1), mar=c(4,4,4,1)+.1)
acf(serielog)
pacf(serielog)
acf(ts(serielog, frequency = 1))
pacf(ts(serielog, frequency = 1))
modelo1=arima(Toneladas_Copacabana.ts, order=c(1,2,1))
modelo1
tsdiag(modelo1)
Box.test(residuals(modelo1), type = "Ljung-Box")
error=residuals(modelo1)
plot(error)

pronostico= forecast::forecast(modelo1, h = 10)
pronostico
plot(pronostico)

nnetar(Toneladas_Copacabana.ts)
fit = nnetar(Toneladas_Copacabana.ts)
fcast =forecast(fit)
plot(fcast)
