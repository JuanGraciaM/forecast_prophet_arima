#ARIMA en R para BTC (mensual - Open)



#Instalar librerías
#install.packages(“readxl”)
#install.packages(“tseries”)
#install.packages(“astsa”)
#install.packages(“forecast”)
#install.packages(“tidyverse”)
#install.packages(“lubridate”)
#install.packages(“foreign”)
#install.packages(“quantmod”)



#Cargar librerias
library(lubridate)
library(tseries)
library(tidyverse)
library(astsa)
library(forecast)
library(readxl)



#Importamos excel
Arimar <- read_excel("Arimar.xlsx")
View(Arimar)



#Attachment
attach(Arimar)
names(Arimar)



#Precio BTC mensual y empieza en 201501 (hasta 202201)
Arimar.ts=ts(Arimar, start=c(2017,1), frequency = 12)
Arimar.ts
plot(Arimar.ts)



#Pruebo estacionar la serie con log
serielog=log(Arimar.ts)
serielog
plot(serielog)
adf.test(serielog,alternative = "stationary")
#p-value = 0.1231>0.05



#Estacionamos la serie con una diferencia
seriedif=diff(Arimar.ts)
seriedif
plot(seriedif)
adf.test(seriedif)
#p-value 0.04835<0.05



#Estacionamos la serie con dos diferencias
seriedif2=diff(Arimar.ts,differences=2)
seriedif2
plot(seriedif2)
adf.test(seriedif2,alternative = "stationary")
#p-value = 0.01<0.05; p-value smaller than printed p-value



#Usamos (p,1,q)
acf(seriedif)
pacf(seriedif)
par(mfrow=c(3,1), mar=c(4,4,4,1)+.1)
plot(seriedif,type="o",lty= "dashed",col="blue",main="Serie de Tiempo BTC (201701-202201)")
#Le ponemos la misma fercuencia a los dos gráficos
acf(ts(seriedif, frequency = 1))
pacf(ts(seriedif, frequency = 1))



#Generamos ARIMA(1,1,1)
modelo1=arima(Arimar.ts,order=c(1,1,1))
modelo1
tsdiag(modelo1)
Box.test(residuals(modelo1),type = "Ljung-Box")
#p-value = 0.8611>0.05 hay ruido blanco, el modelo se ajusta bien



#Generamos ARIMA(1,1,2)
modelo2=arima(Arimar.ts,order=c(1,1,2))
modelo2
tsdiag(modelo2)
Box.test(residuals(modelo2),type = "Ljung-Box")
#p-value = 0.9593>0.05 hay ruido blanco, el modelo se ajusta bien



#Generamos ARIMA(3,1,2)
modelo3=arima(Arimar.ts,order=c(3,1,2))
modelo3
tsdiag(modelo3)
Box.test(residuals(modelo3),type = "Ljung-Box")
#p-value 0.9124>0.05 hay ruido blanco, el modelo se ajusta bien



#Error con media cero varianza constante y no serialemtne correlacionada
error1=residuals(modelo1)
plot(error1)
error2=residuals(modelo2)
plot(error2)
error3=residuals(modelo3)
plot(error3)



#Pronóstico
pronostico1<- forecast::forecast(modelo1, h = 5)
pronostico2<- forecast::forecast(modelo2, h = 5)
pronostico3<- forecast::forecast(modelo3, h = 5)
pronostico1
pronostico2
pronostico3
plot(pronostico1)
plot(pronostico2)
plot(pronostico3)