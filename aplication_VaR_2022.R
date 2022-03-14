#####################
#APLICACIÓN PRÁCTICA: Cálculo VaR
#####################

#The VaR is simply deffined as the expected maximum loss that may be incurred over
#a certain time horizon h and within a speciffied confidence interval given 
#the available information.

#Suppose that an investor holds a portfolio on stocks for which the VaR for the next day
#has been estimated in $ 1million with 99% confidence. Under normal circumstances, the
#investor should expect only a 1% chance for his or her portfolio to suffer a daily loss
#larger than this amount.

#Using the variance-covariance method, to compute the Value at Risk (VaR) for a specific confidence interval we
#need only to multiply the volatility estimation by the appropriate distribution quantile. We then empirically
#compute what percentage of the returns violate the VaR requirement. Ideally, the number of violations should match
#the VaR significance level (1 minus the confidence interval). Having a lower violation rate will usually imply having
#more money saved to maintain the portfolio than strictly needed, and having a lower VaR value (more violations)
#could increase the risk of not being able to cover portfolio losses.

install.packages("tseries")  
library(tseries)

install.packages("fBasics")  
library(fBasics)

install.packages("car")  
library(car)

install.packages("urca")  
library(urca)

install.packages("forecast")  
library(forecast)

install.packages("fGarch")
library(fGarch)

install.packages("rugarch")  
library(rugarch)

install.packages("quantmod")  #Este package es para importar los datos directamente de Yahho finance
library(quantmod)

install.packages("ggplot2") #para la estacionariedad de los modelos de la media
library(ggplot2)

install.packages("fTrading") #para EWMA 
library(fTrading)

getSymbols("^IBEX",from="1994-01-01", to="1999-12-30") 



# como quiero el cierre ajustado
dim(IBEX)         # <=== find the size of the data downloaded
head(IBEX)        # <=== show the first 6 rows of data
tail(IBEX)        #  <=== show the last 6 rows of data
ibex=IBEX[,6]



#################################
# Gráfico de la serie temporal###
#################################

plot(ibex)

#Como hay datos non-available, o los sustituimos por la observación anterior
ibex35<-na.locf(ibex, fromLast = TRUE) 
length(ibex35)
plot(ibex35)


rendibex <- diff(log(ibex35))
rendibex <- rendibex[-1]
length(rendibex)
plot(rendibex)

#############################
#predicción VaR#
############################


#using the variance-covariance method, Value at Risk (VaR) for a specific confidence interval is  calculated by multiplying the standard deviation by the appropriate percentile of the normal distribution
#para un horizonte temporal h=1, gjr-garch_tstd

spec1=ugarchspec(variance.model=list(model="gjrGARCH", garchOrder = c(1,1)), mean.model=list(armaOrder=c(1,0)), distribution.model = "std")
m1=ugarchfit(spec=spec1,data=rendibex)
forc = ugarchforecast(m1, n.ahead=1, n.roll= 0)
show(forc)
var5.gjrgarch <- - qnorm(0.95) * 0.008832 # equivalente a var5.gjrgarch <- qnorm(0.05) * 0.008832
show(var5.gjrgarch)
# lo  cual  significa  que  con  un 95%  de  confianza  la  máxima  perdida  esperada  para  mañana  en  el  IBEX35  es  un 1,45%.
#o que hay una probabilidad del 5% de que el rendimiento mañana sea inferior al -1.45%


#para un horizonte temporal h=1, garch_norm

spec1=ugarchspec(variance.model=list(model="sGARCH", garchOrder = c(1,1)), mean.model=list(armaOrder=c(1,0)), distribution.model = "norm")
m1=ugarchfit(spec=spec1,data=rendibex)
forc = ugarchforecast(m1, n.ahead=1, n.roll= 0)
show(forc)
var5.garch <- - qnorm(0.95) * 0.008976
show(var5.garch)

#para un horizonte temporal h=1 con rolling window, gjr-garch_tstd
spec1=ugarchspec(variance.model=list(model="gjrGARCH", garchOrder = c(1,1)), mean.model=list(armaOrder=c(1,0)), distribution.model = "std")
var.t = ugarchroll(spec1, data = rendibex, n.ahead = 1, forecast.length = 50, refit.every = 1,  refit.window = "rolling",
                   calculate.VaR = TRUE, VaR.alpha = 0.05)
plot(var.t, which = 4, VaR.alpha = 0.05)
report(var.t, VaR.alpha = 0.05)


#####################################
#comparación in sample ##
##################################
vol.ewma0.95 <- EWMA(rendibex^2, lambda = 0.05) # note: in EWMA lambda is actually 1-lambda
spec1=ugarchspec(variance.model=list(model="gjrGARCH", garchOrder = c(1,1)), mean.model=list(armaOrder=c(1,0)), distribution.model = "std")
m12=ugarchfit(spec=spec1,data=rendibex)
v = sigma(m12)
#con EWMA calculada CON LAMBDA=0.95
var5.ewma  <-  - qnorm(0.95) * sqrt(vol.ewma0.95)
var5.gjrgarch <- - qnorm(0.95) * (v)
var1.ewma  <-  - qnorm(0.99) * sqrt(vol.ewma0.95)
var1.gjrgarch <-  - qnorm(0.99) * v

Fechas<-as.Date(rownames(zoo(IBEX)))
Fechas<-Fechas[-1] 

par(mfrow=c(2,2), cex=0.6, mar=c(2,2,3,1))
plot(Fechas, rendibex,type="l", main ="5% VaR EWMA")
lines(Fechas, var5.ewma, col = "blue")
plot(Fechas, rendibex,type="l", main ="5% VaR GJR-GARCH(1,1)")
lines(Fechas,var5.gjrgarch, col ="blue")
plot(Fechas, rendibex,type="l", main ="1% VaR EWMA")
lines(Fechas, var1.ewma, col = "red")
plot(Fechas, rendibex, type="l", main ="1% VaR GJR-GARCH(1,1)")
lines(Fechas, var1.gjrgarch, col ="red")



#calculate the fraction of sample where loss exceed both 1% and 5% VaR for EWMA and GjR-GARCH(1,1)
sum(rendibex < var5.ewma)/length(rendibex) # fraction of sample where loss exceeds 5% VaR for EWMA

sum(rendibex < var5.gjrgarch)/length(rendibex) # fraction of sample where loss exceeds 5% VaR for gjrGARCH(1,1)

sum(rendibex < var1.ewma)/length(rendibex) # fraction of sample where loss exceeds 1% VaR for EWMA

sum(rendibex < var1.gjrgarch)/length(rendibex) # fraction of sample where loss exceeds 1% VaR for gjrGARCH(1,1)
# vemos que es parecido o igual en ambos casos (con lambda=0.95)

#con EWMA calculada CON LAMBDA=0.75
vol.ewma0.75 <- EWMA(rendibex^2, lambda = 0.25) # note: in EWMA lambda is actually 1-lambda

var5.ewma  <-  - qnorm(0.95) * sqrt(vol.ewma0.75)
var5.gjrgarch <-  - qnorm(0.95) * (v)
var1.ewma  <-  - qnorm(0.99) * sqrt(vol.ewma0.75)
var1.gjrgarch <-  - qnorm(0.99) * v

par(mfrow=c(2,2), cex=0.6, mar=c(2,2,3,1))
plot(Fechas, rendibex,type="l", main ="5% VaR EWMA")
lines(Fechas, var5.ewma, col = "blue")
plot(Fechas, rendibex,type="l", main ="5% VaR GJR-GARCH(1,1)")
lines(Fechas, var5.gjrgarch, col ="blue")
plot(Fechas, rendibex,type="l", main ="1% VaR EWMA")
lines(Fechas, var1.ewma, col = "red")
plot(Fechas, rendibex, type="l", main ="1% VaR GJR-GARCH(1,1)")
lines(Fechas, var1.gjrgarch, col ="red")

#es más suave el VaR calculado con GJR-Garch 

#calculate the fraction of sample where loss exceed both 1% and 5% VaR for EWMA and GJRGARCH(1,1)
sum(rendibex < var5.ewma)/length(rendibex) # fraction of sample where loss exceeds 5% VaR for EWMA

sum(rendibex < var5.gjrgarch)/length(rendibex) # fraction of sample where loss exceeds 5% VaR for gjrGARCH(1,1)

sum(rendibex < var1.ewma)/length(rendibex) # fraction of sample where loss exceeds 1% VaR for EWMA

sum(rendibex < var1.gjrgarch)/length(rendibex) # fraction of sample where loss exceeds 1% VaR for gjrGARCH(1,1)

#¿COMO ELEGIMOS LAMBDA? CON GARCH LOS DATOS HABLAN
#Ewma con lambda 0.75 sobreestima el riesgo y la compañia estaría dedicando demasiados recursos al capital regulatorio mínimo.

