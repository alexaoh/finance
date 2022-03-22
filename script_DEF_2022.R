#####################################
#Instalaci?n de packages y libraries# 
#####################################

#install.packages("tseries")  
library(tseries)

#install.packages("fBasics")  
library(fBasics)

#install.packages("car")  
library(car)

#install.packages("urca")  
library(urca)

#install.packages("forecast")  
library(forecast)

#install.packages("fGarch")
library(fGarch)

#install.packages("rugarch")  
library(rugarch)

#install.packages("quantmod")  #Este package es para importar los datos directamente de Yahho finance
library(quantmod)

#install.packages("ggplot2") #para la estacionariedad de los modelos de la media
library(ggplot2)


#####################################
#importo los datos desde yahoo finance#
#####################################

getSymbols("^IBEX",from="1994-01-01", to="1999-12-30") 



# como quiero el cierre ajustado
dim(IBEX)         # <=== find the size of the data downloaded
head(IBEX)        # <=== show the first 6 rows of data
tail(IBEX)        #  <=== show the last 6 rows of data

# como quiero el cierre ajustado
ibex=IBEX[,6]



#################################
# Gr?fico de la serie temporal###
#################################

plot(ibex)

#Como hay datos non-available, o los sustituimos por la observaci?n anterior
ibex35<-na.locf(ibex, fromLast = TRUE) 
length(ibex35)
plot(ibex35)



#o los eliminamos
#ibex35<-na.omit(ibex)
#Estad?sticos b?sicos de las series


#########################################
#An?lisis de estacionariedad de la serie#
#########################################

#Pruebas informales: gr?fico de la serie, fac, facp y Ljung-Box test

win.graph(width=8,height=5)
par(mfrow=c(1,2),font=2,font.lab=4,font.axis=2,las=1) 
acf(ibex35,ylim=c(-1,1),main="ibex35")
pacf(ibex35,ylim=c(-1,1),main="ibex35")


Box.test(ibex35, lag = 1, type = c("Ljung-Box"))
Box.test(ibex35, lag = 5, type = c("Ljung-Box"))
Box.test(ibex35, lag = 10, type = c("Ljung-Box"))
Box.test(ibex35, lag = 15, type = c("Ljung-Box"))
Box.test(ibex35, lag = 20, type = c("Ljung-Box"))


#pruebas formales (contrastes de ra?ces unitarias)

#ADF= funci?n general: ur.df(x, type = c("none", "drift", "trend"), lags = 1, selectlags = c("Fixed", "AIC", "BIC"))  
# Lag selection can be achieved according to the Akaike "AIC" or the Bayes "BIC" information criteria. The maximum number of lags considered is set by lags.
# The default is to use a "fixed" lag length set by lags
# If type is set to "none" neither an intercept nor a trend is included in the test regression. If it is set to "drift" an intercept is added
# and if it is set to "trend" both an intercept and a trend is added

#series de precios

#CON CONSTANTE Y BIC
ibex35.df<-ur.df(ibex35, type = c("drift"), lags=20, selectlags = c("BIC"))
summary(ibex35.df)	
# Residual plot, acfs' and pacfs'.
plot(ibex35.df)

#CON CONSTANTE Y AIC
ibex35.df<-ur.df(ibex35, type = c("drift"), lags=20, selectlags = c("AIC"))
summary(ibex35.df)	

#Con CONSTANTE Y 2 LAGS
ibex35.df<-ur.df(ibex35, type = c("drift"), lags=2)
summary(ibex35.df)	
# Residual plot, acfs' and pacfs'.
plot(ibex35.df)


#CON TENDENCIA
ibex35.df<-ur.df(ibex35, type = c("trend"), lags=2)
summary(ibex35.df)


#series de rendimientos
rendibex=diff(log(ibex35))
plot(rendibex)
rendibex<-rendibex[-1] #eliminamos la primera observaci?n que ahora es NA porque si no, el contraste da un error


rendibex.df<-ur.df(rendibex, type = c("none"), lags=20, selectlags = c("BIC"))
summary(rendibex.df)	


# Residual plot, acfs' and pacfs'.
plot(rendibex.df)



#PP= funci?n general: ur.pp(x, type = c("Z-alpha", "Z-tau"), model = c("constant", "trend"), lags = c("short", "long"), use.lag = NULL) # use.lag=NULL es para especificar el lag
# los resultados no cambian si se usa Z-alpha o Z-thau o short o long
# ojo!!!! los valores cr?ticos son los tabulados por mackinnon que coinciden con los del ADF test (Z-tau los da pero Z-alpha no los da)  
# lags="short" sets the number of lags to (4*(n/100))^(1/4), whereas lags="long" sets the number of lags to (12*(n/100))^(1/4). 

#series de precios

ibex35.pp<-ur.pp(ibex35, type = c("Z-tau"), model = c("constant"), lags = c("long"))
summary(ibex35.pp)	

ibex35.pp<-ur.pp(ibex35, type = c("Z-tau"), model = c("trend"), lags = c("short"))
summary(ibex35.pp)

#series de rendimientos

rendibex.pp<-ur.pp(rendibex, type = c("Z-tau"), model = c("constant"), lags = c("short"))
summary(rendibex.pp)	



#KPSS test= funci?n general: ur.kpss(y, type = c("mu", "tau"), lags = c("short", "long", "nil"), use.lag = NULL) #The test types specify
#as deterministic component either a constant "mu" or a constant with linear trend "tau".If lags="nil" is choosen, then no error correction is made. Furthermore,
#one can specify a different number of maximum lags by setting use.lag accordingly.

#series de precios

ibex35.kpss<-ur.kpss(ibex35, type = c("mu"), lags = c("short"))
summary(ibex35.kpss)


#series de rendimientos

rendibex.kpss<-ur.kpss(rendibex, type = c("mu"), lags = c("short"))
summary(rendibex.kpss)




plot(rendibex)




#Estad?sticos b?sicos de las series

sd(rendibex)
min(rendibex)
max(rendibex)
skewness(rendibex)
kurtosis(rendibex)
summary(rendibex)
normalTest(rendibex,method="jb")
basicStats(rendibex) #esta es la opci?n m?s completa

win.graph(width=8,height=5)
hist(rendibex,breaks=20,freq=F, main = 'Histograma de los rendimientos')
curve(dnorm(x, mean=mean(rendibex), sd=sd(rendibex)), col=2, add=T)

#FASE 1: IDENTIFICACI?N DEL MODELO

#fac, facp y Ljung-Box test

win.graph(width=8,height=5)
par(mfrow=c(1,2),font=2,font.lab=4,font.axis=2,las=1)
acf(rendibex,ylim=c(-1,1),main="rendibex35")
pacf(rendibex,ylim=c(-1,1),main="rendibex35")


#FASE 2: ESTIMACI?N DEL MODELO


#ar(1)
model = arima(rendibex, order = c(1,0,0),include.mean = TRUE)
model
pnorm(c(abs(model$coef)/sqrt(diag(model$var.coef))), mean=0, sd=1, lower.tail=FALSE)

#ma(1)
model2 = arima(rendibex, order = c(0,0,1),include.mean = TRUE)
model2 
pnorm(c(abs(model2$coef)/sqrt(diag(model2$var.coef))), mean=0, sd=1, lower.tail=FALSE)

#arma(1,1) 
model3 = arima(rendibex, order = c(1,0,1),include.mean = TRUE)
model3 
pnorm(c(abs(model3$coef)/sqrt(diag(model3$var.coef))), mean=0, sd=1, lower.tail=FALSE)

#ar(2) 
model4 = arima(rendibex, order = c(2,0,0),include.mean = TRUE)
model4 
pnorm(c(abs(model4$coef)/sqrt(diag(model4$var.coef))), mean=0, sd=1, lower.tail=FALSE)

#MA(2) 
model5 = arima(rendibex, order = c(0,0,2),include.mean = TRUE)
model5 
pnorm(c(abs(model5$coef)/sqrt(diag(model5$var.coef))), mean=0, sd=1, lower.tail=FALSE)



# FASE3: DIAGNOSIS

#Estacionariedad del modelo 
#todas las ra?ces del polinomio caracter?stico deben caer fuera del c?rculo unitario



plot(model4) #Produces a plot of the inverse AR and MA roots of an ARIMA model



# An?lisis de los residuos
tsdiag(model4) #dibuja los residuos estandarizados, la ACF de los residuos y los pvalues del Ljung-Box test


win.graph(width=8,height=5)
par(mfrow=c(1,2),font=2,font.lab=4,font.axis=2,las=1)
acf(model4$residuals,ylim=c(-1,1),main="residuosar2") #vemos que solo el 10 es significativo
pacf(model4$residuals,ylim=c(-1,1),main="residuosar2")

# vemos si tenemos heavy tails
qqnorm(model4$residuals)
qqline(model4$residuals, datax = FALSE)


plot(model4$residuals)
title (main="Gr?fico de los residuos")
normalTest(model4$residuals,method="jb")


# TESTS GARCH Y FASE1 DE IDENTIFICACI?N DEL MODELO GARCH

# ACF y PACF de los residuos al cuadrado 
residuos=model4$residuals
residuos2=residuos^2

win.graph(width=8,height=5)
par(mfrow=c(1,2),font=2,font.lab=4,font.axis=2,las=1)
acf(residuos2,ylim=c(-1,1),main="residuos al cuadrado") 
pacf(residuos2,ylim=c(-1,1),main="residuos al cuadrado")


Box.test(residuos2,lag=1,type='Ljung')
Box.test(residuos2,lag=5,type='Ljung')
Box.test(residuos2,lag=15,type='Ljung')
Box.test(residuos2,lag=20,type='Ljung')

# FASE2 y 3: ESTIMACI?N DEL MODELO GARCH Y DIAGNOSIS


#OJO! Mirar condiciones de estacionariedad y positividad
#garchOrder podemos aumentar el orden del Garch, normalmente ser? (1,1) pero excepcionalmente podemos necesitar aumentar el orden

#Ejemplo AR(2)-GARCH(1,1) con distribuci?n Normal
spec1=ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1)), mean.model=list(armaOrder=c(2,0)), distribution.model = "norm")
m4=ugarchfit(spec=spec1,data=rendibex)
m4 #observamos que el par?metro AR(2) ha dejado de ser significativo


#Ejemplo AR(1)-GARCH(1,1) con distribuci?n Normal
spec1=ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1)), mean.model=list(armaOrder=c(1,0)), distribution.model = "norm")
m4=ugarchfit(spec=spec1,data=rendibex)
m4 


#obtenci?n residuos 
#obtenci?n de los residuos
plot(m4) 

#obtenci?n de la volatilidad estimada 
#opci?n 1 
v = sigma(m4)#para obtener la volatilidad estimada, 
v_anualizada=(250)^0.5*v
plot(v_anualizada)
write.table(v_anualizada,file = "volatility.csv", sep = ";")



#opci?n 2 (volatilidad y rendimientos juntos)

par(mfcol=c(2,1))  # Show volatility and returns
plot(v_anualizada)
plot(rendibex) 
dev.off()

#t-Student

spec1=ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1)), mean.model=list(armaOrder=c(1,0)), distribution.model = "std")
m6=ugarchfit(spec=spec1,data=rendibex)
m6

plot(m6)
#obtenci?n de la volatilidad estimada 

#opci?n 1 
v = sigma(m6)#para obtener la volatilidad estimada, 
v_anualizada=(250)^0.5*v
plot(v_anualizada)
write.table(v_anualizada,file = "volatility.csv", sep = ";")

#opci?n 2 (volatilidad y rendimientos juntos)

par(mfcol=c(2,1))  # Show volatility and returns
plot(v_anualizada)
plot(rendibex) 


#M?s modelos de volatilidad 

# GARCH-M  CON ugarchspec. archpow=1 utiliza la desviaci?n est?ndar. archpow=2 utiliza la varianza
spec1=ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1)), mean.model=list(armaOrder=c(1,0), archm=TRUE, archpow=1), distribution.model = "std")
m7=ugarchfit(spec=spec1,data=rendibex)
m7



#Egarch 
#OJO, el coeficiente de la asimetr?a es el alpha1, mirar documentaci?n INTRODUCTORIA del paquete rugarch

#distribuci?n normal
spec1=ugarchspec(variance.model=list(model="eGARCH", garchOrder = c(1,1)),mean.model=list(armaOrder=c(1,0)), distribution.model = "norm")
m8=ugarchfit(spec=spec1,data=rendibex)
m8

#distribuci?n t-student
spec1=ugarchspec(variance.model=list(model="eGARCH", garchOrder = c(1,1)), mean.model=list(armaOrder=c(1,0)), distribution.model = "std")
m9=ugarchfit(spec=spec1,data=rendibex)
m9
plot(m9)


#GJR-GARCH model 

# distribuci?n normal
spec1=ugarchspec(variance.model=list(model="gjrGARCH", garchOrder = c(1,1)), mean.model=list(armaOrder=c(1,0)), distribution.model = "norm")
m10=ugarchfit(spec=spec1,data=rendibex)
m10

# COMMENTS FROM CLASS: Look at the information criteria (Akaika, Bayes, etc) in order to choose one model over others. 
# Should also look at the residuals, which are given in the output from the model as well. 
# p-values of Ljung-Box Test on standardized residuals larger than 0.05 --> not autocorrelation in the residuals. 
# Could also look at the squared residuals. 
# The residuals should not present autocorrelation --> if they do; we have left something in the data unmodelized. If they are 
# uncorrelated we seem to have modelled the data in a good way. 

# alternativa rugarch, distribuci?n student, este es el mejor
spec1=ugarchspec(variance.model=list(model="gjrGARCH", garchOrder = c(1,1)), mean.model=list(armaOrder=c(1,0)), distribution.model = "std")
m11=ugarchfit(spec=spec1,data=rendibex)
m11
plot(m11)
# 10 or 11: We do not want them to be autocorrelated, since we want to model the signal and not the white noise in the time series. 

#obtenci?n de los residuos
resi=residuals(m11,standardize=T) # Standardized residuals
par(mfcol=c(2,1)) # Obtain ACF & PACF
acf(resi,lag=24)
acf(resi^2,lag=24)


#obtenci?n de la volatilidad estimada 
#opci?n 1 
v = sigma(m11)#para obtener la volatilidad estimada, 
v_anualizada=(250)^0.5*v
plot(v_anualizada)
write.table(v_anualizada,file = "volatility.csv", sep = ";")
dev.off()


#opci?n 2 (volatilidad y rendimientos juntos)

par(mfcol=c(2,1))  # Show volatility and returns
plot(v_anualizada)
plot(rendibex) 





#COMPARARaci?n VOLATILIDAD CON LOS RENDIMIENTOS en valor absoluto

returnsabs=abs(rendibex)
par(mfcol=c(2,1))  # Show volatility and returns
plot(v_anualizada)
plot(returnsabs) 

dev.off()

time = data.frame(returnsabs, v)
ts.plot(time,gpars= list(xlab="time", ylab=",", col = 1:ncol(time)))
legend("topleft", c("returnsabs","v"), lty=c(1,1), col=c("black","red"), cex=0.6)

#AN?LISIS ADICIONALES


###################
#news impact curve# 
###################
# note that newsimpact does not require the residuals (z) as it
# will discover the relevant range to plot against by using the min/max
# of the fitted residuals.
ni=newsimpact(z = NULL, m11)
plot(ni$zx, ni$zy, ylab=ni$yexpr, xlab=ni$xexpr, type="l", main = "News Impact Curve")


#######################
#historical volatility# 
#######################

install.packages("fTrading")  
library(fTrading)

#primero le doy formato 
Fechas<-as.Date(rownames(zoo(IBEX)))
Fechas<-Fechas[-1] #eliminamos la primera observaci?n de Fechas, la hemos perdido al calcular los rendimientos

vol.hist20 <- SMA(rendibex^2, n=20) 
Fechas2<-Fechas[21:1563]#hemos perdido las primeras 20 observaciones para calcular la primera varianza. A?adimos una observaci?n m?s dado que con las 20 ?ltimas observaciones calculamos la volatilidad 1 paso hacia adelante.
plot(Fechas2, vol.hist20, type="l", ylab='variance', main='1 month moving average')


vol.hist80 <- SMA(rendibex^2, n=80) 
Fechas3<-Fechas[81:1563]
plot(Fechas3, vol.hist80, type="l", ylab='variance', main='4 month moving average')

vol.hist160 <- SMA(rendibex^2, n=160) 
Fechas4<-Fechas[161:1563]
plot(Fechas4, vol.hist160, type="l", ylab='variance', main='8 month moving average')

vol.hist240 <- SMA(rendibex^2, n=240) 
Fechas5<-Fechas[241:1563]
plot(Fechas5, vol.hist240, type="l", ylab='variance', main='1 year moving average')

par(mfrow=c(2,2), cex=0.6, mar=c(2,2,3,1))
plot(Fechas2, vol.hist20, type="l", ylab='variance', main='1 month moving average')
plot(Fechas3, vol.hist80, type="l", ylab='variance', main='4 month moving average')
plot(Fechas4, vol.hist160, type="l", ylab='variance', main='8 month moving average')
plot(Fechas5, vol.hist240, type="l", ylab='variance', main='1 year moving average')

###################
#Exponential Weighted Moving Average# 
###################

vol.ewma0.95 <- EWMA(rendibex^2, lambda = 0.05) # note: in EWMA lambda is actually 1-lambda
plot(Fechas, vol.ewma0.95, type="l", ylab='variance', main='EWMA 0.95')

vol.ewma0.75 <- EWMA(rendibex^2, lambda = 0.25) # note: in EWMA lambda is actually 1-lambda
plot(Fechas, vol.ewma0.75, type="l", ylab='variance', main='EWMA 0.75')

vol.ewma0.5 <- EWMA(rendibex^2, lambda = 0.5) # note: in EWMA lambda is actually 1-lambda
plot(Fechas, vol.ewma0.5, type="l", ylab='variance', main='EWMA 0.5')

vol.ewma0.25 <- EWMA(rendibex^2, lambda = 0.75) # note: in EWMA lambda is actually 1-lambda
plot(Fechas, vol.ewma0.25, type="l", ylab='variance', main='EWMA 0.25')

par(mfrow=c(2,2), cex=0.6, mar=c(2,2,3,1))
plot(Fechas, vol.ewma0.95, type="l", ylab='variance', main='EWMA 0.95')
plot(Fechas, vol.ewma0.75, type="l", ylab='variance', main='EWMA 0.75')
plot(Fechas, vol.ewma0.5, type="l", ylab='variance', main='EWMA 0.5')
plot(Fechas, vol.ewma0.25, type="l", ylab='variance', main='EWMA 0.25')

dev.off()
variance=v^2
comparison = data.frame(variance, vol.ewma0.95)
ts.plot(comparison,gpars= list(ylab=",", col = 1:ncol(comparison)))
legend("topleft", c("GJR-GARCH","EWMA 0.95"), lty=c(1,1), col=c("black","red"))

variance=v^2
variance2=variance[21:1562]
volhist=vol.hist20[1:1542]
comparison = data.frame(variance2, volhist)
ts.plot(comparison,gpars= list(ylab=",", col = 1:ncol(comparison)))
legend("topleft", c("GJR-GARCH","1 month moving average"), lty=c(1,1), col=c("black","red"))

########################
#regresores adicionales# 
########################

#script regresores adicionales



#PREDICCI?N

getSymbols("^IBEX",from="1994-01-01", to="2000-01-01") 

ibex=IBEX[,6] #cierre ajustado
length(ibex)
ibex35<-na.locf(ibex, fromLast = TRUE) 
length(ibex35)
plot(ibex35,type="l",col="blue",main="Prices")

rendibex <- dailyReturn(ibex35)
plot(rendibex,type="l",col="blue",main="returns")


spec1=ugarchspec(variance.model=list(model="gjrGARCH", garchOrder = c(1,1)), mean.model=list(armaOrder=c(1,0)), distribution.model = "std")
m12=ugarchfit(spec=spec1,data=rendibex, out.sample = 10)#out of sample son las observaciones que deja fuera para comparar, no las utiliza para estimar
forc = ugarchforecast(m12, n.ahead=10, n.roll= 0) #esto hace predicci?n desde T diez pasos hacia adelante (es decir: desde T predice T+1, T+2, ....)
show(forc) #SERIES ES LA PREDICCI?N DE LA MEDIA Y SIGMA DE LA DESVIACI?N T?PICA
fpm(forc) #forecast performance measures. It requires at least 5 points to calculate the summary measures else will return NA. Son de la media
m12@model$modeldata$T#dice cuantos datos ha utilizado para estimar el modelo
plot(forc)#si no hacemos rolling, solo tiene sentido la opci?n 1 y 3. ojo

uncvariance(m12)^0.5

spec1=ugarchspec(variance.model=list(model="gjrGARCH", garchOrder = c(1,1)), mean.model=list(armaOrder=c(1,0)), distribution.model = "norm")
m13=ugarchfit(spec=spec1,data=rendibex, out.sample = 10)#out of sample son las observaciones que deja fuera para comparar, no las utiliza para estimar
forc = ugarchforecast(m13, n.ahead=1, n.roll= 10) # REESTIMA EL MODELO CADA VEZ!!! (n.ahead son los pasos adelante que calcula cada vez, es decir T+1, T+2... 
#Y N.ROLL EL NUMERO DE VECES QUE REESTIMA, EN ESTE CASO RESTIMA 10 VECES Y CADA VEZ QUE REESTIMA PREDICE PARA 1 PASOS HACIA ADELANTE. sI N.ROLL=1 SOLO REESTIMA 1 VEZ).

fpm(forc) #forecast performance measures. It requires at least 5 points to calculate the summary measures else will return NA
sigma(forc) #predicciones de la desviaci?n t?pica con rolling window, para cada T hace 1 paso hacia adelante
fitted(forc)#predicciones de la media con rolling window, para cada T hace 1 paso para adelante
plot(forc)



spec1=ugarchspec(variance.model=list(model="gjrGARCH", garchOrder = c(1,1)), mean.model=list(armaOrder=c(1,0)), distribution.model = "norm")
m13=ugarchfit(spec=spec1,data=rendibex, out.sample = 100)#out of sample son las observaciones que deja fuera para comparar, no las utiliza para estimar
forc = ugarchforecast(m13, n.ahead=1, n.roll= 100) # REESTIMA EL MODELO CADA VEZ!!! (n.ahead son los pasos adelante que calcula cada vez, es decir T+1, T+2... 
#Y N.ROLL EL NUMERO DE VECES QUE REESTIMA, EN ESTE CASO RESTIMA 100 VECES Y CADA VEZ QUE REESTIMA PREDICE PARA 1 PASOS HACIA ADELANTE. sI N.ROLL=1 SOLO REESTIMA 1 VEZ).

fpm(forc) #forecast performance measures. It requires at least 5 points to calculate the summary measures else will return NA
sigma(forc) #predicciones de la desviaci?n t?pica con rolling window, para cada T hace 1 paso hacia adelante
fitted(forc)#predicciones de la media con rolling window, para cada T hace 1 paso para adelante
plot(forc)
#plot(forc,which="all") #al dibujar el rolling lo que dibuja es la predicci?n a T+1 hecha en cada T



##############################
#modelos GARCH multivariantes#
##############################

##########################
#modelo DCC multivariante# 
##########################
#como no bajo los datos de quantmode, dar? un error en las fechas

#Instalamos paquete
#install.packages("rmgarch")
library(rmgarch)

#Importamos datos
datos<-read.table("bonos2.txt", header=TRUE, sep="")
names(datos)
attach(datos)
# Conversi?n de fecha a formato fecha
date<-as.Date(datos$Date,"%d/%m/%Y")  

# Gr?fico de la serie temporal
plot(date,Austria,type="l",col="blue", ylim=c(50,270),main="Prices")
lines(date,Grecia,type="l",col="red")
legend("bottomleft", c("Austria","Grecia"), lty=c(1,1), col=c("blue","red"))


#C?lculo de rendimientos
rendaustria <- diff(log(Austria))
rendgrecia <- diff(log(Grecia))

rend1=rendaustria
rend2=rendgrecia

#Dibujo de rendimientos
Date<-date[-1] 
plot(Date,rend1,type="l",col="blue",main="returns")
plot(Date,rend2,type="l",col="blue",main="returns")


# Combinamos los rendimientos en un vector
returns = cbind(rend1,rend2) 


# dcc specification - GARCH(1,1) for conditional correlations con diferentes garch para cada serie, en alg?n caso no es modelo asim?trico
spec1 = ugarchspec(mean.model = list(armaOrder = c(1,0)), variance.model = list(garchOrder = c(1,1), model = "sGARCH"), distribution.model = "std") 
spec2 = ugarchspec(mean.model = list(armaOrder = c(1,0)), variance.model = list(garchOrder = c(1,1), model = "gjrGARCH"), distribution.model = "std") 
dcc.garch11.spec = dccspec(uspec = multispec(c(spec1, spec2)), dccOrder = c(1,1), distribution = "mvnorm")
dcc.fit = dccfit(dcc.garch11.spec, data = returns)
dcc.fit

# dcc specification - GARCH(1,1) for conditional correlations, con id?ntico garch para cada serie
dcc.garch11.spec2 = dccspec(uspec = multispec( replicate(2, spec1) ), dccOrder = c(1,1), distribution = "mvnorm")
dcc.fit2 = dccfit(dcc.garch11.spec2, data = returns)
dcc.fit2


# dcc specification - GARCH(1,1) for conditional correlations, con id?ntico garch para cada serie y distribuci?n t-student
dcc.garch11.spec3 = dccspec(uspec = multispec( replicate(2, spec1) ), dccOrder = c(1,1), distribution = "mvt")
dcc.fit3 = dccfit(dcc.garch11.spec3, data = returns)
dcc.fit3


#dcc.fit2@model$sigma #muestra las varianzas estimadas
write.table(dcc.fit3@model$sigma,file="varianzas.csv")



plot(dcc.fit3)
# Make a plot selection (or 0 to exit): 
#   
# 1:   Conditional Mean (vs Realized Returns)
# 2:   Conditional Sigma (vs Realized Absolute Returns) THIS IS INTERESTING TO LOOK AT
# IN THE PLOT: WE WANT TO SEE THAT THE BEHAVIOUR IS SIMILAR, CANNOT LOOK AT THE ABSOLUTE VALUES (SIZES).
# Blue: Std estimated from the model.
# Grey: Rendimientos absolute value (unbiased estimator of A QUANTITY WHICH I CANNOT REMEMBER (probably the standard deviation))
# 3:   Conditional Covariance
# 4:   Conditional Correlation  THIS IS INTERESTING TO LOOK AT
# IN THE PLOT: Estimated correlation from the model. 
# 5:   EW Portfolio Plot with conditional density VaR limits


# extracting correlation series


cor1 = rcor(dcc.fit3) 
dim(cor1) #This tells us that cor1 stores 1908 (2?2) correlation matrices, one for each day of data. 
cor1 #
rcor(dcc.fit3)[,,1] #muestra el primer elemento. Vemos que es una matriz 2x2

plot(Date,rcor(dcc.fit3)[1,2,], type='l', main="Correlaci?n Austria_Grecia")
write.table(rcor(dcc.fit3)[1,2,],file="correlaciones.csv")


par(mfcol=c(2,1))
plot(date,Austria,type="l",col="blue", ylim=c(50,270),main="Prices")
lines(date,Grecia,type="l",col="red")
legend("bottomleft", c("Austria","Grecia"), lty=c(1,1), col=c("blue","red"))
plot(Date,rcor(dcc.fit3)[1,2,], type='l', main="Correlaci?n Austria_Grecia")

dev.off()

#superf?cie de impacto a las noticias#
nisurface(dcc.fit3, type="cor")


#n-ahead steps forecasting
dcc.fit3 = dccfit(dcc.garch11.spec3, data = returns)

dccf1 <- dccforecast(dcc.fit3, n.ahead = 10)



Rf <- dccf1@mforecast$R    # use H instead of R for the covariance forecast
show(Rf) 
str(Rf) #you realise that the object Rf is a list with one element. It turns out that this one list item is then a 2 dimensional matrix/array which contains the the 10 forecasts of 2?2 correlation matrices.

Hf <- dccf1@mforecast$H #variance-covariance matrix forecasts
show(Hf)

corf <- Rf[[1]][1,2,]  # Correlation forecasts between bonds. [ [1] ] tells R to go to the first (and here only) list item and then [1,2,] instructs R to select the (1,2) element of all available correlation matrices.
plot(corf, main="Predicci?n de la correlaci?n")

#otro ejemplo
rm(list = ls())
getSymbols("AAPL",src="yahoo",from="2006-05-31",to="2016-05-27")  #Obtenemos una muestra de 10 a?os de la serie de precios de Apple.
getSymbols("MSFT",src="yahoo",from="2006-05-31",to="2016-05-27")  

AAPL=AAPL[,6]
head(AAPL)
plot(AAPL)
dim(AAPL)

MSFT=MSFT[,6]
head(MSFT)
plot(MSFT)
dim(MSFT)
rAAPL <- dailyReturn(AAPL)
rMSFT <- dailyReturn(MSFT)


rX=cbind(rAAPL, rMSFT)

# dcc specification - GARCH(1,1) for conditional correlations, con id?ntico garch para cada serie
spec1 = ugarchspec(mean.model = list(armaOrder = c(0,0)), variance.model = list(garchOrder = c(1,1), model = "sGARCH"), distribution.model = "std") 
dcc.garch11.spec2 = dccspec(uspec = multispec( replicate(2, spec1) ), dccOrder = c(1,1), distribution = "mvnorm")
dcc.fit2 = dccfit(dcc.garch11.spec2, data = rX)
dcc.fit2

cor1 = rcor(dcc.fit2) 
dim(cor1) #This tells us that cor1 stores 2516 (2?2) correlation matrices, one for each day of data. 
cor1 #Let's have a look at the correlation matrix 

cor_AM <- cor1[1,2,]   # leaving the last dimension empty implies that we want all elements
cor_AM <- as.xts(cor_AM)  # imposes the xts time series format - useful for plotting. If you transformed cor_BG to be a xts series the plot function automatically picks up the date information.
plot(cor_AM)

write.table(cor_AM,file="correlaciones.csv")

##n-ahead steps forecasting 
dcc.fit2 = dccfit(dcc.garch11.spec2, data = rX)


dccf1 <- dccforecast(dcc.fit2, n.ahead = 10)



Rf <- dccf1@mforecast$R    # use H for the covariance forecast
show(Rf) 
str(Rf) #you realise that the object Rf is a list with one element. It turns out that this one list item is then a 3 dimensional matrix/array which contains the the 10 forecasts of 2?2 correlation matrices.


corf <- Rf[[1]][1,2,]  # Correlation forecasts between bonds. [ [1] ] tells R to go to the first (and here only) list item and then [1,2,] instructs R to select the (1,2) element of all available correlation matrices.
plot(corf)


c_AM <- c(tail(cor1[1,2,],20),rep(NA,10))  # gets the last 20 correlation observations
cf_AM <- c(rep(NA,20),corf) # gets the 10 forecasts
plot(c_AM,type = "l",ylim=c(0.20,0.36), main="Correlation Apple and Microsoft")
lines(cf_AM,type = "l", col = "orange")


#rolling window forecasting
dcc.fit2 = dccfit(dcc.garch11.spec2, data = rX, out.sample=11)

dccf1 <- dccforecast(dcc.fit2, n.ahead = 1, n.roll=10)



Rf <- dccf1@mforecast$R    
show(Rf) 
str(Rf) #you realise that the object Rf is a list with eleven elements. 

