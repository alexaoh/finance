#####################################
#Instalación de packages y libraries# 
#####################################


install.packages("quantmod")  
library(quantmod)

install.packages("tseries")  
library(tseries)

install.packages("ggplot2") 
library(ggplot2)

install.packages("car")  
library(car)

install.packages("urca")  
library(urca)

install.packages("forecast")  
library(forecast)

install.packages("fGarch") 
library(fGarch)

install.packages("xts") #para cambiar las frecuencias de las series
library(xts)

#####################################
#Import the data from yahoo finance#
#####################################

#Download the data from Yahoo Finance

getSymbols("^IBEX",from="2000-01-01", to="2010-01-01")


# Check the data
dim(IBEX)         # <=== find the size of the data downloaded
head(IBEX)        # <=== show the first 6 rows of data
tail(IBEX)        #  <=== show the last 6 rows of data



# non-available data, either substitute them by the last observation
IBEX<-na.locf(IBEX, fromLast = TRUE) 


#or delete them
#IBEXN<-na.omit(IBEX)

#weekly freqüency
IBEXM=to.weekly(IBEX)
dim(IBEXM)

#Adjusted close
ibex=IBEXM[,6]
dim(ibex)

# Plot the series
plot(ibex,type="l",col="blue",main="IBEX35")


#########################################
#Analysis of stationarity               #
#########################################

#Informal tools: plot, acf, acpf and Ljung-Box test

win.graph(width=8,height=5)
par(mfrow=c(1,2),font=2,font.lab=4,font.axis=2,las=1) 
#c(2,1) means two rows and one column.
acf(ibex,ylim=c(-1,1),main="ibex")
pacf(ibex,ylim=c(-1,1),main="ibex")

dev.off()



Box.test(ibex, lag = 1, type = c("Ljung-Box"))
Box.test(ibex, lag = 5, type = c("Ljung-Box"))
Box.test(ibex, lag = 10, type = c("Ljung-Box"))
Box.test(ibex, lag = 15, type = c("Ljung-Box"))
Box.test(ibex, lag = 20, type = c("Ljung-Box"))




####################################################
#formal tests (unit root tests)                    #
####################################################

#############
#  ibex    #
#############


#ADF= función general: ur.df(x, type = c("none", "drift", "trend"), lags = 1, selectlags = c("Fixed", "AIC", "BIC"))  
# Lag selection can be achieved according to the Akaike "AIC" or the Bayes "BIC" information criteria. The maximum number of lags considered is set by lags.
# The default is to use a "fixed" lag length set by lags
# If type is set to "none" neither an intercept nor a trend is included in the test regression. If it is set to "drift" an intercept is added
# and if it is set to "trend" both an intercept and a trend is added

#price series

ibex.df<-ur.df(ibex, type = c("none"), selectlags = c("BIC")) 
summary(ibex.df) 
# Residual plot, acfs' and pacfs'.
plot(ibex.df)


ibex.df<-ur.df(ibex, type = c("none"), selectlags = c("AIC")) 
summary(ibex.df)  


# now we obtain tau2 and phi1. Tau2 is PSI=0. phi1 joint hypothesis that PSI=drift=0
ibex.df<-ur.df(ibex, type = c("drift"), selectlags = c("BIC"))
summary(ibex.df)  

# now we obtain tau3 y phi2, phi3. Tau3 is PSI=0. phi2 joint hypothesis rho=drift=trend=0. phi3 joint hypothesis rho=drift=trend=0
ibex.df<-ur.df(ibex, type = c("trend"), lags=2)
summary(ibex.df)  
# Residual plot, acfs' and pacfs'.
plot(ibex.df)

#with lag=1 there is autocorrelation
ibex.df<-ur.df(ibex, type = c("drift"), lags = 2)
summary(ibex.df)  
plot(ibex.df)

#calculate return series

rendibex=diff(log(ibex))
plot(rendibex)
rendibex<-rendibex[-1] #delete the first observation (NA)

rendibex.df<-ur.df(rendibex, type = c("none"), selectlags = c("BIC")) 
summary(rendibex.df)  
plot(rendibex.df)

#PP= función general: ur.pp(x, type = c("Z-alpha", "Z-tau"), model = c("constant", "trend"), lags = c("short", "long"), use.lag = NULL) # use.lag=NULL es para especificar el lag
# los resultados no cambian si se usa Z-alpha o Z-thau. z-tau devuelve los valores críticos
# lags="short" sets the number of lags to (4*(n/100))^(1/4), whereas lags="long" sets the number of lags to (12*(n/100))^(1/4). En apuntes: short

#price series

ibex.pp<-ur.pp(ibex, type = c("Z-tau"), model = c("constant"), lags = c("short"))
summary(ibex.pp)	

ibex.pp<-ur.pp(ibex, type = c("Z-tau"), model = c("constant"), lags = c("long"))
summary(ibex.pp)

#return series 

rendibex.pp<-ur.pp(diff(log(ibex)), type = c("Z-tau"), model = c("constant"), lags = c("short"))
summary(rendibex.pp)	

rendibex.pp<-ur.pp(diff(log(ibex)), type = c("Z-tau"), model = c("constant"), lags = c("long"))
summary(rendibex.pp)


#KPSS test= función general: ur.kpss(y, type = c("mu", "tau"), lags = c("short", "long", "nil"), use.lag = NULL) #The test types specify
#as deterministic component either a constant "mu" or a constant with linear trend "tau".If lags="nil" is choosen, then no error correction is made. Furthermore,
#one can specify a different number of maximum lags by setting use.lag accordingly.

# price series 

ibex.kpss<-ur.kpss(ibex, type = c("mu"), lags = c("short"))
summary(ibex.kpss)

ibex.kpss<-ur.kpss(ibex, type = c("mu"), lags = c("long"))
summary(ibex.kpss)

#return series 

rendibex.kpss<-ur.kpss(diff(log(ibex)), type = c("mu"), lags = c("short"))
summary(rendibex.kpss)

rendibex.kpss<-ur.kpss(diff(log(ibex)), type = c("mu"), lags = c("long"))
summary(rendibex.kpss)




#Descriptive statistics

sd(rendibex)
min(rendibex)
max(rendibex)
skewness(rendibex)
kurtosis(rendibex)
summary(rendibex)
normalTest(rendibex,method="jb")
basicStats(rendibex) 


# Histogram of returns with normal curve
win.graph(width=8,height=5)
hist(rendibex,breaks=20,freq=F, main = 'Histogram of returns')
curve(dnorm(x, mean=mean(rendibex), sd=sd(rendibex)), col=2, add=T)


win.graph(width=8,height=5)
qqnorm(rendibex)
qqline(rendibex, datax = FALSE)


#############################
#SIMULATION               ###
#############################

#Simulation AR2
y1=arima.sim(model=list(ar=c(1.3,-.4)),1000)
win.graph(width=8,height=5)
par(mfrow=c(1,2),font=2,font.lab=4,font.axis=2,las=1)
acf(y1,ylim=c(-1,1),main="y1")
pacf(y1,ylim=c(-1,1),main="y1")

#Simulation AR1
y2=arima.sim(model=list(ar=c(.8)),1000)
win.graph(width=8,height=5)
par(mfrow=c(1,2),font=2,font.lab=4,font.axis=2,las=1)
acf(y2,ylim=c(-1,1),main="y2")
pacf(y2,ylim=c(-1,1),main="y2")

#Simulation MA1
y3=arima.sim(model=list(ma=c(0.8)),1000)
win.graph(width=8,height=5)
par(mfrow=c(1,2),font=2,font.lab=4,font.axis=2,las=1)
acf(y3,ylim=c(-1,1),main="y3")
pacf(y3,ylim=c(-1,1),main="y3")

#Simulation ARMA(1,1)
y4=arima.sim(model=list(ar=c(.8),ma=c(0.8)),1000)
win.graph(width=8,height=5)
par(mfrow=c(1,2),font=2,font.lab=4,font.axis=2,las=1)
acf(y4,ylim=c(-1,1),main="y4")
pacf(y4,ylim=c(-1,1),main="y4")


######################################
#FASE 1: IDENTIFICATION OD THE MODEL #
######################################
#acf, acpf and Ljung-Box test

win.graph(width=8,height=5)
par(mfrow=c(1,2),font=2,font.lab=4,font.axis=2,las=1)
acf(rendibex,ylim=c(-1,1),main="rendibex")
pacf(rendibex,ylim=c(-1,1),main="rendibex")


Box.test(rendibex, lag = 1, type = c("Ljung-Box"))
Box.test(rendibex, lag = 2, type = c("Ljung-Box"))
Box.test(rendibex, lag = 3, type = c("Ljung-Box"))
Box.test(rendibex, lag = 4, type = c("Ljung-Box"))  
Box.test(rendibex, lag = 5, type = c("Ljung-Box"))
Box.test(rendibex, lag = 10, type = c("Ljung-Box"))
Box.test(rendibex, lag = 15, type = c("Ljung-Box"))
Box.test(rendibex, lag = 20, type = c("Ljung-Box"))

#################################
#FASE 2: ESTIMATION            ##
#################################


#ar(1) 
model = arima(rendibex, order = c(1,0,0),include.mean = TRUE)
model
pnorm(c(abs(model$coef)/sqrt(diag(model$var.coef))), mean=0, sd=1, lower.tail=FALSE)


model = arima(rendibex, order = c(1,0,0),include.mean = FALSE)
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
model4 = arima(rendibex, order = c(2,0,0),include.mean = FALSE)
model4 
pnorm(c(abs(model4$coef)/sqrt(diag(model4$var.coef))), mean=0, sd=1, lower.tail=FALSE)

#MA(2) 
model5 = arima(rendibex, order = c(0,0,2),include.mean = TRUE)
model5 
pnorm(c(abs(model5$coef)/sqrt(diag(model5$var.coef))), mean=0, sd=1, lower.tail=FALSE)


#INFORMATION CRITERIA 

#auto.arima, it uses aicc (corrected for small samples)
model6 = auto.arima(rendibex, ic="aic")#if we indicate the information criteria, we don't get the p-values
model6
pnorm(c(abs(model6$coef)/sqrt(diag(model6$var.coef))), mean=0, sd=1, lower.tail=FALSE)




####################
# FASE3: DIAGNOSIS
####################


#Mode stationarity 
#the roots of the characteristic polynomial lie outside the unitary circle
plot(model6) #Produces a plot of the INVERSE AR and MA roots of an ARIMA model



# Residual analysis
tsdiag(model6) 


win.graph(width=8,height=5)
par(mfrow=c(1,2),font=2,font.lab=4,font.axis=2,las=1)
acf(model6$residuals,ylim=c(-1,1),main="residuals arma(1,1)") 
pacf(model6$residuals,ylim=c(-1,1),main="residuals arma(1,1)")


qqnorm(model6$residuals)
qqline(model6$residuals, datax = FALSE)

plot(model6$residuals)
title (main="Plot of the residuals")
normalTest(model6$residuals,method="jb") #or jarque.bera.test(model$residuals)


