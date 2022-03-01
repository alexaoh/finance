install.packages("quantmod")  #Este package es para importar los datos directamente de Yahho finance o la reserva federal
library(quantmod)

getSymbols("AAPL") #<=== get daily Apple stock data from Yahoo Finance
dim(AAPL)         # <=== find the size of the data downloaded
head(AAPL)        # <=== show the first 6 rows of data
tail(AAPL)        #  <=== show the last 6 rows of data
chartSeries(AAPL) #  <=== plot Apple daily closing stock prices with trading volume
#<== Daily closing prices do not adjusted for stock split. You can use adjusted closing price.
chartSeries(AAPL[,6]) #<== Column 6 of the object "AAPL" in R.
chartSeries(AAPL[,6],theme="white")  #<== Same as the previous command, but use "white" background for the plot.

getSymbols("AAPL",from="2005-01-03",to="2017-12-31") #  <== specify the data span
dim(AAPL)         # <=== find the size of the data downloaded
head(AAPL)        # <=== show the first 6 rows of data
tail(AAPL)        #  <=== show the last 6 rows of data
chartSeries(AAPL[,6],theme="white") 

getSymbols("UNRATE",src="FRED")#  <== Load U.S. monthly unemplyment rate from Federal Reserve Bank of St Louis.
#<== src stands for "source", FRED stands for Federal Reserve Economic Data.
dim(UNRATE)         # <=== find the size of the data downloaded
head(UNRATE)        # <=== show the first 6 rows of data
tail(UNRATE)        #  <=== show the last 6 rows of data
chartSeries(UNRATE) #<== plot the U.S. monthly unemployment rate

getSymbols("DEXUSEU",src="FRED")# <== Load Dollar versus Euro daily exchange rates from FRED.
chartSeries(DEXUSEU) #<== plot the daily dollar-euro exchange rates.

getSymbols("^VIX") #<== load daily VIX index
chartSeries(VIX)


