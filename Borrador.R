
# LIBRERIAS ---------------------------------------------------------------
library(quantmod)
library(ggplot2)
#install.packages("TTR")
library(TTR)
library(dygraphs)
library(dplyr)
library(tidyverse)

# IMPORTACION DE DATOS  ---------------------------------------------------


# Importación de los datos necesarios 

activo <- getSymbols(Symbols = "MSFT",src = "yahoo", from="2020-01-01", to=Sys.Date(),
                     periodicity = "daily",auto.assign = FALSE)
chartSeries(activo,
            type="candlesticks",
            up.col = 'green',
            down.col = 'red',
            subset = "last 10 months",
            theme=chartTheme('white'))

addEMA(n=5,on=1,col = "blue")
addEMA(n=20,on=1,col = "red")
addEMA(n=200,on=1,col = "#FF0099")

addBBands(n=20,sd=2)
addMACD(fast=12,slow=26,signal=9,type="EMA")

dygraph(tail(activo,30)) %>%
  dyCandlestick()

# MEDIAS MOVILES EXPONENCIALES --------------------------------------------

# Construcción de la función de media móvil 


EMA <- function (price,n){
  ema <- c()
  ema[1:(n-1)] <- NA
  ema[n]<- mean(price[1:n])
  beta <- 2/(n+1)
  for (i in (n+1):length(price)){
    ema[i]<-beta * price[i] + 
      (1-beta) * ema[i-1]
  }
  ema <- reclass(ema,price)
  return(ema)
}

EMA_20 <- EMA(activo$MSFT.Close,20)
EMA_50 <- EMA(activo$MSFT.Close,50)
EMA_200<- EMA(activo$MSFT.Close,200)

signal_buy <- c()
for (i in 50:nrow(EMA_50)) {
  if ((EMA_20[i,1]>EMA_50[i,1])&&(EMA_20[i-1,1]<EMA_50[i-1,1])) {
    signal_buy[i] <- 1
  }else{
    signal_buy[i] <- 0
  }
}

signal_sell <- c()
for (i in 50:nrow(EMA_50)) {
  if ((EMA_20[i,1]<EMA_50[i,1])&&(EMA_20[i-1,1]>EMA_50[i-1,1])) {
    signal_buy[i] <- 1
  }else{
    signal_buy[i] <- 0
  }
}

x <- cbind(EMA_20,EMA_50,signal_buy)

filtro <- filter(as.data.frame(x),signal_buy==1)
filtro


dygraph(x)%>%
  dyRangeSelector()



# MACD --------------------------------------------------------------------

MACD <- function (precio,Short,Large,sig){
  MACD <- na.omit(EMA(precio,Short) - EMA(precio,Large))
  signal <- EMA(MACD,sig)
  tabla <- cbind(MACD,signal)
  colnames(tabla) <- c("MACD","signal")
  return(tabla)
}

MACD_data <- MACD(precio = activo$MSFT.Close,Short = 12,Large = 26,sig = 9)

dygraph(MACD_data)%>%dyRangeSelector()

cola <- tail(activo, n=90)
graph<-dygraph(OHLC(cola))
dyCandlestick(graph)
  

