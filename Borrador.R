
# LIBRERIAS ---------------------------------------------------------------
library(quantmod)
library(ggplot2)
install.packages("TTR")
library(TTR)
# IMPORTACION DE DATOS  ---------------------------------------------------


# Importación de los datos necesarios 

activo <- getSymbols(Symbols = "MSFT",src = "yahoo", from="2020-01-01", to=Sys.Date(),
                     periodicity = "daily",auto.assign = FALSE)
chartSeries(activo,
            type="candlesticks",
            up.col = 'green',
            down.col = 'red',
            subset = "last 4 months",
            theme=chartTheme('white'))

addEMA(n=20,on=1,col = "blue")
addEMA(n=50,on=1,col = "red")
addEMA(n=18,on=1,col = "#FF0099")

addBBands(n=20,sd=2)
addMACD(fast=12,slow=26,signal=9,type="EMA")



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

signal <- c()
for (i in 50:nrow(EMA_50)) {
  if (EMA_20[i,1]>EMA_50[i,1]) {
    signal[i] <- 1
  }else{
    signal[i] <- 0
  }
}

x <- cbind(EMA_20,EMA_50)
x



# MACD --------------------------------------------------------------------

MACD <- function (precio,Short,Large,sig){
  MACD <- na.omit(EMA(precio,Short) - EMA(precio,Large))
  signal <- EMA(MACD,sig)
  tabla <- cbind(MACD,signal)
  colnames(tabla) <- c("MACD","signal")
  return(tabla)
}

MACD(precio = activo$MSFT.Close,Short = 12,Large = 26,sig = 9)
