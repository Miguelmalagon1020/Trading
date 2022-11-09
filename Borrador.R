
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

addEMA(n=4,on=1,col = "blue")
addEMA(n=9,on=1,col = "red")
addEMA(n=18,on=1,col = "#FF0099")

addBBands(n=20,sd=2)
addMACD(fast=12,slow=26,signal=9,type="EMA")
