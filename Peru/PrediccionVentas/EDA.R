
library(data.table)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(lubridate)
library(stringr)

train <- fread('train.csv')


head(train)

# FECHA_BOLETO
hora <- str_sub(train$FECHA_BOLETO,-8,-1)
unique(hora)

# Se aprecia que todos tienen la misma hora, 00horas.
rm(hora)

# Validación de la información del PDF:
# fecha <- as.Date(train$FECHA_BOLETO)
train$FECHA_BOLETO <- ymd_hms(train$FECHA_BOLETO)
train$PERIODO <- format(train$FECHA_BOLETO,'%Y%m')

train[CODIGO_CONGLOMERADO_CLIENTE=='20102939795',
      .(MONTO_TOTAL_VENTAS = sum(MONTO_VENTA_SOL-MONTO_DESCUENTO_SOL)),
      by=.(CODIGO_CONGLOMERADO_CLIENTE,PERIODO)][order(PERIODO)]
 
train[abs(MONTO_VENTA_SOL)<abs(MONTO_DESCUENTO_SOL)]

df <- train[between(as.numeric(PERIODO),201701,201712),.N,by=CODIGO_CLIENTE]
df$N <- 1

