#############################################################################################

# Ejemplos prácticos de Series Temporales en R
# - por Martin Vedani, UTN Business Intelligence

############################################################################################

# Instalar y cargar los paquetes que utilizaremos
if(! "forecast" %in% installed.packages()) install.packages("forecast", depend = TRUE)
if(! "zoo" %in% installed.packages()) install.packages("zoo", depend = TRUE)
if(! "timeDate" %in% installed.packages()) install.packages("timeDate", depend = TRUE)
if(! "ggplot2" %in% installed.packages()) install.packages("ggplot2", depend = TRUE)

## Cargar los paquetes y los datos a utilizar en la sesión actual de R

library(forecast)
library(zoo)
library(ggplot2)

# Series de tiempo y pronósticos

# R cuenta con amplias capacidades para el análisis de datos de series de tiempo. En esta
# sección veremos brevemente la creación de una serie de tiempo, su descomposición 
# estacional, modelado con modelos exponencial y ARIMA, y pronósticos con el paquete 
# forecast.

# Cargar datos de los objetos a utilizar
gripes <- read.csv("datosBaseGripes.csv", header = T)

# o

gripes  <- read.csv(file.choose(), header = TRUE, sep = ",") 

# Inspeccionar los datos
head(gripes)
tail(gripes)
str(gripes)

# Bien. Tenemos una data.frame con 2803 observaciones numéricas realizadas durante 2803 días.

# Necesitamos convertir nuestro objeto "gripes" en un objeto que R
# reconozca como Serie Temporal.  Cubriremos dos de todos los posibles formatos,
#  zoo y ts.

# "zoo" ("Zeileis' ordered observations" por sus siglas en inglés) es una clase de series de 
# tiempo muy flexible creada por Achim Zeileis y Gabor Grothendieck y disponible en el 
# paquete zoo que se cargó en nuestra sesión de R al comienzo.

# La clase zoo fue diseñada para manejar datos de series de tiempo ordenadas por 
# índices de tiempo arbitrarios. 
# Decimos arbitrario porque un úndice de ordenamiento por tiempo puede ser regular, donde el 
# espacio entre observaciones es el mismo (observaciones diarias, semanales, etc.), o las 
# observaciones pueden tener separaciones irregulares (un día entre observaciones 1 y 2,
# cuatro días entre observaciones 2 y 3, seis días entre la observaciones 3 y 4, etc.).

# Esencialmente, un objeto zoo atribuye la información de fecha almacenado en un vector 
# junto al resto de los datos de cada observación almacenados en una matriz.

# Para crear un objeto zoo se necesitan datos ordenados por un índice de tiempo.
# El índice de tiempo debe tener el mismo número de filas como el objeto de datos
# y este índice de tiempo puede ser cualquier vector que contiene observaciones ordenadas. 
# Generalmente, el índice de tiempo es un objeto de la clase "Date", "POSIXct", "yearmon",
# "yearqtr" o "TimeDate".

# Ya que tenemos los datos ordenados por año, mes y día, lo primero que necesitamos hacer 
# entonces es unir las 3 variables (o columnas) "Year", "Month" y "Day", en 
# un objeto de formato "Date", "POSIXct", "yearmon", "yearqtr" o "TimeDate"

# Para lograrlo, utilizaremos la función ISOdate(). Si tuviéramos horas del día,
# podríamos usar ISOdatetime, pero no las tenemos así que no será necesario.

?ISOdate
?with
fechas <- with(gripes, ISOdate(Year, Month, Day))

# Inspeccionamos "fechas" para confirmar que tenemos un objeto en alguno de los formatos
# requerido por zoo.
str(fechas)
length(fechas)

# Y efectivamente confirmamos que ahora tenemos un vector de 2803 observaciones de formato
# POSIXct, que es uno de los formato que requiere zoo. También tenemos, para cada día, el 
# dato de horario ("12:00:00") que es el mismo para todas las observaciones y como no 
# estaban originalmente, ahora tampoco nos sirven, no agregan ningún valor así que podemos
# eliminar los horarios cambiando del formato "POSIXct" al formato "Date", el cual 
# también es aceptado por zoo.

fechas <- as.Date(fechas)

# Inspeccionamos "fechas" nuevamente para confirmar que tenemos un objeto en alguno de los 
# formatos requerido por zoo.
str(fechas)
length(fechas)

# Excelente, tenemos ahora los datos para crear nuestro objeto zoo sin adicionales que no
# suman valor.

gripes.z <- zoo(x = gripes$Amount, order.by = fechas)

# Y chequeamos la nuevo objeto zoo

# Primero confirmamos los formatos necesarios para cálculos de tiempo están presentes
str(gripes.z)

# Y luego que los datos están ordenados de igual manera que nuestra base de datos 
# (data.frame) original

head(gripes.z, 10); head(gripes,10)
tail(gripes.z, 10); tail(gripes,10)

# Excelente. Veamos el grafico de la cantidad de gripes observadas en función de las fechas
# en que estas fueron observadas

plot(gripes.z)

# Bien, a primera vista vemos que el ciclo estacional aparenta ser de 1 año, con las
# cantidades de casos de gripe aumentando dos veces (dos picos por año).

# Una serie de tiempo se puede descomponer en tres componentes:

# 1) tendencia, 
# 2) su ciclo estacional, y 
# 3) su propia regularidad o irregularidad pura (la gripe libre de tendencia y efectos 
#    estacionales). 

# Esta descomposición (separación) se logra mediante la función stl(). 

# Como stl() necesita entender la frecuencia de nuestros datos, debemos entonces
# convertir entonces nuestra data.frame original en un serie de tiempo ts con periodicidad 
# de 365 días por año ya que cada observación corresponde a un día durante aproximadamente
# unos  8 años (1999-2007). La frecuencia siempre debe ser un entero.

unique(gripes$Year)
# [1] 1999 2000 2001 2002 2003 2004 2005 2006 2007

gripes.ts <- ts(gripes$Amount, start=1999, frequency = 365)

modelo <- stl(gripes.ts, s.window = "periodic")

plot(modelo)
summary(modelo)

# Modelos exponenciales

# Tenemos la función de filtros HoltWinters() en la instalación base de R y se puede 
# utilizar para ajustar modelos exponenciales.

?HoltWinters

# simple exponencial - modela nivel (remainder), sin tendencia ni componente estacional
modelo.simple <- HoltWinters(gripes.ts, beta=FALSE, gamma=FALSE)

modelo.simple

# doble exponencial - modela nivel (remainder) y tendencia, sin componente estacional
modelo.doble <- HoltWinters(gripes.ts, gamma=FALSE)
modelo.doble

# triple exponencial - modela datos, tendencia y componente estacional
modelo.completo <- HoltWinters(gripes.ts)
summary(modelo.completo)

# Predicciones de los próximos 365 días (para el año 2007-2008)

prediccion <- predict(modelo.completo, n.ahead=365)

# Utilizando el paquete forecast
prediccion2 <- forecast(modelo.completo, h=365, level = .95)

prediccion
prediccion2

# Las próximas órdenes se pueden utilizar para graficar nuestras predicciones

# Utilizando la función de predicción y gráficos del paquete base de R

plot(gripes.ts, xlim=c(1999,2008))
lines(prediccion, col="red")

# Y utilizando la función de predicción y gráficos del paquete "forecast" y comparamos los
# dos gráficos resultantes
plot(prediccion2)
autoplot(prediccion2)

# Predicciones automáticas utilizando modelos exponenciales

?ets
modelo.ets <- ets(gripes.ts)
# no puede utilizar frecuencias superiores a 24 y, por ende, ignora el componente estacional.
# En nuestro caso es muy importante así que probamos con otra sugerencia.

?stlf
modelo.stlf <- stlf(gripes.ts)

summary(modelo.stlf)

prediccion3 <- forecast(modelo.stlf)

plot(prediccion3)
autoplot(prediccion3)

# Excelente, nos da intervalos de confianza. Si miramos el resumen del pronóstico3, 
# podremos ver a qué intervalos se refiere cada color y explicarlo al presentar el grafico.

summary(prediccion3)
# Forecasts:
# Point Forecast       Lo 80     Hi 80       Lo 95     Hi 95

# Vemos que los intervalos son 80% y 95%

# Adicionalmente, en este caso particular donde no puede haber una cantidad de personas 
# negativas con gripe, claramente no necesitamos las predicciones negativas, así que 
# podemos modificar un poco el grafico

plot(prediccion3, ylim = c(0,1500), fcol = "red", 
              shadecols = c("grey", "lightblue"))
legend("topleft", c("Prediccion", "Int. Conf. 80%", "Int. Conf. 95%"), pch = 19,
       col = c("red","lightblue", "grey"))

# Predicción automática usando modelo ARIMA.

#Paciencia que tarda un poquito.
modelo.arima <- auto.arima(gripes.ts)

class(modelo.arima)
summary(modelo.arima)

prediccion.arima <- forecast(modelo.arima)

plot(prediccion.arima)
legend("topleft", c("Prediccion", "Int. Conf. 80%", "Int. Conf. 95%"), pch = 19,
       col = c("red","darkgrey", "lightgrey"))

autoplot(prediccion.arima)

# Dada la fuerte tendencia a la baja luego del pico máximo en 2004, estas proyecciones
# ARIMA parecen tener más sentido que las anteriores.

# Hay MUCHO que se puede hacer con series de tiempo. Voy a subir una análisis técnico sobre
# acciones de la bolsa de Wall Street para jugar en mucha más profundidad y detalle
# con una base de datos históricos mucho más amplia.
