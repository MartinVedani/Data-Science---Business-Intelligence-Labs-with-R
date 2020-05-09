# Arboles 1, 2 y 3. Business Intelligence UTN. 
#  Resolución Guía de Trabajos Prácticos por Martin Vedani.

# Este es un desarrollo para leer línea por línea e ir ejecutando orden por orden
# manualmente, de forma interactiva.

# Todo texto a la derecha del símbolo "#" son comentarios del autor. Es irrelevante
# para R si lo resaltas o no con el mouse antes de hacer clic en "run",
# ya que R simplemente ejecutara todo el texto anterior a # en cada l?nea e ignorar?
# lo que se encuentre a la derecha de # (en cada l?nea, claro). En el editor de texto 
# (editor de script) de mi versi?n actual de RSTUDIO, el texto que R ignora al ejecutar 
# un comando se visualiza en verde.

######################################################################################
######################################################################################

# Primero que nada, juntemos nuestras herramientas.
# Debemos instalar el paquete de algoritmos de Arboles de R (desde el mirror de CRAN 
# más cercano) para que la bajada sea lo más veloz posible. Cualquier mirror de CRAN 
# sirve obviamente porque son todos iguales. RSTUDIO tiene su propio espejo de CRAN
# en algún lugar.

install.packages("rpart") #Recursive Partitioning and Regression Trees

# Cargar dicho paquete en la sesión activa de R.

library(rpart) #Hay que cargar el paquete cada vez que se inicia la sesión de R.

# Importemos ahora los archivos a la sesión de trabajo activa creando variables.
# Es necesario tener los datos crudos de cada árbol separados de antemano en 
# 3 archivos diferentes y guardados como *.CSV (desde Excel en nuestro caso).
# A continuación veamos 2 métodos de lograr nuestras variables. Hay mucho métodos y
# formatos adicionales muy bien explicados en un enorme número de foros y YouTube.
# Los 2 primeros métodos abren un ventana de dialogo 
# donde se puede navegar manualmente hasta la ubicación de los archivos.
# En el tercer método se necesita saber de antemano la ubicación de(los) archivo(s) lo
# cual no requiere interrupción para interacción manual del usuario.

arbol1 <- read.csv(file.choose(), header = TRUE, sep = ",") #buscar manualemnte el archivo del arbol 1.
arbol2 <- read.table(file.choose(), header = T, sep = ",") #buscar manualemnte el archivo del arbol 2.
arbol3 <- read.csv(
  "path to file \Arbol3CSV.csv",
  header = T)

# Veamos y entendamos la clase y estructura para cada de las variables reci?n creadas

str(arbol1) #data frame de 10 mil observaciones con 7 variables
str(arbol2) #data frame de 20 mil observaciones con 7 variables
str(arbol3) #data frame de 40 mil observaciones con 7 variables


###################################################################################
################################# Ejercicio 1.4.1 #################################
###################################################################################

# Generar un árbol de decisión para los datos contenidos en la tabla arbol1.
# Utilizaremos las funciones y algoritmos dentro del paquete rpart.

# La función rpart() requiere de argumentos. Veamos su documentación para entender 
# mejor.

?rpart

# Ok, la documentación nos dice que vamos a necesitar de al menos 3 argumentos.
# formula: La variable de interés o dependiente (y), en nuestro caso "Resultado", 
# en función de ("~") todas las variables independientes o predictivas (x), 
# en nuestro caso atributos 1:6 (atributos del 1 al 6). Matemáticamente se escribe 
# y = f(x1, x2, x3, x4, x5, x6)
# data: el set de datos que se desea utilizar para la construcción del árbol de decisión o predictivo
# method: tipo de predicción que se desea lograr.

# Bien, ahora que entendemos los argumentos, creemos una nueva variable con nuestro
# primer árbol.


fit.arbol1 <- rpart(formula = Resultado ~ Atributo.1 + Atributo.2 + Atributo.3
              + Atributo.4 + Atributo.5 + Atributo.6, data = arbol1, method = "class")

# Veamos los resultados para saber que hay en nuestro primer ?rbol

printcp(fit.arbol1)
summary(fit.arbol1)
rsq.rpart(fit.arbol1)

# Que cantidad de información. Veamos si graficando se entiende mejor.

plot(fit.arbol1)
text(fit.arbol1)

# Demasiado básico, o súper avanzado, como para entender bien.
# Tratemos de graficar algo más útil para un nivel de comprensión más modesto.

par(mfrow = c(1,2)) #para dividir la sección de gráficos en 1x2 ( 1 fila y 2 columnas)
plotcp(fit.arbol1)
plot(fit.arbol1, uniform = TRUE, main = "arbol1")
text(fit.arbol1, use.n = T, all = T, cex = .7) #Agrega texto al grafico anterior, el grafico #2 en este caso
par(mfrow = c(1,1)) #resetear la sección de gráficos a 1 x 1 (1 fila, 1 columna)

# Estos gráficos me siguen dando trabajo para entender bien la información.

# Por suerte existen paquetes adicionales en R que nos facilitan lograr gráficos 
# aún mucho más interesantes

install.packages(c("rattle", "rpart.plot", "RColorBrewer")) #Instalar 1 sola vez
library(rattle) #cargar en cada sesion de R
library(rpart.plot) #cargar en cada sesion de R
library(RColorBrewer) #cargar en cada sesion de R

# Estudiemos la documentación de la nueva función de gráficos que utilizaremos.
# (Entender la documentación es cuestión de práctica y experiencia)

?fancyRpartPlot

# OK, más allá de título y subtitulo, ambos opcionales, solo le tenemos que señalar la 
# variable que contiene a nuestro árbol (nuestro modelo predictivo). 
# La documentación nos indica que debe ser un objeto de rpart - YA LO HICIMOS, NO HAY PROBLEMA!!

par(mfrow = c(1,2)) #dividir sección de gráficos para comparar lado a lado
plot(fit.arbol1, uniform = TRUE, main = "arbol1") #Feo gráfico 1,
text(fit.arbol1, use.n = T, all = T, cex = .7) # Agrega texto al feo gráfico 1
fancyRpartPlot(fit.arbol1) #Gráfico 2
par(mfrow = c(1,1)) #resetear la sección de gráficos

# Grafico 2 es mucho más claro y útil. 
# Y Gratis => Gracias a la Comunidad R por desarrollar paquetes tan útiles!
# Especialmente gracias a:

?citation
citation("rpart")
citation("rattle")
citation("rpart.plot")
citation("RColorBrewer")

# Veamos el grafico 2 detenidamente y en grande para apreciar el detalle y 
# entender que nos muestra

fancyRpartPlot(fit.arbol1)

# Podemos apreciar que el orden de importancia predictiva (poder predictivo) de 
# los atributos es: 1 -> 6 -> 2 -> 5 (leyendo cada apertura de ramas, de arriba hacia abajo).
# Se lee: 
# Atributo.1 < 0.5 "yes" (o sea tendiente a "0" o "FALSE"), 
# Atributo.1 <0.5 "no" (o sea tendiente a "1" o "TRUE"),
# y así sucesivamente con los atributos 6, 2, y 5.

# Los demás atributos (3 y 4) no nos ayudarían a predecir, serían potencialmente
# irrelevantes.

# Momento, ¿no teníamos 7 variables dentro de nuestros datos crudos?.
# Sí, pero la variable 7 es el Resultado y ~ x1:6. Una de dos, o es dependiente (y) o es
# independiente (x). Ambas, no!


###################################################################################
################################# Ejercicio 1.4.2 #################################
###################################################################################

# Seleccionar un conjunto de datos al azar de la tabla arbol2 y construir un árbol de 
# decisión para ese conjunto.
# Hacer lo mismo sobre los datos no utilizados y comparar.

# Tenemos que dividir arbol2 a la mitad. Una mitad será nuestra variable de entrenamiento y la 
# otra mitad la variable de testeo.

arbol2.train <- arbol2[1:10000,]
arbol2.test <- arbol2[10001:20000,]

# Ver la clase y estructura de cada nueva variable

str(arbol2.train) #data frame de 10 mil observaciones con 7 variables
str(arbol2.test) #data frame de 10 mil observaciones con 7 variables

# Generar un árbol de decisión para los datos contenidos en la tabla arbol2.train

fit.arbol2.train <- rpart(formula = Resultado ~ Atributo.1 + Atributo.2 + Atributo.3
                    + Atributo.4 + Atributo.5 + Atributo.6, 
                    data = arbol2.train, method = "class")

# Generar un árbol de decisión para los datos contenidos en la tabla arbol2.test

fit.arbol2.test <- rpart(formula = Resultado ~ Atributo.1 + Atributo.2 + Atributo.3
                          + Atributo.4 + Atributo.5 + Atributo.6, 
                          data = arbol2.test, method = "class")

# ¿Serí cierto que "Todas los padres y las madres educan a sus hijos por igual"?
# Supongamos que sí, que rpart() es completamente subjetiva.

# Comparemos ambos aprendizajes y pongamos a prueba nuestra confianza.

# ¿Un resultado TRUE = idénticos demostraría que tenemos razón, y un
# resultado FALSE = No idénticos demostraría que nuestra confianza no tiene
# razón de ser?

# Queremos poder testear (sobre arbol2.test) lo que 
# aprendimos (con el arbol2.train) sobre dos bases diferentes y evaluar si
# las predicciones funcionan o no.

identical(fit.arbol2.train, fit.arbol2.test)

# FALSO, lo que significa que nuestros modelos aprendieron cosas diferentes, 
# pero ... ¿en base a datos iguales o diferentes?

identical(arbol2.train, arbol2.test)
# FALSE

# Ok, hasta ahora sabemos que rpart() le enseñó cosas diferentes a las dos mitades
# (bases de datos crudos) de nuestra base de datos arbol2. 
# ¿rpart() educó a uno de sus hij@s mejor que al otr@ entonces?

# Veamos, visualmente, si todos los caminos nos llevaran a Roma y recobramos
# nuestra confianza de que rpart() educa siempre igual a tod@s.

par(mfrow = c(1,2))
fancyRpartPlot(fit.arbol2.train)
fancyRpartPlot(fit.arbol2.test)
par(mfrow = c(1,1)) #resetear la seccion de graficos a 1x1

# Vemos que la importancia de los atributos predictivos 1, 6, 2 y 5 es la misma.
# La función rpart() obviamente es objetiva y sabe cómo ajustarse
# a sus diferentes hij@s para darles igualdad de oportunidad a que amb@s lleguen 
# alcansen su potencial.
# ¡¡Excelente!! Vamos por buen camino.


###################################################################################
################################# Ejercicio 1.4.3 #################################
###################################################################################

# Seleccionar tres conjuntos de datos al azar (a,b,c) a partir de la tabla arbol3.
# Construir una árbol de decisión a partir del subconjunto a.
# Realizar una poda usando el subconjunto b.
# Verificar qué poder predictivo tiene cada nodo usando el subconjunto c.

arbol3.train <- arbol3[1:15000,] #base para el árbol de decisión
arbol3.prune <- arbol3[15001:30000,] # base para la poda
arbol3.test <- arbol3[30001:40000,] #base para verificar el poder predictivo del árbol

str(arbol3.train) #data frame de 15 mil observaciones con 7 variables
str(arbol3.prune) #data frame de 15 mil observaciones con 7 variables
str(arbol3.test) #data frame de 10 mil observaciones con 7 variables

identical(arbol3.train, arbol3.prune) #Buscamos FALSE
identical(arbol3.prune, arbol3.test) #Buscamos FALSE
identical(arbol3.train, arbol3.test) #Buscamos FALSE

# Bien, todas las bases de datos son diferentes. 
# Del ejercicio anterior ya sabemos que con rpart(), diferentes árboles o modelos de predicción
# aprenden diferentes cosas (los modelos dependen de la base de datos
# utilizada para fabricarlos) pero logran las mismas predicciones (un mismo modelo 
# puede predecir correctamente sobre bases de datos diferentes).

# Sabiendo esto, no necesitamos 3 modelos predictivos. Fabricar fit.arbol3.prune y 
# fit.arbol3.test no es necesario así que no lo haremos. El leñador entusiasta
# es más que bienvenido a hacerlo por su cuenta.

# Hagamos crecer entonces (fabriquemos, aprendamos) nuestro árbol predictivo.

fit.arbol3.train <- rpart(formula = Resultado ~ Atributo.1 + Atributo.2 + Atributo.3
                          + Atributo.4 + Atributo.5 + Atributo.6, 
                          data = arbol3.train, method = "class")

# Y ahora que lo tenemos, usemoslo para predecir algo

#Estudiemos la nueva función que vamos a necesitar

?predict

# OK... con el tiempo ya iremos entendiendo más de estos documentos a medida que
# acumulemos experiencia

prediccion.arbol3.test <- predict(fit.arbol3.train, arbol3.test, type = "class")

# Eso parece haber hecho algo interesante a una gran velocidad
# ¿Que tiene esta nueva variable "prediccion.arbol3.test" que nos sea útil?
# Al llamar a fit.arbol3.train, utilizamos el árbol de predicción que fabricamos
# (aprendimos) utilizando la base de datos de entrenamiento arbol3.train - (¡Bien!)

# Recordemos la fórmula de nuestro modelo

printcp(fit.arbol3.train)

# formula = Resultado ~ Atributo.1 + Atributo.2 + Atributo.3 + Atributo.4 + Atributo.5 
# + Atributo.6, data = arbol3.train, method = "class")

# Al definir arbol3.test para llegar a nuestra variable "prediccion.arbol3.test", 
# le pedimos a rpart() una predicción de la variable "Resultado" para cada una de
# las 10,000 observaciones (líneas) dentro de arbol3.test utilizando los atributos 1:6 
# también dentro arbol3.test. Y le pedimos a rpart() que prediga "Resultado" de la forma
# que aprendimos al crecer o fabricar el árbol llamado "fit.arbol3.train"
# Ok, tiene sentido.

# Veamos que resultados tenemos entonces.

summary(prediccion.arbol3.test)
str(prediccion.arbol3.test)

# 9,004 0s (o FALSE) y 996 1s (o TRUE)

# OK. A ver graficamente:

fancyRpartPlot(prediccion.arbol3.test)

# No funciona, nos da error. Esta funcion es poderosa pero no sirve para un vector 
# de 1 sólo factor con 2 niveles (1s y 0s / TRUE y FALSE). 
# A ver con algo mas basico

plot(prediccion.arbol3.test)

# La misma información que obtuvimos con la función summary()
# Nada nuevo = Frustrante... así es la vida del analista de datos.

scatter.smooth(prediccion.arbol3.test)

# Peor. Un gráfico para decirnos q tenemos 2 niveles en 10 mil observaciones.
# Menos mal que estudiamos la función str() y ya sabemos que tenemos 0 y 1 nada más.
# ¿Estamos con mala racha? Sigamos, la turbulencia ligera no derriba aviones.
# Me parece que la estamos complicando demasiado, no nos damos cuenta lo que tenemos
# en nuestras manos.

# Veamos las primeras líneas de los resultados en crudo

head(prediccion.arbol3.test)

# Hmm, ¿1s y 0s con cada observación enumerada?
# Quiero ver un poco más, las primeras 50 líneas

head(prediccion.arbol3.test, 50)

# Ok, parece que sí, hay un orden ascendente de observaciones con resultados 1s y 0s.
# Cuando ejecutamos la función str(), había 10000 líneas, entonces veamos cómo termina
#  nuestra base de predicciones de resultados.

tail(prediccion.arbol3.test, 50)

# Aja!! Armamos la variable Arbol3.test con las últimas observaciones (30,001 a 40,000)
# de nuestra base original llamada arbol3.

# Definitivamente tenemos 10 mil predicciones de Resultados 0s y 1s en base a 
# los atributos 1:6 de la base de datos "arbol3.test" (NO en base a los atributos de
# la base arbol3.train que contiene diferente observaciones que "arbol3.test", que usamos 
# para aprender y construir el árbol (modelo predictivo) "fit.arbol3.train").

# Todo esto ya es un traba lenguas de datos, obeservaciones y variables ... 
# Es fundamental seguirle el paso a todas nuestra variables, hemos utilizado
# nombres largos y no prácticos para ilustrar este detalle logístico, ya que algunas
# variables son:
# i) datos crudos: arbol1, arbol2, arbol3; 
# ii) subdivisiones de datos crudos: 
#       arbol3.train (15,000 obs: 1 a 15,000), 
#       arbol3.prune (15,000 obs: 15,001 a 30,000), y 
#       arbol3.test (10,000 obs: 30,001 a 40,000).
# iii) modelos predictivos: fit.arbol3.train;
# y ahora también 
# iv) resultados de predicciones: prediccion.arbol3.test

# Genial. Tenemos la cantidad correcta de predicciones, según los atributos
# correctos, para las observaciones (líneas) originales correctas (30,001 a 40,000)
# gracias a un modelo bien aprendido o bien enseñado por rpart()

# ¿Cómo presentamos estas predicciones?
# En cantidades y proporciones sería una buena idea.

table(prediccion.arbol3.test)
prop.table(table(prediccion.arbol3.test))

# Estamos anticipando 9,004 0s o FALSE (90.04%) y 996 1s o TRUE (9.96%).

# Y ya que sabemos los resultados reales al mirar la variable (columna) "Resultado" de
# nuestra base original arbol3.test...

# ...¿Qué tan buena es nuestra predicción? ¿Cuál es el resultado de este testeo sobre
# la base de datos arbol.3.test. ¿Cuál es el propósito de todo este ejercicio?

# Comparemos manzanas con manzanas

table(arbol3.test$Resultado)
prop.table(table(arbol3.test$Resultado))

# Guau, la diferencia parece pequeñaa, ¿de cuánto es?

9004-8982; ((9004-8982)/8982)*100; 1-abs(((9004-8982)/8982))

# Para 0s o FALSE tenemos 22 unidades en aproximadamente 10 mil o 0.24% de delta. 
# Esto significa que nuestro modelo predictivo (árbol) tiene un 99.75% de certeza 
# para 0s o FALSE.

996-1018; ((996-1018)/1018)*100; 1-abs(((996-1018)/1018))

# Para 1s o TRUE. También tenemos 22 unidades, sobre aproximadamente 1 mil o 2.2% de 
# delta. Esto significa que nuestro modelo predictivo (árbol) tiene 
# un 97.8% de certeza para 1s o TRUE.

# ¿Cuál es la capacidad de precisión atómica, línea por línea, si comparamos cada predicción
# versus exactamente esa misma observación histórica (que ya sabemos porque tenemos el resultado 
# histórico original).
# En otras palabras, ¿cuántas observaciones originalmente FALSAS, predecimos como FALSAS sin 
# equivocarnos?

sum(arbol3.test$Resultado==prediccion.arbol3.test)/length(prediccion.arbol3.test)

# 98.06%.

# Excelente!! Si supiera mejor, diría que estos datos que nos han dado son totalmente 
# ficticios dado el alto nivel de certeza que, la experiencia indica, es bastante más difícil 
# lograr esto en la realidad... Pero bueno, estamos contentos, y no estamos
# perdiendo plata con malas decisiones (ejemplo, otorgar o no un prestamo bancario en base a 
# lo atributos 1, 2 5, etc de nuestros clientes).

# Hora de exportar y enviar estas predicciones a los usuarios interesados.
# Se los enviaremos por email en un archivo que puedan utilizar fácilmente.
# Vamos a ser proactivos y agregar una columna llamada "Observaciones" para facilitar
# el uso de nuestras predicciones a nuestros clientes internos/jefes
# /usuarios (o al ayudante de cátedra para la corrección de ejercicios con vlookup
# en Excel)

# También vamos a crear una variable nueva para no tocar la predicción que hemos
# realizado y no arruinarla si nos equivocamos en alguna orden o comando.


prediccion.arbol3.compartir <- data.frame(Observacion = 30001:40000, 
                                    Resultado = prediccion.arbol3.test)

write.csv(prediccion.arbol3.compartir, file = "prediccion.arbol3.csv", 
          row.names = FALSE)

# Para saber dónde R ha grabado nuestro nuevo archivo e ir a buscarlo,
# debemos saber el directorio de nuestro espacio de trabajo (work directory).

getwd()

# Ahora recordemos el primer ejercicio, 1.4.1, donde los atributos 3 y 4
# parecían potencialmente irrelevantes. 

# ¿Puede ser que nuestro modelo predictivo (nuestro árbol) entonces tenga ramas
# que se puedan podar? Veamos si es así y cómo impactaría sobre los resultados de 
# la predicción SIN poda que acabamos de hacer.

# El objetivo detrás de podar el árbol es evitar sobreajuste de los datos. 
# Por lo general, queremos seleccionar un tamaño de árbol que minimice el error
# de validación cruzada, la columna de xerror impreso por printcp().

printcp (fit.arbol3.train)

# Examinando los resultados de error con validación cruzada, debemos seleccionar
# el parámetro de complejidad (CP) asociado con el xerror mínimo, 
# y colocarlo en la función de prune(). En este caso es fácil de identificar, es el cp
# número 5 (xerror = 0.19092).
# Alternativamente, cuando los datos sean muchísimo más extensos se puede utilizar el 
# siguiente fragmento de código para que prune() lo busque automáticamente:

fit.arbol3.train$cptable[which.min(fit.arbol3.train$cptable[,"xerror"]),"CP"]
# [1] 0.01 que es el CP número 5

poda.arbol3 <- prune(fit.arbol3.train, cp = 0.01000000)

poda.arbol3.cpAuto <- prune(fit.arbol3.train, cp = fit.arbol3.train$cptable
                            [which.min(fit.arbol3.train$cptable[,"xerror"]),"CP"])

# ¿Confiamos en este código "automático" que alguien posteo en algún foro de internet?

identical(poda.arbol3, poda.arbol3.cpAuto)

# TRUE. Bien, son iguales! La fuente es de alta calidad y en el futuro volvería ella
# por ayuda. Gracias a HSAUR por la idea (http://www.statmethods.net/about/books.html)
# Ahora voy a remover una de los 2 variables idénticas para no hacerme lío, y mantener
# mi escritorio  (global environment) lo más limpio y ordenado posible.

rm(poda.arbol3.cpAuto)

# Vamos a graficar el árbol podado y compararlo con la versión anterior a la poda

par(mfrow = c(1,2)) # ya sabemos para que esto
fancyRpartPlot(fit.arbol3.train) # antes de la poda
fancyRpartPlot(poda.arbol3) # despues de la poda
par(mfrow = c(1,1)) # reset

# Visualmente no notamos nada. ¿A ver si identical() detecta algo?

identical(fit.arbol3.train, poda.arbol3)

# identical() está de acuerdo, no hay diferencias. En la realidad, esto sería una
# rara excepción que también podría darse. rpart() es muy hábil y, sin saberlo,
# nos percatamos tempranamente que los atributos 3 y 4 habían sido ignorados (podados)
# por rpart().

# Teníamos un subgrupo de datos creados en la variable arbol3.prune
# Los desafío a realizar este ejercicio 1.4.3 completamente desde cero
# utilizando arbol3.prune para "entrenamiento" del modelo predictivo y arbol3.test 
# como el árbol de testeo.


###################################################################################
################################## Random Forest ##################################
###################################################################################

# En términos simples, la técnica Random Forest (Bosque aleatoria) se enfoca
# en el problema de sobreajuste (overfitting) de un árboles de decisión. 

# ¿Cuándo ocurre este sobreajuste? 

# Sucede cuando un árbol de decisión (un modelo predictivo)
# aprende "demasiado", cuando se ajusta con demasiada exactitud a la base de datos de 
# entrenamiento, y su capacidad de realizar predicciones útiles sobre otras bases de 
# datos diferentes (i.e. testing) disminuye. Por ejemplo, en el último ejercicio
# vimos como rpart() ignoró algunos atributos (el 3 y 4) al generar el árbol predictivo
# y, por ende, asumimos eso como la razón por la cual la poda no fue necesaria.
# Esto no es completamente correcto. Atributos 3 y 4 pudieron ser irrelevantes sin tener
# nada que ver con overfitting.

# Random forest es una forma de poda más avanzada que rpart() y prune(). 
# Involucra la creación de múltiples árboles de diferentes niveles de ajuste a
# la base de datos de entrenamiento. En el momento de la predicción, para
# cada línea u observación (por ejemplo, un potencial cliente que pide un prestamo bancario), 
# cada árbol fabricado (entrenado) generar su 
# predicción y cuenta como su "voto". Por ejemplo, si tenemos 3 árboles, de 
# los cuales 2 predicen 0 o FALSE (no pagará), y el tercero predice 1 o TRUE (sí pagará),
# la observación en cuestión quedará predica como 0 o FALSE por "mayoría de votos".
# Esta técnica de sobre entrenamiento de árboles con diferentes ajuste, llega a la
# decisión final de clasificación (valor predicho) de forma democrática y evita sobreajuste.

# Lo primero que debemos asegurarnos es que nuestras bases de datos no tengan valores
# faltantes o identificar la observación (fila) y variable (columna) a completar.

# En nuestras bases de datos, no hay valores faltantes. En la práctica, lo más común
# es que sí los haya, y ehasta en grandes cantidades. 
# Las soluciones son variadas. Se pueden eliminar la(s) observacion(es) con
# datos faltantes, se pueden completar los valores faltantes con valores medios o
# promedios calculados con los valores de las demás observaciones para esa variable 
# en particular, o,  sorprendentemente, se pueden predecir los valores faltantes con árboles!!

# Por ejemplo, queremos predecir un valor para la variable "retraso en el pago" 
# que puede tomar un valor en días, meses, TRUE o FALSE, etc. 
# Un árbol posible sería:

# retraso.predicto <- rpart(Retraso ~ Sueldo + Deuda + Nivel Educativo + ...
# +                 data=data.frame[!is.na(data.frame$Retraso),], method="anova")

# De esta forma, estaríamos usando todos los datos "no" faltantates "!is.na" (no nulos o faltantes)
# de la variable "Retraso" de nuestra base de datos llamada "data.frame"
# para fabricar un arbol que nos sirva para predecir el retraso del/los cliente(s)
# (observaciones) que tienen este valor incompleto.

# Un vez que tenemos el árbol, es cuestión de predecir los valores faltantes y 
# cargarlos en las celdas correspondientes en nuestra data.frame, 
# con el siguiente código por ejemplo:

# data.frame$Retraso[is.na(data.frame$Retraso)] <- predict(retraso.predicto, 
# +                                         data.frame[is.na(data.frame$Retraso),])

# Borrar la observación en vez de hacer todo este lío es muy tentador. Pero si
# dicha observación (cliente) tiene una deuda enorme, tal vez sea más útil
# hacer el trabajo duro de predicción y llenado de celdas.

# Por último, una vez que ya no nos falte ningún dato, podemos iniciar con el primer 
# paso requerido para la creación de un árbol, que sería el de dividir la
# data.frame en partes, i.e. data.frame.train y data.frame.test

?randomForest2Rules

# Para ejecutar la función randomForest(), debemos instalar el paquete ramdomForest

install.packages("randomForest") # sólo es necesario hacerlo una vez
library("randomForest") # se debe cargar en cada nueva sesión de R
?randomForest

# randomForest() es muy similar a rpart(). El primer argumento es la fórmula, igual
# formato que en rpart(). Al no haber un argumento de método, hay que convertir
# todas la variable dependiente "Y" en factor dentro del argumento "formula":

# formula = as.factor(Resultado) ~ Atributo.1 + Atributo.2 + Atributo.3
# +                                         +Atributo.4 + Atributo.5 + Atributo.6

# Una forma de resumir la cantidad de código en R, es usar un sinónimo para "TODOS LOS
# ATRIBUTOS". Y ese sinónimo es el punto "."

# Quedaría entonces más corto, de la siguiente forma

# formula = as.factor(Resultado) ~ . 

# Se lee: Resultado (en formato "factor") en función de todas las variables (columnas)
# existente (sin incluir la columna titulada "Resultado" obviamente). El
# problema con esta forma corta de incluir todo, es que no nos permite alterar el
# orden de las variables que le alimentamos a la formula. Nunca hemos alterado 
# el orden de la variables en nuestros ejercicios hasta ahora, pero bien podríamos 
# desear hacerlo en la práctica. Por ejemplo, queremos poner las variables "sexo" 
# y "edad" primero si es que en nuestra base de datos se encuentran en las 
# columnas 3 y 8 (por decir cualquier orden).

# El argumento data es igual que en rpart().

# El argumento "importance = TRUE" nos permitirá visualizar la importancia de cada
# variable en la predicción de los arboles (modelos) creados por randomForest().

# El argumento ntrees limita el número de árboles generados por randomForest.
# Esto puede ayudarnos con la gestión de recursos computacionales cuando estos son
# limitados.

# El argumento set.seed(123) asegura que el generador de números aleatorios de 
# randomForest() siempre arranque igual para poder reproducir los mismos 
# resultados muchas veces. De otra forma, siempre arrancara desde un random diferente
# y nunca serí posible que 2 programdores, en diferentes momentos, puedan 
# reproducir exactamente los mismo resultados.
# Esto es realmente útil para explicar en detalle una predicción de ser necesario, como
# por ejemplo a los alumnos o jefes que quieren ver exactamente cómo y de dónde salieron 
# los números que se presentaron en power point, o un reporte PDF, etc. etc. etc.

# Una vez generado el modelo predictivo de randomForest (con una cantidad de ntrees 
# que sea), se podrá utilizar la función predict() ya lo hemos hecho.

# Veamos un ejemplo con las bases de datos ya generadas

str(arbol3.train) # 15 mil observaciones, 7 variables
str(arbol3.test) # 10 mil observaciones, 7 variables

# Vamos a marcar el inicio del seed  del randomizador para poder reproducir exactamente
# los mismos resultados en el futuro cuantas veces lo necesitemos

set.seed(123); fit.bosque.a3.train <- randomForest(as.factor(Resultado) ~ ., 
                                          data = arbol3.train, importance = TRUE,
                                              ntree = 1000)

# Para entretenernos un poco, veamos si hay diferencia entre los modelos
# predictivos fit.arbol.3 versus fit.bosque.arbol.3

identical(fit.bosque.a3.train, fit.arbol3.train)

# FALSE. Era de esperarse, 1 arbol versus 1000 arboles. Analicemos en mayor detalle.

printcp (fit.bosque.a3.train)
printcp (fit.arbol3.train)

# No nos sirve, fit.bosque no es un objeto de rpart(), es un objeto de randomForest()

# Empecemos por analizar las variables importantes de nuestra randomForest

varImpPlot(fit.bosque.a3.train)

# El grafico de la izquierda nos muestra cuanto decaería la precisión o calidad
# de predicción del bosque en general al remover cada variable individualmente.
# El grafico de la derecha muestra el coeficiente de Gini que nos dice la importancia 
# de cada variable dentro del modelo.
# Dos formas diferentes de decir cosas parecidas, pero no iguales.
# El gráfico de la izquierda habla de la caída en performance del modelo en general 
# según se remueva cada variable. El coeficiente de Gini nos dice el aporte
# específico de cada variable, independientemente del resto de las variables. 
# En nuestro caso, el orden descendiente de las variables es el mismo en ambos gráficos,
# pero no siempre se da de esta forma. Dos atributos o variables, A y B con orden de 
# importancia 2 y 3 en el gráfico de Gini de la derecha, podrían perfectamente tener un orden
# diferente en el gráfico de la izquierda.
# En ambos gráficos, cuanto mayor es el desplazamiento del valor para cada
# variable hacia la derecha, mayor la importancia de esa variable.
# Dentro del modelo predictivo. El orden de importancia es 1 -> 6 -> 2 -> 5 
# con 3 y 4 aproximadamente en cero.

# Recordando fit.arbol3.train:

fancyRpartPlot(fit.arbol3.train)

# Habiamos exactamente lo mismo con rpart(): 1 -> 6 -> 2 -> 5

#Veamos si podemos hacer una comparacion grafica

par(mfrow = c(1,2))
fancyRpartPlot(fit.arbol3.train)
fancyRpartPlot(fit.bosque.a3.train) # Ups, error
plot(fit.bosque.a3.train) # Ahora tenemos algo, pero no nos sirve demasiado.
par(mfrow = c(1,1))

# Diferentes gráficos, comparar rpart() vs randomForest() de esta forma es como
# comparar peras con manzanas. No nos sirve.

# OK, la habilidad de cada jugador se ve en la cancha - lo que realmente importa son los resultados.

# Hagamos predicciones usando arbol3.test

pred.bosque.a3.test <- predict(fit.bosque.a3.train, arbol3.test)

#Comparemos predicciones

identical(prediccion.arbol3.test, pred.bosque.a3.test)

# FALSE. OK, interesante

head(prediccion.arbol3.test, 15)
head(pred.bosque.a3.test, 15)

tail(prediccion.arbol3.test, 15)
tail(pred.bosque.a3.test, 15)

# OK, parecen iniciar y terminar iguales.

# Vamos a las tablas, para ver si los totales y proporciones agregados nos dan
# parecidos.

table(prediccion.arbol3.test)
table(pred.bosque.a3.test)

# OK. El ramdomForest() predice más 0s o FALSE, y menos 1s o TRUE que rpart()
# Tenemos los datos originales ("reales") de la base arbol3.test para comparar
# predicciones versus realidad histórica. Vemos como queda.

# ¿Quién se lleva el beneficio de las predicciones, los 0s o FALSE, o los 1s o TRUE?

table(arbol3.test$Resultado)
table(prediccion.arbol3.test)
table(pred.bosque.a3.test)

9004-8982; ((9004-8982)/8982)*100; 1-abs(((9004-8982)/8982))
9060-8982; ((9060-8982)/8982)*100; 1-abs(((9060-8982)/8982))

# Para 0s o FALSE tenemos 22 unidades en aproximadamente 10 mil o 0.24% de delta
# con rpart() y 78 unidades o 0.87% de delta con randomForest(). 
# Esto significa que rpart() tiene un 99.75% de certeza para 0s o FALSE y 
# randomForest() tiene 99.13%.
# Rpart() era más acertado para 0s o FALSOS que randomForest()

996-1018; ((996-1018)/1018)*100; 1-abs(((996-1018)/1018))
940-1018; ((940-1018)/1018)*100; 1-abs(((940-1018)/1018))

# Para 1s o TRUE, rpart() predijo 22 unidades menos, o 2.2% de delta, dándonos 
# un 97.8% de certeza para 1s o TRUE. randomForest() predice 78 unidades menos, 
# o 7.7% de delta, dándonos un 92.3% de certeza para 1s o TRUE.

# Hmm, en cantidades agregadas, rpart() claramente funciona mejor que randomForest().

# Pero a nivel atómico, observación por observación, ¿es también as??

# Sabíamos que rpart() tenía una exactitud, comparando observación por observación
# a nivel atómico (i.e. línea 35667 histórica vs. 35667 predicha), del 98.06%. 
# Recordemos:

sum(arbol3.test$Resultado==prediccion.arbol3.test)/length(prediccion.arbol3.test)

# Calculemos para randomForest()

sum(arbol3.test$Resultado==pred.bosque.a3.test)/length(pred.bosque.a3.test)

# 98.16%. 

# Guau, randomForest() predijo cada observacion específica mejor que rpart(). 
# Lo cual, lo hace más certero a nivel atómico.

98.16-98.06

# La democracia entre los 1000 árboles votantes dentro de randomForest()
# nos ha generado una mejora del 0.10%.ñ

# Pequeña diferencia sin duda, de importancia RELATIVA!! Cada caso de la realidad,
# con datos reales, dictará cual será la verdadera importancia de tan pequeñas mejoras.

# Solo nos resta crear una data.frame con 2 columnas para compartir

# Cambiemos el nombre para no molestar la variable con las predicciones

pred.bosque.a3 <- data.frame(Observacion = 30001:40000, 
                                  Resultado = pred.bosque.a3.test)

# Exportamos a un archivo compartible

write.csv(pred.bosque.a3, file = "pred.bosque.arbol3.csv", row.names = FALSE)

# Listo!!!

# Teníamos un subgrupo de datos creados en la variable arbol3.prune
# Los desafío a realizar este ejercicio de randomForest completamente desde cero
# utilizando arbol3.prune para "entrenamiento" del modelo predictivo y arbol3.test 
# como el árbol de testeo.