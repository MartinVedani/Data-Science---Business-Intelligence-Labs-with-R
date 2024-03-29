# Arboles 1, 2 y 3. Por Martin Vedani.

# Este es un desarrollo para leer l�nea por l�nea e ir ejecutando orden por orden
# manualmente, de forma interactiva.

# Todo texto a la derecha del s�mbolo "#" son comentarios del autor. Es irrelevante
# para R si lo resaltas o no con el mouse antes de hacer clic en "run",
# ya que R simplemente ejecutara todo el texto anterior a # en cada l�nea e ignorar�
# lo que se encuentre a la derecha de # (en cada l�nea, claro). En el editor de texto 
# (editor de script) de mi versi�n actual de RSTUDIO, el texto que R ignora al ejecutar 
# un comando se visualiza en verde.

######################################################################################
######################################################################################

# Primero que nada, juntemos nuestras herramientas.
# Debemos instalar el paquete de algoritmos de Arboles de R (desde el mirror de CRAN 
# m�s cercano) para que la bajada sea lo m�s veloz posible. Cualquier mirror de CRAN 
# sirve obviamente porque son todos iguales. RSTUDIO tiene su propio espejo de CRAN
# en alg�n lugar.

install.packages("rpart") #Solo se hace una vez, luego queda instalado

# Cargar dicho paquete en la sesi�n activa de R.

library(rpart) #Hay que cargar el paquete cada vez que se inicia la sesi�n de R.

# Importemos ahora los archivos a la sesi�n de trabajo activa creando variables.
# Es necesario tener los datos crudos de cada �rbol separados de antemano en 
# 3 archivos diferentes y guardados como *.CSV (desde Excel en nuestro caso).
# A continuaci�n veamos 2 m�todos de lograr nuestras variables. Hay mucho m�todos y
# formatos adicionales muy bien explicados en un enorme n�mero de foros y YouTube.
# Los 2 primeros m�todos abren un ventana de dialogo en Windows
# donde se puede navegar manualmente hasta la ubicaci�n de los archivos.
# En el tercer m�todo se necesita saber de antemano la ubicaci�n de(los) archivo(s) lo
# cual no requiere interrupci�n para interacci�n manual del usuario.

arbol1 <- read.csv("C:\\Users\\Martins\\Google Drive\\myWork\\myRwork\\Swirl - Decision Trees\\Arbol1CSV.csv", header = T) 
arbol2 <- read.csv("C:\\Users\\Martins\\Google Drive\\myWork\\myRwork\\Swirl - Decision Trees\\Arbol2CSV.csv", header = T)
arbol3 <- read.csv("C:\\Users\\Martins\\Google Drive\\myWork\\myRwork\\Swirl - Decision Trees\\Arbol3CSV.csv", header = T)

# Veamos y entendamos la clase y estructura para cada de las variables reci�n creadas

str(arbol1) #data frame de 10 mil observaciones con 7 variables
str(arbol2) #data frame de 20 mil observaciones con 7 variables
str(arbol3) #data frame de 40 mil observaciones con 7 variables


###################################################################################
##################################### Ejercicio 1 #################################
###################################################################################

# Generar un �rbol de decisi�n para los datos contenidos en la tabla arbol1.
# Utilizaremos las funciones y algoritmos dentro del paquete rpart.

# La funci�n rpart() requiere de argumentos. Veamos su documentaci�n para entender 
# mejor.

?rpart

# Ok, la documentaci�n nos dice que vamos a necesitar de al menos 3 argumentos.
# formula: La variable de inter�s o dependiente (y), en nuestro caso "Resultado", 
# en funci�n de ("~") todas las variables independientes o predictivas (x), 
# en nuestro caso atributos 1:6 (atributos del 1 al 6). Matem�ticamente se escribe 
# y = f(x1, x2, x3, x4, x5, x6)
# data: el set de datos que se desea utilizar para la construcci�n del �rbol de decisi�n o predictivo
# method: tipo de predicci�n que se desea lograr.

# Bien, ahora que entendemos los argumentos, creemos una nueva variable con nuestro
# primer �rbol.


fit.arbol1 <- rpart(formula = Resultado ~ Atributo.1 + Atributo.2 + Atributo.3
              + Atributo.4 + Atributo.5 + Atributo.6, data = arbol1, method = "class")

# Veamos los resultados para saber que hay en nuestro primer �rbol

printcp(fit.arbol1)
summary(fit.arbol1)
rsq.rpart(fit.arbol1)

# Que cantidad de informaci�n. Veamos si graficando se entiende mejor.

plot(fit.arbol1)
text(fit.arbol1)

# Demasiado b�sico, o s�per avanzado, como para entender bien.
# Tratemos de graficar algo m�s �til para nuestro modesto nivel de estudiantes.

par(mfrow = c(1,2)) #para dividir la secci�n de gr�ficos en 1x2 ( 1 fila y 2 columnas)
plotcp(fit.arbol1)
plot(fit.arbol1, uniform = TRUE, main = "arbol1")
text(fit.arbol1, use.n = T, all = T, cex = .7) #Agrega texto al grafico anterior, el grafico #2 en este caso
par(mfrow = c(1,1)) #resetear la secci�n de gr�ficos a 1 x 1 (1 fila, 1 columna)

# Estos gr�ficos me siguen dando un poco de dolor de cabeza. Y adem�s sali� un
# mensaje de error que no se sabe bien que significa.

# Por suerte existen paquetes adicionales en R que nos facilitan lograr gr�ficos 
# a�n mucho m�s interesantes

install.packages(c("rattle", "rpart.plot", "RColorBrewer")) #Instalar 1 sola vez
library(rattle) #cargar en cada sesion de R
library(rpart.plot) #cargar en cada sesion de R
library(RColorBrewer) #cargar en cada sesion de R

# Estudiemos la documentaci�n de la nueva funci�n de gr�ficos que utilizaremos.
# (Entender la documentaci�n es cuesti�n de pr�ctica y experiencia)

?fancyRpartPlot

# OK, m�s all� de t�tulo y subtitulo, ambos opcionales, solo le tenemos que se�alar la 
# variable que contiene a nuestro �rbol (nuestro modelo predictivo). 
# La documentaci�n nos indica que debe ser un objeto de rpart - SIN PROBLEMA!!

par(mfrow = c(1,2)) #dividir secci�n de gr�ficos para comparar lado a lado
plot(fit.arbol1, uniform = TRUE, main = "arbol1") #Feo gr�fico 1,
text(fit.arbol1, use.n = T, all = T, cex = .7) # Agrega texto al feo gr�fico 1
fancyRpartPlot(fit.arbol1) #Gr�fico 2
par(mfrow = c(1,1)) #resetear la secci�n de gr�ficos

# Grafico 2 es mucho m�s claro y �til. 
# Y Gratis => �Gracias a la Comunidad R por desarrollar paquetes tan �tiles!
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
# los atributos es: 1 -> 6 -> 2 -> 5.
# Se lee: 
# Atributo.1 < 0.5 "yes" (o sea tendiente a "0" o "FALSE"), 
# Atributo.1 <0.5 "no" (o sea tendiente a "1" o "TRUE"),
# y as� sucesivamente con los atributos 6, 2, y 5.

# Los dem�s atributos (3 y 4) no nos ayudar�an a predecir, ser�an potencialmente
# irrelevantes.

# Momento, �no ten�amos 7 variables dentro de nuestros datos crudos?.
# S�, pero la variable 7 es el Resultado y ~ x1:6. O es dependiente (y) o es
# independiente (x). Ambas, no!


###################################################################################
##################################### Ejercicio 2 #################################
###################################################################################

# Seleccionar un conjunto de datos al azar de la tabla arbol2 y construir un �rbol de 
# decisi�n para ese conjunto.
# Hacer lo mismo sobre los datos no utilizados y comparar.

# Dividir arbol2 a la mitad. Una mitad ser� nuestra variable de entrenamiento y la 
# otra mitad la variable de testeo

arbol2.train <- arbol2[1:10000,]
arbol2.test <- arbol2[10001:20000,]

# Ver la clase y estructura de cada nueva variable

str(arbol2.train) #data frame de 10 mil observaciones con 7 variables
str(arbol2.test) #data frame de 10 mil observaciones con 7 variables

# Generar un �rbol de decisi�n para los datos contenidos en la tabla arbol2.train

fit.arbol2.train <- rpart(formula = Resultado ~ Atributo.1 + Atributo.2 + Atributo.3
                    + Atributo.4 + Atributo.5 + Atributo.6, 
                    data = arbol2.train, method = "class")

# Generar un �rbol de decisi�n para los datos contenidos en la tabla arbol2.test

fit.arbol2.test <- rpart(formula = Resultado ~ Atributo.1 + Atributo.2 + Atributo.3
                          + Atributo.4 + Atributo.5 + Atributo.6, 
                          data = arbol2.test, method = "class")

# �Ser� cierto que "Todas las madres quieren a sus hijos por igual"?
# Supongamos que s�, que rpart() es completamente subjetiva.

# Comparemos ambos aprendizajes y pongamos a prueba nuestra confianza.
# �Un resultado TRUE = id�nticos demostrar�a que tenemos raz�n, y un
# resultado FALSE = No id�nticos demostrar�a que nuestra confianza no tiene
# raz�n de ser?
# Queremos poder testear (sobre arbol2.test) lo que 
# aprendimos (con el arbol2.train) sobre dos bases diferentes y evaluar si
# las predicciones funcionan o no.

identical(fit.arbol2.train, fit.arbol2.test)

# FALSO, lo que significa que nuestros modelos aprendieron cosas diferentes, 
# pero ... �en base a datos iguales o diferentes?

identical(arbol2.train, arbol2.test)
# FALSE

# Ok, hasta ahora sabemos que rpart() nos ense�� cosas diferentes sobre dos mitades
# (bases de datos crudos) diferentes. �rpart() tiene un hijo favorito entonces?

# Veamos, visualmente, si todos los caminos nos llevaran a Roma y recobramos
# nuestra confianza

par(mfrow = c(1,2))
fancyRpartPlot(fit.arbol2.train)
fancyRpartPlot(fit.arbol2.test)
par(mfrow = c(1,1)) #resetear la seccion de graficos a 1x1

# Vemos que la importancia de los atributos predictivos son iguales.
# La funci�n rpart() obviamente es objetiva y sabe c�mo ajustarse
# a sus diferentes hijos para darles igualdad de oportunidad a que ambos lleguen 
# a Roma, independientemente del camino o la forma.
# ��Excelente!! Vamos por buen camino. rpart() es una gran maestra y nadie necesitara
# terapia.


###################################################################################
##################################### Ejercicio 3 #################################
###################################################################################

# Seleccionar tres conjuntos de datos al azar (a,b,c) a partir de la tabla arbol3.
# Construir una �rbol de decisi�n a partir del subconjunto a.
# Realizar una poda usando el subconjunto b.
# Verificar que poder predictivo tiene cada nodo usando el subconjunto c.

arbol3.train <- arbol3[1:15000,] #base para el �rbol de decisi�n
arbol3.prune <- arbol3[15001:30000,] # base para la poda
arbol3.test <- arbol3[30001:40000,] #base para verificar el poder predictivo del �rbol

str(arbol3.train) #data frame de 15 mil observaciones con 7 variables
str(arbol3.prune) #data frame de 15 mil observaciones con 7 variables
str(arbol3.test) #data frame de 10 mil observaciones con 7 variables

identical(arbol3.train, arbol3.prune) #Buscamos FALSE
identical(arbol3.prune, arbol3.test) #Buscamos FALSE
identical(arbol3.train, arbol3.test) #Buscamos FALSE

# Bien, todas las bases de datos son diferentes. 
# Del ejercicio Ya sabemos que con rpart(), diferentes �rboles o modelos de predicci�n
# aprenden diferentes cosas (los modelos dependen de la base de datos
# utilizada para fabricarlos) pero logran las mismas predicciones (un mismo modelo 
# puede predecir correctamente sobre bases de datos diferentes y as�, llegar a Roma).

# Sabiendo esto, no necesitamos 3 modelos predictivos. Fabricar fit.arbol3.prune y 
# fit.arbol3.test no es necesarios as� que no lo haremos. El le�ador entusiasta
# es m�s que bienvenido a hacerlo por su cuenta, la pr�ctica de copy/paste/edit
# hace al analista y es un requisito clave de todo aspirante a programador

# Hagamos crecer entonces (fabriquemos, aprendamos) nuestro �rbol predictivo.

fit.arbol3.train <- rpart(formula = Resultado ~ Atributo.1 + Atributo.2 + Atributo.3
                          + Atributo.4 + Atributo.5 + Atributo.6, 
                          data = arbol3.train, method = "class")

# Y ahora que lo tenemos, usemoslo para predecir algo �no?

#Estudiemos la nueva funci�n que vamos a necesitar

?predict

# OK... con el tiempo ya iremos entendiendo m�s de estos documentos a medida que
# acumulemos experiencia

prediccion.arbol3.test <- predict(fit.arbol3.train, arbol3.test, type = "class")

# Eso parece haber hecho algo interesante a una gran velocidad
# �Que tiene esta nueva variable "prediccion.arbol3.test" que nos sea �til?
# Al llamar fit.arbol3.train, utilizamos nuestro �rbol de predicci�n que fabricamos
# (aprendimos) utilizando la base de datos de entrenamiento arbol3.train - (Bien!)

# Recordemos la f�rmula de nuestro modelo

printcp(fit.arbol3.train)

# formula = Resultado ~ Atributo.1 + Atributo.2 + Atributo.3 + Atributo.4 + Atributo.5 
# + Atributo.6, data = arbol3.train, method = "class")

# Al definir arbol3.test para llegar a nuestra variable "prediccion.arbol3.test", 
# le pedimos a predict() una predicci�n de la variable "Resultado" para cada una de
# las 10,000 observaciones (l�neas) dentro de arbol3.test utilizando los atributos 1:6 
# tambi�n dentro arbol3.test. Y le pedimos a predict() que prediga "Resultado" 
# de la forma que aprendimos al crecer o fabricar el �rbol llamado "fit.arbol3.train"
# utilizando rpart(). Ok, tiene sentido.

# Veamos que resultados tenemos entonces.

summary(prediccion.arbol3.test)
str(prediccion.arbol3.test)

# 9,004 0s (o FALSE) y 996 1s (o TRUE)

# OK. A ver graficamente:

fancyRpartPlot(prediccion.arbol3.test)

# No funciona, nos da error. Esta funcion es poderosa pero no sirve para un vector 
# de 1 s�lo factor con 2 niveles (1s y 0s / TRUE y FALSE). 
# A ver con algo mas basico

plot(prediccion.arbol3.test)

# La misma informaci�n que obtuvimos con la funci�n summary()
# Nada nuevo = Frustrante... as� es la vida del analista de datos y del
# pronosticador.

scatter.smooth(prediccion.arbol3.test)

# Peor. Un gr�fico para decirnos q tenemos 2 niveles en 10 mil observaciones.
# Menos mal que estudiamos la funci�n str() y ya sabemos que tenemos 0 y 1 nada m�s.
# �Estamos con mala racha? Sigamos, la turbulencia ligera no derriba aviones.
# Me parece que la estamos complicando demasiado, no nos damos cuenta lo que tenemos
# en nuestras manos.

# Veamos las primeras l�neas de los resultados en crudo

head(prediccion.arbol3.test)

# Hmm, �1s y 0s con cada observaci�n enumerada?
# Quiero ver un poco m�s, las primeras 50 l�neas

head(prediccion.arbol3.test, 50)

# Ok, parece que s�, hay un orden ascendente de observaciones con resultados 1s y 0s.
# Cuando ejecutamos la funci�n str(), hab�a 10000 l�neas, entonces,
# �a ver c�mo termina nuestra base de predicciones de resultados?

tail(prediccion.arbol3.test, 50)

# Aja!! Armamos la variable Arbol3.test con las �ltimas observaciones (30,001 a 40,000)
# de nuestra base original y completa llamada arbol3.

# Definitivamente tenemos 10 mil predicciones de Resultados 0s y 1s en base a 
# los atributos 1:6 de la base de datos "arbol3.test" (NO en base a los atributos de
# la base arbol3.train, diferente a "arbol3.test", que usamos para aprender y construir 
# el �rbol (modelo predictivo) "fit.arbol3.train").

# Todo esto ya es un traba lenguas de variables ... 
# Es fundamental seguirle el paso a todas nuestra variables, hemos utilizado
# nombres largos y no pr�cticos para ilustrar este detalle log�stico, ya que algunas
# variables son:
# i) datos crudos: arbol1, arbol2, arbol3; 
# ii) subdivisiones de datos crudos: arbol3.train, arbol3.prune, arbol3.test;
# iii) modelos predictivos: fit.arbol3.train;
# y ahora tambi�n 
# iv) resultados de predicciones: prediccion.arbol3.test

# Genial. Tenemos la cantidad correcta de predicciones, seg�n los atributos
# correctos, para las observaciones (l�neas) originales correctas (30,001 a 40,000)
# gracias a un modelo bien aprendido o bien ense�ado por rpart()

# �C�mo presentamos estas predicciones?
# En cantidades y proporciones ser�a una buena idea.

table(prediccion.arbol3.test)
prop.table(table(prediccion.arbol3.test))

# Estamos anticipando 9,004 0s o FALSE (90.04%) y 996 1s o TRUE (9.96%).

# Y ya que sabemos los resultados reales al mirar la variable (columna) "Resultado" de
# nuestra base original arbol3.test...

# ...�Qu� tan buena es nuestra predicci�n? �Cu�l es el resultado de este testeo sobre
# la base de datos arbol.3.test. �Cu�l es el prop�sito de todo este ejercicio?

# Comparemos manzanas con manzanas

table(arbol3.test$Resultado)
prop.table(table(arbol3.test$Resultado))

# Guau, la diferencia parece peque�a, �de cu�nto es?

9004-8982; ((9004-8982)/8982)*100; 1-abs(((9004-8982)/8982))

# Para 0s o FALSE tenemos 22 unidades en aproximadamente 10 mil o 0.24% de delta. 
# Esto significa que nuestro modelo predictivo (�rbol) tiene un 99.75% de certeza 
# para 0s o FALSE a nivel agregado (hol�stico).

996-1018; ((996-1018)/1018)*100; 1-abs(((996-1018)/1018))

# Para 1s o TRUE. Tambi�n tenemos 22 unidades, sobre aproximadamente 1 mil o 2.2% de 
# delta. Esto significa que nuestro modelo predictivo (�rbol) tiene 
# un 97.8% de certeza para 1s o TRUE a nivel agregado

# �Cu�l ser� la precisi�n at�mica, l�nea por l�nea, observaci�n especifica predicha
# versus exactamente esa misma observaci�n especifica real? En otras palabras, �cu�ntas
# observaciones originalmente FALSAS, predecimos como FALSAS sin equivocarnos?

sum(arbol3.test$Resultado==prediccion.arbol3.test)/length(prediccion.arbol3.test)

# 98.06%.

# Excelente!! Si supiera mejor, dir�a que estos datos que nos han dado son totalmente 
# ficticios dado el alto nivel de certeza que, la experiencia indica, es imposible 
# alcanzar en la realidad... Pero bueno, estamos contentos, y no estamos
# invirtiendo plata ... a�n.

# Hora de exportar y enviar estas predicciones a los usuarios interesados.
# Se los enviaremos por email en un archivo que puedan utilizar f�cilmente.
# Vamos a ser proactivos y agregar una columna llamada "Observaciones" para facilitar
# el uso de nuestras predicciones a nuestros clientes internos/jefes
# /usuarios.

# Tambi�n vamos a crear una variable nueva para no tocar la predicci�n que hemos
# hecho y no arruinarla si nos equivocamos en alguna orden.


prediccion.arbol3.compartir <- data.frame(Observacion = 30001:40000, 
                                    Resultado = prediccion.arbol3.test)

write.csv(prediccion.arbol3.compartir, file = "prediccion.arbol3.csv", 
          row.names = FALSE)

# Para saber d�nde R ha grabado nuestro nuevo archivo e ir a buscarlo,
# debemos saber el directorio de nuestro espacio de trabajo (work directory).

getwd()

# Ahora recordemos el primer ejercicio, 1.4.1, donde los atributos 3 y 4
# parec�an potencialmente irrelevantes. 

# �Puede ser que nuestro modelo predictivo (nuestro �rbol) entonces tenga ramas
# que se puedan podar? Veamos si es as� y c�mo impactar�a sobre los resultados de 
# la predicci�n SIN poda que acabamos de hacer.

# El objetivo detr�s de podar el �rbol es evitar sobreajuste los datos. 
# Por lo general, queremos seleccionar un tama�o de �rbol que minimice el error
# de validaci�n cruzada, la columna de xerror impreso por printcp ().

printcp (fit.arbol3.train)

# Examinando los resultados de error con validaci�n cruzada, debemos seleccionar
# el par�metro de complejidad (cp) asociado con el error m�nimo, 
# y colocarlo en la funci�n de prune(). En este caso es f�cil de identificar, es el cp
# n�mero 5.
# Alternativamente, cuando los datos sean much�simo m�s extensos se puede utilizar el 
# siguiente fragmento de c�digo para que prune() lo busque autom�ticamente:
# fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"]

poda.arbol3 <- prune(fit.arbol3.train, cp = 0.01000000)

poda.arbol3.cpAuto <- prune(fit.arbol3.train, cp = fit.arbol3.train$cptable
                            [which.min(fit.arbol3.train$cptable[,"xerror"]),"CP"])

# �Confiamos en este c�digo "autom�tico" que alguien posteo en alg�n foro de internet?

identical(poda.arbol3, poda.arbol3.cpAuto)

# TRUE. Bien, son iguales! La fuente es de alta calidad y en el futuro volver� a ella
# por ayuda. Gracias a HSAUR por la idea (http://www.statmethods.net/about/books.html)
# Ahora voy a remover una de los 2 variables id�nticas para no hacerme l�o, y mantener
# mi escritorio lo m�s limpio y ordenado posible.

rm(poda.arbol3.cpAuto)

# Vamos a graficar el �rbol podado y compararlo con la versi�n anterior a la poda

par(mfrow = c(1,2)) # ya sabemos para que esto
fancyRpartPlot(fit.arbol3.train) # antes de la poda
fancyRpartPlot(poda.arbol3) # despues de la poda
par(mfrow = c(1,1)) # como habla este pibe, basta de comentarios por favor!

# Visualmente no notamos nada. �A ver si identical() detecta algo?

identical(fit.arbol3.train, poda.arbol3)

# No, identical() est� de acuerdo, no hay diferencias. En la realidad, esto ser�a una
# rara excepci�n que tambi�n podr�a darse. Rpart() es muy h�bil y, sin saberlo,
# nos percatamos tempranamente que los atributos 3 y 4 hab�an sido ignorados (podados)
# por rpart().

# Ten�amos un subgrupo de datos creados en la variable arbol3.prune
# Los desaf�o a realizar este ejercicio 1.4.3 completamente desde cero
# utilizando arbol3.prune para "entrenamiento" del modelo predictivo y arbol3.test 
# como el �rbol de testeo.


###################################################################################
################################## Random Forest ##################################
###################################################################################

# En t�rminos simples, la t�cnica Random Forest (Bosque aleatoria) se enfoca
# en el problema de sobreajuste (overfitting) de un �rboles de decisi�n. �Cu�ndo ocurre
# este sobreajuste? Sucede cuando un �rbol de decisi�n (un modelo predictivo)
# aprende "demasiado", cuando se ajusta con demasiada exactitud a la base de datos de 
# entrenamiento, y su capacidad de realizar predicciones �tiles sobre otras bases de 
# datos diferentes (i.e. testing) disminuye. Por ejemplo, en el �ltimo ejercicio
# vimos como rpart() ignoro algunos atributos (el 3 y 4) al generar el �rbol predictivo
# y, por ende, asumimos eso como la raz�n por la cual la poda no fue necesaria.
# Esto no es completamente correcto. Atributos 3 y 4 pudieron ser irrelevantes sin tener
# nada que ver con overfitting.

# Random forest es una forma de poda m�s avanzada que rpart() y prune(). 
# Involucra la creaci�n de m�ltiples �rboles de diferentes niveles de ajuste a
# la base de datos de entrenamiento. En el momento de la predicci�n, para
# cada l�nea u observaci�n, cada �rbol fabricado (entrenado) generar su 
# predicci�n y cuenta como su "voto". Por ejemplo, si tenemos 3 �rboles, de 
# los cuales 2 predicen 0 o FALSE, y el tercero predice 1 o TRUE,
# la observaci�n en cuesti�n quedar� predica como 0 o FALSE por "mayor�a de votos".
# Esta t�cnica de sobre entrenamiento de �rboles con diferentes ajuste, llega a la
# decisi�n final de clasificaci�n (valor predicho) de forma democr�tica y evita sobreajuste.

# Lo primero que debemos asegurarnos es que nuestras bases de datos no tengan valores
# faltantes o identificar la observaci�n (fila) y variable (columna) a completar.

# En nuestras bases de datos, no hay valores faltantes. En la pr�ctica, lo m�s com�n
# es que s� los haya. y en grandes cantidades. 
# Las soluciones son variadas. Se puede eliminar la(s) observaci�n(es) con
# datos faltantes, se pueden completar los valores faltantes con valores medios o
# promedios calculados con los valores de las dem�s observaciones para esa variable 
# en particular, o,  sorprendentemente, se pueden predecir los valores faltantes con
# �rboles!!

# Una vez que ya no nos falte ning�n dato, podemos iniciar con el primer 
# paso requerido para la creaci�n de un �rbol, que ser�a el de dividir la
# data.frame en partes, i.e. data.frame.train y data.frame.test

?randomForest2Rules

# Para ejecutar la funci�n randomForest(), debemos instalar el paquete ramdomForest

install.packages("randomForest") # s�lo es necesario hacerlo una vez
library("randomForest") # se debe cargar en cada nueva sesi�n de R
?randomForest

# randomForest() es muy similar a rpart(). El primer argumento es la f�rmula, igual
# formato que en rpart(). Al no haber un argumento de m�todo, hay que convertir
# todas la variable dependiente "Y" en factor dentro del argumento "formula":

# formula = as.factor(Resultado) ~ Atributo.1 + Atributo.2 + Atributo.3
# +                                         +Atributo.4 + Atributo.5 + Atributo.6

# Una forma de resumir la cantidad de tipeo en R, es usar un sin�nimo para "TODOS LOS
# ATRIBUTOS". Y ese sin�nimo es el punto ".". Quedar�a entonces m�s corto, de la siguiente forma

# formula = as.factor(Resultado) ~ . 

# Se lee: Resultado (en formato "factor") en funci�n de todas las variables (columnas)
# existente (sin incluir la columna "Resultado" obviamente). El
# problema con esta forma corta de incluir todo, es que no nos permite alterar el
# orden de las variables que le alimentamos a la formula. Nunca hemos alterado 
# el orden de la variables en nuestros ejercicios hasta ahora, pero bien podr�amos 
# desear hacerlo en la pr�ctica. Por ejemplo, queremos poner las variables "sexo" 
# y "edad" primero si es que en nuestra base de datos se encuentran en las 
# columnas 3 y 8 (por decir cualquier orden).

# El argumento data es igual que en rpart().

# El argumento "importance = TRUE" nos permitir� visualizar la importancia de cada
# variable en la predicci�n de los arboles (modelos) creados por randomForest().

# El argumento ntrees limita el n�mero de �rboles generados por randomForest.
# Esto puede ayudarnos con la gesti�n de recursos computacionales cuando estos son
# limitados.

# El argumento set.seed(123) asegura que el generador de n�meros aleatorios de R que
# utiliza randomForest() siempre arranque igual para poder reproducir los mismos 
# resultados muchas veces. De otra forma, siempre arrancara desde un random diferente
# y nunca ser� posible reproducir exactamente los mismo resultados.
# Esto es realmente �til para explicar en detalle una predicci�n de ser necesario, como
# por ejemplo a los alumnos o jefes que quieren ver exactamente c�mo y de d�nde salieron 
# los n�meros que se presentaron en power point, o un reporte PDF, etc. etc. etc.

# Una vez generado el modelo predictivo de randomForest (con una cantidad de ntrees 
# dentro de �l), se podr� utilizar la funci�n predict() como venimos haciendolo.

# Veamos un ejemplo usaremos las bases de datos ya generadas

str(arbol3.train) # 15 mil observaciones, 7 variables
str(arbol3.test) # 10 mil observaciones, 7 variables

# Vamos a marcar el inicio del seed para el randomizador para poder reproducir exactamente
# los mismos resultados en el futuro cuantas veces lo necesitemos

set.seed(123); fit.bosque.a3.train <- randomForest(as.factor(Resultado) ~ ., 
                                          data = arbol3.train, importance = TRUE,
                                              ntree = 1000)

# Para entretenernos un poco, veamos si hay diferencia entre los modelos
# predictivos fit.bosque.a3.train versus fit.arbol3.train

identical(fit.bosque.a3.train, fit.arbol3.train)

# FALSE. Era de esperarse, 1 arbol versus 1000 arboles. Analicemos en mayor detalle.

printcp (fit.arbol3.train)
printcp (fit.bosque.a3.train)

# No nos sirve, fit.bosque no es un objeto de rpart(), es un objeto de randomForest()

# Empecemos por analizar las variables importantes de nuestra randomForest

varImpPlot(fit.bosque.a3.train)

# El grafico de la izquierda nos muestra cuanto decaer�a la precisi�n o calidad
# de predicci�n del bosque en general al remover cada variable individualmente.
# El grafico de la derecha muestra el coeficiente de Gini que nos dice la importancia 
# de cada variable dentro del modelo.
# Dos formas diferentes de decir cosas parecidas, pero no iguales.
# El grafico de la izquierda habla de la ca�da en performance del modelo en general 
# seg�n se remueva cada variable. El coeficiente de Gini nos dice el aporte
#espec�fico de cada variable, independientemente del resto de las variables. 
# En nuestro caso, el orden descendiente de importancia es el mismo en ambos gr�ficos,
# pero no siempre se da de esta forma. Dos variables, A y B con orden de importancia 2 y 3
# respectivamente en el gr�fico de Gini de la derecha, podr�an perfectamente tener un orden
# opuesto en el gr�fico de la izquierda.
# En ambos gr�ficos, cuanto mayor es el desplazamiento del valor para cada
# variable hacia la derecha, mayor la importancia de esa variable.
# Dentro del modelo predictivo. El orden de importancia es 1 -> 6 -> 2 -> 5 
# con 3 y 4 aproximadamente en cero.

# Recordando fit.arbol3.train:

fancyRpartPlot(fit.arbol3.train)

# Exactamente lo mismo con rpart(): 1 -> 6 -> 2 -> 5

# Veamos si podemos hacer una comparacion grafica

par(mfrow = c(1,2))
fancyRpartPlot(fit.arbol3.train)
fancyRpartPlot(fit.bosque.a3.train) # Ups, error
plot(fit.bosque.a3.train) # Ahora tenemos algo, pero no nos sirve demasiado.
par(mfrow = c(1,1))

# Diferentes gr�ficos, comparar rpart() vs randomForest() de esta forma es como
# comparar peras con manzanas. No nos sirve.

# No obstante, el segundo gr�fico no dice algo muy �til:

# ntrees = 1000 en randomForest() fue un buen argumento ya que la varianza de error
# se aplana despu�s �rbol n�mero 550 aproximadamente. Genial!!

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

# OK. El ramdomForest() predice m�s 0s o FALSE, y menos 1s o TRUE que rpart()
# Tenemos los datos originales ("reales") de la base arbol3.test para comparar
# predicciones versus realidad. Vemos como queda.

# �Qui�n se lleva el beneficio de las predicciones, los 0s o FALSE, o los 1s o TRUE?

table(arbol3.test$Resultado)
table(prediccion.arbol3.test)
table(pred.bosque.a3.test)

9004-8982; ((9004-8982)/8982)*100; 1-abs(((9004-8982)/8982))
9060-8982; ((9060-8982)/8982)*100; 1-abs(((9060-8982)/8982))

# Para 0s o FALSE tenemos 22 unidades en aproximadamente 10 mil o 0.24% de delta
# con rpart() y 78 unidades o 0.87% de delta con randomForest(). 
# Esto significa que rpart() tiene un 99.75% de certeza para 0s o FALSE y 
# randomForest() tiene 99.13%.
# Rpart() era m�s acertado para 0s o FALSOS que randomForest()

996-1018; ((996-1018)/1018)*100; 1-abs(((996-1018)/1018))
940-1018; ((940-1018)/1018)*100; 1-abs(((940-1018)/1018))

# Para 1s o TRUE, rpart() predijo 22 unidades menos, o 2.2% de delta, d�ndonos 
# un 97.8% de certeza para 1s o TRUE. randomForest() predice 78 unidades menos, 
# o 7.7% de delta, d�ndonos un 92.3% de certeza para 1s o TRUE.

# Hmm, en cantidades agregada rpart() supera claramente a randomForest(). �Es esta
# Comparaci�n correcta, claro, la matem�tica funciona. 
# �Es una comaparaci�n justa, ��Realmente NO!!

# A nivel at�mico, observaci�n por observaci�n, �qui�n es mejor?

# Sab�amos que rpart() ten�a una exactitud, comparando observaci�n por observaci�n
# a nivel at�mico (i.e. l�nea 35667 real vs. 35667 predicha), del 98.06%. 
# Recordemos:

sum(arbol3.test$Resultado==prediccion.arbol3.test)/length(prediccion.arbol3.test)

# Calculemos para randomForest()

sum(arbol3.test$Resultado==pred.bosque.a3.test)/length(pred.bosque.a3.test)

# 98.16%. 

# Guau, randomForest() predijo cada observacion individual mejor que rpart(). 
# Lo cual significa que randomForest() es el m�s certero a nivel at�mico.

98.16-98.06

# La democracia entre los 1000 �rboles votantes dentro de randomForest()
# nos ha generado una mejora del 0.10%.

# Peque�a diferencia sin duda, de importancia RELATIVA!! Cada caso de la realidad,
# con datos reales, dictar� cual ser� la verdadera importancia de tan peque�as mejoras.

# Solo nos resta ahora creamos una data.frame con 2 columnas para compartir

# Creemos una variable nueva para no molestar a la variable que contiene nuestras
# predicciones

pred.bosque.a3 <- data.frame(Observacion = 30001:40000, 
                                  Resultado = pred.bosque.a3.test)

# Exportamos a un archivo compartible

write.csv(pred.bosque.a3, file = "pred.bosque.arbol3.csv", row.names = FALSE)

# Listo!!!

# Ten�amos un subgrupo de datos creados en la variable arbol3.prune
# Los desaf�o a realizar este ejercicio de randomForest completamente desde cero
# utilizando arbol3.prune para "entrenamiento" del modelo predictivo y arbol3.test 
# como el �rbol de testeo.