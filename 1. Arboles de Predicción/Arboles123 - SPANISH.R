# Arboles 1, 2 y 3. Por Martín Vedani, originalmente como ayudante de la materia
# en la UTN (educación a distancia).

# Este es un desarrollo para leer línea por línea e ir ejecutando orden por orden
# manualmente, de forma interactiva.

# Todo texto a la derecha del símbolo "#" son comentarios del autor. Es irrelevante
# para R si lo resaltas o no con el mouse antes de hacer clic en "run",
# ya que R simplemente ejecutara los comandos anteriores a # en cada línea e ignorará
# lo que se encuentre a la derecha de #.

######################################################################################
######################################################################################

# Primero que nada, juntemos nuestras herramientas.
# Debemos instalar el paquete de algoritmos de Arboles de R (desde el mirror de CRAN 
# más cercano) para que la bajada sea lo más veloz posible. Cualquier mirror de CRAN 
# sirve obviamente porque son todos iguales. RSTUDIO tiene su propio espejo de CRAN
# en algún lugar.

install.packages("rpart") #Solo se hace una vez, luego queda instalado

# Cargar dicho paquete en la sesión activa de R.

library(rpart) #Hay que cargar el paquete cada vez que se inicia la sesión de R.

# Importemos ahora los archivos a la sesión de trabajo activa creando variables.
# Es necesario tener los datos crudos de cada árbol separados de antemano en 
# 3 archivos diferentes y guardados como *.CSV (desde Excel en nuestro caso).
# A continuación veamos 2 métodos de lograr nuestras variables. Hay mucho métodos y
# formatos adicionales muy bien explicados en un enorme número de foros y YouTube.
# Los 2 primeros métodos abren un ventana de dialogo en Windows
# donde se puede navegar manualmente hasta la ubicación de los archivos.
# En el tercer método se necesita saber de antemano la ubicación de(los) archivo(s) lo
# cual no requiere interrupción para interacción manual del usuario.

arbol1 <- read.csv("C:\\Users\\admin\\Documents\\GitHub\\Data-Science---Business-Intelligence-Labs-with-R\\1. Arboles de Predicción\\Arbol1CSV.csv", header = T) 
arbol2 <- read.csv("C:\\Users\\admin\\Documents\\GitHub\\Data-Science---Business-Intelligence-Labs-with-R\\1. Arboles de Predicción\\Arbol2CSV.csv", header = T)
arbol3 <- read.csv("C:\\Users\\admin\\Documents\\GitHub\\Data-Science---Business-Intelligence-Labs-with-R\\1. Arboles de Predicción\\Arbol3CSV.csv", header = T)

# Veamos y entendamos la clase y estructura para cada de las variables recién creadas

str(arbol1) #data frame de 10 mil observaciones con 7 variables
str(arbol2) #data frame de 20 mil observaciones con 7 variables
str(arbol3) #data frame de 40 mil observaciones con 7 variables


###################################################################################
##################################### Ejercicio 1 #################################
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

# Veamos los resultados para saber que hay en nuestro primer árbol

printcp(fit.arbol1)
summary(fit.arbol1)
rsq.rpart(fit.arbol1)

# Que cantidad de información. Veamos si graficando se entiende mejor.

plot(fit.arbol1)
text(fit.arbol1)

# Tratemos de graficar algo más útil.

par(mfrow = c(1,2)) #para dividir la sección de gráficos en 1x2 ( 1 fila y 2 columnas)
plotcp(fit.arbol1)
plot(fit.arbol1, uniform = TRUE, main = "arbol1")
text(fit.arbol1, use.n = T, all = T, cex = .7) #Agrega texto al grafico anterior, el grafico #2 en este caso
par(mfrow = c(1,1)) #resetear la sección de gráficos a 1 x 1 (1 fila, 1 columna)

# Salió un mensaje de error que no se sabe bien que significa.

# Por suerte existen paquetes adicionales en R que nos facilitan lograr gráficos 
# más interesantes

install.packages(c("rattle", "rpart.plot", "RColorBrewer")) #Instalar 1 sola vez
library(rattle) # cargar en cada sesión de R
library(rpart.plot) # cargar en cada sesión de R
library(RColorBrewer) # cargar en cada sesión de R

# Estudiemos la documentación de la nueva función de gráficos que utilizaremos.
# (Entender la documentación es cuestión de práctica y experiencia)

?fancyRpartPlot

# OK, mas allá de titulo y subtitulo, ambos opcionales, solo le tenemos que señalar la 
# variable que contiene a nuestro árbol (nuestro modelo predictivo). 
# La documentación nos indica que debe ser un objeto de rpart - SIN PROBLEMA!!

par(mfrow = c(1,2)) #dividir sección de gráficos para comparar lado a lado
plot(fit.arbol1, uniform = TRUE, main = "arbol1") #Feo grafico 1,
text(fit.arbol1, use.n = T, all = T, cex = .7) # Agrega texto al feo grafico 1
fancyRpartPlot(fit.arbol1) #Grafico 2
par(mfrow = c(1,1)) #resetear la sección de gráficos

# Grafico 2 es mucho mas claro y útil. 
# Gracias a:

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
# y así sucesivamente con los atributos 6, 2, y 5.

# Los demás atributos (3 y 4) no nos ayudarían a predecir, serian
# irrelevantes.

###################################################################################
##################################### Ejercicio 2 #################################
###################################################################################

# Seleccionar un conjunto de datos al azar de la tabla arbol2 y construir un árbol de 
# decisión para ese conjunto.
# Hacer lo mismo sobre los datos no utilizados y comparar.

# Dividir arbol2 a la mitad. Una mitad sera nuestra variable de entrenamiento y la 
# otra mitad la variable de testeo

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

# Comparemos ambos aprendizajes.
# Un resultado TRUE = idénticos 
# resultado FALSE = No idénticos
# Queremos poder testear (sobre arbol2.test) lo que 
# aprendimos (con el arbol2.train) sobre dos bases diferentes y evaluar si
# las predicciones funcionan o no.

identical(fit.arbol2.train, fit.arbol2.test)

# FALSO, lo que significa que nuestros modelos aprendieron cosas diferentes, 
# pero ... ¿en base a datos iguales o diferentes?

identical(arbol2.train, arbol2.test)
# FALSE

# Ok, hasta ahora sabemos que rpart() “aprendió” cosas diferentes sobre dos mitades
# (o subconjuntos) diferentes de una misma bases de datos.

# Veamos visualmente el resultado

par(mfrow = c(1,2))
fancyRpartPlot(fit.arbol2.train)
fancyRpartPlot(fit.arbol2.test)
par(mfrow = c(1,1)) #resetear la sección de gráficos a 1x1

# Vemos que la importancia de los atributos predictivos son iguales.
# La función rpart() obviamente es objetiva y sabe como ajustarse
# a sus diferentes datos (subgrupos) de una misma familia (base de datos).

###################################################################################
##################################### Ejercicio 3 #################################
###################################################################################

# Seleccionar tres conjuntos de datos al azar (a,b,c) a partir de la tabla arbol3.
# Construir una árbol de decisión a partir del subconjunto a.
# Realizar una poda usando el subconjunto b.
# Verificar que poder predictivo tiene cada nodo usando el subconjunto c.

arbol3.train <- arbol3[1:15000,] #base para el árbol de decisión
arbol3.prune <- arbol3[15001:30000,] # base para la poda
arbol3.test <- arbol3[30001:40000,] #base para verificar el poder predictivo del árbol

str(arbol3.train) #data frame de 15 mil observaciones con 7 variables
str(arbol3.prune) #data frame de 15 mil observaciones con 7 variables
str(arbol3.test) #data frame de 10 mil observaciones con 7 variables

identical(arbol3.train, arbol3.prune) #Buscamos FALSE
identical(arbol3.prune, arbol3.test) #Buscamos FALSE
identical(arbol3.train, arbol3.test) #Buscamos FALSE

# Bien, los 3 subconjuntos de la misma bases de datos son diferentes. 
# Del ejercicio Ya sabemos que con rpart(), diferentes arboles o modelos de predicción
# aprenden diferentes cosas (los modelos dependen del subconjunto
# utilizado para fabricarlos) pero logran las mismas predicciones (un mismo modelo 
# puede predecir correctamente sobre los demas subconjuntos y, por ende, sobre toda la bases de datos).

# Sabiendo esto, no necesitamos 3 modelos predictivos. Fabricar fit.arbol3.prune y 
# fit.arbol3.test no es necesarios así que no lo haremos.

# Generemos entonces nuestro árbol predictivo.

fit.arbol3.train <- rpart(formula = Resultado ~ Atributo.1 + Atributo.2 + Atributo.3
                          + Atributo.4 + Atributo.5 + Atributo.6, 
                          data = arbol3.train, method = "class")

# Y ahora que lo tenemos, usemoslo para predecir algo

#Estudiemos la documentación de la nueva función que vamos a necesitar

?predict

prediccion.arbol3.test <- predict(fit.arbol3.train, arbol3.test, type = "class")

# Eso parece haber hecho algo interesante a una gran velocidad
# ¿Que tiene esta nueva variable "prediccion.arbol3.test" que nos sea útil?
# Al llamar fit.arbol3.train, utilizamos nuestro árbol de prediccion que fabricamos
# (aprendimos) utilizando la base de datos de entrenamiento arbol3.train

# Recordemos la formula de nuestro modelo

printcp(fit.arbol3.train)

# formula = Resultado ~ Atributo.1 + Atributo.2 + Atributo.3 + Atributo.4 + Atributo.5 
# + Atributo.6, data = arbol3.train, method = "class")

# Al definir arbol3.test para llegar a nuestra variable "prediccion.arbol3.test", 
# le pedimos a predict() una prediccion de la variable "Resultado" para cada una de
# las 10,000 observaciones (lineas) dentro de arbol3.test utilizando los atributos 1:6 
# también dentro arbol3.test. Y le pedimos a predict() que prediga "Resultado" 
# de la forma que aprendimos al crecer o fabricar el árbol llamado "fit.arbol3.train"
# utilizando rpart().

# Veamos que resultados tenemos entonces.

summary(prediccion.arbol3.test)
str(prediccion.arbol3.test)

# 9,004 0s (o FALSE) y 996 1s (o TRUE)

# OK. A ver gráficamente:

fancyRpartPlot(prediccion.arbol3.test)

# No funciona, nos da error. Esta función es poderosa pero no sirve para un vector 
# de 1 solo factor con 2 niveles (1s y 0s / TRUE y FALSE). 
# A ver con algo mas básico

plot(prediccion.arbol3.test)

# La misma información que obtuvimos con la función summary()
# Nada nuevo

scatter.smooth(prediccion.arbol3.test)

# Simplifiquemos.

# Veamos las primeras lineas de los resultados en crudo

head(prediccion.arbol3.test)

# Para ver un poco mas, las primeras 50 lineas:

head(prediccion.arbol3.test, 50)

# Es fundamental seguirle el paso a todas nuestra variables, ya que algunas
# variables son:
# i) datos crudos: arbol1, arbol2, arbol3; 
# ii) subdivisiones de datos crudos: arbol3.train, arbol3.prune, arbol3.test;
# iii) modelos predictivos: fit.arbol3.train;
# iv) resultados de predicciones: prediccion.arbol3.test

# Para ver como termina nuestra base de predicciones de resultados:

tail(prediccion.arbol3.test, 50)

# Tenemos 10 mil predicciones de Resultados 0s y 1s en base a 
# los atributos 1:6 de la base de datos "arbol3.test"

# Genial. Tenemos la cantidad correcta de predicciones, según los atributos
# correctos, para las observaciones (lineas) originales correctas (30,001 a 40,000)
# gracias a un modelo bien aprendido utilizando rpart()

# ¿Cómo presentamos estas predicciones?
# En cantidades y proporciones seria una buena idea.

table(prediccion.arbol3.test)
prop.table(table(prediccion.arbol3.test))

# Estamos anticipando 9,004 0s o FALSE (90.04%) y 996 1s o TRUE (9.96%).

# Y ya que sabemos los resultados reales al mirar la variable (columna) "Resultado" de
# nuestra base original arbol3.test...

# ...¿Qué tan buena es nuestra predicción? ¿Cuál es el resultado de este testeo sobre
# árbol.3.test ?

table(arbol3.test$Resultado)
prop.table(table(arbol3.test$Resultado))

# la diferencia parece poca

9004-8982; ((9004-8982)/8982)*100; 1-abs(((9004-8982)/8982))

# Para 0s o FALSE tenemos 22 unidades en aproximadamente 10 mil o 0.24% de delta. 
# Esto significa que nuestro modelo predictivo (árbol) tiene un 99.75% de certeza 
# para 0s o FALSE a nivel agregado.

996-1018; ((996-1018)/1018)*100; 1-abs(((996-1018)/1018))

# Para 1s o TRUE. También tenemos 22 unidades, sobre aproximadamente 1 mil o 2.2% de 
# delta. Esto significa que nuestro modelo predictivo tiene 
# un 97.8% de certeza para 1s o TRUE a nivel agregado.

# ¿Cual sera la precisión atómica de la predicción linea por linea
# versus lo que realmente sucedió para cada caso de la realidad? En otras palabras, ¿cuantas
# observaciones originalmente FALSE/TRUE, predecimos como FALSE/TRUE sin equivocarnos?

sum(arbol3.test$Resultado==prediccion.arbol3.test)/length(prediccion.arbol3.test)

# 98.06%.

# Nada mal. Si estuviésemos prediciendo buenos y malos pagadores de una cartera
# de préstamos, el riesgo de equivocarnos es menos del 2%.

# Hora de exportar y enviar estas predicciones a los usuarios interesados.

# Crearemos una variable nueva con las observaciones (tomadores de préstamo) 
# en la primera columna, y la predicción de si cada uno de ellos será buen o 
# mal pagador en la segunda columna.


prediccion.arbol3.compartir <- data.frame(Observacion = 30001:40000, 
                                          Resultado = prediccion.arbol3.test)

# Crearemos un archivo que nuestros usuarios internos puedan utilizar fácilmente.

write.csv(prediccion.arbol3.compartir, file = "prediccion.arbol3.csv", 
          row.names = FALSE)

# Para saber donde R ha grabado nuestro nuevo archivo e ir a buscarlo,
# debemos saber el directorio de nuestro espacio de trabajo (work directory).

getwd()

# Ahora recordemos el primer ejercicio, 1.4.1, donde los atributos 3 y 4
# parecían potencialmente irrelevantes. 

# ¿Puede ser que nuestro modelo predictivo (nuestro árbol) entonces tenga ramas
# que se puedan podar? Veamos si es así y como impactaría sobre los resultados de 
# la prediccion SIN poda que acabamos de hacer.

# El objetivo detrás de podar el árbol es evitar sobre ajuste los datos. 
# Por lo general, queremos seleccionar un tamaño de árbol que minimice el error
# de validación cruzada, la columna de xerror impreso por printcp ().

printcp (fit.arbol3.train)

# Examinando los resultados de error con validación cruzada, debemos seleccionar
# el parámetro de complejidad (cp) asociado con el error mínimo, 
# y colocarlo en la función de prune(). En este caso es fácil de identificar, es el cp
# numero 5.
# Alternativamente, cuando los datos sean muchísimo mas extensos se puede utilizar el 
# siguiente fragmento de código para que prune() lo busque automáticamente:
# fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"]

poda.arbol3 <- prune(fit.arbol3.train, cp = 0.01000000)

poda.arbol3.cpAuto <- prune(fit.arbol3.train, cp = fit.arbol3.train$cptable
                            [which.min(fit.arbol3.train$cptable[,"xerror"]),"CP"])

# ¿Confiamos en este código "automático"?

identical(poda.arbol3, poda.arbol3.cpAuto)

# TRUE. Bien, son iguales! Gracias a HSAUR por la idea (http://www.statmethods.net/about/books.html) # Ahora voy a remover una de los 2 variables idénticas para mantener # el escritorio de trabajo lo mas limpio y ordenado posible.  rm(poda.arbol3.cpAuto)  # Vamos a graficar el árbol podado y compararlo con la versión anterior a la poda  par(mfrow = c(1,2)) # ya sabemos para que esto fancyRpartPlot(fit.arbol3.train) # antes de la poda fancyRpartPlot(poda.arbol3) # después de la poda par(mfrow = c(1,1))

# Visualmente no notamos nada. ¿A ver si identical() detecta algo?

identical(fit.arbol3.train, poda.arbol3)

# Para identical() no hay diferencias.
# Rpart() es muy hábil desde el inicio.
# Los atributos 3 y 4 ya habían sido ignorados (podados) por rpart().

# Teníamos un subgrupo de datos creados en la variable arbol3.prune
# Los desafío a realizar este ejercicio 1.4.3 completamente desde cero
# utilizando arbol3.prune para "entrenamiento" del modelo predictivo y arbol3.test 
# como el árbol de testeo.


###################################################################################
################################## Random Forest ##################################
###################################################################################

# En términos simples, la técnica Random Forest (Bosque Aleatorio) se enfoca
# en el problema de sobre ajuste (overfitting) de un arboles de decisión. ¿Cuando ocurre
# este sobre ajuste? Sucede cuando un árbol de decisión. (un modelo predictivo)
# aprende "demasiado", cuando se ajusta con demasiada exactitud a la base de datos de 
# entrenamiento, y su capacidad de realizar predicciones útiles sobre otras subgrupo de 
# datos de la misma base original (i.e. testing) disminuye.

# Random forest es una forma de poda mas avanzada que rpart() y prune(). 
# Involucra la creación de múltiples arboles de diferentes niveles de ajuste al
# subgrupo de datos de entrenamiento. En el momento de la prediccion, para
# cada linea u observación, cada árbol fabricado (entrenado) generar su 
# prediccion y cuenta como su "voto". Por ejemplo, si tenemos 5 arboles, de 
# los cuales tres predicen 0 o FALSE, y dos predicen 1 o TRUE,
# la observación en cuestión quedara predicha como 0 o FALSE por "mayoría de votos".
# Esta técnica de sobre entrenamiento de arboles con diferentes ajuste, llega a la
# decisión. final de clasificación (valor predicho) de forma democrática y evita sobre ajuste.

# Lo primero que debemos asegurarnos es que nuestras bases de datos no tengan valores
# faltantes o identificar la observación (fila) y variable (columna) a completar.

# En nuestras bases de datos, no hay valores faltantes. En la practica, lo mas común
# es que sí los haya, y en grandes cantidades. 
# Las soluciones para resolver falta de datos son variadas. 
# Se puede eliminar la(s) observación(es) con
# datos faltantes, se pueden completar los valores faltantes con valores medios o
# promedios calculados con los valores de las demás observaciones para esa variable 
# en particular, o,  sorprendentemente, se pueden predecir los valores faltantes con
# arboles!!

# Una vez que ya no nos falte ningún dato, podemos iniciar con el primer 
# paso requerido para la creación de un árbol, que seria el de dividir la
# data.frame en partes, i.e. data.frame.train y data.frame.test

?randomForest2Rules

# Para ejecutar la función randomForest(), debemos instalar el paquete ramdomForest

install.packages("randomForest") # solo es necesario hacerlo una vez
library("randomForest") # se debe cargar en cada nueva sesión de R
?randomForest

# randomForest() es muy similar a rpart(). El primer argumento es la formula, igual
# formato que en rpart(). Al no haber un argumento de método, hay que convertir
# todas la variable dependiente "Y" en factor dentro del argumento "formula":

# formula = as.factor(Resultado) ~ Atributo.1 + Atributo.2 + Atributo.3
# +                                         +Atributo.4 + Atributo.5 + Atributo.6

# Una forma de resumir la cantidad de tipeo en R, es usar un sinónimo que signifique 
# "TODOS LOS ATRIBUTOS". Y ese sinónimo es el punto "."
# Quedaría entonces mas corto, de la siguiente forma

# formula = as.factor(Resultado) ~ . 

# Se lee: Resultado (en formato "factor") en función de todas las variables (columnas)
# existente (sin incluir la columna "Resultado" obviamente). El
# problema con esta forma corta de incluir todo, es que no nos permite alterar el
# orden de las variables que le alimentamos a la formula. Nunca hemos alterado 
# el orden de la variables en nuestros ejercicios hasta ahora, pero bien podríamos 
# desear hacerlo en la practica. Por ejemplo, queremos poner las variables "sexo" 
# y "edad" primero si es que en nuestra base de datos se encuentran en las 
# columnas 3 y 8 (por decir cualquier orden).

# El argumento data es igual que en rpart().

# El argumento "importance = TRUE" nos permitirá visualizar la importancia de cada
# variable en la prediccion de los arboles (modelos) creados por randomForest().

# El argumento ntrees limita el numero de arboles generados por randomForest.
# Esto puede ayudarnos con la gestión de recursos computacionales cuando estos son
# limitados.

# El argumento set.seed(123) asegura que el generador de números aleatorios de R que
# utiliza randomForest() siempre arranque igual para poder reproducir los mismos
# resultados muchas veces. De otra forma, siempre arrancara desde un random diferente
# y nunca sera posible reproducir exactamente los mismo resultados.
# Esto es realmente útil para explicar en detalle una prediccion de ser necesario, como
# por ejemplo al querer demostrar exactamente cómo y de dónde salieron 
# las predicciones.

# Una vez generado el modelo predictivo de randomForest (con una cantidad de ntrees 
# dentro), se podrá utilizar la función predict() como venimos haciéndolo.

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

# FALSE. Era de esperarse, 1 árbol versus 1000 arboles. Analicemos en mayor detalle.

printcp (fit.arbol3.train)
printcp (fit.bosque.a3.train)

# No nos sirve, fit.bosque no es un objeto de rpart(), es un objeto de randomForest()

# Empecemos por analizar las variables importantes de nuestra randomForest

varImpPlot(fit.bosque.a3.train)

# El grafico de la izquierda nos muestra cuanto decaería la precisión o calidad
# de prediccion del bosque en general al remover cada variable individualmente.
# El grafico de la derecha muestra el coeficiente de Gini que nos dice la importancia 
# de cada variable dentro del modelo.
# Dos formas diferentes de decir cosas parecidas, pero no iguales.
# El grafico de la izquierda habla de la caída en performance del modelo en general 
# según se remueva cada variable. El coeficiente de Gini nos dice el aporte
#especifico de cada variable, independientemente del resto de las variables. 
# En nuestro caso, el orden descendiente de importancia es el mismo en ambos gráficos,
# pero no siempre se da de esta forma.
# En ambos gráficos, cuanto mayor es el desplazamiento del valor para cada
# variable hacia la derecha, mayor la importancia de esa variable.
# Dentro del modelo predictivo. El orden de importancia es 1 -> 6 -> 2 -> 5 
# con 3 y 4 aproximadamente en cero.

# Recordando fit.arbol3.train:

fancyRpartPlot(fit.arbol3.train)

# Exactamente lo mismo con rpart(): 1 -> 6 -> 2 -> 5

# Veamos si podemos hacer una comparación grafica

par(mfrow = c(1,2))
fancyRpartPlot(fit.arbol3.train)
fancyRpartPlot(fit.bosque.a3.train) # Ups, error
plot(fit.bosque.a3.train) # Ahora tenemos algo, pero no nos sirve demasiado.
par(mfrow = c(1,1))

# El segundo grafico no dice algo muy útil:

# ntrees = 1000 en randomForest() fue un buen argumento ya que la gran varianza de error
# se aplana después del árbol numero 550 aproximadamente. Este sería el mínimo de arboles
# necesarios para minimizar errores de prediccion.

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

# OK. El ramdomForest() predice mas 0s o FALSE, y menos 1s o TRUE que rpart()
# Tenemos los datos originales ("reales") de la base arbol3.test para comparar
# predicciones versus realidad. Vemos como queda.

# ¿Quien se lleva el beneficio de las predicciones, los 0s o FALSE, o los 1s o TRUE?

table(arbol3.test$Resultado)
table(prediccion.arbol3.test)
table(pred.bosque.a3.test)

9004-8982; ((9004-8982)/8982)*100; 1-abs(((9004-8982)/8982))
9060-8982; ((9060-8982)/8982)*100; 1-abs(((9060-8982)/8982))

# Para 0s o FALSE tenemos 22 unidades en aproximadamente 10 mil o 0.24% de delta
# con rpart() y 78 unidades o 0.87% de delta con randomForest(). 
# Esto significa que rpart() tiene un 99.75% de certeza para 0s o FALSE y 
# randomForest() tiene 99.13%.
# Rpart() es mas acertado para 0s o FALSOS que randomForest()

996-1018; ((996-1018)/1018)*100; 1-abs(((996-1018)/1018))
940-1018; ((940-1018)/1018)*100; 1-abs(((940-1018)/1018))

# Para 1s o TRUE, rpart() predijo 22 unidades menos, o 2.2% de delta, dándonos 
# un 97.8% de certeza para 1s o TRUE. randomForest() predice 78 unidades menos, 
# o 7.7% de delta, dándonos un 92.3% de certeza para 1s o TRUE.

# En cantidades agregadas rpart() supera claramente a randomForest().

# A nivel atómico, observación por observación, ¿quien predice mejor?

# rpart() tenia una exactitud, comparando observación por observación
# a nivel atómico (i.e. linea 35667 real vs. 35667 predicha), del 98.06%. 
# Recordemos:

sum(arbol3.test$Resultado==prediccion.arbol3.test)/length(prediccion.arbol3.test)

# Calculemos para randomForest()

sum(arbol3.test$Resultado==pred.bosque.a3.test)/length(pred.bosque.a3.test)

# 98.16%. 

# randomForest() predijo cada observación individual mejor que rpart(). 
# Lo cual significa que randomForest() es el mas certero a nivel atómico.

98.16-98.06

# La democracia entre los 1000 arboles votantes dentro de randomForest()
# nos ha generado una mejora del 0.10%.

# Solo nos resta ahora creamos una data.frame con 2 columnas para compartir con nuestros
# clientes internos

# Creemos una variable nueva igual que hicimos anteriormente

pred.bosque.a3 <- data.frame(Observacion = 30001:40000, 
                             Resultado = pred.bosque.a3.test)

# Exportamos a un archivo compartible

write.csv(pred.bosque.a3, file = "pred.bosque.arbol3.csv", row.names = FALSE)

# Listo!!!

# Teníamos un subgrupo de datos creados en la variable arbol3.prune
# Los desafío a realizar este ejercicio de randomForest completamente desde cero
# utilizando arbol3.prune para "entrenamiento" del modelo predictivo y arbol3.test 
# como el árbol de testeo.