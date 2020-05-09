#############################################################################################

# Ejemplos prácticos de Redes Neuronales en R - por Martin Vedani, UTN Business Intelligence

############################################################################################

# Instalar y cargar los paquetes que utilizaremos
if(! "neuralnet" %in% installed.packages()) install.packages("neuralnet", depend = TRUE)
if(! "NeuralNetTools" %in% installed.packages()) install.packages("NeuralNetTools",
                                                                  depend = TRUE)
library(neuralnet)
library(NeuralNetTools)


# Cargar los datos que utilizaremos
neuralNetData <- read.csv("neuralNetData.csv", header = T)
head(neuralNetData)
str(neuralNetData)

# Tenemos 101 observaciones de datos numéricos de los cuales, 9 variables son de entrada "E" 
# que resultan en una respuesta "Y". En total 10 variables. 
# Para que los números no sean tan abstractos, podemos pensarlos de esta forma: 10 variables 
# (columnas) representa datos de 9 datos y 1 resultado para 101 personas (filas) que han 
# pagado o no han pagado (respuesta Y) un préstamo, entiendo 1 = TRUE, 0 = FALSE.
# Estos datos los hemos recolectado a lo largo del tiempo, lo que no ha dado la "experiencia"
# (la data.frame de 101 x 10 que acabamos de subir con read.csv) y que ahora hemos de
# de utilizar para entrenar a nuestra red neuronal (de forma muy similar a lo que hicimos con
# arboles de decisiones).

# Dividir los datos en grupo de entrenamiento y otro de testeo

entrenamiento <- neuralNetData[1:50, ]

testeo <- neuralNetData[51:101, ]

# Ahora vamos a construir una red neuronal con 4 nodos ocultos (una red neuronal se compone
# de una entrada, oculta y nodos de salida). Se elige el número de nodos de aquí sin 
# un método claro, sin embargo hay algunas reglas generales. La opción LifeSign se refiere 
# al nivel de detalle. El ouput no es lineal y vamos a utilizar un valor umbral del 10%. 
# El paquete NeuralNet utiliza backpropagation elástico con regresión por pesos como su 
# algoritmo estándar.

?neuralnet
set.seed(1234)
nuestra.red <- neuralnet(Y ~ E1 + E2 + E3 + E4 + E5 + E6 + E7 + E8 + E9, entrenamiento, 
                         hidden = 4, lifesign = "minimal", linear.output = FALSE, 
                         threshold = 0.1)

# Veamos la forma de nuestra red y la importancia relativa de las variables de entrada

plot(nuestra.red, rep = "best")
garson(nuestra.red)
olden(nuestra.red, out_var = "Y")

# Testeamos
?subset
sub.testeo <- subset(testeo, select = c("E1", "E2", "E3", "E4", "E5", "E6", "E7", "E8", "E9"))
head(sub.testeo)
str(sub.testeo)

# El conjunto de datos sub.testeo es un subconjunto de nuestra base de datos de testeo, 
# contiene sólo las variables (columnas) de entrada, no hemos incluido la columna de 
# respuesta "Y". 
# El conjunto se ve de la siguiente manera:

?compute
computos <- compute(nuestra.red, sub.testeo)

# Observemos los resultados de que tan bien aprendió nuestra red neuronal
# comparando las predicciones vs con la información real Y que tenemos.



resultados <- data.frame(real = testeo$Y, prediccion = computos$net.result)
head(resultados)

# Podemos redondear al entero más próximo para mejorar el entendimiento:

resultados$prediccion <- round(resultados$prediccion)
head(resultados)

# Vemos que las redes neuronales son muy parecidas a los arboles de decisión, se utilizan
# para el mismo propósito como hemos visto en este ejercicio de hoy, y los anteriores en la
# unidad de árboles. Hay muchas opiniones respecto de cuál es mejor. A nivel muy alto, se
# podría decir:

# Árboles De Decisión

# - Pueden ser más rápido una vez entrenados (aunque ambos algoritmos pueden entrenar 
#   lentamente dependiendo algoritmo exacto y la cantidad / dimensionalidad de los datos). 
#   Esto se debe a un árbol de decisión intrínsecamente "tira a la basura", cuenta con la 
#   entrada que no lo encuentra útil, mientras que una red neuronal utilizará todos ellos a 
#   menos que hagas algo de la selección de características como una etapa de 
#   pre-procesamiento.

# - Si es importante para entender lo que el modelo está haciendo, los árboles son muy 
#   interpretable.

# - Probablemente se quiera estar seguro de podar el árbol para evitar el exceso de ajuste.
#   randomForest soluciona este problema como hemos visto.

# Redes Neuronales

# - Más lento (tanto para la formación y clasificación), y menos interpretable.

# - Si los datos llegan por stream, se pueden hacer actualizaciones incrementales 
#   estocásticos a diferencia de los árboles de decisión que utilizan algoritmos de 
#   aprendizaje por "batch" (por lote)

# - Se pueden modelar funciones más arbitrarias (interacciones no lineales, etc.) y por lo 
#   tanto podrían ser más precisos, siempre que haya suficientes datos de entrenamiento. 
#   Pero pueden ser propensas al sobre-ajuste ajuste también.

# La recomendación es siempre entender bien el flujo de información para las variables de
# entrada y, en función de costo y beneficio, elegir el algoritmo más apropiado. Claro,
# para poder hacer el análisis de costo vs. beneficio, hay que implementar ambos algoritmos
# y compararlos en términos de calidad predictiva, solución de overfitting, y del tiempo y 
# recursos necesarios para el procesamiento.
