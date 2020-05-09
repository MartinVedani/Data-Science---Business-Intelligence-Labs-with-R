# Ejercicios de Agrupamiento en R - por Martin Vedani, UTN Business Intelligence

#########################################################################################
#################################### Ejercicio 2.1.1 ####################################
#########################################################################################

# Terminar con el agrupamiento del ejemplo de la página 2 y luego continuar el 
# dendrograma, que es la representación gráfica del proceso de agrupamiento jerárquico 
# y permite observar la distancia a la que se van uniendo los elementos (es fundamental 
# al momento de definir el criterio de parada).

# Agrupamiento jerárquico es útil en las primeras etapas de análisis de datos cuando se está 
# tratando de tener una comprensión de los datos. Por ejemplo, la búsqueda de una jerarquía 
# entre diferentes factores o variables.

# https://es.wikipedia.org/wiki/Agrupamiento_jer%C3%A1rquico

# Lo primero que debemos hacer es definir distancias sobre las cuales definir jerarquías por
# cercanía.

# ¿cómo definimos cerca? Este es el paso más importante y hay varias posibilidades en función 
# de las cuestiones que se está tratando de responder y los datos que se tienen. 

# Distancia o similitud suelen ser las métricas utilizadas. Hay varias maneras de medir la 
# distancia o similitud.

# Distancia euclídea es la distancia "vuelo de pájaro", la distancia más corta en línea
# recta. Muchas aplicaciones, sin embargo, no puede utilizar de manera realista la distancia 
# euclídea. Coches, por ejemplo, no pueden atravesar edificios y obstáculos y deben
# seguir las calles y caminos. Por ende, en términos realistas, y también para los argumentos
# de funciones en R, también existe la distancia o método de "Manhattan", en 2 o más dimensiones
# al igual que con distancia euclidiana.

# Distancia euclídea y la similitud de correlación son medidas continuas, mientras que la 
# distancia de Manhattan es una medida binaria.

# En este ejercicio nos concentraremos en distancia euclídea.

# El método de Manhattan no lo veremos implementado pero en la unidad de Algorítmos 
# Genéticos, en dilema del viajante se implementa una solucion de ciudades muy interesante.

getwd() # Por defecto R toma los archivos de esta carpeta
generoPeliculas <- read.csv("generoPeliculas.csv", header = TRUE)

# o

generoPeliculas <- read.table(file.choose(), header = TRUE, sep = ",")

generoPeliculas

# Tenemos un data frame con datos numericos, characteres. Necesitamos lograr
# una matriz de solamente numeros para calcular distancias.

Pelis <- generoPeliculas[,2:4] #toma columnas de 2 a 4
Pelis
rownames(Pelis) <- generoPeliculas$Caso #cambiea el nomrbre de las filas al mismo del archivo original
Pelis 

# Ahora tenemos la matriz Pelis de 5 observaciones con 3 coordenadas cartesianas
# x = Comed_1, y = Darama_2, z = Accion_3 (o cualquier otro orden, es indiferente)

# Ahora generamos una matriz triangular inferior de todas las distancias euclidianas posibles
# entre todos los puntos
distanciaPelis <- dist(Pelis, method = "euclidean")
distanciaPelis

# Con la suerte de nuestro lado, R nos proporciona una función simple que nos crea el
# dendrograma de agrupamiento jerárquico. Se llama hclust() y toma como argumento la matriz 
# triangular inferior de distancia que miramos en el paso anterior.

?hclust
agruparPelis <- hclust(distanciaPelis)
agruparPelis

# Veamos algunos resultados
par(mfrow = c(1,2))
plot(agruparPelis)
plot(as.dendrogram(agruparPelis))
par(mfrow = c(1,1))

# El primer grafico nos da bastante información, pero para identificar claramente 
# la cantidad de grupos a diferentes distancias, el segundo en más útil. 
# Veamoslo más grande para entender por qué.

plot(as.dendrogram(agruparPelis))

# Si quisiésemos saber, por ejemplo, cuantos grupos tenemos a una distancia de 0.5 (sin
# unidades), ¿qué contestaríamos?

abline(h = 0.5, col="red")

# Contestaríamos 5 grupos, la misma cantidad de observaciones con las que comenzamos.
# Esta distancia (o calificación de película) es irrelevante.

# Y a una distancia de 2.5, ¿cuantos grupos de observaciones similares tenemos?

abline(h = 2.5, col="red")

# 3, porque nuestra línea corta 3 "ramas". Y los grupos similares son A+C y B+D, con E
# por sí sola quien, claramente, tiene un gusto en películas estadísticamente diferente
# a de las otra 4 personas.

#########################################################################################
#################################### Ejercicio 2.1.2 ####################################
#########################################################################################

# Tomar el conjunto de puntos X,Y representado en la tabla "cluster1" de la base de 
# datos Clusters y aplicarle un algoritmo bottom up basado en la distancia euclídea 
# utilizando como representación de un conjunto el promedio entre los puntos. Detenerse 
# al llegar a 5 conjuntos.

getwd() # Por defecto R toma los archivos de esta carpeta
cluster <- read.csv("cluster.txt", header = FALSE)
# o
cluster  <- read.table(file.choose(), header = FALSE, sep = ",")

head(cluster) # R asignó autom?ticamente el nombre de las columnas (variables) V1 y V2

# Cambiamos los nombres de las columnas para que representen exactamente lo que son
colnames(cluster) <- (c("x","y"))
head(cluster)

plot(cluster)

# Parecería que hay 5 agrupamientos, busquemos major detalle calculando distancias

distanciaCluster <- dist(cluster, method = "euclidean")

# Creamos la agrupación jerárquica
agruparCluster <- hclust(distanciaCluster)

# Veamos los resultados
plot(as.dendrogram(agruparCluster))

# Genial. Vemos que a partir de la distancia 1.4 aproximadamente, ya tenemos agrupaciones
# relevantes que nos pueden ayudar a comenzar a estudiar un gran número de observaciones
# por grupos con algún sentido común entre ellos - la investigación luego nos dirá en qué son
# parecidos los conjuntos compuestos por tantas observaciones.

abline (h = 1.4, col = "red")

# ¿Cuántas agrupaciones tenemos en la distancia 1.4? ¿Cuántas ramas corta la línea roja
# que acabamos de dibujar sobre el gráfico?

#########################################################################################
#################################### Ejercicio 2.1.3 ####################################
#########################################################################################

# El dendrograma incluido en la teoría, grafica el proceso de agrupamiento jerárquico de 
# provincias de argentina según variables del censo de población 2010. Analizarlo y comentar:

# a) Según este resultado, ¿en cuántos niveles agruparía a las provincias?
#    La decisión de la cantidad de los grupos depende de las distancia. Por ejemplo, a 
#    distancia 10, haríamos 5 grupos, porque esas son las cantidades de ramas que
#    cortaríamos al dibujar un línea en distancia = 10.


# b) Si tuviera que formar 4 grupos, ¿qué provincias incluiría en cada uno?
#    A una distancia de 13 aproximadamente, se formarían 4 grupos. Siguiendo las ramificaciones,
#    los grupos serían:
#              Grupo 1: Salta + Santiago del Estero + AMBA_3C + La Rioja +
#                       Catamarca + Santa Fe + Misiones
#              Grupo 2: Jujuy solamente
#              Grupo 3: AMBA + La Pampa
#              Grupo 4: Buenos Aires + Santa Cruz + Corrientes + Entre Ríos

#########################################################################################
#################################### Ejercicio 2.1.4 ####################################
#########################################################################################

# Tomar el conjunto de puntos x,y representado en la tabla "cluster1" de la base de datos
# Clusters y aplicarle K-medias para llegar a 5 conjuntos tomando los datos según X 
# creciente.

# Esta Técnica es más útil en las primeras etapas de análisis de datos cuando se está 
# tratando de tener una comprensión de los datos. Por ejemplo, la búsqueda de un cierto 
# patrón o relación entre diferentes factores o variables.

# Estamos de suerte otra vez, el paquete kmeans ya viene disponible en R.

?kmeans

# La documentación de R nos dice que el método k-means "pretende dividir los puntos
# en una cantidad de grupos K tal que se minimice la suma de las distancias cuadradas 
# desde cada hasta el centro del grupo ("cluster") al que ha sido asignada.

# Aunque tiene varios parámetros, sólo hablaremos de cuatro. Estos son x, (la matriz numérica
# o data frame de los datos), centers, iter.max y nstart. El segundo de estos (centers) puede 
# ser un número de grupos o un conjunto de centroides iniciales. El tercero, iter.max, 
# especifica el número máximo de iteraciones permitidas, y nstart es el número de arranques al 
# azar que se desean probar si centros es un número.

# Paso 2: Creación de los datos:
# Luego es necesario crear un "data frame" con los datos que tenemos para elaborar 
# los clusters
# Para eso podemos recurrir a una función que carga directamente un archivo del tipo CSV que tiene
# solamente los datos que queremos agrupar:

getwd() # Por defecto R toma los archivos de esta carpeta
cluster <- read.csv("cluster.txt", header = FALSE) 

# Alternativa para buscar archivo manualmente
cluster <- read.table(file.choose(), header = FALSE)
head(cluster) # R asign? autom?ticamente el nombre de las columnas (variables) V1 y V2

# Cambiamos los nombres de las columnas para que representen exactamente lo que son
colnames(cluster) <- (c("x","y"))
head(cluster)

# Tenemos 1000 puntos (observaciones o filas en R), cada uno sus coordenadas x,y en el 
# eje cartesiano (columnas o variables en R).

# Paso 3: Agrupamiento:

plot(cluster)

# Paso 4: Observando el plot, k-means clustering requiere cierta distancia métrica 
# (digamos euclidiana), un número fijo e (estimativo o hipotético) de clusters, y una 
# estimación inicial de la ubicación de los centroides para cada dicho número estimado o hipotético 
# de clusters.

# A primer vista se pueden identificar 5 agrupamientos, por lo que invocamos la función
# kmeans() y "adivinaremos" que deseamos agrupar alrededor de 5 centros, 

# Fijamos set.seed() para duplicar resultados y guardamos los resultados de kmeans() en
# una variable.

set.seed(123)
agrupar5 <- kmeans(cluster, centers = 5)

# Paso 5: visualizando los resultados.

# Al finalizar, k-means clustering devuelve una posición final del centroide de cada grupo, 
# así como la asignación de cada punto de datos u observación a un cluster.

# Invocando la variable creada con kmeans() obtenemos la información del tamaño de clusters, 
# y las coordenadas x,y de los centros de cada uno de los 5 grupos.

agrupar5

# K-means clustering with 5 clusters of sizes 420, 87, 101, 191, 202 
# (depende tambien del sistema orperativo, set seed es para duplicar los mismo resultados en mismos
# sistemas. Yo estoy usando mac, es probable que Windows o Linux den resultados diferentes con 
# set.seed(123) o con versiones differentes de las fórmula kmeans().

# Cluster means:
#   x          y
# 1 2.976254 -0.9909601
# 2 6.522571  1.0127496
# 3 5.545365  0.9713600
# 4 2.984065  3.0040805
# 5 4.979240  3.9473686

# Clustering vector:
# [1] 4 3 3 1 1 4 4 1 4 5 1....

# Within cluster sum of squares by cluster:
#  [1] 484.39693  15.00750  18.16448  32.87873  31.78665
# (between_SS / total_SS =  90.9 %)

plot(cluster)
points(agrupar5$centers, col="black", bg="red", pch=23, cex=2)

# Interesante, son 5 centros, pero no son exactamente los 5 que habíamos 
# "adivinado" y estabamos esperando, ¿verdad?

# probemos con 6 centros
set.seed(123)
agrupar6 <- kmeans(cluster, 6)
agrupar6 # total_SS =  91.0 % => Mejoramos un poco, 0.1%

plot(cluster)
points(agrupar6$centers, col="black", bg="red", pch=23, cex=2) 
# no mejoramos lo suficiente como para justificar un centro adicional que, visualmente,
# parece redundante.

# probemos con 4 centros
set.seed(123)
agrupar4 <- kmeans(cluster,4)
agrupar4 # total_SS =  83.4 %, empeoramos un poco

plot(cluster)
points(agrupar4$centers, col="black", bg="red", pch=23, cex=2) 
# no solo empeoramos, sino que ahora parece que tenemos 2 centros "fuera o lejos de grupo".

# Dejemos que kmean() elija la mejor combinación de 5 centros de 
# 25 intentos completamente aleatorios
set.seed(123)
agruparMejor5 <- kmeans(cluster, 5, nstart=25)
agruparMejor5 # total_SS =  96.7 % es una mejora MUY interesante

plot(cluster)
points(agruparMejor5$centers, col="black", bg="red", pch=23, cex=2)
#Ahora sí tenemos los 5 centros esperados que "adivinamos por aproximación 
# visual" al comienzo.

# Otra forma interesante de graficar los grupos (con o sin centroides) sería la 
# siguiente
?plot
plot(cluster[,"x"],cluster[,"y"], col=agruparMejor5$cluster,pch=19,cex=1)
# al argumentar colores = clusters, R automáticamente elige una cantidad de 
# colores  igual a la cantidad de agrupamientos que genero kmeans(), que son 5.

#Para incluir centroides en el grafico anterior, incluir la siguiente línea de 
# código:
points(agruparMejor5$centers, col="black", bg="red", pch=23, cex=2)

# Si con los mejores 5 de 25 intentos aleatorios nos fue tan bien, ¿nos irá mejor
# con 2,500 intentos iniciales?
set.seed(123)
agruparMejor5de2500 <- kmeans(cluster, 5, nstart=2500)
agruparMejor5de2500 

# total_SS =  96.7 % nos significó gastar 100 veces más recursos de CPU, por un 
# beneficio que tal vez no se note hasta la milésima, diezmilésima, etc. 
# El despilfarro no se notó porque nuestros datos son pocos, no obstante, es una
# consideración que todo programador debe tener en cuenta al evaluar los costos vs.
# los beneficios de incrementar significativamente los argumentos (parámetros) en
# algoritmos de este o cualquier tipo.

# Repasemos:
# 1) K-means clustering requiere que se especifique un número de grupos antes de empezar.
# 2) K-means clustering NO requiere que se especifique un número de iteraciones antes de 
#    empezar
# 3) K-means clustering NO siempre se detendrá luego de la misma cantidad de iteraciones
# 4) Cada conjunto de datos NO tiene un número fijo de agrupamientos.
# 5) Al iniciar kmeans() con un numero de centroides/agrupamientos al azar, NO se terminar
#    siempre con la misma agrupación final.

# Prueba los mejores 5 centros de 50 mil y 100 mil intentos por ti mismo para ver
# qué sucede. ¿Se ve una mejora notable? R puede tardar un poco en procesar 
# tantos intentos, así que ten paciencia al ejecuta las ordenes.

#########################################################################################
#################################### Ejercicio 2.1.5 ####################################
#########################################################################################

# Ídem 2.1.4 pero tomando los datos según Y creciente.

# Para ordenar una data frame en R, se utiliza la función de order(). Por defecto, la 
# clasificación es ascendente.

?order

# Ordenar por Y ascendente
head(cluster)
clusterYasc <- cluster[order(cluster$y), 1:2] 
head(clusterYasc)

set.seed(123)
agruparYasc <- kmeans(clusterYasc, 5, nstart=25)
agruparYasc

plot(cluster)
points(agruparYasc$centers, col="black", bg="red", pch=23, cex=2)

# ¿Los resultados resultan familiares? ¿El orden de los datos, influencia realmente sus 
# coordenadas, distancias euclidianas y los agrupamientos de kmeans()?

# Para extraer los componentes de  # "clusters", podemos utilizar sus referencias al pie de:

agruparYasc

agruparYasc[1] # "clusters"
agruparYasc[2] # "centers"
agruparYasc[3] # "totss"
agruparYasc[4] # "withinss"
agruparYasc[5] # "tot.withinss"
agruparYasc[6] # "betweenss" 
agruparYasc[7] # "size"
agruparYasc[8] # "iter"
agruparYasc[9] # "ifault"

