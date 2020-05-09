#############################################################################################

# Ejemplos prácticos de Algoritmos Genéticos en R
# El Problema del Viajante
# https://es.wikipedia.org/wiki/Problema_del_viajante
# - por Martin Vedani, UTN Business Intelligence

############################################################################################

# Instalar y cargar los paquetes que utilizaremos
if(! "TSP" %in% installed.packages()) install.packages("TSP", depend = TRUE)

# El siguiente ejemplo esta publicados en la siguiente página de la implementación del paquete
# TSP (por las siglas en ingles de "Travelling Sales Person")
# http://tsp.r-forge.r-project.org/

# Se recomienda enfáticamente leer el documento TSP.pdf y trabajar los ejemplos. Están en inglés por lo cual, de no leerlo, se pueden seguir los pasos debajo sin problema.
# Fuente: https://cran.r-project.org/web/packages/TSP/vignettes/TSP.pdf


## Cargar los paquetes y los datos a utilizar en la sesión actual de R
library("TSP")
data("USCA50")

## Revisar el objeto con el que trabajaremos
USCA50
# object of class 'TSP' 
# 50 cities (distance 'euclidean') 

# Calculamos giras del viajante (tours) con diferentes heurísticas y almacenamos los 
# resultados en la lista de recorridos "tours". Como ejemplo, mostramos la primera gira que 
# muestra el método empleado, el número de ciudades y la distancia total. Todas las longitudes
# de camino se comparan utilizando la tabla de puntos en la figura 1.
# Para el gráfico, agregamos un punto para la solución óptima que tiene una longitud de 
# recorrido de 14497. La solución óptima se puede encontrar utilizando Concorde 
# (method = "concorde"). Se omite aquí ya que Concorde tiene que ser instalado por separado.

metodos <- c("nearest_insertion", "farthest_insertion", "cheapest_insertion",
             "arbitrary_insertion", "nn", "repetitive_nn", "two_opt")

set.seed(123)
viajes <- sapply(metodos, FUN = function(m) solve_TSP(USCA50, method = m), simplify = FALSE)

# viajes$concorde <- solve_TSP(tsp, method = "concorde")
# Solucion Optima= 14497

viajes[[1]]
# object of class 'TOUR' 
# result of method 'nearest_insertion' for 50 cities
# tour length: 17378 

#Visualizar resultados de las distancias generadas por cada método
dotchart(sort(c(sapply(viajes, tour_length), optimal = 14497)),
         xlab = "tour length", xlim = c(0, 20000))
title("Figura 2")

# El camino de Hamilton

data("USCA312")

tsp <- insert_dummy(USCA312, label = "cut")
tsp
#object of class 'TSP' 
# 313 cities (distance 'euclidean')

# El TSP contiene ahora una ciudad ficticia adicional y podemos tratar de resolver este TSP

set.seed(123)
gira <- solve_TSP(tsp, method="farthest_insertion")
gira
# object of class 'TOUR' 
# result of method 'farthest_insertion' for 313 cities
# tour length: 39438 

# Como la ciudad ficticia ("dummy") tiene distancia cero a todas las demás ciudades, la longitud del 
# camino es igual a la longitud de la gira que nos informó R anteriormente. La ruta se inicia
# con la primera ciudad en la lista después de la ciudad ficticia y termina con la ciudad 
# justo antes de ella. Utilizamos cut_tour() para crear un camino y mostramos las primeras
# y últimas 6 ciudades a continuación.

camino <- cut_tour(gira, "cut")

head(labels(camino))
# [1] "Prince Rupert, BC" "Vancouver, BC"     "Victoria, BC"      "Bellingham, WA"   
# [5] "Seattle, WA"       "Tacoma, WA" 

tail(labels(camino))
# [1] "Carson City, NV" "Reno, NV"        "Eureka, CA"      "Hilo, HI"        "Honolulu, HI"   
# [6] "Lihue, HI"  

# La gira que se encuentra en este ejemplo nos da un camino desde Prince Rupper (Canada) 
# a Lihue (HAwaii). Un camino de este tipo también se puede visualizar mediante el paquetes 
# sp, mapas y MapTools (Pebesma y Bivand 2005).

if(! "sp" %in% installed.packages()) install.packages("sp", depend = TRUE)
if(! "maps" %in% installed.packages()) install.packages("maps", depend = TRUE)
if(! "maptools" %in% installed.packages()) install.packages("maptools", depend = TRUE)
if(! "rgeos" %in% installed.packages()) install.packages("rgeos", depend = TRUE)

library("maps")
library("sp")
library("maptools")
data("USCA312_map")

plot_path <- function(path){
  plot(as(USCA312_coords, "Spatial"), axes = TRUE)
  plot(USCA312_basemap, add = TRUE, col = "gray")
  points(USCA312_coords, pch = 3, cex = 0.4, col = "red")
  path_line <- SpatialLines(list(Lines(list(Line(USCA312_coords[path,])), ID="1")))
  plot(path_line, add=TRUE, col = "black")
  points(USCA312_coords[c(head(path,1), tail(path,1)),], pch = 19, col = "black")
}

plot_path(camino)
title("Figura 3")

# A modo de ejemplo, elegimos Nueva York como la ciudad de partida. Transformamos los datos 
# en un objeto ATSP ("Asymetric" Travelling Sales Person) y establecemos la columna 
# correspondiente a Nueva York como punto cero antes de resolverlo. Por lo tanto, la 
# distancia para volver desde la última ciudad hasta
# Nueva York, no contribuye a la longitud de la trayectoria o camino. Nosotros usamos la 
# heurística del vecino más cercano ("nn" por sus siglas en ingles del nearest neighbor)
# para calcular un recorrido inicial.

atsp <- as.ATSP(USCA312)
ny <- which(labels(USCA312) == "New York, NY")
atsp[, ny] <- 0
set.seed(123)
tour_inicial <- solve_TSP(atsp, method="nn")
tour_inicial 
#object of class 'TOUR' 
# result of method 'nn' for 312 cities
# tour length: 49173 

# Y ahora mejoramos el tour_inicial usando el metodos 2-Opt, controlando versus
# tour_incial

tour <- solve_TSP(atsp, method ="two_opt", control = list(tour = tour_inicial))
tour
# object of class 'TOUR' 
# result of method 'two_opt' for 312 cities
# tour length: 40214 

path <- cut_tour(tour, ny, exclude_cut = FALSE)
head(labels(path))
# [1] "New York, NY"    "Jersey City, NJ" "Paterson, NJ"    "Newark, NJ"      "Elizabeth, NJ"  
# [6] "Trenton, NJ"

tail(labels(path))
# [1] "Hilo, HI"          "Prince Rupert, BC" "Juneau, AK"        "Whitehorse, YK"   
# [5] "Dawson, YT"        "Fairbanks, AK"

plot_path(path)
title("Figura 4")

# Para encontrar el camino más corto de Hamilton también podemos restringir los dos puntos
# extremos, el de partida y el final. Este problema puede ser transformado a un TSP mediante
# la sustitución de las dos ciudades por una sola ciudad que contiene las distancias desde el
# punto de inicio en sus columnas y las distancias al punto final en sus filas.

# Para el siguiente ejemplo, sólo estamos interesados en caminos que empiezan en Nueva York y
# que terminan en Los Ángeles. Por lo tanto, eliminamos las dos ciudades de la matriz de 
# distancias, creamos un TSP asimétrico e insertamos una ciudad ficticia llamada "LA / NY". 
# Las distancias de "DESDE" esta ciudad ficticia
# la ciudad se sustituyen por las distancias de Nueva York y las distancias "HACIA" se
# sustituyen por las distancias hacia Los Ángeles.

m <- as.matrix(USCA312)
ny <- which(labels(USCA312) == "New York, NY")
la <- which(labels(USCA312) == "Los Angeles, CA")
atsp <- ATSP(m[-c(ny,la), -c(ny,la)])
atsp <- insert_dummy(atsp, label = "LA/NY")
la_ny <- which(labels(atsp) == "LA/NY")
atsp[la_ny, ] <- c(m[-c(ny,la), ny], 0)
atsp[, la_ny] <- c(m[la, -c(ny,la)], 0)

# Utilizamos la heurística de inserción más cercana

set.seed(123)
tour <- solve_TSP(atsp, method ="nearest_insertion")
tour
# object of class 'TOUR' 
# result of method 'nearest_insertion' for 311 cities
# tour length: 45585 

path_labels <- c("New York, NY", labels(cut_tour(tour, la_ny)), "Los Angeles, CA")
path_ids <- match(path_labels, labels(USCA312))
head(path_labels)
# [1] "New York, NY"      "Jersey City, NJ"   "Newark, NJ"        "Elizabeth, NJ"    
# [5] "Central Islip, NY" "Bridgeport, CT" 

tail(path_labels)
# [1] "Stockton, CA"      "Santa Barbara, CA" "San Diego, CA"     "Yuma, AZ"         
# [5] "Tucson, AZ"        "Los Angeles, CA" 

plot_path(path_ids)
title("Figura 5")

# La ruta que se muestra en la Figura 5 contiene múltiples cruces que indican que la solución
# es sub-óptima. La solución óptima generada por reformular el problema como un TSP y el uso 
# de Concorde sólo tiene una longitud de recorrido de 38.489.

# Reordenamiento por agrupación

# La idea es que los objetos en un grupo son visitados en orden consecutivo y
# de un grupo al siguiente más grande es necesario hacer "saltos".

# Este tipo de reordenamiento por agrupación (si recordamos la Unidad 2.1 - Agrupamiento o 
# Clustering) sugiere encontrar automáticamente los límites de cluster o numero k de grupos 
# mediante la adición de un numero k de ciudades ficticias que tienen 
# distancia constante c a un centro. En la solución óptima del TSP, la
# ciudades ficticias deben separar las ciudades más distantes y por lo tanto representan los
# límites óptimos para clusters k.
# Para el siguiente ejemplo, utilizamos el conocido conjunto de datos de R llamado iris. 
# Como sabemos que el conjunto de datos contiene tres clases o tipos diferentes de especies
# identificadas bajo la variable "Species", insertamos tres ciudades ficticias en el TSP 
# para los datos de la base iris y realizamos reordenamiento por clustering usando el método
# por defecto (algoritmo de inserción más cercano). Tenga en cuenta que este algoritmo no 
# encuentra la solución óptima y no se garantiza que las ciudades ficticias presentarán los
# mejores límites de cluster.

data("iris")
str(iris)
unique(iris$Species)

tsp <- TSP(dist(iris[-5]), labels = iris[, "Species"])
tsp_dummy <- insert_dummy(tsp, n = 3, label = "boundary")
set.seed(1234)
tour <- solve_TSP(tsp_dummy)
tour
# object of class 'TOUR' 
# result of method 'arbitrary_insertion+two_opt' for 153 cities
# tour length: 47.67833 

# A continuación, trazamos la matriz de distancia permutada del TSP utilizando el sombreado 
# para representar distancias.
# El resultado se muestra en la Figura 6. Las áreas más claras representan distancias grandes.
# Las líneas rojas representan las posiciones de las ciudades ficticias en la gira, que 
# marcan los límites de los clusters obtenidos.

# Matrix de distancia

image(tsp_dummy, tour, xlab = "objects", ylab ="objects")
title("Figura 6")

## dibujar líneas donde se ubican las ciudades ficticias

abline(h = which(labels(tour)=="boundary"), col = "red")
abline(v = which(labels(tour)=="boundary"), col = "red")

# Un par de líneas horizontales y verticales rojas separa exactamente la areas más oscura
# de las más claras.
# El segundo par se produce en el interior del bloque oscuro más grande. Podemos ver lo bien
# que la partición obtenida se ajusta a la estructura de los datos dados por el campo de las
# especies en el conjunto de datos. Dado que usamos a la especie como etiquetas para las 
# ciudades en el TSP, las etiquetas en la solución "tour" representan la partición con
# las ciudades ficticias llamadas "boundary" (limites) que separan a los diferentes clusters.
# El resultado se puede resumir mirando la longitud obtenida bajo la etiqueta del cluster
# para cada tour obtenido:

out <- rle(labels(tour))
data.frame(Species = out$values, Lenghts = out$lengths, Pos = cumsum(out$lengths))

#       Species Lenghts Pos
# 1      setosa      32  32
# 2    boundary       1  33
# 3  versicolor       4  37
# 4    boundary       1  38
# 5   virginica       1  39
# 6  versicolor      40  79
# 7   virginica      32 111
# 8  versicolor       1 112
# 9   virginica      15 127
# 10 versicolor       1 128
# 11  virginica       1 129
# 12 versicolor       1 130
# 13  virginica       1 131
# 14 versicolor       3 134
# 15   boundary       1 135
# 16     setosa      18 153

# Una frontera (boundary) divide perfectamente los datos en un grupo que contiene sólo 
# ejemplos de especies 'Setosa' y un segundo grupo que contiene ejemplos para 'versicolor'. 
# Sin embargo, el segundo límite sólo se separa por 1 (un) ejemplos de especie Virginica 'de 
# otros 40 ejemplos de la misma especie. Incluso en el recorrido óptimo encontrado por 
# Concorde, este problema se produce.
# La razón por la que la agrupación por reordenamiento falla para dividir los datos en tres 
# grupos es la cercanía entre los grupos Virginica 'y' versicolor '. Para inspeccionar este 
# problema aún más, podemos proyectar los puntos de datos en los dos primeros componentes
# principales del conjunto de datos y agregar el segmentos de trazado que resultaron del 
# resolver de TSP.

prc <- prcomp(iris[1:4])
plot(prc$x, pch = as.numeric(iris[,5]), col = as.numeric(iris[,5]))
indices <- c(tour, tour[1])
indices[indices > 150] <- NA
lines(prc$x[indices,])
title("Figura 7")

# El resultado se muestra en la Figura 7. Las tres especies se identifican por diferentes 
# marcadores y todos los puntos conectados por una sola ruta representan una agrupación o 
# cluster encontrado. Claramente, los dos grupos a la derecha están demasiado cerca para ser 
# separados correctamente utilizando sólo las distancias entre puntos individuales. Este 
# problema es similar al efecto de encadenamiento conocido en la agrupación jerárquica 
# utilizando el método de un solo vínculo.

#                                           Conclusión

# En este trabajo los autores del paquete TSP presentan el paquete de extensión TSP R que 
# implementa una infraestructura para manejar y resolver el problema del viajero.
# El paquete introduce clases de descripciones de los problemas simétricos y asimétricos
# (TSP y ATSP respectivamente) y de solución (TOUR o GIRA).


