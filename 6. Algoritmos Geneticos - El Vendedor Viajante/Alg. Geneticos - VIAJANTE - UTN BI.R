#############################################################################################

# Ejemplos prácticos de Algoritmos Genéticos en R
# El Problema del Viajante
# https://es.wikipedia.org/wiki/Problema_del_viajante
# - por Martin Vedani, UTN Business Intelligence

############################################################################################

# Instalar y cargar los paquetes que utilizaremos
if(! "TSP" %in% installed.packages()) install.packages("TSP", depend = TRUE)
install.packages("doParallel", depend = TRUE)

# El siguiente ejemplo esta publicados en la siguiente página de la implementación del 
# paquete TSP (por las siglas en ingles de "Travelling Sales Person")
# http://tsp.r-forge.r-project.org/

# Se recomienda enfáticamente leer el documento TSP.pdf y trabajar los ejemplos. 
# Está en inglés por lo cual, de no leerlo, se pueden seguir los pasos debajo sin problema.
# Fuente: https://cran.r-project.org/web/packages/TSP/vignettes/TSP.pdf


# El Pronblema es un clásico y dice así:

# Dada una lista de ciudades y las distancias entre cada par de ciudades, ¿cuál es la 
# ruta más corta posible que visita cada ciudad al menos una vez y regresa a la ciudad 
# de origen?

# Pues veamos como resolverlo:

## Cargar los paquetes y los datos a utilizar en la sesión actual de R
library("TSP")
data("USCA50")
library(doParallel)

?registerDoParallel
?makePSOCKcluster

cl <- parallel::makeCluster(3, setup_timeout = 0.5)

# setup_timeout values > 1 s consistently did not at the time of this publication.The issue
# is being worked on https://github.com/rstudio/rstudio/issues/6692

registerDoParallel(cl)


## Revisar el objeto con el que trabajaremos
USCA50
# object of class 'TSP' 
# 50 cities (distance 'euclidean') 

# Calculamos giras del viajante (viajes) con diferentes heurísticas y almacenamos los 
# resultados en la lista de recorridos "viajes". Como ejemplo, mostramos la primera gira que 
# muestra el método empleado, el número de ciudades y la distancia total. Todas las 
# longitudes de camino se comparan utilizando la tabla de puntos en la figura 1.
# Para el gráfico, agregamos un punto para la solución óptima que tiene una longitud de 
# recorrido de 14497. La solución óptima se puede encontrar utilizando Concorde 
# (method = "concorde"). Se omite aquí ya que Concorde tiene que ser instalado por separado.

?solve_TSP

# Actualmente, los siguiente metodos estan disponibles

metodos <- c("nearest_insertion", "cheapest_insertion", "farthest_insertion", 
             "arbitrary_insertion", "nn", "repetitive_nn", "two_opt")

set.seed(123)
viajes <- sapply(metodos, FUN = function(m) solve_TSP(USCA50, method = m), 
                                                                      simplify = FALSE)

# Agregamos algunos recorridos usando repeticiones y refinamientos de dos opciones

viajes$'nn+two_opt' <- solve_TSP(USCA50, method="nn", two_opt=TRUE)
viajes$'nn+rep_10' <- solve_TSP(USCA50, method="nn", rep=10)
viajes$'nn+two_opt+rep_10' <- solve_TSP(USCA50, method="nn", two_opt=TRUE, rep=10)
viajes$'arbitrary_insertion+two_opt' <- solve_TSP(USCA50)

# viajes$concorde <- solve_TSP(tsp, method = "concorde")
# Solucion Optima= 14497

#Visualizar resultados de las distancias generadas por cada método
dotchart(sort(c(sapply(viajes, tour_length), optimal = 14497)),
         xlab = "tour length")
title("Figura 1 - Viajes")

viajes[["nn+two_opt+rep_10"]]
# oobject of class ‘TOUR’ 
# result of method ‘nn+two_opt_rep_10’ for 50 cities
# tour length: 14826

labels(viajes[["nn+two_opt+rep_10"]])
# [1] "Baltimore, MD"     "Allentown, PA"     "Atlantic City, NJ" "Central Islip, NY"
# [5] "Bridgeport, CT"    "Albany, NY"        "Brattleboro, VT"   "Brockton, MA"     
# [9] "Boston, MA"        "Cambridge, MA"     "Augusta, ME"       "Bangor, ME"       
# [13] "Burlington, VT"    "Alert, NT"         "Anchorage, AK"     "Bellingham, WA"   
# [17] "Calgary, AB"       "Brandon, MB"       "Bismarck, ND"      "Billings, MT"     
# [21] "Butte, MT"         "Boise, ID"         "Carson City, NV"   "Berkeley, CA"     
# [25] "Bakersfield, CA"   "Albuquerque, NM"   "Amarillo, TX"      "Abilene, TX"      
# [29] "Austin, TX"        "Beaumont, TX"      "Baton Rouge, LA"   "Biloxi, MS"       
# [33] "Birmingham, AL"    "Atlanta, GA"       "Augusta, GA"       "Asheville, NC"    
# [37] "Ashland, KY"       "Bowling Green, KY" "Bloomington, IL"   "Cedar Rapids, IA" 
# [41] "Battle Creek, MI"  "Bay City, MI"      "Ann Arbor, MI"     "Akron, OH"        
# [45] "Canton, OH"        "Brantford, ON"     "Burlington, ONT"   "Buffalo, NY"      
# [49] "Belleville, ON"    "Binghamtom, NY" 


# El camino de Hamilton

data("USCA312")

tsp <- insert_dummy(USCA312, label = "cut")
tsp
#object of class 'TSP' 
# 313 cities (distance 'euclidean')

# El TSP contiene ahora una ciudad ficticia adicional y podemos tratar de resolver este TSP

set.seed(123)
gira <- sapply(metodos, FUN = function(m) solve_TSP(tsp, method = m), 
                 simplify = FALSE)

# Agregamos algunos recorridos usando repeticiones y refinamientos de dos opciones

gira$'nn+two_opt' <- solve_TSP(tsp, method="nn", two_opt=TRUE)
gira$'nn+rep_10' <- solve_TSP(tsp, method="nn", rep=10)
gira$'nn+two_opt+rep_10' <- solve_TSP(tsp, method="nn", two_opt=TRUE, rep=10)
gira$'arbitrary_insertion+two_opt' <- solve_TSP(tsp)

#Visualizar resultados de las distancias generadas por cada método
dotchart(sort(c(sapply(gira, tour_length))), xlab = "tour length")
title("Figura 2 - Gira")

# Como la ciudad ficticia ("dummy") tiene distancia cero a todas las demás ciudades, 
# la longitud del camino es igual a la longitud de la gira que nos informó R 
# anteriormente. La ruta se inicia con la primera ciudad en la lista después de la 
# ciudad ficticia y termina con la ciudad justo antes de ella. 

gira[["nn+two_opt+rep_10"]]
# object of class ‘TOUR’ 
# result of method ‘nn+two_opt_rep_10’ for 313 cities
# tour length: 36182

# Utilizamos cut_tour() para crear un camino y mostramos las primeras y últimas 6 ciudades 
# a continuación.

gira <- gira[["nn+two_opt+rep_10"]]
camino <- cut_tour(gira, "cut")

head(labels(camino))
# [1] "Lihue, HI"       "Honolulu, HI"    "Hilo, HI"        "Carson City, NV" "Reno, NV"       
# [6] "Boise, ID" 

tail(labels(camino))
# [1] "Whitehorse, YK" "Dawson, YT"     "Fairbanks, AK"  "Anchorage, AK"  "Nome, AK"      
# [6] "Alert, NT"   

# La gira que se encuentra en este ejemplo nos da un camino desde Lihue, Hawaii 
# a San Juan de Puerto Rico. Un camino de este tipo también se puede visualizar mediante el 
# los paquetes sp, mapas y MapTools (Pebesma y Bivand 2005).

install.packages("sp", depend = TRUE)
install.packages("maps", depend = TRUE)
install.packages("maptools", depend = TRUE)
install.packages("rgeos", depend = TRUE)

library("maps")
library("sp")
library("maptools")
data("USCA312_GPS")

# Busca todo el path en tu máquina local al archivo USCA312_map.rda provisto.
# Solía ser parte del paquete TSP pero fue actualizada a nuevo formato bajo el nombre 
# USCA312_GPS. Puedes observar las diferencais por tú mismo ya que hemos cargado ambos 
# formatos como variables globales y estan disponibles en RStudio.

load(file = "<path to>/USCA312_map.rda")

# Creamos un grafico especial que utilizaremos varias veces.
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

# como próximo paso, vamos a elegir Nueva York como la ciudad de partida. Transformamos 
# los datos en un objeto ATSP ("Asymetric" Travelling Sales Person) y establecemos la 
# columna correspondiente a Nueva York como punto cero antes de resolverlo. Por lo tanto, 
# la distancia para volver desde la última ciudad hasta Nueva York, no contribuye a la 
# longitud de la trayectoria o camino. Nosotros usamos la heurística del vecino más 
# cercano ("nn" por sus siglas en ingles del nearest neighbor) para calcular un recorrido
# inicial.

atsp <- as.ATSP(USCA312)
ny <- which(labels(USCA312) == "New York, NY")
atsp[, ny] <- 0

set.seed(123)
tour_inicial <- sapply(metodos, FUN = function(m) solve_TSP(atsp, method = m), 
               simplify = FALSE)

# Agregamos algunos recorridos usando repeticiones y refinamientos de dos opciones

tour_inicial$'nn+two_opt' <- solve_TSP(atsp, method="nn", two_opt=TRUE)
tour_inicial$'nn+rep_10' <- solve_TSP(atsp, method="nn", rep=10)
tour_inicial$'nn+two_opt+rep_10' <- solve_TSP(atsp, method="nn", two_opt=TRUE, rep=10)
tour_inicial$'arbitrary_insertion+two_opt' <- solve_TSP(atsp)

#Visualizar resultados de las distancias generadas por cada método
dotchart(sort(c(sapply(tour_inicial, tour_length))), xlab = "tour length")
title("Figura 4 - Tour Inicial NY")

# Como la ciudad ficticia ("dummy") tiene distancia cero a todas las demás ciudades, 
# la longitud del camino es igual a la longitud de la gira que nos informó R 
# anteriormente. La ruta se inicia con la primera ciudad en la lista después de la 
# ciudad ficticia y termina con la ciudad justo antes de ella. 

tour_inicial[["nearest_insertion"]]
# object of class ‘TOUR’ 
# result of method ‘nearest_insertion’ for 313 cities
# tour length: 39538

# Utilizamos cut_tour() para crear un camino y mostramos las primeras y últimas 6 ciudades 
# a continuación.

tour_inicial <- tour_inicial[["nearest_insertion"]]

camino2 <- cut_tour(tour_inicial, cut = "New York, NY", 
                    exclude_cut = FALSE)

head(labels(camino2))
# [1] "New York, NY"     "Jersey City, NJ"  "Elizabeth, NJ"    "Newark, NJ"      
# [5] "Paterson, NJ"     "White Plains, NY"

tail(labels(camino2))
# [1] "Oakland, CA"       "Berkeley, CA"      "San Francisco, CA" "Hilo, HI"         
# [5] "Honolulu, HI"      "Lihue, HI" 

plot_path(camino2)
title("Figura 5 - Tour Inicial NY")

# Para encontrar el camino más corto de Hamilton también podemos restringir los dos puntos
# extremos, el de partida y el final. Este problema puede ser transformado a un TSP mediante
# la sustitución de las dos ciudades por una sola ciudad que contiene las distancias desde
# el punto de inicio en sus columnas y las distancias al punto final en sus filas.

# Para el siguiente ejemplo, sólo estamos interesados en caminos que empiezan en Nueva 
# York y que terminan en Los Ángeles. Por lo tanto, eliminamos las dos ciudades de la 
# matriz de distancias, creamos un TSP asimétrico e insertamos una ciudad ficticia 
# llamada "LA / NY". Las distancias de "DESDE" esta ciudad ficticia
# se sustituyen por las distancias desde Nueva York y las distancias "HACIA" se
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
tour_final <- sapply(metodos, FUN = function(m) solve_TSP(atsp, method = m), 
                       simplify = FALSE)

# Agregamos algunos recorridos usando repeticiones y refinamientos de dos opciones

tour_final$'nn+two_opt' <- solve_TSP(atsp, method="nn", two_opt=TRUE)
tour_final$'nn+rep_10' <- solve_TSP(atsp, method="nn", rep=10)
tour_final$'nn+two_opt+rep_10' <- solve_TSP(atsp, method="nn", two_opt=TRUE, rep=10)
tour_final$'arbitrary_insertion+two_opt' <- solve_TSP(atsp)

#Visualizar resultados de las distancias generadas por cada método
dotchart(sort(c(sapply(tour_final, tour_length))), xlab = "tour length")
title("Figura 6 - Tour de NY a LA")

tour_final[['nn+two_opt+rep_10']]
# object of class 'TOUR' 
# result of method ‘nn+two_opt_rep_10’ for 311 cities
# tour length: 40019

tour_final <- tour_final[['nn+two_opt+rep_10']]
path_labels <- c("New York, NY", labels(cut_tour(tour_final, la_ny)), "Los Angeles, CA")
path_ids <- match(path_labels, labels(USCA312))
head(path_labels)
# [1] "New York, NY"     "Jersey City, NJ"  "Elizabeth, NJ"    "Newark, NJ"      
# [5] "Paterson, NJ"     "White Plains, NY"

tail(path_labels)
# [1] "Prince Rupert, BC" "Hilo, HI"          "Honolulu, HI"      "Lihue, HI"        
# [5] "Pasadena, CA"      "Los Angeles, CA"  

plot_path(path_ids)
title("Figura 7 - Tour de NY a LA")

# La ruta que se muestra en la Figura 7 contiene algunos cruces que indican que la 
# solución es sub-óptima. La solución óptima generada por reformular el problema como 
# un TSP y el uso de Concorde sólo tiene una longitud de recorrido de 38.489 
# aproximadamente.

## Reordenamiento por agrupación

# La idea es que los objetos en un grupo son visitados en orden consecutivo y para pasar
# de un grupo al siguiente más grande es necesario hacer "saltos".

# Este tipo de reordenamiento por agrupación (si recordamos la Unidad 2.1 - Agrupamiento o 
# Clustering) sugiere encontrar automáticamente los límites de cluster o numero k de grupos 
# mediante la adición de un numero k de ciudades ficticias que tienen 
# distancia constante c a un centro. En la solución óptima del TSP, las
# ciudades ficticias deben separar las ciudades más distantes y por lo tanto representan 
# los límites óptimos para clusters k.

##########################################################################################
#          PAUSA, cambiamos de datos y nos vamos a las ciencias anturales un rato        #
##########################################################################################

# Para ilustrar esta estrategia, y verla más claramente, vamos utilizar un conocido conjunto 
# de datos de R llamado iris, dejando de lado las ciudades por un rato.
# El conjunto de datos Iris contiene tres clases o tipos diferentes de especies de flores
# identificadas bajo la variable "Species" y se miden las dimensiones/tamaños de sus hojas.

data("iris")
str(iris)
tsp <- TSP(dist(iris[-5]), labels = iris[, "Species"])

unique(iris$Species)
# [1] setosa     versicolor virginica 
# Levels: setosa versicolor virginica

# Como hay 3 especies en el conjunto iris, es una buena razon para empezar con n = 3  
# clusters. Por más que tenemos una buena razon, estadisticamente hablando, no deja de 
# ser arbirtaria hasta que se demuestre lo contrario (no será el caso y lo veremos bien).

tsp_dummy <- insert_dummy(tsp, n = 3, label = "boundary")
set.seed(123)
tour_iris <- sapply(metodos, FUN = function(m) solve_TSP(tsp_dummy, method = m), 
                     simplify = FALSE)

# Agregamos algunos recorridos usando repeticiones y refinamientos de dos opciones

tour_iris$'nn+two_opt' <- solve_TSP(tsp_dummy, method="nn", two_opt=TRUE)
tour_iris$'nn+rep_10' <- solve_TSP(tsp_dummy, method="nn", rep=10)
tour_iris$'nn+two_opt+rep_10' <- solve_TSP(tsp_dummy, method="nn", two_opt=TRUE, rep=10)
tour_iris$'arbitrary_insertion+two_opt' <- solve_TSP(tsp_dummy)

#Visualizar resultados de las distancias generadas por cada método
dotchart(sort(c(sapply(tour_iris, tour_length))), xlab = "tour length")
title("Figura 8 - Tour Iris")

tour_iris[['nn+two_opt+rep_10']]
# object of class ‘TOUR’ 
# result of method ‘nn+two_opt_rep_10’ for 153 cities
# tour length: 47.69911

# A continuación, trazamos la matriz de distancia permutada del TSP utilizando el sombreado 
# para representar distancias.

# El resultado se muestra en la Figura 9. 

# Las áreas más claras representan distancias grandes.
# Las líneas rojas representan las posiciones de los 3 "boundaries", que, CREEMOS, deberían
# marcan los límites de los clusters obtenidos (los lmites entre 3 especies diferentes).

# Matrix de distancia

tour_iris <- tour_iris[['nn+two_opt+rep_10']]

image(tsp_dummy, tour_iris, xlab = "objects", ylab ="objects")
title("Figura 9")

## dibujar l?neas donde se ubican los limites de cluster "boundaries"

abline(h = which(labels(tour_iris)=="boundary"), col = "red")
abline(v = which(labels(tour_iris)=="boundary"), col = "red")

# Tres líneas rojas horizontales y tres verticales separan exactamente la areas más oscuras
# de las más claras.
# Podemos ver lo bien que la partición obtenida se ajusta a la estructura de los datos 
# dados por el campo de las especies en el conjunto iris. Dado que usamos a la especie
# como etiquetas para reemplazar a las "ciudades" en el TSP, las etiquetas en la solución 
# "tour_iris" representan la partición con las "ciudades ficticias" llamadas "boundary" 
# (limites que separan a los diferentes clusters).
# El resultado se puede resumir mirando la longitud obtenida bajo la etiqueta del cluster
# para cada tour obtenido:

out <- rle(labels(tour_iris))
data.frame(Species = out$values, Lenghts = out$lengths, Pos = cumsum(out$lengths))

#       Species Lenghts Pos
# 1   virginica      27  27
# 2    boundary       1  28  <- boundary o "ciudad ficticia" número 1
# 3      setosa      50  78
# 4    boundary       1  79  <- boundary o "ciudad ficticia" número 2
# 5  versicolor      12  91
# 6   virginica       1  92
# 7  versicolor      23 115
# 8   virginica       1 116
# 9  versicolor       1 117
# 10  virginica       5 122
# 11 versicolor      13 135
# 12  virginica       1 136
# 13 versicolor       1 137
# 14  virginica       6 143
# 15   boundary       1 144  <- boundary o "ciudad ficticia" número 3
# 16  virginica       9 153

# Fronteras (boundaries) 1 y 2 dividen perfectamente los datos de la especies 'Virginica' y 
# la especie "Setosa". A partir de la frontera 2, las especies "Versicolor" y "Virginica" 
# comparten una extensa tercer sección del listado. 

# La razón por la que la agrupación por reordenamiento falla para dividir los datos en tres 
# grupos perfectos es la cercanía entre las especies Virginica y versicolor. Para 
# inspeccionar este problema aún más, podemos proyectar los puntos de datos componentes 
# principales del conjunto de datos y agregar el segmentos de trazado que resultaron del
# resolver de TSP.

prc <- prcomp(iris[1:4])
plot(prc$x, pch = as.numeric(iris[,5]), col = as.numeric(iris[,5]))
indices <- c(tour_iris, tour_iris[1])
indices[indices > 150] <- NA
lines(prc$x[indices,])
title("Figura 10")

# El resultado se muestra en la Figura 10. Las tres especies se identifican por diferentes 
# marcadores y todos los puntos conectados por una sola ruta representan una agrupación o 
# cluster encontrado. Claramente, los dos grupos a la derecha están demasiado cerca para ser 
# separados correctamente utilizando sólo las distancias entre puntos individuales. Este 
# problema es similar al efecto de encadenamiento conocido en la agrupación jerárquica 
# utilizando el método de un solo vínculo.

##########################################################################################
#   HABIENDO DEMOSTRADO LO QUE HAREMOS, volvemos al dilema del vendedor viajero          #
##########################################################################################

# Para aaplicagar clustering a las ciudades, pensemos en inciar con un n = 4 clusters
# en base a las principales zonas horarias principales de USA (Este, Centro, Montaña, 
# Pacifico). Esto simplemente se me ocurre de forma completamente arbitraria.

atsp_dummy <- insert_dummy(atsp, n = 4, label = "boundary")
set.seed(123)
tour_clusters <- sapply(metodos, FUN = function(m) solve_TSP(atsp_dummy, method = m), 
                     simplify = FALSE)

# Agregamos algunos recorridos usando repeticiones y refinamientos de dos opciones

tour_clusters$'nn+two_opt' <- solve_TSP(atsp_dummy, method="nn", two_opt=TRUE)
tour_clusters$'nn+rep_10' <- solve_TSP(atsp_dummy, method="nn", rep=10)
tour_clusters$'nn+two_opt+rep_10' <- solve_TSP(atsp_dummy, method="nn", two_opt=TRUE, rep=10)
tour_clusters$'arbitrary_insertion+two_opt' <- solve_TSP(atsp_dummy)

#Visualizar resultados de las distancias generadas por cada método
dotchart(sort(c(sapply(tour_clusters, tour_length))), xlab = "tour length")
title("Figura 11 - Tour de NY a LA por Cluster")

tour_clusters[['arbitrary_insertion+two_opt']]
# object of class ‘TOUR’ 
# result of method ‘arbitrary_insertion+two_opt’ for 317 cities
# tour length: 32416 

# A continuación, trazamos la matriz de distancia permutada del TSP utilizando el sombreado 
# para representar distancias.
# El resultado se muestra en la Figura 9. Las áreas más claras representan distancias 
# grandes. Las líneas rojas representan las posiciones de las ciudades ficticias en la 
# gira, que marcan los límites de los clusters obtenidos.

# Matrix de distancia

tour_clusters <- tour_clusters[['arbitrary_insertion+two_opt']]

image(atsp_dummy, tour_clusters, xlab = "objects", ylab ="objects")
title("Figura 12")

# creados con líneas rojas que es donde se ubican las ciudades ficticias

abline(h = which(labels(tour_clusters)=="boundary"), col = "red")
abline(v = which(labels(tour_clusters)=="boundary"), col = "red")

# Parecen quedar 3 líneas verticales y horizontales claramente sin resaltar en rojo, 
# sumemoslas a n = 4 + 3 y usemos 7 boundaries entonces.

atsp_dummy <- insert_dummy(atsp, n = 7, label = "boundary")
set.seed(123)
tour_clusters <- sapply(metodos, FUN = function(m) solve_TSP(atsp_dummy, method = m), 
                        simplify = FALSE)

# Agregamos algunos recorridos usando repeticiones y refinamientos de dos opciones

tour_clusters$'nn+two_opt' <- solve_TSP(atsp_dummy, method="nn", two_opt=TRUE)
tour_clusters$'nn+rep_10' <- solve_TSP(atsp_dummy, method="nn", rep=10)
tour_clusters$'nn+two_opt+rep_10' <- solve_TSP(atsp_dummy, method="nn", two_opt=TRUE, rep=10)
tour_clusters$'arbitrary_insertion+two_opt' <- solve_TSP(atsp_dummy)

#Visualizar resultados de las distancias generadas por cada método
dotchart(sort(c(sapply(tour_clusters, tour_length))), xlab = "tour length")
title("Figura 13 - Tour de NY a LA por Clusters 2")

tour_clusters[['arbitrary_insertion+two_opt']]
# object of class ‘TOUR’ 
# result of method ‘arbitrary_insertion+two_opt’ for 318 cities
# tour length: 28238 

tour_clusters <- tour_clusters[['arbitrary_insertion+two_opt']]

image(atsp_dummy, tour_clusters, xlab = "objects", ylab ="objects")
title("Figura 14")

abline(h = which(labels(tour_clusters)=="boundary"), col = "red")
abline(v = which(labels(tour_clusters)=="boundary"), col = "red")

# Aun podemos ver 1 linea horizontal y otra vertical claramente sin resaltar en rojo.
# Hagamos un ultimo intento con n = 8 boundaries.

atsp_dummy <- insert_dummy(atsp, n = 8, label = "boundary")
set.seed(123)
tour_clusters <- sapply(metodos, FUN = function(m) solve_TSP(atsp_dummy, method = m), 
                        simplify = FALSE)

tour_clusters$'nn+two_opt' <- solve_TSP(atsp_dummy, method="nn", two_opt=TRUE)
tour_clusters$'nn+rep_10' <- solve_TSP(atsp_dummy, method="nn", rep=10)
tour_clusters$'nn+two_opt+rep_10' <- solve_TSP(atsp_dummy, method="nn", two_opt=TRUE, rep=10)
tour_clusters$'arbitrary_insertion+two_opt' <- solve_TSP(atsp_dummy)

#Visualizar resultados de las distancias generadas por cada método
dotchart(sort(c(sapply(tour_clusters, tour_length))), xlab = "tour length")
title("Figura 15 - Tour de NY a LA por Cluster 3")

tour_clusters[['farthest_insertion']]
# object of class ‘TOUR’ 
# result of method ‘arbitrary_insertion+two_opt’ for 318 cities
# tour length: 28132 

# La mejora de 28238 a 28132 ya es bastnate poca, estamos cerca del mejor resultado posible.

tour_clusters <- tour_clusters[['farthest_insertion']]

image(atsp_dummy, tour_clusters, xlab = "objects", ylab ="objects")
title("Figura 16")

abline(h = which(labels(tour_clusters)=="boundary"), col = "red")
abline(v = which(labels(tour_clusters)=="boundary"), col = "red")

# Listo, no queda ninguna línea clara que no este resaltada en rojo. Esto significa
# que hemos alcanzado el número óptimo de clusters en 8 límites o boundaries. 
# Cada cluster podría jsutificar 1 vendedor viajero para cada uno ... pero es ya será una 
# decisión de negocio.

path_labels <- c("New York, NY", labels(cut_tour(tour_clusters, la_ny)), "Los Angeles, CA")
path_ids <- match(path_labels, labels(USCA312))
head(path_labels)
# [1] "New York, NY"  "boundary"      "Flagstaff, AZ" "Phoenix, AZ"   "Tucson, AZ"   
# [6] "El Paso, TX" 

tail(path_labels)
# [1] "Las Vegas, NV"      "San Bernardino, CA" "Pasadena, CA"       "San Diego, CA"     
# [5] "Yuma, AZ"           "Los Angeles, CA"  

# El recorrido completo es el siguiente:

path_labels
# [1] "New York, NY"         "boundary"             "Flagstaff, AZ"       
# [4] "Phoenix, AZ"          "Tucson, AZ"           "El Paso, TX"         
# [7] "Albuquerque, NM"      "Gallup, NM"  etc etc etc etc etc

# Removemos los boundaries para poder ilustrar la ruta sobre el mapa

x <- path_labels
x <- x[x != "boundary"]

path_ids_no_NA <- match(x, labels(USCA312))

plot_path(path_ids_no_NA)
title("Figura 17 - Tour de NY a LA en 8 Clusters")

# Al terminar, es buena práctica parar los clusters de procesos paralelos. Estos deberían
# de todas formas frenar automáticamente cuando la sesión de R se cierra.

stopCluster(cl)


#                                           Conclusión

# En este trabajo los autores del paquete TSP presentan el paquete de extensión TSP R que 
# implementa una infraestructura para manejar y resolver el problema del viajero.
# El paquete introduce clases de descripciones de los problemas simétricos y asimétricos
# (TSP y ATSP respectivamente) y de solución (TOUR o GIRA).


#                                           DESAFÍO

# Los desafío a realizar una ruta aplicando al estrategia de cluster a USCA312 sin 
# ninguna restricción de donde comenzar ni dónde terminar el recorrido de Hamilton.
