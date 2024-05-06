#############################################################################################

# Ejemplos prácticos de Algoritmos Genéticos en R
# El Problema del Viajante
# https://es.wikipedia.org/wiki/Problema_del_viajante
# - por Martín Vedani, UTN Business Intelligence

############################################################################################

# Instalar y cargar los paquetes que utilizaremos
if(! "TSP" %in% installed.packages()) install.packages("TSP", depend = T)
if(! "doParallel" %in% installed.packages()) install.packages("doParallel", depend = T)

# El siguiente ejemplo esta publicados en la siguiente página de la implementación del 
# paquete TSP (por las siglas en ingles de "Travelling Sales Person")
# http://tsp.r-forge.r-project.org/

# Se recomienda enfáticamente leer el documento TSP.pdf y trabajar los ejemplos. 
# Está en inglés por lo cual, de no leerlo, se pueden seguir los pasos debajo sin problema.
# Fuente: https://cran.r-project.org/web/packages/TSP/vignettes/TSP.pdf


# El Problema es un clásico y dice así:

# Dada una lista de ciudades y las distancias entre cada par de ciudades, ¿cuál es la 
# ruta más corta posible que visita cada ciudad al menos una vez y regresa a la ciudad 
# de origen?

# Pues veamos como resolverlo:

## Cargar los paquetes y los datos a utilizar en la sesión actual de R
library("TSP")
data("USCA50")

## Revisar el objeto de datos con el que trabajaremos al principio
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

# Actualmente, los siguiente métodos están disponibles

metodos <- c("identity", "random", "nearest_insertion", "farthest_insertion", "cheapest_insertion", 
             "arbitrary_insertion", "nn", "repetitive_nn", "two_opt")

# Empezaremos siempre con la misma semilla del randomizador para poder duplicar los mismos
# resultados, aunque entre diferentes sistemas operativos esto no es siempre exactamente
# posible

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
# object of class ‘TOUR’ 
# result of method ‘nn+two_opt_rep_10’ for 50 cities
# tour length: 14749

labels(viajes[["nn+two_opt+rep_10"]])
# [1] "Allentown, PA"     "Binghamtom, NY"    "Belleville, ON"    "Buffalo, NY"      
# [5] "Burlington, ONT"   "Brantford, ON"     "Canton, OH"        "Akron, OH"        
# [9] "Ann Arbor, MI"     "Bay City, MI"      "Battle Creek, MI"  "Cedar Rapids, IA" 
# [13] "Bloomington, IL"   "Bowling Green, KY" "Ashland, KY"       "Asheville, NC"    
# [17] "Augusta, GA"       "Atlanta, GA"       "Birmingham, AL"    "Biloxi, MS"       
# [21] "Baton Rouge, LA"   "Beaumont, TX"      "Austin, TX"        "Abilene, TX"      
# [25] "Amarillo, TX"      "Albuquerque, NM"   "Bakersfield, CA"   "Berkeley, CA"     
# [29] "Carson City, NV"   "Boise, ID"         "Butte, MT"         "Billings, MT"     
# [33] "Bismarck, ND"      "Brandon, MB"       "Calgary, AB"       "Bellingham, WA"   
# [37] "Anchorage, AK"     "Alert, NT"         "Burlington, VT"    "Bangor, ME"       
# [41] "Augusta, ME"       "Cambridge, MA"     "Boston, MA"        "Brockton, MA"     
# [45] "Brattleboro, VT"   "Albany, NY"        "Bridgeport, CT"    "Central Islip, NY"
# [49] "Atlantic City, NJ" "Baltimore, MD"  


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
# tour length: 37231

# Utilizamos cut_tour() para crear un camino y mostramos las primeras y últimas 6 ciudades 
# a continuación.

gira <- gira[["nn+two_opt+rep_10"]]
camino <- cut_tour(gira, "cut")

head(labels(camino))
# [1] "Lihue, HI" "Honolulu, HI" "Hilo, HI" "Butte, MT" "Helena, MT"     
# [6] "Great Falls, MT"  

tail(labels(camino))
# [1] "Whitehorse, YK" "Dawson, YT" "Fairbanks, AK" "Anchorage, AK" "Nome, AK"      
# [6] "Alert, NT"  

# La gira que se encuentra en este ejemplo nos da un camino desde Lihue, HI hasta Alert, NT. 
# Un camino de este tipo también se puede visualizar mediante el 
# los paquetes sp, mapas y MapTools (Pebesma y Bivand 2005).

if(! "sp" %in% installed.packages()) install.packages("sp", depend = T)
if(! "maps" %in% installed.packages()) install.packages("maps", depend = T)
# if(! "maptools" %in% installed.packages()) install.packages("maptools", depend = T)
# if(! "rgeos" %in% installed.packages()) install.packages("rgeos", depend = T)

library("maps")
library("sp")
# library("maptools")
# library("rgeos")
data("USCA312_GPS")

# Busca todo el path en tu máquina local al archivo USCA312_map.rda provisto.
# Solía ser parte del paquete TSP pero fue actualizada a nuevo formato bajo el nombre 
# USCA312_GPS. Puedes observar las diferencias por tú mismo ya que hemos cargado ambos 
# formatos como variables globales y están disponibles en RStudio.

load(file = "<path to>/USCA312_map.rda")

# Creamos un gráfico especial que utilizaremos varias veces.
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

tour_inicial[["nn+two_opt+rep_10"]]
# object of class ‘TOUR’ 
# result of method ‘nearest_insertion’ for 313 cities
# result of method ‘nn+two_opt_rep_10’ for 312 cities
# tour length: 38756 

# Utilizamos cut_tour() para crear un camino y mostramos las primeras y últimas 6 ciudades 
# a continuación.

tour_inicial <- tour_inicial[["nn+two_opt+rep_10"]]

camino2 <- cut_tour(tour_inicial, cut = "New York, NY", 
                    exclude_cut = FALSE)

head(labels(camino2))
# [1] "New York, NY" "Jersey City, NJ"  "Elizabeth, NJ" "Newark, NJ" "Paterson, NJ"    
# [6] "White Plains, NY"

tail(labels(camino2))
# [1] "Fairbanks, AK" "Anchorage, AK" "Nome, AK" "Hilo, HI" "Honolulu, HI" 
# [6] "Lihue, HI"

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

tour_final[['arbitrary_insertion+two_opt']]
# object of class 'TOUR' 
# result of method 'arbitrary_insertion+two_opt' for 311 cities
# tour length: 41186

tour_final <- tour_final[['arbitrary_insertion+two_opt']]
path_labels <- c("New York, NY", labels(cut_tour(tour_final, la_ny)), "Los Angeles, CA")
path_ids <- match(path_labels, labels(USCA312))

head(path_labels)
# [1] "New York, NY" ...

tail(path_labels)
# ... "Los Angeles, CA"

plot_path(path_ids)
title("Figura 7 - Tour de NY a LA")

# La ruta que se muestra en la Figura 7 puede contener algunos cruces que indican que la 
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
#          PAUSA, cambiamos de datos y nos vamos a las ciencias naturales un rato        #
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

# Como hay 3 especies en el conjunto iris, es una buena razón para empezar con n = 3  
# clusters. Por más que tenemos una buena razón, estadísticamente hablando, no deja de 
# ser arbitraria hasta que se demuestre lo contrario.

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
# tour length: 47.28361

# A continuación, trazamos la matriz de distancia permutada del TSP utilizando el sombreado 
# para representar distancias.

# El resultado se muestra en la Figura 9. 

# Las áreas más claras representan distancias grandes.
# Las líneas rojas representan las posiciones de los 3 "boundaries", que, CREEMOS, deberían
# marcan los límites de los clusters obtenidos (los límites entre 3 especies diferentes).

# Matrix de distancia

tour_iris <- tour_iris[['nn+two_opt+rep_10']]

image(tsp_dummy, tour_iris, xlab = "objects", ylab ="objects")
title("Figura 9")

## dibujar líneas donde se ubican los limites de cluster "boundaries"

abline(h = which(labels(tour_iris)=="boundary"), col = "red")
abline(v = which(labels(tour_iris)=="boundary"), col = "red")

# Tres líneas rojas horizontales y verticales separan exactamente la áreas más oscuras
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
# 1    boundary       1   1 <--------------- boundary
# 2      setosa      49  50
# 3    boundary       1  51 <--------------- boundary
# 4      setosa       1  52
# 5    boundary       1  53 <--------------- boundary
# 6  versicolor       5  58
# 7   virginica       1  59
# 8  versicolor      23  82
# 9   virginica       9  91
# 10 versicolor       1  92
# 11  virginica       6  98
# 12 versicolor       1  99
# 13  virginica       1 100
# 14 versicolor      20 120
# 15  virginica      33 153

# La especie setosa esta perfectamente separada de las otros dos clusters.
# La razón por la que la agrupación por reordenamiento falla para dividir los datos en tres 
# grupos perfectos es la cercanía entre las especies Virginica y versicolor. Para 
# inspeccionar este problema aún más, podemos proyectar los puntos de datos componentes 
# principales del conjunto de datos y agregar los segmentos de trazado que resultaron del
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

# Para aplicar clustering a las ciudades, pensemos en iniciar con un n = 4 clusters
# en base a las principales zonas horarias principales de USA (Este, Centro, Montaña, 
# Pacifico). Esto simplemente se me ocurre de forma completamente arbitraria.

atsp_dummy <- insert_dummy(atsp, n = 4, label = "boundary")

set.seed(123)
tour_clusters4 <- sapply(metodos, FUN = function(m) solve_TSP(atsp_dummy, method = m), 
                     simplify = FALSE)

# Agregamos algunos recorridos usando repeticiones y refinamientos de dos opciones

tour_clusters4$'nn+two_opt' <- solve_TSP(atsp_dummy, method="nn", two_opt=TRUE)
tour_clusters4$'nn+rep_10' <- solve_TSP(atsp_dummy, method="nn", rep=10)
tour_clusters4$'nn+two_opt+rep_10' <- solve_TSP(atsp_dummy, method="nn", two_opt=TRUE, rep=10)
tour_clusters4$'arbitrary_insertion+two_opt' <- solve_TSP(atsp_dummy)


#Visualizar resultados de las distancias generadas por cada método
dotchart(sort(c(sapply(tour_clusters4, tour_length))), xlab = "tour length")
title("Figura 11 - Tour de NY a LA con 4 Clusters")

tour_clusters4[['two_opt']]
# object of class ‘TOUR’ 
# result of method ‘arbitrary_insertion+two_opt’ for 315 cities
# tour length: 32496

# Bastante mejor que la solución óptima de concorde sin clusters que daba un tour length 
# de 38.489. 

# ¿Podemos mejorar aún más?

# A continuación, trazamos la matriz de distancia permutada del TSP utilizando el sombreado 
# para representar distancias.
# El resultado se muestra en la Figura 12. Las áreas más claras representan distancias 
# grandes. Las líneas rojas representan las posiciones de las ciudades ficticias en la 
# gira, que marcan los límites de los clusters obtenidos.

# Matrix de distancia

tour_clusters4 <- tour_clusters4[['two_opt']]

image(atsp_dummy, tour_clusters4, xlab = "objects", ylab ="objects")
title("Figura 12")

# creados con líneas rojas que es donde se ubican las ciudades ficticias

abline(h = which(labels(tour_clusters4)=="boundary"), col = "red")
abline(v = which(labels(tour_clusters4)=="boundary"), col = "red")

# Parecen quedar al menos 1 línea vertical y horizontal claramente sin resaltar en rojo, 
# sumemoslas a n = 4 + 1 y usemos 5 boundaries entonces.

atsp_dummy <- insert_dummy(atsp, n = 5, label = "boundary")

set.seed(123)
tour_clusters5 <- sapply(metodos, FUN = function(m) solve_TSP(atsp_dummy, method = m), 
                        simplify = FALSE)

# Agregamos algunos recorridos usando repeticiones y refinamientos de dos opciones

tour_clusters5$'nn+two_opt' <- solve_TSP(atsp_dummy, method="nn", two_opt=TRUE)
tour_clusters5$'nn+rep_10' <- solve_TSP(atsp_dummy, method="nn", rep=10)
tour_clusters5$'nn+two_opt+rep_10' <- solve_TSP(atsp_dummy, method="nn", two_opt=TRUE, rep=10)
tour_clusters5$'arbitrary_insertion+two_opt' <- solve_TSP(atsp_dummy)


#Visualizar resultados de las distancias generadas por cada método
dotchart(sort(c(sapply(tour_clusters5, tour_length))), xlab = "tour length")
title("Figura 13 - Tour de NY a LA con 5 Clusters")

tour_clusters5[['arbitrary_insertion+two_opt']]
# object of class ‘TOUR’ 
# result of method ‘arbitrary_insertion+two_opt’ for 316 cities
# tour length: 29745

tour_clusters5 <- tour_clusters5[['arbitrary_insertion+two_opt']]

image(atsp_dummy, tour_clusters5, xlab = "objects", ylab ="objects")
title("Figura 14")

abline(h = which(labels(tour_clusters5)=="boundary"), col = "red")
abline(v = which(labels(tour_clusters5)=="boundary"), col = "red")

# Aun podemos ver 1 línea horizontal y 1 vertical (tal vez con un leve o claro contraste 
#  formando 1 cruz) sin resaltar en rojo. Hagamos un intento con n = 6 boundaries.

atsp_dummy <- insert_dummy(atsp, n = 6, label = "boundary")

set.seed(123)
tour_clusters6 <- sapply(metodos, FUN = function(m) solve_TSP(atsp_dummy, method = m), 
                        simplify = FALSE)

tour_clusters6$'nn+two_opt' <- solve_TSP(atsp_dummy, method="nn", two_opt=TRUE)
tour_clusters6$'nn+rep_10' <- solve_TSP(atsp_dummy, method="nn", rep=10)
tour_clusters6$'nn+two_opt+rep_10' <- solve_TSP(atsp_dummy, method="nn", two_opt=TRUE, rep=10)
tour_clusters6$'arbitrary_insertion+two_opt' <- solve_TSP(atsp_dummy)


#Visualizar resultados de las distancias generadas por cada método
dotchart(sort(c(sapply(tour_clusters6, tour_length))), xlab = "tour length")
title("Figura 15 - Tour de NY a LA con 6 Clusters")

tour_clusters6[['arbitrary_insertion+two_opt']]
# object of class ‘TOUR’ 
# result of method ‘arbitrary_insertion+two_opt’ for 317 cities
# tour length: 30170 

tour_clusters6 <- tour_clusters6[['arbitrary_insertion+two_opt']]

image(atsp_dummy, tour_clusters6, xlab = "objects", ylab ="objects")
title("Figura 16")

abline(h = which(labels(tour_clusters6)=="boundary"), col = "red")
abline(v = which(labels(tour_clusters6)=="boundary"), col = "red")

# Todavía nos queda al menos 1 cruz claramente SIN resaltar en rojo, hagamos un intento 
# de mejora adicional con n = 7. 
# (Yo veo 2 cruces en realidad, la segunda muy clarita, que por ahora voy a ignorar).

atsp_dummy <- insert_dummy(atsp, n = 7, label = "boundary")

set.seed(123)
tour_clusters7 <- sapply(metodos, FUN = function(m) solve_TSP(atsp_dummy, method = m), 
                        simplify = FALSE)

tour_clusters7$'nn+two_opt' <- solve_TSP(atsp_dummy, method="nn", two_opt=TRUE)
tour_clusters7$'nn+rep_10' <- solve_TSP(atsp_dummy, method="nn", rep=10)
tour_clusters7$'nn+two_opt+rep_10' <- solve_TSP(atsp_dummy, method="nn", two_opt=TRUE, rep=10)
tour_clusters7$'arbitrary_insertion+two_opt' <- solve_TSP(atsp_dummy)

#Visualizar resultados de las distancias generadas por cada método
dotchart(sort(c(sapply(tour_clusters7, tour_length))), xlab = "tour length")
title("Figura 17 - Tour de NY a LA con 7 Clusters")

tour_clusters7[['farthest_insertion']]
# object of class ‘TOUR’ 
# result of method ‘arbitrary_insertion+two_opt’ for 318 cities
# tour length: 29509

tour_clusters7 <- tour_clusters7[['farthest_insertion']]

image(atsp_dummy, tour_clusters7, xlab = "objects", ylab ="objects")
title("Figura 18")

abline(h = which(labels(tour_clusters7)=="boundary"), col = "red")
abline(v = which(labels(tour_clusters7)=="boundary"), col = "red")

# Aun queda 1 curz clara SIN resaltar en rojo, con la mejora conseguido entre 
# 6 y 7 clusters, vale la pena intentar con n = 8.

atsp_dummy <- insert_dummy(atsp, n = 8, label = "boundary")

set.seed(123)
tour_clusters8 <- sapply(metodos, FUN = function(m) solve_TSP(atsp_dummy, method = m), 
                        simplify = FALSE)

tour_clusters8$'nn+two_opt' <- solve_TSP(atsp_dummy, method="nn", two_opt=TRUE)
tour_clusters8$'nn+rep_10' <- solve_TSP(atsp_dummy, method="nn", rep=10)
tour_clusters8$'nn+two_opt+rep_10' <- solve_TSP(atsp_dummy, method="nn", two_opt=TRUE, rep=10)
tour_clusters8$'arbitrary_insertion+two_opt' <- solve_TSP(atsp_dummy)


#Visualizar resultados de las distancias generadas por cada método
dotchart(sort(c(sapply(tour_clusters8, tour_length))), xlab = "tour length")
title("Figura 19 - Tour de NY a LA con 8 Clusters")

tour_clusters8[['farthest_insertion']]
# object of class ‘TOUR’ 
# result of method ‘arbitrary_insertion+two_opt’ for 319 cities
# tour length: 28704

# Otra mejora interesante, veamos la matriz de distancias para ver si podemos
# mejorar más.

tour_clusters8 <- tour_clusters8[['farthest_insertion']]

image(atsp_dummy, tour_clusters8, xlab = "objects", ylab ="objects")
title("Figura 20")

abline(h = which(labels(tour_clusters8)=="boundary"), col = "red")
abline(v = which(labels(tour_clusters8)=="boundary"), col = "red")

# Sí, hay cruz (o cruces) todavía SIN resaltar, por lo que hay que intentar con 
# n = 9 entonces.

atsp_dummy <- insert_dummy(atsp, n = 9, label = "boundary")

set.seed(123)
tour_clusters9 <- sapply(metodos, FUN = function(m) solve_TSP(atsp_dummy, method = m), 
                         simplify = FALSE)

tour_clusters9$'nn+two_opt' <- solve_TSP(atsp_dummy, method="nn", two_opt=TRUE)
tour_clusters9$'nn+rep_10' <- solve_TSP(atsp_dummy, method="nn", rep=10)
tour_clusters9$'nn+two_opt+rep_10' <- solve_TSP(atsp_dummy, method="nn", two_opt=TRUE, rep=10)
tour_clusters9$'arbitrary_insertion+two_opt' <- solve_TSP(atsp_dummy)


#Visualizar resultados de las distancias generadas por cada método
dotchart(sort(c(sapply(tour_clusters9, tour_length))), xlab = "tour length")
title("Figura 21 - Tour de NY a LA con 9 Clusters")

tour_clusters9[['arbitrary_insertion+two_opt']]
# object of class ‘TOUR’ 
# result of method 27685 

# La mejora ahora es menor, puede ser que ya tengamos el óptimo número de 
# cluster, comprobemos con la matriz de distancias representadas en colores.

tour_clusters9 <- tour_clusters9[['arbitrary_insertion+two_opt']]

image(atsp_dummy, tour_clusters9, xlab = "objects", ylab ="objects")
title("Figura 22")

abline(h = which(labels(tour_clusters9)=="boundary"), col = "red")
abline(v = which(labels(tour_clusters9)=="boundary"), col = "red")

# Aún hay una cruz, la mejora no será mucha, pero veamos que nos da TSP con n = 10 
# de todas formas.

atsp_dummy <- insert_dummy(atsp, n = 10, label = "boundary")

set.seed(123)
tour_clusters10 <- sapply(metodos, FUN = function(m) solve_TSP(atsp_dummy, method = m), 
                         simplify = FALSE)

tour_clusters10$'nn+two_opt' <- solve_TSP(atsp_dummy, method="nn", two_opt=TRUE)
tour_clusters10$'nn+rep_10' <- solve_TSP(atsp_dummy, method="nn", rep=10)
tour_clusters10$'nn+two_opt+rep_10' <- solve_TSP(atsp_dummy, method="nn", two_opt=TRUE, rep=10)
tour_clusters10$'arbitrary_insertion+two_opt' <- solve_TSP(atsp_dummy)


#Visualizar resultados de las distancias generadas por cada método
dotchart(sort(c(sapply(tour_clusters10, tour_length))), xlab = "tour length")
title("Figura 23 - Tour de NY a LA con 10 Boundaries")

tour_clusters10[['farthest_insertion']]
# object of class ‘TOUR’ 
# result of method ‘arbitrary_insertion+two_opt’ for 320 cities
# tour length: 27438

# Mejoró muy poco. Supongamos que tour_length hubiese empeorado, el número de clusters 
# óptimo hubiese sido n = 9. AVancemos con este supuesto.

path_labels <- c("New York, NY", labels(cut_tour(tour_clusters9, la_ny)),
                 "Los Angeles, CA")
path_ids <- match(path_labels, labels(USCA312))
head(path_labels)
# [1] "New York, NY" ...

tail(path_labels)
# ... "Los Angeles, CA" 

# El recorrido completo es el siguiente:

path_labels
# [1] "New York, NY" ... "Los Angeles, CA"

# Removemos los boundaries para poder ilustrar la ruta sobre el mapa

x <- path_labels
x <- x[x != "boundary"]

path_ids_no_NA <- match(x, labels(USCA312))

plot_path(path_ids_no_NA)
title("Figura 24 - Tour de NY a LA con 9 Clusters")

# No parece nada simple, lo viajes de un cluster a otro no se cuenta, veamos entonce de 
# graficar los clusters por separado.

# Busquemos cada "boundary"

path_labels
# [1] "New York, NY" ... "Los Angeles, CA" 

which (path_labels %in% "boundary") 
# Los boundaries están en las posiciones 31  33  35  37  41  43  47  61 217
# 9 boundaries nos dan 10 clusters:

cluster1 <- path_labels[1:30]
cluster2 <- path_labels[32]
cluster3 <- path_labels[34]
cluster4 <- path_labels[36]
cluster5 <- path_labels[38:40]
cluster6 <- path_labels[42]
cluster7 <- path_labels[44:46]
cluster8 <- path_labels[48:60]
cluster9 <- path_labels[62:216]
cluster10 <- path_labels[218:321]

# Chequear en caso de errores
which (cluster1 %in% "boundary") 
# integer(0)
which (cluster2 %in% "boundary") 
# integer(0)
which (cluster3 %in% "boundary") 
# integer(0)
which (cluster4 %in% "boundary") 
# integer(0)
which (cluster5 %in% "boundary") 
# integer(0)
which (cluster6 %in% "boundary") 
# integer(0)
which (cluster7 %in% "boundary") 
# integer(0)
which (cluster8 %in% "boundary") 
# integer(0)
which (cluster9 %in% "boundary") 
# integer(0)
which (cluster10 %in% "boundary") 
# integer(0)

# ¿Nos sobra un cluster? Tenemos 9 boundaries, sigamos investigando los resultados de
# TSP.

cluster1_ids <- match(cluster1, labels(USCA312))
plot_path(cluster1_ids)
title("Figura 25 - - Cluster 1")

cluster2_ids <- match(cluster2, labels(USCA312))
plot_path(cluster2_ids)
title("Figura 26 - Cluster 2")

cluster3_ids <- match(cluster3, labels(USCA312))
plot_path(cluster3_ids)
title("Figura 27 - Cluster 3")

cluster4_ids <- match(cluster4, labels(USCA312))
plot_path(cluster4_ids)
title("Figura 28 - Cluster 4")

cluster5_ids <- match(cluster5, labels(USCA312))
plot_path(cluster5_ids)
title("Figura 29 - Cluster 5")

cluster6_ids <- match(cluster6, labels(USCA312))
plot_path(cluster6_ids)
title("Figura 30 - Cluster 6")

cluster7_ids <- match(cluster7, labels(USCA312))
plot_path(cluster7_ids)
title("Figura 31 - Cluster 7")

cluster8_ids <- match(cluster8, labels(USCA312))
plot_path(cluster8_ids)
title("Figura 32 - Cluster 8")

cluster9_ids <- match(cluster9, labels(USCA312))
plot_path(cluster9_ids)
title("Figura 33 - Cluster 9")

cluster10_ids <- match(cluster10, labels(USCA312))
plot_path(cluster10_ids)
title("Figura 34 - Cluster 10")

# Ok, Como las distancias de viaje entre clusters no se consideran en la suma de distancia,
# hay lugares remotos o en islas que tiene sentido que formen su propio cluster.

# Hay decisiones de negocio que tomar aquí, en cuanto a la cantidad de vendedores que se
# podrían contratar .... etc etc.

# PRIMERO QUE NADA, yo aconsejaría quitarle restricciones al modelo, y aquí les queda 
# el desafío:

#                                       DESAFÍO

# Los desafío a realizar una nueva ruta, aplicando la estrategia de cluster a USCA312, 
# SIN ninguna restricción de donde comenzar ni dónde terminar el recorrido de Hamilton.
# En otras palabras, encontrar una solución por cluster simétrica.

#                                    CONCLUSIÓN

# En este trabajo los autores del paquete TSP presentan el paquete de extensión TSP R que 
# implementa una infraestructura para manejar y resolver el problema del viajero.
# El paquete introduce clases de descripciones de los problemas simétricos y asimétricos
# (TSP y ATSP respectivamente) y de solución (TOUR o GIRA).
