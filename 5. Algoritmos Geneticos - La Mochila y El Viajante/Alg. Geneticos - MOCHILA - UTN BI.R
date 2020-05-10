#############################################################################################

# Ejemplos prácticos de Algoritmos Genéticos en R
# El Problema de la mochila.
# https://es.wikipedia.org/wiki/Problema_de_la_mochila
# - por Martin Vedani, UTN Business Intelligence

############################################################################################

# Instalar y cargar los paquetes que utilizaremos
if(! "genalg" %in% installed.packages()) install.packages("genalg", depend = TRUE)

# El documento original se encuentra online y en inglés, por lo cual, de no leerlo, se pueden 
# seguir los pasos debajo sin problema.
# Fuente: http://www.r-bloggers.com/genetic-algorithms-a-simple-r-example/


## Cargar los paquetes y los datos a utilizar en la sesión actual de R
library(genalg)

# Cargar datos de los objetos a utilizar
objetos <- read.csv("mochila.csv", header = T)

objetos

# Fijamos límite de peso para la mochila 
# (la restricción de nuestro problema de optimización)
limite_de_peso <- 20

# Antes de crear el modelo tenemos que configurar una función de evaluación. La función de 
# evaluación evaluará los diferentes individuos (cromosomas) de la población sobre el valor de
# su configuración genética.

# Un individuo por ejemplo tiene la siguiente configuración gen: 1001100. 

# Cada número de esta cadena binaria representa si tomar o no un elemento. Un valor de 1 
# se refiere a poner el elemento específico en la mochila, mientras que un 0 se refiere a 
# dejar el objeto en casa. Dada la configuración de ejemplo gen tomaríamos los siguientes 
# elementos;

cromosoma = c(1, 0, 0, 1, 1, 0, 0)
objetos[cromosoma == 1, ]

#            Objeto Puntaje.Sobrevivencia Peso
# 1        cuchillo                    10    1
# 4        cebollas                     2    1
# 5 bolsa de dormir                    30    7

# Podemos comprobar qué cantidad de puntos de sobrevivencia suma esta configuración

cat(cromosoma %*% objetos$Puntaje.Sobrevivencia)
# 42

# Arriba le dimos un valor a la configuración de genes de un cromosoma determinado. Esto es 
# exactamente lo que hace la función de evaluación.

# El algoritmo genalg intenta optimizar hacia el valor mínimo. Por lo tanto, el valor se 
# calcula como anteriormente y se multiplica por -1. Una configuración que lleva a superar 
# la restricción de peso devuelve un valor de 0 (el valor más alto se puede también dar).
# Se define la función de evaluación de la siguiente manera.

funcEval <- function(x) {
  puntaje_solucion_actual <- x %*% objetos$Puntaje.Sobrevivencia
  peso_solucion_actual <- x %*% objetos$Peso
  
  if (peso_solucion_actual > limite_de_peso) 
    return(0) else return(-puntaje_solucion_actual)
}

# A continuación, elegimos el número de iteraciones, diseño y corremos el modelo

iter <- 100
modelo_GA <- rbga.bin(size = 7, popSize = 200, iters = iter, mutationChance = 0.01, 
                      elitism = T, evalFunc = funcEval)

cat(summary(modelo_GA))
# GA Settings
# Type                  = binary chromosome
# Population size       = 200
# Number of Generations = 100
# Elitism               = TRUE
# Mutation Chance       = 0.01
# 
# Search Domain
# Var 1 = [,]
# Var 0 = [,]
# 
# GA Results
# Best Solution : 1 1 0 1 1 1 1 

# La mejor solución encontrada es 1111101. 
# Esto nos conduciría a cargar nuestra mochila con los siguientes objetos

solucion <- c(1, 1, 1, 1, 1, 0, 1)
objetos[solucion == 1, ]

#            Objeto Puntaje.Sobrevivencia Peso
# 1        cuchillo                    10    1
# 2        frijoles                    20    5
# 3           papas                    15   10
# 4        cebollas                     2    1
# 5 bolsa de dormir                    30    7
# 7         brujula                    30    1

# Y así podemos calcular el puntaje de sobrevivencia (solución vs objetos disponibles)

cat(paste(solucion %*% objetos$Puntaje.Sobrevivencia, "/",
          sum(objetos$Puntaje.Sobrevivencia)))
# 107 / 117

# Grafiquemos la evolución "genética" de nuestro modelo

plot(-modelo_GA$mean, type = "l", col = "red", ylab="Puntaje de Sobrevivencia",
     xlab="Iteración de simulación \n (Generaciones de Evolución Genética)",
     main="Evolución del modelo de optimización")
lines(-modelo_GA$best, type = "l", col = "blue")
abline(h = 107, col="green")
legend(60,60, c("Promedio", "Mejor"), lty=1,
       col = c("red", "Blue"))

# El eje x denota las diferentes generaciones. La línea roja muestra la
# solución de toda la población de esa generación media, mientras que la línea azul
# muestra la mejor solución de esa generación. Como se puede ver, al modelos solo le
# toma unas pocas generaciones para llegar a la mejor solución. Después de alcanzada,
# es sólo una cuestión de tiempo hasta que la media de la población de
# generaciones posteriores evolucione hacia la mejor solución.
