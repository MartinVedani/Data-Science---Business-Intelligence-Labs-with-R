#########################################################################################

# Ejercicios de Reglas de Asociacion en R - por Martin Vedani, UTN Business Intelligence

#########################################################################################

# Se recomiendo enfaticamente realizar estos ejercicios luego de leer el archivo pdf 
# "arules" y "arulesViz" detenidamente. Estan en inglés por lo cual, de no leerlo, se 
# pueden seguir los pasos debajo sin problema.
# Fuente: Intelligent Data Analysis Lab, https://lyle.smu.edu/IDA/arules/

# Instalacion y carga del paquete de R a ser utilizado
if(! "arules" %in% installed.packages()) install.packages("arules", depend = TRUE)
if(! "arulesViz" %in% installed.packages()) install.packages("arulesViz", depend = TRUE)
if(! "seriation" %in% installed.packages()) install.packages("seriation", depend = TRUE)
library(arules)
library(arulesViz)
library(seriation)

#########################################################################################
#################################### Ejercicio 2.2.1 ####################################
#########################################################################################
# Tomar los datos contenidos en la tabla casos1 de la database reglas y encontrar todas 
# las reglas de asociación que tengan más de 10 casos y una confianza superior al 80%
#########################################################################################

### Entender los datos que hemos recibido y convertir clases importadas a tipo de clase
### requerida por arules 

# Importar y entender Caso 1
Caso1 <- read.csv("Caso1.csv", head = T, sep="," , as.is = T)
str(Caso1)
summary(Caso1)


# Convertir columnas de Caso 1 de clase "int" a "factores" para trabajar con transacciones
Caso1[,1] <- factor(Caso1[,1])
Caso1[,2] <- factor(Caso1[,2])
Caso1[,3] <- factor(Caso1[,3])
Caso1[,4] <- factor(Caso1[,4])
Caso1[,5] <- factor(Caso1[,5])
Caso1[,6] <- factor(Caso1[,6])
Caso1[,7] <- factor(Caso1[,7])
Caso1 <- Caso1[,c(1,2,3,4,5,6,7)]

str(Caso1)
summary(Caso1)

# Convertir columnas de Caso 1 de clase "factores" a "transacciones" para trabajar con
# arules

Caso1trans <- as(Caso1, "transactions")
Caso1trans
summary(Caso1trans)

# Importar y entender Caso 2
Caso2 <- read.csv("Caso2.csv", head = T, sep="," , as.is = T)
str(Caso2)
summary(Caso2)

# Convertir columnas de Caso 2 de clase "int" a "factores" para trabajar con transacciones
Caso2[,1] <- factor(Caso2[,1])
Caso2[,2] <- factor(Caso2[,2])
Caso2[,3] <- factor(Caso2[,3])
Caso2[,4] <- factor(Caso2[,4])
Caso2[,5] <- factor(Caso2[,5])
Caso2[,6] <- factor(Caso2[,6])
Caso2[,7] <- factor(Caso2[,7])
Caso2 <- Caso2[,c(1,2,3,4,5,6,7)]

str(Caso2)
summary(Caso2)

# Convertir columnas de Caso 2 de clase "factores" a "transacciones" para trabajar con
# arules

Caso2trans <- as(Caso2, "transactions")
Caso2trans
summary(Caso2trans)

# Importar y entender Caso 3
Caso3 <- read.csv("Caso3.csv", head = T, sep="," , as.is = T)
str(Caso3)
summary(Caso3)

# Convertir columnas de Caso 3 de clase "int" a "factores" para trabajar con transacciones
Caso3[,1] <- factor(Caso3[,1])
Caso3[,2] <- factor(Caso3[,2])
Caso3[,3] <- factor(Caso3[,3])
Caso3[,4] <- factor(Caso3[,4])
Caso3[,5] <- factor(Caso3[,5])
Caso3[,6] <- factor(Caso3[,6])
Caso3[,7] <- factor(Caso3[,7])
Caso3 <- Caso3[,c(1,2,3,4,5,6,7)]

str(Caso3)
summary(Caso3)

# Convertir columnas de Caso 2 de clase "factores" a "transacciones" para trabajar con
# arules

Caso3trans <- as(Caso3, "transactions")
Caso3trans
summary(Caso3trans)

### Chequear frecuencia de items

itemFrequencyPlot(Caso1trans, topN=50, cex.names=.5)
itemFrequencyPlot(Caso2trans, topN=50, cex.names=.5)
itemFrequencyPlot(Caso3trans, topN=50, cex.names=.5)

### Crear e inspeccionar reglas

reglas1 <- apriori(Caso1trans)
reglas2 <- apriori(Caso2trans)
reglas3 <- apriori(Caso3trans)

summary(reglas1)
summary(reglas2)
summary(reglas3)

subreglas1 <- reglas1[quality(reglas1)$confidence > 0.99]
subreglas2 <- reglas2[quality(reglas2)$confidence > 0.99]
subreglas3 <- reglas3[quality(reglas3)$confidence > 0.99]

subreglas1
subreglas2
subreglas3

plot(subreglas1, method="matrix", measure="lift")
plot(subreglas2, method="matrix", measure="lift")
plot(subreglas3, method="matrix", measure="lift")


# Bajar el soporte a 10 casos o mas

10/nrow(Caso1trans) #[1] 0.0009999
10/nrow(Caso2trans) #[1] 0.0009999
10/nrow(Caso3trans) #[1] 0.0009999

reglas1diez80 <- apriori(Caso1trans, parameter = list(supp = 0.001, confidence = 0.8))
reglas2diez80 <- apriori(Caso2trans, parameter = list(supp = 0.001, confidence = 0.8))
reglas3diez80 <- apriori(Caso3trans, parameter = list(supp = 0.001, confidence = 0.8))

#########################################################################################
#################################### Ejercicio 2.2.2 ####################################
#########################################################################################
# Tomar las reglas obtenidas en el ejercicio 2.2.1 y ordenarlas desde las más 
# dependientes a las más independientes.
#########################################################################################

# Inspeccionar reglas, en orden descendente (de mayor a menor) segun "lift" 
# la cual es una medida popular de la fuerza regla

inspect(head(sort(reglas1diez80, by="lift"), n=10))
inspect(head(sort(reglas2diez80, by="lift"), n=10))
inspect(head(sort(reglas3diez80, by="lift"), n=10))

plot(reglas1diez80)
head(quality(reglas1diez80))
plot(reglas1diez80, measure=c("support", "lift"), shading="confidence")
plot(reglas1diez80, shading="order", control=list(main = "Two-key plot"))

sel <- plot(reglas1diez80, measure=c("support", "lift"), shading="confidence", 
            interactive=TRUE)


#########################################################################################
#################################### Ejercicio 2.2.3 ####################################
#########################################################################################
# Encontrar i y l para los datos del ejercicio 2.2.1 tales que obtenga el máximo valor 
# dentro de alguna matriz de interés.
#########################################################################################

plot(reglas1diez80, method="matrix3D", measure="lift", control=list(reorder=TRUE))

plot(reglas1diez80, method="matrix", measure=c("lift", "confidence"))
plot(reglas1diez80, method="matrix", measure=c("lift", "confidence"), 
     control=list(reorder=TRUE))

plot(reglas1diez80, method="grouped")
plot(reglas1diez80, method="grouped", control=list(k=50))
sel2 <- plot(reglas1diez80, method="grouped", interactive=TRUE)

plot(reglas1diez80, method="graph")
plot(reglas1diez80, method="graph", control=list(type="itemsets"))
saveAsGraph(head(sort(reglas1diez80, by="lift"),1000), file="rules.graphml")

plot(reglas1diez80, method="paracoord")
plot(reglas1diez80, method="paracoord", control=list(reorder=TRUE))

#########################################################################################
#################################### Ejercicio 2.2.4 ####################################
#########################################################################################
# Consulte el tutorial "reglas Weka" en c:\tutorials de su máquina virtual para usar el
# paquete Weka.associations.apriori para reconstruir los cálculos realizados en el 
# ejercicio 2.2.1
#########################################################################################