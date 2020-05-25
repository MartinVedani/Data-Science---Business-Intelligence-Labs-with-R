# Instalacion y carga del paquete de R a ser utilizado
if(! "truncSP" %in% installed.packages()) install.packages("truncSP", depend = TRUE)
library(truncSP)

# REAL DATA
realData <- read.csv('realData.csv', header=TRUE, sep=",")

# o

realData <- read.csv(file.choose(), header = TRUE, sep = ",")

### TRUNCATED DATA

truncatedData <- read.csv('truncatedData.csv', header=TRUE, sep=",")

# o

truncatedData <- read.csv(file.choose(), header = TRUE, sep = ",")

realY <- realData[,1]
truncatedY <- truncatedData[,1]

realXX <- realData[,2:5]
truncatedXX <- truncatedData[,2:5]

hist(realY)
hist(truncatedY)

fitReal <- lm(Y ~ X2 + X3 + X4, data = realData)
fitTruncated <- lm(Y ~ X2 + X3 + X4, data = truncatedData)

?lt

truncatedFIXED <- lt(Y ~ X2 + X3 + X4, data = truncatedData)

# summary(fitReal)
# summary(fitTruncated)
# summary(truncatedFIXED)

betasReal <- coef(fitReal)
betasTruncated <- coef(fitTruncated)
betasFixed <- coef(truncatedFIXED)

sigmaReal <- var(residuals(fitReal))
sigmaTruncated <- var(residuals(fitTruncated))
sigmaFixed <- var(residuals(truncatedFIXED))[[1]]

solucionReal <- as.matrix(c(betasReal, sigmaReal))
rownames(solucionReal) <- c('Beta1','Beta2','Beta3','Beta4','Sigma')
colnames(solucionReal) <- c('RealData')

solucionTruncated <- as.matrix(c(betasTruncated, sigmaTruncated))
colnames(solucionTruncated) <- c('TruncatedData')

solucionFixed <- as.matrix(c(betasFixed, sigmaFixed))
colnames(solucionFixed) <- c('TruncationFixed')

solucionTuncationProblem <- cbind(solucionReal, solucionTruncated,
                                  solucionFixed)
solucionTuncationProblem