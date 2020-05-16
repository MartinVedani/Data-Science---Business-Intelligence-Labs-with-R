## Análsis de Mercado Financieros utiliizando herrmaientas de Series de Tiempo por Martín Vedani ##

## Unicializar los paquetes en el siguiente orden
library(knitr)
library(kfigr)
library(data.table)
library(ggvis)
library(Quandl)
Quandl.api_key("zc5J562RDUqZTsRHjQvz")
library(quantmod)
library(corrplot)
library(forecast)
library(rugarch)
library(quadprog)

## La ~ en la siguiente ruta de origen debe actualizarse con su propia ruta después de descargar estos 
# scripts de GitHub

#Source garchAuto para adaptarse a la volatilidad en quantmodity in quantmod
source('~/Series Temporales II -  Simulaciones de Montecarlo/scripts/garchAuto.R')
#Source addGuppy para análisis técnico en quantmod
source('~/Series Temporales II -  Simulaciones de Montecarlo/scripts/addGuppy.R')
# Source change2nysebizday para cambiar yearmon (Mmm YYY) a NYSE bizday date (YYYY-mm-dd)
source('~/Series Temporales II -  Simulaciones de Montecarlo/scripts/change2nysebizday.R')
# Source cor.mtest para combinar corrplot con Test de Significancia
source('~/Series Temporales II -  Simulaciones de Montecarlo/scripts/scripts/cor.mtest.R')
# Source funciones de efficient portfolio
source('~/Series Temporales II -  Simulaciones de Montecarlo/scripts/my.eff.frontier.R')
source('~/Series Temporales II -  Simulaciones de Montecarlo/scripts/plotEfficientFrontier.R')
#Source función funggcast para graficar forecast in ggplot
source('~/Series Temporales II -  Simulaciones de Montecarlo/scripts/funggcast.R')
# Source función SDAFE2.R del libro :Statistics and Data Analysis for Financial Engineering, 2nd Edition
source('~/Series Temporales II -  Simulaciones de Montecarlo/scripts/SDAFE2_copy.R')
# Source funciones para el análisis de portfolios
source('~/Series Temporales II -  Simulaciones de Montecarlo/scripts/util_fns.R')
source('~/Series Temporales II -  Simulaciones de Montecarlo/scripts/portfolio_copy.R')
# --fin de la inicialización--


library(BatchGetSymbols)
# Fuente: https://cran.r-project.org/web/packages/BatchGetSymbols/vignettes/BatchGetSymbols-vignette.html

# Configurar fechas
first.date <- as.Date("2018-01-01") # cambiar manualmente
last.date <- Sys.Date()
freq.data <- 'daily'
# Tickers de las acciones a ser analizadas: Amazon, Ebay, y Mercado Libre
tickers <- c("AMZN","EBAY","MELI")

l.out <- BatchGetSymbols(tickers = tickers, first.date = first.date, last.date = last.date, 
                         freq.data = freq.data, cache.folder = file.path(tempdir(), 
                                                                         'BGS_Cache') ) # cache in tempdir()

print(l.out$df.control)

library(ggplot2)

# Ver los gráficos de los tickers
p <- ggplot(l.out$df.tickers, aes(x = ref.date, y = price.close))
p <- p + geom_line()
p <- p + facet_wrap(~ticker, scales = 'free_y') 
print(p)

detach("package:BatchGetSymbols", unload=TRUE)

###### Otro método para conseguir datos históricos completos y manipularlos ######

# Configurar fechas
first.date <- as.Date("2018-01-01") # cambiar manualmente

# Download full histroy of OHLC columns from Yahoo Finance
getSymbols(c("AMZN","EBAY","MELI"), from = first.date )
getSymbols(c("^IRX", "^IXIC","^GSPC", "^MERV"), from = first.date )
getSymbols(c("^BVSP","^MXX", "^N225", "000001.ss"), from = first.date )
# Adjust OHLC columns with both splits and dividends
meli <- adjustOHLC(MELI)
ebay <- adjustOHLC(EBAY)
amzn <- adjustOHLC(AMZN)

###***********
mergedStocks <- merge.xts(Ad(meli), Ad(ebay),Ad(amzn))
###***********

# Markets / Indexes
nasdaq <- IXIC
sp500 <- GSPC
colnames(sp500) <- c("sp500.Open","sp500.High","sp500.Low","sp500.Close",
                     "sp500.Volume","sp500.Adjusted")
braBovespa <- BVSP
argMerval <- MERV
mexMexbol <- MXX
japNikkei <- N225
chinaSSE <- `000001.SS`
colnames(chinaSSE) <- c("chinaSSE.Open","chinaSSE.High","chinaSSE.Low","chinaSSE.Close",
                        "chinaSSE.Volume","chinaSSE.Adjusted")
nasdaqLogRet <- dailyReturn(nasdaq, type="log")
colnames(nasdaqLogRet) <- c("nasdaq.log.ret")
sp500LogRet <- dailyReturn(sp500, type="log")
colnames(sp500LogRet) <- c("sp500.log.ret")
braBovespaLogRet <- dailyReturn(braBovespa, type="log")
colnames(braBovespaLogRet) <- c("braBovespa.log.ret")
argMervalLogRet <- dailyReturn(argMerval, type="log")
colnames(argMervalLogRet) <- c("argMerval.log.ret")
mexMexbolLogRet <- dailyReturn(mexMexbol, type="log")
colnames(mexMexbolLogRet) <- c("mexMexbol.log.ret")
japNikkeiLogRet <- dailyReturn(japNikkei, type="log")
colnames(japNikkeiLogRet) <- c("japNikkei.log.ret")
chinaSSELogRet <- dailyReturn(chinaSSE, type="log") 
colnames(chinaSSELogRet) <- c("chinaSSE.log.ret")

###**********
mergedMarketIndexLogRet <- merge.xts(nasdaqLogRet, sp500LogRet, braBovespaLogRet, argMervalLogRet, 
                                     mexMexbolLogRet, japNikkeiLogRet, chinaSSELogRet)
###**********

# Risk Free Rates for Risk Management

tbill13 <- Ad(IRX)
colnames(tbill13) <- "usTbill13weeks"
us10yLTcomposite <- Quandl("USTREASURY/LONGTERMRATES", start_date = first.date,
                           type="xts", order = "asc")
us10yLTcomposite <- us10yLTcomposite[,"LT Composite > 10 Yrs"]
colnames(us10yLTcomposite) <- "US.LT.above.10yrs"

###**********
mergedRiskManagement <- merge.xts(tbill13,us10yLTcomposite, Cl(sp500))
###**********

# Calculate simple returns and merge all in one matrix

meliRet <- dailyReturn(meli)
colnames(meliRet) <- c("meli.returns")
ebayRet <- dailyReturn(ebay)
colnames(ebayRet) <- c("ebay.returns")
amznRet <- dailyReturn(amzn)
colnames(amznRet) <- c("amzn.returns")

# ###**********
mergedSimpleReturns <- merge.xts(meliRet, ebayRet,amznRet)
# ###**********

# Calculate Gross Cumulative returns (normalize to 1 for each company) and update 
# column names, and merge all in one matrix
meliGrossRet <- cumprod(meliRet + 1)
colnames(meliGrossRet) <- c("meli.gross.ret")
ebayGrossRet <- cumprod(ebayRet + 1)
colnames(ebayGrossRet) <- c("ebay.gross.ret")
amznGrossRet <- cumprod(amznRet + 1)
colnames(amznGrossRet) <- c("amzn.gross.ret")

# ###**********
mergedGrossRet <- merge.xts(meliGrossRet, ebayGrossRet, amznGrossRet)
# ###**********

# Calculate LOGARITMIC returns and merge all in one matrix

meliLogRet <- dailyReturn(meli, type="log")
colnames(meliLogRet) <- c("meli.log.ret")
ebayLogRet <- dailyReturn(ebay, type="log")
colnames(ebayLogRet) <- c("ebay.log.ret")
amznLogRet <- dailyReturn(amzn, type="log")
colnames(amznLogRet) <- c("amzn.log.ret")

###*******************
mergedLogRet <- merge.xts(meliLogRet, ebayLogRet, amznLogRet)
###*******************

# Build MASTER correlation matrix without mergedStocks, mergedGrossRet, mergedFxRates,
# mergedRiskFree, and mergedEconFund

merged.master.matrix <- merge.xts(mergedLogRet, mergedMarketIndexLogRet,
                                  mergedRiskManagement)

#Calculate correlation on masterMergedMatrix with argument: use="pairwise.complete.obs"
master.corr.matrix  <- cor(merged.master.matrix, use="pairwise.complete.obs")


res1 <- cor.mtest(merged.master.matrix, conf.level = 0.95)

## Correlation Matrix - specialized the insignificant value according to the significant level
corrplot(master.corr.matrix, p.mat = res1[[1]], sig.level = 0.05, order = "hclust",
         type = "lower", pch.cex = .8, tl.srt = 60)

# ********************************************************************************* #
# Technical charts

# Set YTD range
ytd <- paste(as.numeric(format(Sys.Date(), '%Y')),"::", sep = "")

# MERCADO LIBRE
chartSeries(meli, theme = "white", subset = ytd, type = "bar",
            name = "(NASDAQ:MELI)", TA="addBBands();addRSI();addVo()")
addTA(RSI(Cl(meli)) > 70, col="lightgreen", border=NA, on= -(1:3))
addVolatility(col = "red", lwd = 2, legend = "Volatility")
addGuppy(col=c(rep("blue",6), rep("black",6)), legend = "GMMA: Blue(short), Black(long)")
reChart(major.ticks='months')

# EBAY
chartSeries(ebay, theme = "white", subset = ytd, type = "bar",
            name = "(NASDAQ:EBAY)", TA="addBBands();addRSI();addVo()")
addTA(RSI(Cl(ebay)) > 70, col="lightgreen", border=NA, on= -(1:3))
addVolatility(col = "red", lwd = 2, legend = paste("Volatility"))
addGuppy(col=c(rep("blue",6), rep("black",6)), legend = "GMMA: Blue(short), Black(long)")

# AMAZON
chartSeries(amzn, theme = "white", subset = ytd, type = "bar",
            name = "(NASDAQ:AMZN)", TA="addBBands();addRSI();addVo()")
addTA(RSI(Cl(amzn)) > 70, col="lightgreen", border=NA, on= -(1:3))
addVolatility(col = "red", lwd = 2, legend = "Volatility")
addGuppy(col=c(rep("blue",6), rep("black",6)), legend = "GMMA: Blue(short), Black(long)")


# ********************************************************************************* #

###### meli.model (DECOMPOSED)

# Decompose trend and seasonal factros with Seasonal Decomposition of Time Series by Loess

?stl

meli.ts <- ts(meli[,"MELI.Close"], start=2018, frequency = 252)

stl.meli <- stl(meli.ts[,"MELI.Close"], s.window = "periodic", robust = T)

summary(stl.meli)

plot(stl.meli, main = "MELI Factors Decomposition")

abline(v=c(2018,2019,2020), col="blue", lty=2)

# MELI is seasonal, with a trend, and remainder data - as expected with financial data.

#### ***** VOLATILITY *******
# # Rolling realized volatility over a 20 day window
# # http://www.spiderfinancial.com/support/documentation/numxl/tips-and-tricks/volatility-101
# # http://stackoverflow.com/questions/12823445/faster-way-of-calculating-rolling-realized-volatility-in-r

autoplot(meliLogRet, major.ticks = "years", main = "MELI's daily log returns")

?runSD

realized.vol.meli.20days <- runSD(meliLogRet, n=20) * sqrt(252)
plot.xts(realized.vol.meli.20days, major.ticks = "years",
         main = "MELI's realized volatility over 20 days")

# # Forecast using STL (Seasonal Decomposition of Time Series by Loess)

# There are 3 similar methods, we will use stl.
?stl
?stlm
?stlf

stl.forecast.meli <- forecast(stl.meli, h=90, fan = F)

summary(stl.forecast.meli)

autoplot(stl.forecast.meli)

# # Forecast using ets (Exponential smoothing state space model)

?ets

ets.meli <- ets(meli[,"MELI.Close"], allow.multiplicative.trend = T)

ets.forecast.meli <- forecast.ets(ets.meli, h=90)

summary(ets.forecast.meli)

autoplot(ets.forecast.meli)

## Arima Forecasts:
# # ***** xreg	Optionally, a vector or matrix of external regressors, which must have the 
# # same number of rows as x.

?auto.arima

arima.meli <- auto.arima(meli.ts[,"MELI.Close"], approximation = FALSE, stepwise = FALSE,
                         allowdrift = TRUE, allowmean = TRUE, trace = TRUE, lambda="auto")

arima.forecast.meli <- forecast(arima.meli, h=90)

summary(arima.forecast.meli)

autoplot(arima.forecast.meli)

## COMPARE ACCURACY with Diebold-Mariano test for predictive accuracy

str(stl.forecast.meli) # stl.forecast.meli$residuals
str(ets.forecast.meli) # ets.forecast.meli$residuals
str(arima.forecast.meli) # arima.forecast.meli$residuals

?dm.test

# # Usage
# 
# # dm.test(e1, e2, alternative=c("two.sided","less","greater"),
# #         h=1, power=2)
# 
# # Arguments
# 
# # e1 Forecast errors from method 1.
# 
# # e2 Forecast errors from method 2.
# 
# # alternative a character string specifying the alternative hypothesis, must be one of
# # "two.sided" (default), "greater" or "less". You can specify just the initial letter.
# 
# # h The forecast horizon used in calculating e1 and e2.
# 
# # power The power used in the loss function. Usually 1 or 2.
# 
# # Details
# 
# # The null hypothesis is that the two methods have the same forecast accuracy. 
# # For alternative="less", the alternative hypothesis is that method 2 is less accurate than
# # method 1. 
# # For alternative="greater", the alternative hypothesis is that method 2 is more accurate 
# # than method 1. 
# # For alternative="two.sided", the alternative hypothesis is that method 1 and method 2 have 
# # different levels of accuracy
# 
# # A smaller p-value means that there is stronger evidence in favor of the alternative hypothesis

# # STLM vs. STLF

dm.test(stl.forecast.meli$residuals, ets.forecast.meli$residuals,
        alternative = "two.sided", h = 90)

# p-value = 0.0001384is low, H0 rejected, stl and ets have different levels of accuracy.

dm.test(stl.forecast.meli$residuals, ets.forecast.meli$residuals,
        alternative = "greater", h = 90)

# p-value = 6.921e-05, H0 rejected: ets IS NOT more accurate than stl.

dm.test(stl.forecast.meli$residuals, ets.forecast.meli$residuals,
        alternative = "less", h = 90)

# p-value = 0.9999, H0 accepted: ets IS less accurate than stl.

##########

# # STL vs. ARIMA 
dm.test(stl.forecast.meli$residuals, arima.forecast.meli$residuals,
         alternative = "two.sided", h = 90)
 
# p-value = 0.03161 is pretty low, H0 ejected, stl and arima DO have
# different levels of accuracy.

dm.test(stl.forecast.meli$residuals, arima.forecast.meli$residuals,
        alternative = "less", h = 90)

# # p-value = 0.0158 H0 ejected: arima is NOT less accurate than stl.

dm.test(stl.forecast.meli$residuals, arima.forecast.meli$residuals,
        alternative = "greater",h = 90)

# # p-value = 0.9842, H0 accewpted, arima IS more accurate than stl.

# # So ARIMA si the better predictor on "this ocassion", it may not always be case.

# # ********************************************************************************* #

# Lets use MONTECARLO simulations now

auto.arima.meli <- arima.meli
auto.arima.meli
# ARIMA(2,1,2)

# In choosing ARMA(p,q) the theory of difference equations suggests that
# we should choose ARMA(p+1,p), which gives rise to a solution of the difference
# equation with p+1 terms. You can of course have some zero orders
# This is explained in my book H. D. Vinod,
# "Hands-On Intermediate Econometrics Using R: Templates for Extending Dozens of Practical Examples." (2008) World Scientific Publishers: Hackensack, NJ.
# (http://www.worldscibooks.com/economics/6895.html)

?ugarchspec

auto.arima.meli$arma[1]+1
# 3 -> checked vs. ARIMA(2 +1 = 3,1,2)

arma.garch.meli.spec.std <- ugarchspec(variance.model=list(garchOrder=c(1,1)),
                                 mean.model=list(armaOrder=c(auto.arima.meli$arma[1]+1,
                                         auto.arima.meli$arma[2]), archm=T, archpow=1),
                                       distribution.model = "sged")

arma.garch.meli.fit.std <- ugarchfit(spec=arma.garch.meli.spec.std, data=meliLogRet)

# http://unstarched.net/r-examples/rugarch/a-short-introduction-to-the-rugarch-package/


arma.garch.meli.sim <- ugarchsim(fit = arma.garch.meli.fit.std, startMethod = "sample",
                                 n.sim = 90, m.sim = 20000, rseed = 123)



plot(arma.garch.meli.sim, which = 2)
plot(arma.garch.meli.sim, which = 1)
plot(arma.garch.meli.sim, which = 4)
plot(arma.garch.meli.sim, which = 3)


# Extract fitted forecasted log returns from simulation variable
fitted.meli <- fitted(arma.garch.meli.sim)
# convert forecasted log returns to prices
# a1 <- c(88.23, 88.44, 88.55, 88.77, 88.99)
# a2 <- diff(log(a1))
# price.converted.example <- a1[1] * c(1, exp(cumsum(a2)))
# first.meli.price <- as.numeric(Ad(meli)[1])
# prices.convertedBack.meli <- first.meli.price * c(1, exp(cumsum(meliLogRet[-1,])))
meli.fitted.mean <- apply(fitted.meli, 1, mean)
last.meli.price <- as.numeric(last(Ad(meli)))
projected.ret.as.price <- last.meli.price * c(1, exp(cumsum(meli.fitted.mean)))
meli.target.price.66d <- as.numeric(last(projected.ret.as.price))

meli.target.price.66d
# 968.0871

autoplot(arima.forecast.meli)
# MAKES SENSE !!!

####***************************************
