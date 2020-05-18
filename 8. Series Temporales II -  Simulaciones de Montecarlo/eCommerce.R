#############################################################################################
#  This is a BONUS Lab in R, where will apply Montecarlo Simulations and Efficient Portfolio 
#  Management theory to Financial Time Series to determine how much of 5 Global eCommerce 
#  stocks we should buy for our investment portfolio. Namly, we will be looking at:
#
#  Mercado Libre (Latam), Amazon and Ebay (USA/Europe), Alibaba & JD.com (Asia/China)
#
#
# - por Martin Vedani, UTN Business Intellgence
#
############################################################################################

## Unicializar los paquetes en el siguiente orden
library(knitr)
library(kfigr)
library(data.table)
library(ggvis)
library(quantmod)
library(corrplot)
library(forecast)
library(rugarch)
library(quadprog)
library(ggplot2)

## La ~ en la siguiente ruta de origen debe actualizarse con su propia ruta después de descargar estos 
# scripts de GitHub

#Source garchAuto para adaptarse a la volatilidad en quantmodity in quantmod
source('~/8. Series Temporales II -  Simulaciones de Montecarlo/scripts/garchAuto.R')
#Source addGuppy para análisis técnico en quantmod
source('~/8. Series Temporales II -  Simulaciones de Montecarlo/scripts/addGuppy.R')
# Source change2nysebizday para cambiar yearmon (Mmm YYY) a NYSE bizday date (YYYY-mm-dd)
source('~/8. Series Temporales II -  Simulaciones de Montecarlo/scripts/change2nysebizday.R')
# Source cor.mtest para combinar corrplot con Test de Significancia
source('~/8. Series Temporales II -  Simulaciones de Montecarlo/scripts/cor.mtest.R')
# Source funciones de efficient portfolio
source('~/8. Series Temporales II -  Simulaciones de Montecarlo/scripts/my.eff.frontier.R')
source('~/8. Series Temporales II -  Simulaciones de Montecarlo/scripts/plotEfficientFrontier.R')
#Source función funggcast para graficar forecast in ggplot
source('~/8. Series Temporales II -  Simulaciones de Montecarlo/scripts/funggcast.R')
# Source función SDAFE2.R del libro :Statistics and Data Analysis for Financial Engineering, 2nd Edition
source('~/8. Series Temporales II -  Simulaciones de Montecarlo/scripts/SDAFE2_copy.R')
# Source funciones para el análisis de portfolios
source('~/8. Series Temporales II -  Simulaciones de Montecarlo/scripts/util_fns.R')
source('~/8. Series Temporales II -  Simulaciones de Montecarlo/scripts/portfolio_copy.R')
# Set YTD range
ytd <- paste(as.numeric(format(Sys.Date(), '%Y')),"::", sep = "")
# --end of start up --


library(BatchGetSymbols)
# Fuente: https://cran.r-project.org/web/packages/BatchGetSymbols/vignettes/BatchGetSymbols-vignette.html

# Configurar fechas
first.date <- as.Date("2018-01-01") # cambiar manualmente

last.date <- as.Date("2020-05-16") # cambiar manualmente, d+1 for last.date may 15 2020 in this example
# last.date <- Sys.Date() # use this option to get up to the latest date available

freq.data <- 'daily'
# Tickers de las acciones a ser analizadas: Amazon, Ebay, y Mercado Libre
tickers <- c("AMZN","EBAY","MELI", "JD", "BABA")

l.out <- BatchGetSymbols(tickers = tickers, first.date = first.date, last.date = last.date, 
                         freq.data = freq.data, cache.folder = file.path(tempdir(), 
                                                                         'BGS_Cache') ) # cache in tempdir()

print(l.out$df.control)

# Ver los gráficos de los tickers
p <- ggplot(l.out$df.tickers, aes(x = ref.date, y = price.close))
p <- p + geom_line()
p <- p + facet_wrap(~ticker, scales = 'free_y') 

detach("package:BatchGetSymbols", unload=TRUE)

print(p)

###### Otro método para conseguir datos históricos completos y manipularlos más fácil######

# Configurar fechas
first.date <- as.Date("2018-01-01") # cambiar manualmente 
last.date <- as.Date("2020-05-16") # cambiar manualmente, d+1 for last.date may 15 2020 in this example
# last.date <- Sys.Date() # use this option to get up to the latest date available

# Download full histroy of OHLC columns from Yahoo Finance
getSymbols(c("AMZN","EBAY","MELI", "BABA", "JD"), from = first.date , to = last.date)
getSymbols(c("^IRX", "^TNX","^IXIC","^GSPC", "^MERV"), from = first.date , to = last.date)
getSymbols(c("^BVSP","^MXX", "^N225", "000001.ss"), from = first.date , to = last.date)
# Adjust OHLC columns with both splits and dividends
meli <- adjustOHLC(MELI)
ebay <- adjustOHLC(EBAY)
amzn <- adjustOHLC(AMZN)
baba <- adjustOHLC(BABA)
jd <- adjustOHLC(JD)

###***********
mergedStocks <- merge.xts(Ad(meli), Ad(ebay),Ad(amzn),Ad(baba),Ad(jd))
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

# Short term - 13 week US Treasury Bill
tbill13 <- Ad(IRX)
colnames(tbill13) <- "usTbill13weeks"

# Long Term - 10 year US Treasury Bonds
us10yLTcomposite <- Ad(TNX)
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
babaRet <- dailyReturn(baba)
colnames(babaRet) <- c("baba.returns")
jdRet <- dailyReturn(jd)
colnames(jdRet) <- c("jd.returns")

# ###**********
mergedSimpleReturns <- merge.xts(meliRet, ebayRet, amznRet, babaRet, jdRet)
# ###**********

# Calculate Gross Cumulative returns (normalize to 1 for each company) and update 
# column names, and merge all in one matrix
meliGrossRet <- cumprod(meliRet + 1)
colnames(meliGrossRet) <- c("meli.gross.ret")
ebayGrossRet <- cumprod(ebayRet + 1)
colnames(ebayGrossRet) <- c("ebay.gross.ret")
amznGrossRet <- cumprod(amznRet + 1)
colnames(amznGrossRet) <- c("amzn.gross.ret")
babaGrossRet <- cumprod(babaRet + 1)
colnames(babaGrossRet) <- c("baba.gross.ret")
jdGrossRet <- cumprod(jdRet + 1)
colnames(jdGrossRet) <- c("jd.gross.ret")

# ###**********
mergedGrossRet <- merge.xts(meliGrossRet, ebayGrossRet, amznGrossRet, babaGrossRet, jdGrossRet)
# ###**********

# Calculate LOGARITMIC returns and merge all in one matrix

meliLogRet <- dailyReturn(meli, type="log")
colnames(meliLogRet) <- c("meli.log.ret")
ebayLogRet <- dailyReturn(ebay, type="log")
colnames(ebayLogRet) <- c("ebay.log.ret")
amznLogRet <- dailyReturn(amzn, type="log")
colnames(amznLogRet) <- c("amzn.log.ret")
babaLogRet <- dailyReturn(baba, type="log")
colnames(babaLogRet) <- c("baba.log.ret")
jdLogRet <- dailyReturn(jd, type="log")
colnames(jdLogRet) <- c("jd.log.ret")

###*******************
mergedLogRet <- merge.xts(meliLogRet, ebayLogRet, amznLogRet, babaLogRet, jdLogRet)
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
         type = "upper", pch.cex = .8, tl.srt = 60)


#****************************************************************************************

# ********************************************************************************* #
# Technical charts

# Set Year-to-Date (YTD) range
ytd <- paste(as.numeric(format(Sys.Date(), '%Y')),"::", sep = "")

# MERCADO LIBRE
chartSeries(meli, theme = "white", subset = ytd, type = "bar",
            name = "(NASDAQ:MELI)", TA="addBBands();addRSI();addVo()")
addTA(RSI(Cl(meli)) > 70, col="lightyellow", border=NA, on= -(1:3)) #over-bought, expect fall in price
addVolatility(col = "red", lwd = 2, legend = "Volatility")
addGuppy(col=c(rep("blue",6), rep("black",6)), legend = "GMMA: Blue(short), Black(long)")
reChart(major.ticks='months')

# AMAZON
chartSeries(amzn, theme = "white", subset = ytd, type = "bar",
            name = "(NASDAQ:AMZN)", TA="addBBands();addRSI();addVo()")
addTA(RSI(Cl(amzn)) > 70, col="lightyellow", border=NA, on= -(1:3)) #over-bought, expect fall in price
addVolatility(col = "red", lwd = 2, legend = "Volatility")
addGuppy(col=c(rep("blue",6), rep("black",6)), legend = "GMMA: Blue(short), Black(long)")

# EBAY
chartSeries(ebay, theme = "white", subset = ytd, type = "bar",
            name = "(NASDAQ:EBAY)", TA="addBBands();addRSI();addVo()")
addTA(RSI(Cl(ebay)) > 70, col="lightyellow", border=NA, on= -(1:3)) #over-bought, expect fall in price
addVolatility(col = "red", lwd = 2, legend = paste("Volatility"))
addGuppy(col=c(rep("blue",6), rep("black",6)), legend = "GMMA: Blue(short), Black(long)")

# Alibaba
chartSeries(baba, theme = "white", subset = ytd, type = "bar",
            name = "(NYSE:BABA)", TA="addBBands();addRSI();addVo()")
addTA(RSI(Cl(baba)) > 70, col="lightyellow", border=NA, on= -(1:3)) #over-bought, expect fall in price
addVolatility(col = "red", lwd = 2, legend = "Volatility")
addGuppy(col=c(rep("blue",6), rep("black",6)), legend = "GMMA: Blue(short), Black(long)")

# JD.com
chartSeries(jd, theme = "white", subset = ytd, type = "bar",
            name = "(NASDAQ:JD)", TA="addBBands();addRSI();addVo()")
addTA(RSI(Cl(baba)) > 70, col="lightyellow", border=NA, on= -(1:3)) #over-bought, expect fall in price
addVolatility(col = "red", lwd = 2, legend = "Volatility")
addGuppy(col=c(rep("blue",6), rep("black",6)), legend = "GMMA: Blue(short), Black(long)")

CAPM.table

#****************************************************************************************
###### meli.model (DECOMPOSED)

#### ***** VOLATILITY *******
# # Rolling realized volatility over a 20 day window
# # http://www.spiderfinancial.com/support/documentation/numxl/tips-and-tricks/volatility-101
# # http://stackoverflow.com/questions/12823445/faster-way-of-calculating-rolling-realized-volatility-in-r

autoplot(meliLogRet, main = "MELI's daily log returns")

?runSD

realized.vol.meli.20days <- runSD(meliLogRet, n=20) * sqrt(252)
plot.xts(realized.vol.meli.20days, major.ticks = "months",
         main = "Volatility over 20-day-rolling-window")

# Decompose trend and seasonal factros with Seasonal Decomposition of Time Series by Loess

?stl

head(meli[,6])
# checked MELI.Adjusted closing price

meli.ts <- ts(meli[,6], start = c(2018), frequency = 252)

stl.meli <- stl(meli.ts[,"MELI.Adjusted"], s.window = "periodic", robust = T)

summary(stl.meli)

plot(stl.meli, main = "MELI Factors Decomposition")

abline(v=c(2018,2019,2020), col="blue", lty=2)
abline(v=c(2018.25, 2018.5, 2018.75,
           2019.25, 2019.5, 2019.75,
           2020.25, 2020.5, 2020.75), col="salmon", lty=2)

# MELI is seasonal, with a trend, and remainder data - as expected with financial data.

# # Forecast using STL (Seasonal Decomposition of Time Series by Loess)

# 3 months is 90 calendar days, approcimately 66 business/trading days, so we will forecast 66 periods
# ahead

# There are 3 similar methods, we will use stl.
?stl
?stlm
?stlf

set.seed(123) # to be able to reproduce de same resutls
stl.forecast.meli <- forecast(stl.meli, h=66)

summary(stl.forecast.meli)

autoplot(stl.forecast.meli)

# # Forecast using ets (Exponential smoothing state space model)

?ets

head(meli[,6])
# checked MELI.Adjusted closing price

ets.meli <- ets(meli[,6], allow.multiplicative.trend = T)

set.seed(123) # to be able to reproduce de same resutls
ets.forecast.meli <- forecast.ets(ets.meli, h=66)

summary(ets.forecast.meli)

autoplot(ets.forecast.meli)

## Arima Forecasts:
# # ***** xreg	Optionally, a vector or matrix of external regressors, which must have the 
# # same number of rows as x.

?auto.arima

auto.arima.meli <- auto.arima(meli.ts[,"MELI.Adjusted"], approximation = FALSE, stepwise = FALSE, 
                              seasonal = TRUE, lambda = "auto",
                              allowdrift =  TRUE, allowmean = TRUE, trace = TRUE)


set.seed(123) # to be able to reproduce de same resutls
arima.forecast.meli <- forecast(auto.arima.meli, h=66)

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
# #         h=1, power=1)
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
# # power The power used in the loss function. Usually 1 or 2.  The choice of power is entirely due 
# # to the loss function. If we lose x dollars if the forecast error is x - then our loss function is 
# # linear and we should use the option power = 1. If we lose x^2 dollars when the forecast error is x,
# # then we should use power = 2.
# 
# # Details
# 
# # The null hypothesis is that the two methods have the same forecast accuracy. 
# #
# # For alternative="two.sided", the alternative hypothesis is that method 1 and method 2 have 
# # different levels of accuracy
# #
# # For alternative="less", the alternative hypothesis is that method 2 is less accurate than
# # method 1. 
# #
# # For alternative="greater", the alternative hypothesis is that method 2 is more accurate 
# # than method 1. 
# 
# # A smaller p-value means that there is stronger evidence in favor of the alternative hypothesis

# # STLM vs. ETS

dm.test(stl.forecast.meli$residuals, ets.forecast.meli$residuals, power = 1,
        alternative = "two.sided", h = 66)

# p-value = 2.426e-14 is very low, H0 rejected, stl and ets have different levels of accuracy.

dm.test(stl.forecast.meli$residuals, ets.forecast.meli$residuals, power = 1,
        alternative = "greater", h = 66)

# p-value = 1.213e-14 is very low, H0 rejected: ets IS MORE accurate than stl.

dm.test(stl.forecast.meli$residuals, ets.forecast.meli$residuals, power = 1,
        alternative = "less", h = 66)

# p-value = 1, H0 accepted: ets IS NOT LESS accurate than stl.

## So ETS is more accurate than STL in this case.

##########

# # ETS vs. ARIMA 
dm.test(ets.forecast.meli$residuals, arima.forecast.meli$residuals, power = 1,
        alternative = "two.sided", h = 66)

# p-value = 1.062e-06 is very low, H0 rejected, ets and arima have different levels of accuracy.

dm.test(ets.forecast.meli$residuals, arima.forecast.meli$residuals, power = 1,
        alternative = "less", h = 66)

# # p-value = 5.308e-07 is very low, H0 is rejected: arima IS LESS accurate than ets.

dm.test(ets.forecast.meli$residuals, arima.forecast.meli$residuals, power = 1,
        alternative = "greater",h = 66)

# # p-value = 1, H0 accepted, arima IS NOT more accurate than ets.

# # So ETS si the better predictor on "this ocassion", it may not always be case.

# # ********************************************************************************* #

# Lets use MONTECARLO simulations now adn run 20,000 simulations 66 trading days into the future.

# Alternatively to auto.arima which can take very long and be very heavy on computational resources,
# there is the option to use auto ARFIMA out of the rugarcch library.

# The Rugarch package is one of the more robust R libraries for financial data analysis and forecating. 

# It allows to forecast more than just prices, specifically volatility which is very importat
# for VaR estimations at the time to decide what porfoloio allocations are on the efficient 
# fontier and which allocations will give us less than optimal returns for a given level of
# risk. 

# Even more so, it can accept streaming data and build it into the models, but we will not cover that
# here - it is just nice to know.

# # Sources: http://www.unstarched.net/r-examples/rugarch/a-short-introduction-to-the-rugarch-package/
# #          https://palomar.home.ece.ust.hk/MAFS6010R_lectures/Rsession_time_series_modeling.html

library(rugarch)

auto.arfima.meli <- autoarfima(meliLogRet, ar.max = 2, ma.max = 2, 
                               criterion = "AIC", method = "partial",
                               solver = "hybrid")

show(head(auto.arfima.meli$rank.matrix))
#   AR MA Mean ARFIMA       AIC converged
# 1  2  2    0      0 -3.970398         1

# So ARMA(2,2) model WITHOUT a mean

# Parameters for ARIMA(p,d,q)

# If convergence problems arise

# In choosing ARMA(p,q) the theory of difference equations suggests that
# we should choose ARMA(p+1,p), which gives rise to a solution of the difference
# equation with p+1 terms. You can of course have some zero orders
# This is explained in my book H. D. Vinod,
# "Hands-On Intermediate Econometrics Using R: Templates for Extending Dozens of Practical Examples." (2008) World Scientific Publishers: Hackensack, NJ.
# (http://www.worldscibooks.com/economics/6895.html)

meli.p <- auto.arfima.meli$rank.matrix[1,"AR"]
meli.q <- auto.arfima.meli$rank.matrix[1,"MA"]
meli.mean <- auto.arfima.meli$rank.matrix[1,"Mean"]
meli.p
meli.q
meli.mean

?ugarchspec

# No convergence problems so we will use p and q with/without mean as estimated:

arma.garch.meli.spec.std <- ugarchspec(
        variance.model = list(model = "sGARCH", garchOrder=c(1,1)),
        mean.model = list(armaOrder=c(meli.p, meli.q), include.mean = meli.mean), 
        distribution.model = "sged")

arma.garch.meli.fit.std <- ugarchfit(spec=arma.garch.meli.spec.std, data=meliLogRet)


coef(arma.garch.meli.fit.std)
# see all coeficients

?ugarchsim

# Run 20,000 simulations, 66 trading days into the future. Set seed 123 to be able to reproduce same
# results.

T = 20000

arma.garch.meli.sim <- ugarchsim(fit = arma.garch.meli.fit.std, startMethod = "sample",
                                 n.sim = 66, m.sim = T, rseed = 123)

# Series Plot
plot(arma.garch.meli.sim, which = 2)

# Sigma Plot
plot(arma.garch.meli.sim, which = 1)

# Kernel Density Plot
plot(arma.garch.meli.sim, which = 4)

# Conditional Sigma - Kernel Density
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
meli.projected.ret.as.price <- last.meli.price * c(1, exp(cumsum(meli.fitted.mean)))
meli.target.price.66d <- as.numeric(last(meli.projected.ret.as.price))

meli.target.price.66d 
# 773.2526

chart.meli <- autoplot(ets.forecast.meli)
chart.meli + geom_hline(yintercept = meli.target.price.66d, color = "red")
# MAKES SENSE !!!


#*************************************************************
## Investments and Portfolio Management
#*************************************************************
## First, we need to run the same simulations for everyone else

######  Amazon ###### 
head(amzn[,6])
# checked AMZN.Adjusted closing price

ets.amzn <- ets(amzn[,6], allow.multiplicative.trend = T)

set.seed(123) # to be able to reproduce de same resutls
ets.forecast.amzn <- forecast.ets(ets.amzn, h=66)

summary(ets.forecast.amzn)

autoplot(ets.forecast.amzn)

auto.arfima.amzn <- autoarfima(amznLogRet, ar.max = 2, ma.max = 2, 
                               criterion = "AIC", method = "partial",
                               solver = "hybrid")

show(head(auto.arfima.amzn$rank.matrix))
#   AR MA Mean ARFIMA       AIC converged
# 1  1  2    1      0 -4.926614         1

amzn.p <- auto.arfima.amzn$rank.matrix[1,"AR"]
amzn.q <- auto.arfima.amzn$rank.matrix[1,"MA"]
amzn.mean <- auto.arfima.amzn$rank.matrix[1,"Mean"]
amzn.p
amzn.q
amzn.mean

# No convergence problems so we will use p and q with/without mean as estimated:

arma.garch.amzn.spec.std <- ugarchspec(
        variance.model = list(model = "sGARCH", garchOrder=c(1,1)),
        mean.model = list(armaOrder=c(amzn.p, amzn.q), include.mean = amzn.mean), 
        distribution.model = "sged")

arma.garch.amzn.fit.std <- ugarchfit(spec=arma.garch.amzn.spec.std, data=amznLogRet)

coef(arma.garch.amzn.fit.std)

# 20,000 simulations, 66 trading days into the future. Set seed 123

arma.garch.amzn.sim <- ugarchsim(fit = arma.garch.amzn.fit.std, startMethod = "sample",
                                 n.sim = 66, m.sim = T, rseed = 123)

plot(arma.garch.amzn.sim, which = 2)
plot(arma.garch.amzn.sim, which = 1)
plot(arma.garch.amzn.sim, which = 4)
plot(arma.garch.amzn.sim, which = 3)

# Extract fitted forecasted log returns from simulation variable
fitted.amzn <- fitted(arma.garch.amzn.sim)
amzn.fitted.mean <- apply(fitted.amzn, 1, mean)
last.amzn.price <- as.numeric(last(Ad(amzn)))
amzn.projected.ret.as.price <- last.amzn.price * c(1, exp(cumsum(amzn.fitted.mean)))
amzn.target.price.66d <- as.numeric(last(amzn.projected.ret.as.price))

amzn.target.price.66d 
# [1] 2664.956

chart.amzn <- autoplot(ets.forecast.amzn) 
chart.amzn + geom_hline(yintercept = amzn.target.price.66d, color = "red")
# MAKES SENSE !!!

###### EBAY ###### 
head(ebay[,6])
# checked EBAY.Adjusted closing price

ets.ebay <- ets(ebay[,6], allow.multiplicative.trend = T)

set.seed(123) # to be able to reproduce de same resutls
ets.forecast.ebay <- forecast.ets(ets.ebay, h=66)

summary(ets.forecast.ebay)

autoplot(ets.forecast.ebay)

auto.arfima.ebay <- autoarfima(ebayLogRet, ar.max = 2, ma.max = 2, 
                               criterion = "AIC", method = "partial",
                               solver = "hybrid")

show(head(auto.arfima.ebay$rank.matrix))
#   AR MA Mean ARFIMA       AIC converged
# 1  1  0    0      0 -5.041950         1

ebay.p <- auto.arfima.ebay$rank.matrix[1,"AR"]
ebay.q <- auto.arfima.ebay$rank.matrix[1,"MA"]
ebay.mean <- auto.arfima.ebay$rank.matrix[1,"Mean"]
ebay.p
ebay.q
ebay.mean

# No convergence problems so we will use p and q with/without mean as estimated:

arma.garch.ebay.spec.std <- ugarchspec(
        variance.model = list(model = "sGARCH", garchOrder=c(1,1)),
        mean.model = list(armaOrder=c(ebay.p, ebay.q), include.mean = ebay.mean), 
        distribution.model = "sged")

arma.garch.ebay.fit.std <- ugarchfit(spec=arma.garch.ebay.spec.std, data=ebayLogRet)

coef(arma.garch.ebay.fit.std)

# 20,000 simulations, 66 trading days into the future. Set seed 123

arma.garch.ebay.sim <- ugarchsim(fit = arma.garch.ebay.fit.std, startMethod = "sample",
                                 n.sim = 66, m.sim = T, rseed = 123)

plot(arma.garch.ebay.sim, which = 2)
plot(arma.garch.ebay.sim, which = 1)
plot(arma.garch.ebay.sim, which = 4)
plot(arma.garch.ebay.sim, which = 3)

# Extract fitted forecasted log returns from simulation variable
fitted.ebay <- fitted(arma.garch.ebay.sim)
ebay.fitted.mean <- apply(fitted.ebay, 1, mean)
last.ebay.price <- as.numeric(last(Ad(ebay)))
ebay.projected.ret.as.price <- last.ebay.price * c(1, exp(cumsum(ebay.fitted.mean)))
ebay.target.price.66d <- as.numeric(last(ebay.projected.ret.as.price))

ebay.target.price.66d 
# [1] 42.11713

chart.ebay <- autoplot(ets.forecast.ebay) 
chart.ebay + geom_hline(yintercept = ebay.target.price.66d, color = "red")
# MAKES SENSE !!!

###### ALIBABA ###### 
head(baba[,6])
# checked BABA.Adjusted closing price

ets.baba <- ets(baba[,6], allow.multiplicative.trend = T)

set.seed(123) # to be able to reproduce de same resutls
ets.forecast.baba <- forecast.ets(ets.baba, h=66)

summary(ets.forecast.baba)

autoplot(ets.forecast.baba)

auto.arfima.baba <- autoarfima(babaLogRet, ar.max = 2, ma.max = 2, 
                               criterion = "AIC", method = "partial",
                               solver = "hybrid")

show(head(auto.arfima.baba$rank.matrix))
#   AR MA Mean ARFIMA       AIC converged
# 1  2  2    0      0 -4.840443         1

baba.p <- auto.arfima.baba$rank.matrix[1,"AR"]
baba.q <- auto.arfima.baba$rank.matrix[1,"MA"]
baba.mean <- auto.arfima.baba$rank.matrix[1,"Mean"]
baba.p
baba.q
baba.mean

# No convergence problems so we will use p and q with/without mean as estimated:

arma.garch.baba.spec.std <- ugarchspec(
        variance.model = list(model = "sGARCH", garchOrder=c(1,1)),
        mean.model = list(armaOrder=c(baba.p, baba.q), include.mean = baba.mean), 
        distribution.model = "sged")

arma.garch.baba.fit.std <- ugarchfit(spec=arma.garch.baba.spec.std, data=babaLogRet)

coef(arma.garch.baba.fit.std)

# 20,000 simulations, 66 trading days into the future. Set seed 123

arma.garch.baba.sim <- ugarchsim(fit = arma.garch.baba.fit.std, startMethod = "sample",
                                 n.sim = 66, m.sim = T, rseed = 123)

plot(arma.garch.baba.sim, which = 2)
plot(arma.garch.baba.sim, which = 1)
plot(arma.garch.baba.sim, which = 4)
plot(arma.garch.baba.sim, which = 3)

# Extract fitted forecasted log returns from simulation variable
fitted.baba <- fitted(arma.garch.baba.sim)
baba.fitted.mean <- apply(fitted.baba, 1, mean)
last.baba.price <- as.numeric(last(Ad(baba)))
baba.projected.ret.as.price <- last.baba.price * c(1, exp(cumsum(baba.fitted.mean)))
baba.target.price.66d <- as.numeric(last(baba.projected.ret.as.price))

baba.target.price.66d 
# [1] 203.7607

chart.baba <- autoplot(ets.forecast.baba) 
chart.baba + geom_hline(yintercept = baba.target.price.66d, color = "red")
# MAKES SENSE !!!

###### JD.com ###### 
head(jd[,6])
# checked JS.Adjusted closing price

ets.jd <- ets(jd[,6], allow.multiplicative.trend = T)

set.seed(123) # to be able to reproduce de same resutls
ets.forecast.jd <- forecast.ets(ets.jd, h=66)

summary(ets.forecast.jd)

autoplot(ets.forecast.jd)

auto.arfima.jd <- autoarfima(jdLogRet, ar.max = 2, ma.max = 2, 
                             criterion = "AIC", method = "partial",
                             solver = "hybrid")

show(head(auto.arfima.jd$rank.matrix))
#   AR MA Mean ARFIMA       AIC converged
# 1  1  1    0      0 -4.315711         1

jd.p <- auto.arfima.jd$rank.matrix[1,"AR"]
jd.q <- auto.arfima.jd$rank.matrix[1,"MA"]
jd.mean <- auto.arfima.jd$rank.matrix[1,"Mean"]
jd.p
jd.q
jd.mean

# No convergence problems so we will use p and q with/without mean as estimated:

arma.garch.jd.spec.std <- ugarchspec(
        variance.model = list(model = "sGARCH", garchOrder=c(1,1)),
        mean.model = list(armaOrder=c(jd.p, jd.q), include.mean = jd.mean), 
        distribution.model = "sged")

arma.garch.jd.fit.std <- ugarchfit(spec=arma.garch.jd.spec.std, data=jdLogRet)

coef(arma.garch.jd.fit.std)

# 20,000 simulations, 66 trading days into the future. Set seed 123

arma.garch.jd.sim <- ugarchsim(fit = arma.garch.jd.fit.std, startMethod = "sample",
                               n.sim = 66, m.sim = T, rseed = 123)

plot(arma.garch.jd.sim, which = 2)
plot(arma.garch.jd.sim, which = 1)
plot(arma.garch.jd.sim, which = 4)
plot(arma.garch.jd.sim, which = 3)

# Extract fitted forecasted log returns from simulation variable
fitted.jd <- fitted(arma.garch.jd.sim)
jd.fitted.mean <- apply(fitted.jd, 1, mean)
last.jd.price <- as.numeric(last(Ad(jd)))
jd.projected.ret.as.price <- last.jd.price * c(1, exp(cumsum(jd.fitted.mean)))
jd.target.price.66d <- as.numeric(last(jd.projected.ret.as.price))

jd.target.price.66d 
# [1] 49.99154

chart.jd <- autoplot(ets.forecast.jd) 
chart.jd + geom_hline(yintercept = jd.target.price.66d, color = "red")
# MAKES SENSE !!!


#****************************************************************************************
### eCommerce porfolio allocation with and without short selling

# The optimal or efficient portfolio mixes that follow were formulated using:  
# . Projected daily prices based on the simulations for all companies that we discussed earlier.  
# . Computing arithmetic daily returns of said projected prices.  
# . Limitation on the maximum portfolio participation of any single stock to not go above a ceiling of 40% long, and not below 20% short. I chose these constraints for arbitrary reasons: a rule of thumb of twice the amount of an equal weights portfolio (100% divided by 5) for long and no more than once the balanced portfolio for short positions.    
# . Sharpe ratio calculated using stocks daily simple returns in excess of risk free rate. 
# . 13 week US Treasury Bill Risk Free rate

meli.projected <- as.data.frame(meli.projected.ret.as.price)
amzn.projected <- as.data.frame(amzn.projected.ret.as.price)
ebay.projected <- as.data.frame(ebay.projected.ret.as.price)
baba.projected <- as.data.frame(baba.projected.ret.as.price)
jd.projected <- as.data.frame(jd.projected.ret.as.price)

projected.prices <- cbind(meli.projected, amzn.projected, ebay.projected, baba.projected, jd.projected)
colnames(projected.prices) <- c("MELI", "AMZN", "EBAY", "BABA", "JD")

# calculate simple returns and multiple by 100 to make them as percentages
n <- dim(projected.prices)[1]
returns <- as.matrix(100*(projected.prices[2:n,]/projected.prices[1:(n-1),] - 1))

# input value of risk-free interest rate
# usTbill13weeks are in percentage and annual terms, so must be divided by 252 to make daily percentage
tbill13 <- (Ad(IRX["2018::"]))/252
mufree <- as.numeric(round(mean(na.omit(tbill13)),4))

mean_vect <- apply(returns,2,mean, na.rm = T)

#####################################################
# ***************WITH SHORT SALES********************
#####################################################

eff_Short <- my.eff.frontier(returns,  mufree = mufree, short=-0.2,
                             risk.premium.up=max(mean_vect), risk.increment=.005)

# Find the optimal portfolio
eff.optimal.point_Short <- eff_Short[eff_Short$sharpe==max(eff_Short$sharpe),]

#####################################################
# ***************WITHOUT SHORT SALES*****************
#####################################################

eff_noShort <- my.eff.frontier(returns, mufree = mufree, short=0, max.allocation=0.4,
                               risk.premium.up=max(mean_vect), risk.increment=.005)

# Find the optimal portfolio
eff.optimal.point_noShort <- eff_noShort[eff_noShort$sharpe==max(eff_noShort$sharpe),]

# Print weights
meli.weight.short <- round(eff.optimal.point_Short["MELI"],2)
amzn.weight.short <-round(eff.optimal.point_Short["AMZN"],2)
ebay.weight.short <- round(eff.optimal.point_Short["EBAY"],2)
baba.weight.short <- round(eff.optimal.point_Short["BABA"],2)
jd.weight.short <- round(eff.optimal.point_Short["JD"],2)


meli.weight.no.short <- round(eff.optimal.point_noShort["MELI"],2)
amzn.weight.no.short <-round(eff.optimal.point_noShort["AMZN"],2)
ebay.weight.no.short <- round(eff.optimal.point_noShort["EBAY"],2)
baba.weight.no.short <- round(eff.optimal.point_noShort["BABA"],2)
jd.weight.no.short <- round(eff.optimal.point_noShort["JD"],2)


weights.short <- cbind(meli.weight.short, amzn.weight.short, ebay.weight.short,
                       baba.weight.short, jd.weight.short)
rownames(weights.short) <- "Weights w/ Short Sales Allowed"

weights.no.short <- cbind(meli.weight.no.short, amzn.weight.no.short, ebay.weight.no.short,
                          baba.weight.no.short, jd.weight.no.short)
rownames(weights.no.short) <- "Weights w/ Short Sales Prohibited"

weights.short.no.short <- rbind(weights.short, weights.no.short)

weights.short.no.short

# Find the optimal portfolio again, using "eff.optimal.point" as variable name so that it 
# works with plotEfficientFrontier()
eff.optimal.point <- eff_Short[eff_Short$sharpe==max(eff_Short$sharpe),]
plotEfficientFrontier(eff_Short, header = "Optimal Portfolio w/ Short Sales Allowed")

# Find the optimal portfolio again, using "eff.optimal.point" as variable name so that it 
# works with plotEfficientFrontier()
eff.optimal.point <- eff_noShort[eff_noShort$sharpe==max(eff_noShort$sharpe),]
plotEfficientFrontier(eff_noShort, header = "Optimal Portfolio w/ Short Sales Prohibited")

#### Portfolio and Components' Value-at-Risk ([VaR](http://www.investopedia.com/terms/v/var.asp)) and 
# Expected Shortfall ([ES](https://en.wikipedia.org/wiki/Expected_shortfall))  

#### Portfolio and Components' Value-at-Risk ([VaR](http://www.investopedia.com/terms/v/var.asp)) and Expected Shortfall ([ES](https://en.wikipedia.org/wiki/Expected_shortfall))  

# Key points to keep in mind:   
#         . VaR and ES are approximation measures to the worst that could happen (max losses) on one 
# single day out of twenty (5%).   
# . ES looks at the entire first five percent quantile of the low end of the return distribution tail so 
# its larger than VaR.   
# . With positive correlation, asset prices tend to move together and this increases the volatility.  
# . Without short sales, it is impossible to go above the expected return of the stock with the highest 
# expected return.   
# . When short sales are allowed, there is no upper bound on the expected return nor on the risk. And 
# the projected fall for Alibaba, a company with a market cap above 100B, should not be for the long 
# run. A short sale strategy should be combined with a [stop-loss 
#                 limit order](http://www.investopedia.com/terms/s/stop-limitorder.asp).

library(PerformanceAnalytics)

meli.sim <- as.matrix(meli.fitted.mean)
colnames(meli.sim) <- "MELI"
ebay.sim <- as.matrix(ebay.fitted.mean)
colnames(ebay.sim) <- "EBAY"
amzn.sim <- as.matrix(amzn.fitted.mean)
colnames(amzn.sim) <- "AMZN"
jd.sim <- as.matrix(jd.fitted.mean)
colnames(jd.sim) <- "JD"
baba.sim <- as.matrix(baba.fitted.mean)
colnames(baba.sim) <- "BABA"

mergedSIM <- cbind(meli.sim, ebay.sim, amzn.sim, jd.sim,baba.sim)

mergedSIM <- as.zooreg(mergedSIM)
index(mergedSIM) <- as.yearmon(index(mergedSIM))

cov_matrix <- cov(mergedSIM)

VaR_Short <- VaR(mergedSIM, portfolio_method="component",
                 weights = as.numeric(weights.short),
                 sigma = cov_matrix, mu = mean_vect, method = "modified")

VaR_noShort <- VaR(mergedSIM, portfolio_method="component",
                   weights = as.numeric(weights.no.short),
                   sigma = cov_matrix, mu = mean_vect, method = "modified")

ES_Short <- ETL(mergedSIM, portfolio_method="component",
                weights = as.numeric(weights.short),
                sigma = cov_matrix, mu = mean_vect, method = "modified")

ES_noShort <- ETL(mergedSIM, portfolio_method="component",
                  weights = as.numeric(weights.no.short),
                  sigma = cov_matrix, mu = mean_vect, method = "modified")

#### Build table for VaR and ES results
VaR_Short <- cbind(paste(round(as.numeric(VaR_Short$MVaR)*100,2),"%",sep=""),
                   paste(round(as.numeric(VaR_Short$contribution[1])*100,2),"%",sep=""),
                   paste(round(as.numeric(VaR_Short$contribution[2])*100,2),"%",sep=""),
                   paste(round(as.numeric(VaR_Short$contribution[3])*100,2),"%",sep=""),
                   paste(round(as.numeric(VaR_Short$contribution[4])*100,2),"%",sep=""),
                   paste(round(as.numeric(VaR_Short$contribution[5])*100,2),"%",sep=""))
colnames(VaR_Short) <- c("Total", "MELI", "EBAY", "AMZN", "JD", "BABA")

VaR_noShort <- cbind(paste(round(as.numeric(VaR_noShort$MVaR)*100,2),"%",sep=""),
                     paste(round(as.numeric(VaR_noShort$contribution[1])*100,2),"%",sep=""),
                     paste(round(as.numeric(VaR_noShort$contribution[2])*100,2),"%",sep=""),
                     paste(round(as.numeric(VaR_noShort$contribution[3])*100,2),"%",sep=""),
                     paste(round(as.numeric(VaR_noShort$contribution[4])*100,2),"%",sep=""),
                     paste(round(as.numeric(VaR_noShort$contribution[5])*100,2),"%",sep=""))
colnames(VaR_noShort) <- c("Total", "MELI", "EBAY", "AMZN", "JD", "BABA")

ES_Short <- cbind(paste(round(as.numeric(ES_Short$MES)*100,2),"%",sep=""),
                  paste(round(as.numeric(ES_Short$contribution[1])*100,2),"%",sep=""),
                  paste(round(as.numeric(ES_Short$contribution[2])*100,2),"%",sep=""),
                  paste(round(as.numeric(ES_Short$contribution[3])*100,2),"%",sep=""),
                  paste(round(as.numeric(ES_Short$contribution[4])*100,2),"%",sep=""),
                  paste(round(as.numeric(ES_Short$contribution[5])*100,2),"%",sep=""))
colnames(ES_Short) <- c("Total", "MELI", "EBAY", "AMZN", "JD", "BABA")

ES_noShort <- cbind(paste(round(as.numeric(ES_noShort$MES)*100,2),"%",sep=""),
                    paste(round(as.numeric(ES_noShort$contribution[1])*100,2),"%",sep=""),
                    paste(round(as.numeric(ES_noShort$contribution[2])*100,2),"%",sep=""),
                    paste(round(as.numeric(ES_noShort$contribution[3])*100,2),"%",sep=""),
                    paste(round(as.numeric(ES_noShort$contribution[4])*100,2),"%",sep=""),
                    paste(round(as.numeric(ES_noShort$contribution[5])*100,2),"%",sep=""))
colnames(ES_noShort) <- c("Total", "MELI", "EBAY", "AMZN", "JD", "BABA")

VarEs.table <- rbind(VaR_Short, VaR_noShort,ES_Short, ES_noShort)
rownames(VarEs.table) <- c("VaR(5%) - Short Sales Allowed",
                           "VaR(5%) - Short Sales Prohibited",
                           "ES(5%) - Short Sales Allowed",
                           "ES(5%) - Short Sales Prohibited")

VarEs.table

#****************************************************************************************

# References:
#         R Core Team (2015). R: A language and environment for statistical computing. R
# Foundation for Statistical Computing, Vienna, Austria. <URL https://www.R-project.org/>.
# All Things R. Pull Yahoo Finance Key-Statistics Instantaneously Using XML and XPath in R. <URL: http://allthingsr.blogspot.com.ar/2012/10/pull-yahoo-finance-key-statistics.html>.
# Raymond McTaggart and Gergely Daroczi (2015). Quandl: API Wrapper for Quandl.com. R
# package version 2.6.0. <URL: http://CRAN.R-project.org/package=Quandl>.
# Taiyun Wei (2013). corrplot: Visualization of a correlation matrix. R package version
# 0.73. <URL: http://CRAN.R-project.org/package=corrplot>.
# Jeffrey A. Ryan (2015). quantmod: Quantitative Financial Modelling Framework. R
# package version 0.4-5. <URL: http://CRAN.R-project.org/package=quantmod>.
# Hyndman RJ (2015). _forecast: Forecasting functions for time series and linear models_.
# R package version 6.1, <URL: http://github.com/robjhyndman/forecast>.
# Hyndman RJ and Khandakar Y (2008). "Automatic time series forecasting: the forecast
# package for R." _Journal of Statistical Software_, *26*(3), pp. 1-22. <URL:
#         http://ideas.repec.org/a/jss/jstsof/27i03.html>.
# Alexios Ghalanos (2015). rugarch: Univariate GARCH models. R package version 1.3-6.
# Andrew Matuszak. the Economist at Large. Efficient Frontier and plotEfficientFrontier. <URL:   http://economistatlarge.com/>.
# David Ruppert. Statistics and Data Analysis for Financial Engineering (Springer Science+Business Media, LLC, 233 Spring Street, New York, NY 10013, USA).
# John L. Weatherwax, PhD. A Solution Manual for: Statistics and Data Analysis for Financial Engineering by David Rupert. <URL: http://www.waxworksmath.com>.
# S original by Berwin A. Turlach R port by Andreas Weingessel
# <Andreas.Weingessel@ci.tuwien.ac.at> (2013). quadprog: Functions to solve Quadratic
# Programming Problems.. R package version 1.5-5.
# <URL: http://CRAN.R-project.org/package=quadprog>.
# Brian G. Peterson and Peter Carl (2014). PerformanceAnalytics: Econometric tools for
# performance and risk analysis. R package version 1.4.3541.
# <URL: http://CRAN.R-project.org/package=PerformanceAnalytics>.
# Yihui Xie (2015). knitr: A General-Purpose Package for Dynamic Report Generation in R.
# R package version 1.11.
# Yihui Xie (2015) Dynamic Documents with R and knitr. 2nd edition. Chapman and
# Hall/CRC. ISBN 978-1498716963.
# Yihui Xie (2014) knitr: A Comprehensive Tool for Reproducible Research in R. In
# Victoria Stodden, Friedrich Leisch and Roger D. Peng, editors, Implementing
# Reproducible Computational Research. Chapman and Hall/CRC. ISBN 978-1466561595.
# JJ Allaire, Jeffrey Horner, Vicent Marti and Natacha Porte (2015). markdown:
#         'Markdown' Rendering for R. R package version 0.7.7. <http://CRAN.R-project.org/package=markdown>.
#****************************************************************************************
