# create a function that returns our GMMA
GMMA <- function(x) {
  fastMA <- c(3,5,8,10,12,15)
  slowMA <- c(30,35,40,45,50,60)
  x <- sapply(c(fastMA,slowMA), function(xx) EMA(x,xx))
  return(x)}
# create an addGuppy function with newTA
addGuppy <- newTA(FUN=GMMA, preFUN=Cl, col=c(rep(3,6), rep("#333333",6)), legend="GMMA")
class(addGuppy)