# Change yearmon (Mmm YYY) to NYSE bizday date (YYYY-mm-dd)

change2nysebizday <- function(x) {
  require(RQuantLib);
  index(x) <- adjust("UnitedStates/NYSE",as.Date(format(index(x), "%Y-%m-%d")))
  detach("package:RQuantLib", unload=TRUE)
  return(index(x))
}
