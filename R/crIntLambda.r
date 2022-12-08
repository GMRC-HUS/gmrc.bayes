# crIntLambda.r
# written by JuG
# December 08 2022


#' Compute the credibility interval for a Poisson model
#' @author JuG
#' @description Compute the credibility interval for a Poisson model
#' @param x a vector of observations
#' @param percentile a vector of length 2 defining the credibility intervalle lower and upper limits
#' @details .
#' @examples
#' x <- rpois(10, lambda = 3)
#' crIntLambda(x)
#' @return a vecteur with the lower and upper limits of the crInt
#' @export


crIntLambda <- function(x, percentile = c(.025,.975)){
  x <- as.integer(x)
  n <- sum(!is.na(x))
  sumX <- sum(x, na.rm=T)
  crInt <- qgamma(p = percentile, shape = (sumX+1), rate = (n+1))
  return(crInt)
}
