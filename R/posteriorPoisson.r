# posteriorPoisson.r
# written by JuG
# December 08 2022


#' Generate simulated poisson posterior data
#' @author JuG
#' @description
#' @param x a vector of observations
#' @param  npoints number of points to draw the posterior
#' @details
#' @examples
#' x <- rpois(10, lambda = 3)
#' hist(posteriorPoisson(x))
#' @return
#' @export


posteriorPoisson <- function(x, npoints = 1e5){
  x <- as.integer(x)
  n <- sum(!is.na(x))
  sumX <- sum(x, na.rm=T)
  return(rgamma(n = npoints, shape = (sumX+1), rate = (n+1)))
}
