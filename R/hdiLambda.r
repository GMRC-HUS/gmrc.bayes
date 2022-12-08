# hdiLambda.r
# written by JuG
# December 08 2022


#' Compute the HDI for a poisson model
#' @author JuG
#' @description Compute the HDI for a poisson model
#' @param x a vector of observations
#' @param credMass credibility mass (from 0 to 1)
#' @param tol tolerance
#' @details
#' @examples
#' x <- rpois(10, lambda = 3)
#' hdiLambda(x)
#' hdiLambda(x = x, credMass = .7)
#' @return
#' @export


hdiLambda <- function(x, credMass = 0.95, tol = 1e-08,...){
  x <- as.integer(x)
  n <- sum(!is.na(x))
  sumX <- sum(x, na.rm=T)
  return(utilitR::HDIofICDF( qgamma , shape = (sumX+1), rate = (n+1),
                             credMass = credMass, tol=tol, ...))
}
