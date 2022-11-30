# lim_function.R
# written by TF
# Nov 30 2022

#' Calcul de limite inf pour les graphes
#'
#' @param x valeur de la limite basse
#' @param offset offset
#'
#' @return
#' @export
#'
#' @examples
lim_function_low<- function(x,offset=0.3){
  (x- offset*abs(x))
}



#' Calcul de limite sup pour les graphes
#'
#' @param x valeur de la limite haute
#' @param offset offset
#'
#' @return
#' @export
#'
#' @examples
lim_function_up<- function(x,offset=0.3){
  (x+ offset*abs(x))
  }
