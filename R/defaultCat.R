# defaultPlot.R
# written by TF
# Nov 30 2022




#' Default cat oneMeanEstim
#'
#' @param x objet de type oneMeanEstim
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
 print.oneMeanEstim <- function(x, ...){
 print.default(x, digits = max(3L, getOption("digits") - 3L) )
   invisible(x)
   }
