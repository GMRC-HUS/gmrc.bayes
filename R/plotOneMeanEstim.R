# plotOneMeanEstim.R
# written by TF
# Nov 30 2022



#' Graphique oneMeanEstim
#'
#' @param oneMeanEstimRes object return from oneMeanEstim
#' @param twoIt Boolean to display twoIt
#' @param area Boolean to display twoIt area
#' @import ggplot2
#' @importFrom ggdist pstudent_t qstudent_t
#' @return
#' @export
#'
#' @examples
plotOneMeanEstim <- function(oneMeanEstimRes, twoIt=T, area=T,xlim=NULL){

  if(is.null(oneMeanEstimRes$twoIt)& twoIt){
    warning("No two It in oneMeanEstimRes")
    twoIt<-F
    area = F
    oneMeanEstimRes$twoIt$low =oneMeanEstimRes$Posterior$low[1]
    oneMeanEstimRes$twoIt$up = oneMeanEstimRes$Posterior$up[1]
  }

  if(!is.null(xlim)& !length(xlim)==2) warning("Length xlim should be 2, defaults values used")

  if(!is.null(xlim)&length(xlim)==2){

    xlim<- c(xlim[1],xlim[2])
  }else if(!twoIt) {
    xlim<- c(lim_function_low(oneMeanEstimRes$Posterior$low[1]),
             lim_function_up( oneMeanEstimRes$Posterior$up[1]))

  } else{
  xlim<- c(lim_function_low(min(oneMeanEstimRes$twoIt$low, oneMeanEstimRes$Posterior$low[1])),
             lim_function_up(max(oneMeanEstimRes$twoIt$up, oneMeanEstimRes$Posterior$up[1])))
  }




  df = data.frame(x = seq(xlim[1],xlim[2],(xlim[2]-xlim[1])/1000))
  df$density=dstudent_t(df$x,
                        df=oneMeanEstimRes$prob["moy_ddl"],
                        mu =oneMeanEstimRes$prob["moy_m"],
                        sigma = oneMeanEstimRes$prob["moy_sd"]
  )
  p1<- ggplot(df, aes(x=x, y=density)) + geom_line()+
    geom_vline(aes(xintercept = oneMeanEstimRes$Posterior$low[1],color=oneMeanEstimRes$Posterior$IC[1]),linetype="dashed" )+
    geom_vline(aes(xintercept = oneMeanEstimRes$Posterior$up[1],color=oneMeanEstimRes$Posterior$IC[1]),linetype="dashed")+
    scale_color_manual(name = "",values=c('#999999'))+theme_light()+theme(legend.position="top")


  if(twoIt) {
    p1<-p1+ geom_vline(aes(xintercept = oneMeanEstimRes$twoIt$low[1],color="effet"),linetype="dashed" )+
      geom_vline(aes(xintercept = oneMeanEstimRes$twoIt$up[1],color="effet"),linetype="dashed" )+
      geom_vline(aes(xintercept = jitter(oneMeanEstimRes$twoIt$low[2]),color="absence effet"),linetype="dashed" )+
      geom_vline(aes(xintercept = oneMeanEstimRes$twoIt$up[2],color="absence effet"),linetype="dashed" )+
      scale_color_manual(name="",values=c('#999999',"#40E0D0","#DE3163"))
  }
  if(area){
    p1<- p1+annotate("rect",xmin=oneMeanEstimRes$twoIt$low[1], xmax=oneMeanEstimRes$twoIt$up[1], ymin=-Inf, ymax=Inf,fill ="#40E0D0" , alpha = 0.2)+
      annotate("rect",xmin=oneMeanEstimRes$twoIt$low[2], xmax=oneMeanEstimRes$twoIt$up[2], ymin=-Inf, ymax=Inf,fill ="#DE3163" , alpha = 0.2)
  }

  return(p1)

}
