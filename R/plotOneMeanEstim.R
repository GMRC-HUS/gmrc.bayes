# plotOneMeanEstim.R
# written by TF
# Nov 30 2022



#' Graphique oneMeanEstim
#'
#' @param oneMeanEstimRes object return from oneMeanEstim
#' @param twit Boolean to display twit
#' @param area Boolean to display twit area
#' @import ggplot2
#' @return
#' @export
#'
#' @examples
plotOneMeanEstim <- function(oneMeanEstimRes, twit=T, area=T){
  if(dim(res$twit)[1]==0){

  warning("Pas de Twit dans l'objet oneMeanEstimRes")
    twit<-F
    area = F
    res$twit$low =res$Posterior$low[1]
    res$twit$up = res$Posterior$up[1]
  }
  xlim<- c(lim_function_low(min(res$twit$low, res$Posterior$low[1])),
           lim_function_up(max(res$twit$up, res$Posterior$up[1])))

  df = data.frame(x = seq(xlim[1],xlim[2],(xlim[2]-xlim[1])/1000))
  df$density=dstudent_t(df$x,
                        df=res$prob["moy_ddl"],
                        mu =res$prob["moy_m"],
                        sigma = res$prob["moy_sd"]
  )
  p1<- ggplot(df, aes(x=x, y=density)) + geom_line()+
    geom_vline(aes(xintercept = res$Posterior$low[1],color=res$Posterior$IC[1]),linetype="dashed" )+
    geom_vline(aes(xintercept = res$Posterior$up[1],color=res$Posterior$IC[1]),linetype="dashed")+
    scale_color_manual(name = "",values=c('#999999'))+theme_light()+theme(legend.position="top")


  if(twit) {
    p1<-p1+ geom_vline(aes(xintercept = res$twit$low[1],color="effet"),linetype="dashed" )+
      geom_vline(aes(xintercept = res$twit$up[1],color="effet"),linetype="dashed" )+
      geom_vline(aes(xintercept = jitter(res$twit$low[2]),color="absence effet"),linetype="dashed" )+
      geom_vline(aes(xintercept = res$twit$up[2],color="absence effet"),linetype="dashed" )+
      scale_color_manual(name="",values=c('#999999',"#40E0D0","#DE3163"))
  }
  if(area){
    p1<- p1+annotate("rect",xmin=res$twit$low[1], xmax=res$twit$up[1], ymin=-Inf, ymax=Inf,fill ="#40E0D0" , alpha = 0.2)+
      annotate("rect",xmin=res$twit$low[2], xmax=res$twit$up[2], ymin=-Inf, ymax=Inf,fill ="#DE3163" , alpha = 0.2)
  }

  return(p1)

}
