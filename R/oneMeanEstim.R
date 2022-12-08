# oneMeanEstim.R
# written by TF
# Nov 30 2022


#'  Estimation d'une moyenne cas conjugé
#' variable continue
#'  ne peut se faire sans l'estimation de la variance
#'  sauf si supposée connue (=cas rare)
#' @param X Variable continue
#' @param alpha Pour le calcul de l'IC
#' @param mu_0
#' @param kappa_0 ?
#' @param alpha_0 Prior sd
#' @param beta_0
#' @param theta_P Bornes inf 2IT
#' @param theta_A Bornes sup 2IT
#' @param seuil
#'  mu|sigma2 ~ dnorm(mu_0, sigma2/kappa_0) kappa_0=taille pseudo-échantillon
#'  tau ~ dgamma(alpha_0/2, beta_0/2)
#'  tau ~ dgamma(alpha_n, beta_n)
#'           alpha_n=(alpha_0+n)/2
#'           beta_n=(beta_0+BSSE)/2
#'           BSSE=(n-1)s² + n kappa_0(mu_0 - y.bar)²/(n+kappa_0)
#'   library(ggdist)
#'   mu ~ student(mu_n, v_n, ddl)
#'           mu_n=mu_0 kappa_0/(kapa_0+n) + y.bar n/(kappa_0+n)
#'           v_n=beta_n/alpha_n/(n+kappa_0)
#'           ddl_n=2 alpha_n
#' Résumé: p50 [p2.5; p97.5] mu et sigma=1/sqrt(tau)
#' Proba de chaque intervalle 2IT
#' Paramètres de la Student a posteriori de la moyenne
#' Graphique mu: prior, posterior, bornes IC, bornes 2IT
#' @return Posteriors marginales
#' @importFrom ggdist pstudent_t qstudent_t
#' @export
#'
#' @examples
#'

#'
#'
#'
oneMeanEstim <- function(X, alpha, mu_0, kappa_0, alpha_0, beta_0, theta_P=NULL, theta_A=NULL,
                   seuil=0){

  #theta sous forme de vecteur
  Y <- X[!is.na(X)]
  #"attention m NA"
  if(sum(is.na(X))>0)  warning(paste0("Attention : ", sum(is.na(X))," NAs"))
  #calculs intermédiaires
  n <- length(Y)
  alpha_0 <- alpha_0*2 #Hoff
  beta_0 <- beta_0*2 #Hoff
  y_bar <- mean(Y)
  var_x <- var(Y)
  #Paramètres pour posterior
  mu_n <- mu_0*kappa_0/(kappa_0+n)+y_bar*n/(kappa_0+n)
  alpha_n <- (alpha_0+n)/2
  BSSE <- (n-1)*var_x + n*kappa_0*(mu_0-y_bar)^2/(n+kappa_0)
  beta_n <- (beta_0+BSSE)/2
  v_n <- beta_n/alpha_n/(n+kappa_0)
  ddl_n <- 2*alpha_n


  #Résumés
  b_L <- alpha/2
  b_U <- 1-alpha/2
  R_sigma <- 1/sqrt(qgamma(c(0.5,b_U,b_L), alpha_n, beta_n))
  R_moy <- qstudent_t(c(0.5,b_L,b_U), df=ddl_n, mu=mu_n, sigma=sqrt(v_n))
  #Proba de supériorité
  Pr_sup=1-pstudent_t(seuil, df=ddl_n, mu=mu_n, sigma=sqrt(v_n))
  #2IT
  #Présence d'effet
  Pr_P <- pstudent_t(theta_P[2], df=ddl_n, mu=mu_n, sigma=sqrt(v_n))-
    pstudent_t(theta_P[1], df=ddl_n, mu=mu_n, sigma=sqrt(v_n))
  #Absence d'effet
  Pr_A <- pstudent_t(theta_A[2], df=ddl_n, mu=mu_n, sigma=sqrt(v_n))-
    pstudent_t(theta_A[1], df=ddl_n, mu=mu_n, sigma=sqrt(v_n))
  #Sortie

  res_pos <-data.frame(param = c("mean","sigma"),
                       mean = c(R_moy[1],R_sigma[1]),
                       low = c(R_moy[2],R_sigma[2]),
                       up = c(R_moy[3],R_sigma[3]),
                       IC=paste0("IC",round(1-alpha,2)*100))
  if(!is.null(theta_P)){
  twit<- data.frame(interval = c("effet","absence effet"),
                    low = c(theta_P[1],theta_A[1]),
                    up = c(theta_P[2],theta_A[2]),
                    Prob=c(Pr_P, Pr_A))
  }else{ twit <-NULL}
  res<- list(Posterior = res_pos,
              twit=twit,
              prob= c(seuil_effet =seuil ,
              Pr_sup=Pr_sup, moy_m=mu_n, moy_sd=sqrt(v_n), moy_ddl=ddl_n))
  class(res)<-"oneMeanEstim"
  return(res)
}
