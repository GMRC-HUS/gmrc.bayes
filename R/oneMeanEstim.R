# oneMeanEstim.R
# written by TF
# Nov 30 2022


#'  Estimation d'une moyenne cas conjugé
#' variable continue
#'  ne peut se faire sans l'estimation de la variance
#'  sauf si supposée connue (=cas rare)
#' @param X A vector of data values.
#' @param mu The prior mean of the population.
#' @param kappa_0 A value related to the prior precision (inverse variance) of mu
#' @param sigma_0 The prior sd of standard deviation of the population.
#' @param nu A value related to the prior precision of sigman_0
#' @param alpha The level of the credibility interval
#' @param theta_P Bornes inf 2IT
#' @param theta_A Bornes sup 2IT
#' @param seuil  A threshold value for the probability calculation.
#'  mu|sigma2 ~ dnorm(mu, sigma2/kappa_0) kappa_0=taille pseudo-échantillon
#'  tau ~ dgamma(alpha_0/2, beta_0/2)
#'  tau ~ dgamma(alpha_n, beta_n)
#'           alpha_n=(alpha_0+n)/2
#'           beta_n=(beta_0+BSSE)/2
#'           BSSE=(n-1)s² + n kappa_0(mu - y.bar)²/(n+kappa_0)
#'   library(ggdist)
#'   mu ~ student(mu_n, v_n, ddl)
#'           mu_n=mu kappa_0/(kapa_0+n) + y.bar n/(kappa_0+n)
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
oneMeanEstim <- function(X,  mu, kappa_0, nu, sigma_0, alpha = 0.05,theta_P=NULL, theta_A=NULL,
                   seuil=0){

  # Remove missing values (NA) from X and store the resulting data in Y
  Y <- X[!is.na(X)]

  # If there were any missing values, print a warning message indicating how many were removed
  if(sum(is.na(X))>0)  warning(paste0("Attention : ", sum(is.na(X))," NAs"))


  n <- length(Y)
  y_bar <- mean(Y)
  var_x <- var(Y)

  # Calculate the posterior mean using the input data and parameters
  mu_n <- mu*kappa_0/(kappa_0+n)+y_bar*n/(kappa_0+n)
  # Calculate the posterior degrees of freedom
  nu_n <- nu+n
  # Calculate the posterior variance
  sigma_n2 <- 1/nu_n*(nu*sigma_0+(n-1)*var_x+n*kappa_0*(mu-y_bar)^2/(n+kappa_0))
  # Calculate the posterior standard deviation
  c_n <- sqrt(sigma_n2/(n+kappa_0))



  b_L <- alpha/2
  b_U <- 1-alpha/2

  # Calculate the lower and upper bounds for a (1-alpha) credibility interval for the standard deviation
  R_sigma <- 1/sqrt(qgamma(c(0.5,b_U,b_L), nu_n/2, nu_n*sigma_n2/2))

  # Calculate the lower and upper bounds for a (1-alpha) credibility interval for the mean
  R_moy <- qstudent_t(c(0.5,b_L,b_U), df=nu_n, mu=mu_n, sigma=c_n)

  # Calculate the probability that the mean exceeds the specified threshold seuil
  Pr_sup=1-pstudent_t(seuil, df=nu_n, mu=mu_n, sigma=c_n)

  Pr_P <- pstudent_t(theta_P[2], df=nu_n, mu=mu_n, sigma=c_n)-
    pstudent_t(theta_P[1], df=nu_n, mu=mu_n, sigma=c_n)

  Pr_A <- pstudent_t(theta_A[2], df=nu_n, mu=mu_n, sigma=c_n)-
    pstudent_t(theta_A[1], df=nu_n, mu=mu_n, sigma=c_n)
  res_prior <- data.frame(param = c("mean","sigma"),
                          mean = c(mu, nu))
  res_pos <-data.frame(param = c("mean","sigma"),
                       median = c(R_moy[1],R_sigma[1]),
                       low = c(R_moy[2],R_sigma[2]),
                       up = c(R_moy[3],R_sigma[3]),
                       IC=paste0("IC",round(1-alpha,2)*100))

  # If theta_P is specified, calculate the probability that the mean falls within the specified range
  if(!is.null(theta_P)){
  twoIt<- data.frame(interval = c("effet","absence effet"),
                    low = c(theta_P[1],theta_A[1]),
                    up = c(theta_P[2],theta_A[2]),
                    Prob=c(Pr_P, Pr_A))
  }else{ twoIt <-NULL}
  res<- list(prior =c(mu=mu, kappa_0=kappa_0, nu=nu, sigma_0=sigma_0),
    Posterior = res_pos,
              twoIt=twoIt,
              prob= c(seuil_effet =seuil ,
              Pr_sup=Pr_sup, moy_m=mu_n, moy_sd=c_n, moy_ddl=nu_n))
  class(res)<-"oneMeanEstim"
  return(res)
}
