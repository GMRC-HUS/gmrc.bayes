test_that("has good dimension", {
  a<-oneMeanEstim(rnorm(97,10,15),alpha = 0.15,mu_0 = 0 ,kappa_0 = 1,alpha_0 = 0.005,beta_0 = 0.01,seuil = 10,theta_P = c(-10,5),theta_A = c(5,12))
  expect_equal(length(a),3)
  expect_equal(names(a),c("Posterior", "twit", "prob"))


})
