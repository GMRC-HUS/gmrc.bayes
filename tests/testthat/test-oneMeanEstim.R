test_that("has good dimension", {
  a<-oneMeanEstim(rnorm(97,10,15),alpha = 0.15,mu = 0 ,kappa_0 = 1,sigma_0 = 0.005,nu = 2,seuil = 10,theta_P = c(-10,5),theta_A = c(5,12))
  expect_equal(length(a),4)
  expect_equal(names(a),c("prior","Posterior", "twoIt", "prob"))


})
