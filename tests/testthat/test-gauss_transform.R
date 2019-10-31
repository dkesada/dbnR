test_that("transform bn.fit to mu-sigma works", {
  net <- bnlearn::model2network("[X1][X3][X2|X1][X4|X2:X3]")
  distX1 <- list(coef = c("(Intercept)" = 1), sd = 2)
  distX3 <- list(coef = c("(Intercept)" = 2), sd = sqrt(3))
  distX2 <- list(coef = c("(Intercept)" = -3.5, "X1" = 0.5), sd = 2)
  distX4 <- list(coef = c("(Intercept)" = 1, "X2" = 2, "X3" = -1), sd = sqrt(3))
  cfit <- bnlearn::custom.fit(net, dist = list(X1 = distX1, X3 = distX3,
                                               X2 = distX2, X4 = distX4))
  
  named_m <- matrix(c(4,0,2,4,0,3,0,-3,2,0,5,10,4,-3,10,26), nrow = 4, ncol = 4)
  colnames(named_m) <- bnlearn::node.ordering(cfit)
  rownames(named_m) <- bnlearn::node.ordering(cfit)
  
  expect_equal(c("X1" = 1, "X2" = -3, "X3" = 2, "X4" = -7), dbnR::calc_mu(cfit))
  expect_equal(named_m, dbnR::calc_sigma(cfit))
})
