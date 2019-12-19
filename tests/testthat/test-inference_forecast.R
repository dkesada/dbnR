test_that("exact dt_inference works with bn.fit", {
  net <- bnlearn::model2network("[X1][X3][X2|X1][X4|X2:X3]")
  distX1 <- list(coef = c("(Intercept)" = 1), sd = 2)
  distX3 <- list(coef = c("(Intercept)" = 2), sd = sqrt(3))
  distX2 <- list(coef = c("(Intercept)" = -3.5, "X1" = 0.5), sd = 2)
  distX4 <- list(coef = c("(Intercept)" = 1, "X2" = 2, "X3" = -1), sd = sqrt(3))
  cfit <- bnlearn::custom.fit(net, dist = list(X1 = distX1, X3 = distX3,
                                               X2 = distX2, X4 = distX4))
  dt_ev_x4 <- data.table(X1 = c(1,2,5), X2 = c(2,3,3), 
                         X3 = c(3,1,4), X4 = c(2,6,3))
  res_ev_x4 <- predict_dt(cfit, dt_ev_x4, "X4", verbose = F)
  
  expect_equal(res_ev_x4$X4, dt_ev_x4$X4)
  
  dt_ev_x2_x4 <- data.table(nrow = c(1,2,3), X2 = c(-3, -2.5, -1),
                            X4 = c(-8, -5, -5))
  res_ev_x2_x4 <- predict_dt(cfit, dt_ev_x4, c("X2","X4"), verbose = F)
                        
  expect_equal(res_ev_x2_x4, dt_ev_x2_x4)
})

test_that("exact dt_inference works with dbn.fit", {
  net <- bnlearn::model2network(paste0("[X1_t_1][X3_t_1][X2_t_1|X1_t_1]",
                                       "[X4_t_1|X2_t_1:X3_t_1][X1_t_0|X1_t_1]",
                                       "[X2_t_0|X1_t_0:X1_t_1:X2_t_1]",
                                       "[X3_t_0|X3_t_1]",
                                       "[X4_t_0|X3_t_0:X2_t_0:X4_t_1]"))
  distX1_t_1 <- list(coef = c("(Intercept)" = 1), sd = 2)
  distX3_t_1 <- list(coef = c("(Intercept)" = 2), sd = sqrt(3))
  distX2_t_1 <- list(coef = c("(Intercept)" = -3.5, "X1_t_1" = 0.5), sd = 2)
  distX4_t_1 <- list(coef = c("(Intercept)" = 1, "X2_t_1" = 2, 
                              "X3_t_1" = -1), sd = sqrt(3))
  distX1_t_0 <- list(coef = c("(Intercept)" = 3, "X1_t_1" = 0.8), sd = sqrt(3))
  distX3_t_0 <- list(coef = c("(Intercept)" = 1.5, "X3_t_1" = 0.6), sd = sqrt(2))
  distX2_t_0 <- list(coef = c("(Intercept)" = -4, "X1_t_1" = 0.5, 
                              "X1_t_0" = 0.2, "X2_t_1" = 0.5), sd = sqrt(2))
  distX4_t_0 <- list(coef = c("(Intercept)" = 3, "X2_t_0" = 2, "X3_t_0" = -1,
                              "X4_t_1" = 0.6), sd = sqrt(2))
  cfit <- bnlearn::custom.fit(net, dist = list(X1_t_0 = distX1_t_0, X3_t_0 = distX3_t_0,
                                               X2_t_0 = distX2_t_0, X4_t_0 = distX4_t_0,
                                               X1_t_1 = distX1_t_1, X3_t_1 = distX3_t_1,
                                               X2_t_1 = distX2_t_1, X4_t_1 = distX4_t_1))
  class(cfit) <- c("dbn.fit", class(cfit))

  dt_ev_fore <- data.table(X1_t_1 = c(1,2,5), X2_t_1 = c(2,3,3),
                           X3_t_1 = c(3,1,4), X4_t_1 = c(2,5,3),
                           X1_t_0 = c(NA,NA,NA), X2_t_0 = c(NA,NA,NA),
                           X3_t_0 = c(NA,NA,NA), X4_t_0 = c(NA,NA,NA))

  res_ev <- data.table(nrow = c(1,2,3),
                       X1_t_0 = c(3.8, 4.6, 7.0),
                       X3_t_0 = c(3.30, 2.1, 3.9),
                       X2_t_0 = c(-1.74, -0.58, 1.40),
                       X4_t_0 = c(-2.58, 2.74, 3.70))

  res_predict <- dbnR::predict_dt(fit = cfit, dt_ev_fore, 
                                  obj_nodes = c("X1_t_0","X2_t_0",
                                                "X3_t_0","X4_t_0"), verbose = F)
  res_predict[, names(res_predict) := round(.SD, 2)]

  expect_equal(res_ev, res_predict)
})

test_that("exact forecasting works", {
  net <- bnlearn::model2network(paste0("[X1_t_1][X3_t_1][X2_t_1|X1_t_1]",
                                       "[X4_t_1|X2_t_1:X3_t_1][X1_t_0|X1_t_1]",
                                       "[X2_t_0|X1_t_0:X1_t_1:X2_t_1]",
                                       "[X3_t_0|X3_t_1]",
                                       "[X4_t_0|X3_t_0:X2_t_0:X4_t_1]"))
  distX1_t_1 <- list(coef = c("(Intercept)" = 1), sd = 2)
  distX3_t_1 <- list(coef = c("(Intercept)" = 2), sd = sqrt(3))
  distX2_t_1 <- list(coef = c("(Intercept)" = -3.5, "X1_t_1" = 0.5), sd = 2)
  distX4_t_1 <- list(coef = c("(Intercept)" = 1, "X2_t_1" = 2, 
                              "X3_t_1" = -1), sd = sqrt(3))
  distX1_t_0 <- list(coef = c("(Intercept)" = 3, "X1_t_1" = 0.8), sd = sqrt(3))
  distX3_t_0 <- list(coef = c("(Intercept)" = 1.5, "X3_t_1" = 0.6), sd = sqrt(2))
  distX2_t_0 <- list(coef = c("(Intercept)" = -4, "X1_t_1" = 0.5, 
                              "X1_t_0" = 0.2, "X2_t_1" = 0.5), sd = sqrt(2))
  distX4_t_0 <- list(coef = c("(Intercept)" = 3, "X2_t_0" = 2, "X3_t_0" = -1,
                              "X4_t_1" = 0.6), sd = sqrt(2))
  cfit <- bnlearn::custom.fit(net, dist = list(X1_t_0 = distX1_t_0, X3_t_0 = distX3_t_0,
                                               X2_t_0 = distX2_t_0, X4_t_0 = distX4_t_0,
                                               X1_t_1 = distX1_t_1, X3_t_1 = distX3_t_1,
                                               X2_t_1 = distX2_t_1, X4_t_1 = distX4_t_1))
  class(cfit) <- c("dbn.fit", class(cfit))
  
  dt_ev_fore <- data.table(X1_t_1 = 1, X2_t_1 = 2, 
                           X3_t_1 = 3, X4_t_1 = 2,
                           X1_t_0 = NA, X2_t_0 = NA, 
                           X3_t_0 = NA, X4_t_0 = NA)
  
  res_ev <- data.table(X1_t_0 = c(3.8, 6.04, 7.83, 9.27, 10.41),
                       X2_t_0 = c(-1.74, -1.76, -0.29, 1.62, 3.53), 
                       X3_t_0 = c(3.30, 3.48, 3.59, 3.65, 3.69),
                       X4_t_0 = c(-2.58, -5.55, -4.51, -0.11, 6.29),
                       exec = c(1,1,1,1,1))
  
  res_fore <- dbnR::forecast_ts(dt_ev_fore, cfit, size = 2, 
                                obj_vars = c("X1_t_0","X2_t_0","X3_t_0","X4_t_0"),
                                len = 5, print_res = F, plot_res = F)
  res_fore$pred[, names(res_fore$pred) := round(.SD, 2)]
  
  expect_equal(res_ev, res_fore$pred)
})
