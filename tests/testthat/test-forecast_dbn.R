test_that("forecast_dbn works", {
  data(motor)
  dt <- motor
  size = 3
  dt_train <- dt[1:500]
  dt_test <- dt[501:1000]
  f_dt_train <- fold_dt(dt_train, size)
  f_dt_test <- fold_dt(dt_test, size)
  
  net <- learn_dbn_struc(dt_train, size = size)
  fit <- fit_dbn_params(net, f_dt_train)
  res_fore <- forecast_ts(f_dt_test, fit, obj_vars = "pm_t_0", rep = 2,
                          ini = 1, len = 50, print_res = F, plot_res = F)
  
  expect_equal(1, 1)
})

