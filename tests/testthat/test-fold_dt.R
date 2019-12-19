test_that("time rename dt works", {
  dt <- data.table(x = c(1,2,3,4), y = c(2,3,4,5), z = c(4,3,2,1))
  dt_expt_rn <- data.table(x_t_0 = c(1,2,3,4), y_t_0 = c(2,3,4,5), z_t_0 = c(4,3,2,1))
  
  expect_equal(time_rename(dt), dt_expt_rn)
})

test_that("regular fold_dt size 2 works", {
  dt <- data.table(x = c(1,2,3,4), y = c(2,3,4,5), z = c(4,3,2,1))
  dt_expt_s2 <- data.table(x_t_0 = c(2,3,4), y_t_0 = c(3,4,5), z_t_0 = c(3,2,1), 
                           x_t_1 = c(1,2,3), y_t_1 = c(2,3,4), z_t_1 = c(4,3,2))
  
  expect_equal(fold_dt(dt, 2), dt_expt_s2)
})

test_that("regular fold_dt size 4 works", {
  dt <- data.table(x = c(1,2,3,4), y = c(2,3,4,5), z = c(4,3,2,1))
  dt_expt_s4 <- data.table(x_t_0 = 4, y_t_0 = 5, z_t_0 = 1, 
                           x_t_1 = 3, y_t_1 = 4, z_t_1 = 2,
                           x_t_2 = 2, y_t_2 = 3, z_t_2 = 3,
                           x_t_3 = 1, y_t_3 = 2, z_t_3 = 4)
  
  expect_equal(fold_dt(dt, 4), dt_expt_s4)
})

