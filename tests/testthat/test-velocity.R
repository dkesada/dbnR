# Also covers Causlist initialization
test_that("velocity initialization works", { 
  ordering <- c("A_t_0", "B_t_0", "C_t_0")
  size <- 3
  
  vl <- Velocity$new(ordering, size)
  res <- list(
    list(
      list(c("A_t_1", "B_t_1", "C_t_1"),
           c(0,0,0)),
      list(c("A_t_1", "B_t_1", "C_t_1"),
           c(0,0,0)),
      list(c("A_t_1", "B_t_1", "C_t_1"),
           c(0,0,0))),
    list(
      list(c("A_t_2", "B_t_2", "C_t_2"),
           c(0,0,0)),
      list(c("A_t_2", "B_t_2", "C_t_2"),
           c(0,0,0)),
      list(c("A_t_2", "B_t_2", "C_t_2"),
           c(0,0,0)))
  )
  
  expect_equal(vl$get_cl(), res)
})

test_that("random velocity generation works", { 
  ordering <- c("A_t_0", "B_t_0", "C_t_0")
  size <- 3
  
  vl <- Velocity$new(ordering, size)
  set.seed(42)
  vl$randomize_velocity(c(15,60,25))
  res <- list(
    list(
      list(c("A_t_1", "B_t_1", "C_t_1"),
           c(-1,-1,0)),
      list(c("A_t_1", "B_t_1", "C_t_1"),
           c(1,1,0)),
      list(c("A_t_1", "B_t_1", "C_t_1"),
           c(1,0,1))),
    list(
      list(c("A_t_2", "B_t_2", "C_t_2"),
           c(1,0,1)),
      list(c("A_t_2", "B_t_2", "C_t_2"),
           c(-1,0,0)),
      list(c("A_t_2", "B_t_2", "C_t_2"),
           c(-1,-1,0)))
  )
  
  expect_equal(vl$get_cl(), res)
  expect_equal(vl$get_abs_op(), 11)
})

test_that("velocity addition works", { 
  ordering <- c("A_t_0", "B_t_0", "C_t_0")
  size <- 3
  
  vl1 <- Velocity$new(ordering, size)
  set.seed(42)
  vl1$randomize_velocity(c(15,60,25))
  
  set.seed(43)
  vl2 <- Velocity$new(ordering, size)
  vl2$randomize_velocity(c(15,60,25))
  
  vl1$add_velocity(vl2)
  
  res <- list(
    list(
      list(c("A_t_1", "B_t_1", "C_t_1"),
           c(-1,-1,0)),
      list(c("A_t_1", "B_t_1", "C_t_1"),
           c(1,1,0)),
      list(c("A_t_1", "B_t_1", "C_t_1"),
           c(1,0,1))),
    list(
      list(c("A_t_2", "B_t_2", "C_t_2"),
           c(0,0,1)),
      list(c("A_t_2", "B_t_2", "C_t_2"),
           c(0,0,0)),
      list(c("A_t_2", "B_t_2", "C_t_2"),
           c(0,-1,0)))
  )
  
  expect_equal(vl1$get_cl(), res)
  expect_equal(vl1$get_abs_op(), 8)
})

test_that("cte times velocity works", { 
  ordering <- c("A_t_0", "B_t_0", "C_t_0")
  size <- 3
  
  vl <- Velocity$new(ordering, size)
  set.seed(42)
  vl$randomize_velocity(c(15,60,25))
  
  vl$cte_times_velocity(1.3)
  
  res <- list(
    list(
      list(c("A_t_1", "B_t_1", "C_t_1"),
           c(-1,-1,0)),
      list(c("A_t_1", "B_t_1", "C_t_1"),
           c(1,1,0)),
      list(c("A_t_1", "B_t_1", "C_t_1"),
           c(1,0,1))),
    list(
      list(c("A_t_2", "B_t_2", "C_t_2"),
           c(1,-1,1)),
      list(c("A_t_2", "B_t_2", "C_t_2"),
           c(-1,1,0)),
      list(c("A_t_2", "B_t_2", "C_t_2"),
           c(-1,-1,1)))
  )
  
  expect_equal(vl$get_cl(), res)
  expect_equal(vl$get_abs_op(), 14)
  
  vl$cte_times_velocity(-5.3)
  
  res <- list(
    list(
      list(c("A_t_1", "B_t_1", "C_t_1"),
           c(1,1,-1)),
      list(c("A_t_1", "B_t_1", "C_t_1"),
           c(-1,-1,1)),
      list(c("A_t_1", "B_t_1", "C_t_1"),
           c(-1,1,-1))),
    list(
      list(c("A_t_2", "B_t_2", "C_t_2"),
           c(-1,1,-1)),
      list(c("A_t_2", "B_t_2", "C_t_2"),
           c(1,-1,1)),
      list(c("A_t_2", "B_t_2", "C_t_2"),
           c(1,1,-1)))
  )
  
  expect_equal(vl$get_cl(), res)
  expect_equal(vl$get_abs_op(), 18)
  
  vl$cte_times_velocity(-0.3)
  
  res <- list(
    list(
      list(c("A_t_1", "B_t_1", "C_t_1"),
           c(0,-1,0)),
      list(c("A_t_1", "B_t_1", "C_t_1"),
           c(0,1,-1)),
      list(c("A_t_1", "B_t_1", "C_t_1"),
           c(0,-1,1))),
    list(
      list(c("A_t_2", "B_t_2", "C_t_2"),
           c(0,0,0)),
      list(c("A_t_2", "B_t_2", "C_t_2"),
           c(0,0,0)),
      list(c("A_t_2", "B_t_2", "C_t_2"),
           c(0,0,0)))
  )
  
  expect_equal(vl$get_cl(), res)
  expect_equal(vl$get_abs_op(), 5)
  
  vl$cte_times_velocity(0.1)
  
  res <- list(
    list(
      list(c("A_t_1", "B_t_1", "C_t_1"),
           c(0,0,0)),
      list(c("A_t_1", "B_t_1", "C_t_1"),
           c(0,0,0)),
      list(c("A_t_1", "B_t_1", "C_t_1"),
           c(0,0,0))),
    list(
      list(c("A_t_2", "B_t_2", "C_t_2"),
           c(0,0,0)),
      list(c("A_t_2", "B_t_2", "C_t_2"),
           c(0,0,0)),
      list(c("A_t_2", "B_t_2", "C_t_2"),
           c(0,0,0)))
  )
  
  expect_equal(vl$get_cl(), res)
  expect_equal(vl$get_abs_op(), 0)
})

