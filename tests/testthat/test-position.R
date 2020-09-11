test_that("position translation works", {
  net <- bnlearn::model2network("[A_t_2][B_t_2][C_t_2][A_t_1][B_t_1][C_t_1][A_t_0|A_t_1:B_t_2:C_t_1][B_t_0|A_t_1:B_t_1][C_t_0|B_t_2:C_t_2]")
  class(net) <- c("dbn", class(net))
  size <- 3

  ps <- Position$new(net, size)
  res <- list(
    list(
      list(c("A_t_1", "B_t_1", "C_t_1"),
           c(1,0,1)),
      list(c("A_t_1", "B_t_1", "C_t_1"),
           c(1,1,0)),
      list(c("A_t_1", "B_t_1", "C_t_1"),
           c(0,0,0))),
    list(
      list(c("A_t_2", "B_t_2", "C_t_2"),
           c(0,1,0)),
      list(c("A_t_2", "B_t_2", "C_t_2"),
           c(0,0,0)),
      list(c("A_t_2", "B_t_2", "C_t_2"),
           c(0,1,1)))
  )
  expect_equal(ps$get_cl(), res)
})

test_that("random network generation works", {
  ordering <- c("A", "B", "C")
  size <- 3

  set.seed(51)
  ps <- Position$new(NULL, size, ordering)

  res <- list(
    list(
      list(c("A_t_1", "B_t_1", "C_t_1"),
           c(1,0,0)),
      list(c("A_t_1", "B_t_1", "C_t_1"),
           c(0,1,0)),
      list(c("A_t_1", "B_t_1", "C_t_1"),
           c(0,0,0))),
    list(
      list(c("A_t_2", "B_t_2", "C_t_2"),
           c(0,1,0)),
      list(c("A_t_2", "B_t_2", "C_t_2"),
           c(0,0,1)),
      list(c("A_t_2", "B_t_2", "C_t_2"),
           c(0,1,1)))
  )
  expect_equal(ps$get_cl(), res)
})

test_that("translation from Position to bn works", {
  net <- bnlearn::model2network("[A_t_2][B_t_2][C_t_2][A_t_1][B_t_1][C_t_1][A_t_0|A_t_1:B_t_2:C_t_1][B_t_0|A_t_1:B_t_1][C_t_0|B_t_2:C_t_2]")
  class(net) <- c("dbn", class(net))
  size <- 3

  ps <- Position$new(net, size)
  res_net <- ps$bn_translate()

  res <- apply(net$arcs, 1, function(x){paste0(x[1], x[2])})
  res_net <- apply(res_net$arcs, 1, function(x){paste0(x[1], x[2])})


  expect_setequal(res_net, res)
})

test_that("position plus velocity works", {
  net <- bnlearn::model2network("[A_t_2][B_t_2][C_t_2][A_t_1][B_t_1][C_t_1][A_t_0|A_t_1:B_t_2:C_t_1][B_t_0|A_t_1:B_t_1][C_t_0|B_t_2:C_t_2]")
  class(net) <- c("dbn", class(net))
  ordering <- c("A_t_0", "B_t_0", "C_t_0")
  size <- 3

  ps <- Position$new(net, size)
  vl <- Velocity$new(ordering, size)
  set.seed(42)
  vl$randomize_velocity(c(15,60,25))
  ps$add_velocity(vl)
  
  res <- list(
    list(
      list(c("A_t_1", "B_t_1", "C_t_1"),
           c(0,0,1)),
      list(c("A_t_1", "B_t_1", "C_t_1"),
           c(1,1,0)),
      list(c("A_t_1", "B_t_1", "C_t_1"),
           c(1,0,1))),
    list(
      list(c("A_t_2", "B_t_2", "C_t_2"),
           c(1,1,1)),
      list(c("A_t_2", "B_t_2", "C_t_2"),
           c(0,0,0)),
      list(c("A_t_2", "B_t_2", "C_t_2"),
           c(0,0,1)))
  )
  expect_equal(ps$get_cl(), res)
  expect_equal(ps$get_n_arcs(), 9)
})

test_that("position minus position works", {
  net <- bnlearn::model2network("[A_t_2][B_t_2][C_t_2][A_t_1][B_t_1][C_t_1][A_t_0|A_t_1:B_t_2:C_t_1][B_t_0|A_t_1:B_t_1][C_t_0|B_t_2:C_t_2]")
  class(net) <- c("dbn", class(net))
  ordering <- c("A_t_0", "B_t_0", "C_t_0")
  size <- 3
  ps1 <- Position$new(net, size)
  
  ordering <- c("A", "B", "C")
  set.seed(51)
  ps2 <- Position$new(NULL, size, ordering)
  
  vl <- ps1$subtract_position(ps2)
  
  res <- list(
    list(
      list(c("A_t_1", "B_t_1", "C_t_1"),
           c(0,0,1)),
      list(c("A_t_1", "B_t_1", "C_t_1"),
           c(1,0,0)),
      list(c("A_t_1", "B_t_1", "C_t_1"),
           c(0,0,0))),
    list(
      list(c("A_t_2", "B_t_2", "C_t_2"),
           c(0,0,0)),
      list(c("A_t_2", "B_t_2", "C_t_2"),
           c(0,0,-1)),
      list(c("A_t_2", "B_t_2", "C_t_2"),
           c(0,0,0)))
  )
  
  expect_equal(vl$get_cl(), res)
  expect_equal(vl$get_abs_op(), 3)
})
