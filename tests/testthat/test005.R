context("Verify functionality of some hhcartr internal functions.")

m = matrix(c( 1, 2,  9, 1,
              2, 2,  8, 1,
              3, 2,  7, 1,
              4, 2,  6, 1,
              5, 2,  5, 1,
              6, 2,  4, 1,
              7, 2,  3, 1,
              8, 2,  2, 1,
              9, 2,  1, 1,
              10, 2,  0, 1,
              20, 9, 99, 2,
              21, 9, 98, 2,
              22, 9, 97, 2,
              23, 9, 96, 2,
              24, 9, 95, 2,
              25, 9, 94, 2,
              26, 9, 93, 2,
              27, 9, 92, 2,
              28, 9, 91, 2,
              29, 9, 90, 2), nrow=20, ncol=4, byrow = TRUE)

X_matrix = matrix(c( 1, 2,  9,
              2, 2, 8,
              3, 2, 7,
              4, 2, 6,
              5, 2, 5,
              6, 2, 4,
              7, 2, 3,
              8, 2, 2,
              9, 2, 1,
             10, 2, 0), nrow=10, ncol=3, byrow = TRUE)

expected_hh <- matrix(c(-0.707107, 0, 0.707107,
                         0.000000, 1, 0.000000,
                         0.707107, 0, 0.707107), nrow=3, ncol=3, byrow = TRUE)

test_that("Test hhcartr internal function best_split_.", {
  # find best split for matrix m
  res <- testbed(m[,1:3], m[,4], 1, TRUE, 2, 3, "best_split_", NULL, NULL)
  idx       <- res[[1]]
  threshold <- res[[2]]
  gini      <- res[[3]]
  expect_equal(idx, 3) #1
  expect_equal(threshold, 49.5) #15
  expect_equal(gini, 0.25)
})

test_that("Test hhcartr internal function split_using_original_data.", {
  # split_using_original_data should return the same as best_split_ as
  # function split_using_original_data does call best_split_.
  # find best split for matrix m
  res <- testbed(m[,1:3], m[,4], 1, TRUE, 2, 3, "split_using_original_data", NULL, NULL)
  idx                <- res[[1]]
  threshold          <- res[[2]]
  householder_matrix <- res[[4]]
  using_householder  <- res[[5]]
  expect_equal(idx, 3) #1
  expect_equal(threshold, 49.5) #15
  expect_equal(householder_matrix, NULL)
  expect_equal(using_householder, FALSE)
})

test_that("Test hhcartr internal function reflect_feature_space.", {
  # find best split for matrix m using householder transform.
  # return: list(idxA, thrA, gidxA, X_houseA, newH_A)
  res <- testbed(m[,1:3], m[,4], 1, TRUE, 2, 3, "reflect_feature_space", 3, X_matrix)
  idx                <- res[[1]]
  threshold          <- res[[2]]
  best_gini          <- res[[3]]
  X_houseA           <- res[[4]]
  householder_matrix <- res[[5]]
  expect_equal(idx, 3) #1
  expect_equal(threshold, 45.60839, tolerance=4e-6) #24.39518
  expect_equal(best_gini, 0.25)
  expect_equal(householder_matrix, expected_hh, tolerance=1e-6)
})

test_that("Test hhcartr internal function hhcart_reflect_feature_space_g.", {
  # find best split for matrix m using householder transform.
  # returns: list(idx, thr, X_house, householder_matrix, using_householder)
  res <- testbed(m[,1:3], m[,4], 1, TRUE, 2, 3, "hhcart_reflect_feature_space_g", 3, NULL)
  idx                <- res[[1]]
  threshold          <- res[[2]]
  householder_matrix <- res[[4]]
  using_householder  <- res[[5]]
  expect_equal(idx, 3) #1
  expect_equal(threshold, 45.60839, tolerance=4e-6) #24.39518
  expect_equal(householder_matrix, expected_hh[,idx], tolerance=1e-6)
  expect_equal(using_householder, TRUE)
})

test_that("Test hhcartr internal function hhcartr_regressor_find_better_split.", {
  # find best split for matrix m when a regression problem.
  # returns: list(var_idx_, split_, score_)
  res <- testbed(m[,1:3], m[,4], NULL, NULL, NULL, 3, "hhcartr_regressor_find_better_split", NULL, NULL)
  var_idx_           <- as.numeric(res[[1]])
  split_             <- as.numeric(res[[2]])
  score_             <- as.numeric(res[[3]])

  expect_equal(var_idx_, 1)
  expect_equal(split_, 20)
  expect_equal(score_, 0, tolerance=1e-6)
})

