

test_that("sparse matrices trigger loading of sparseMatrixStats", {
  library(Matrix)
  # Make a matrix with different features
  mat <- matrix(rnorm(16 * 6), nrow = 16, ncol = 6)
  sp_mat <- as(mat, "dgCMatrix")
  expect_equal(colLogSumExps(sp_mat), colLogSumExps(mat))
})


test_that("sparse matrices trigger loading of sparseMatrixStats", {
  # Make a matrix with different features
  mat <- matrix(rnorm(16 * 6), nrow = 16, ncol = 6)
  sp_mat <- as(mat, "dgCMatrix")
  expect_equal(colVars(sp_mat), colVars(mat))
})



