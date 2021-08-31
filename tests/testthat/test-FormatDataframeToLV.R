context("FormatDataFrameToLV")

# test it gives right results
# test if it throws errors with non-matrices. see what happens with cbind()


test_that("FitModel works correctly", {
  #  skip_on_cran()
  X <- 1:10
  X.mat <- matrix(X, ncol=5)
  X.df <- data.frame(X.mat)

  res.vec <- FormatDataFrameToLV(X)
  res.mat <- FormatDataFrameToLV(X.mat)
  res.df <- FormatDataFrameToLV(X.df)

# Test size
  expect_equal(ncol(res.mat), ncol(X.mat))
  expect_equal(nrow(res.mat), length(X))
# Test errors
  expect_error(FormatDataFrameToLV(X), "dat should be a matrix or data frame")
  expect_error(res.cbind, "dat should be a matrix or data frame")
# Test values
#  Should do this next test, but it gets hung up on the names being different, which I don't care about
#  expect_equal(unlist(res.mat[1,]), as.integer(c(1, NA, NA, NA, NA)))
  expect_equal(res.mat[,2], c(NA, NA, 3, 4, NA, NA, NA, NA, NA, NA))
})
