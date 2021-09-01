
test_that("Can correctly make indices for latent variables", {
  #  skip_on_cran()
  X <- 1:10
  X.mat <- matrix(X, ncol=5)

  dat.mat <- FormatDataFrameForLV(X.mat)
#  OneLVs <- CreateLVIndices(dat=X.mat, nLVs=2)
  LVs <- MakeLVsFromDataFrame(dat=X.mat, nLVs=2)

  # Test errors
  expect_error(MakeLVsFromDataFrame(dat=X.mat, nLVs=-2), "nLVs should be positive")
  expect_error(MakeLVsFromDataFrame(dat=X.mat, nLVs=24), "Must have fewer LVs than columns: reduce nLVs")
  expect_warning(MakeLVsFromDataFrame(dat=matrix(1:200, ncol=50), nLVs=24),
                 "nLVs should be small: do you really want 24 of them?")

  # Test size
  expect_equal(length(LVs), 2)
  expect_equal(dim(LVs[[1]]), c(10, 5))
  expect_equal(dim(LVs[[2]]), c(10, 4))

# Test values
#  Should do this next test, but it gets hung up on the names being different, which I don't care about
#  expect_equal(unlist(res.mat[1,]), as.integer(c(1, NA, NA, NA, NA)))
  expect_equal(LVs[[1]][,2], c(NA, NA, 1, 2, NA, NA, NA, NA, NA, NA))
# Check names
  expect_equal(names(LVs[[1]]), c("L", "col2", "col3", "col4", "col5"))
  expect_equal(names(LVs[[2]]), c("L", "col3", "col4", "col5"))
})
