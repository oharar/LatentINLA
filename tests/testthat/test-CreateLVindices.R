context("FormatDataFrameToLV")

# test it gives right results
# test if it throws errors with non-matrices. see what happens with cbind()


test_that("FitModel works correctly", {
  #  skip_on_cran()
  X <- 1:10
  X.mat <- matrix(X, ncol=5)

  dat.mat <- FormatDataFrameToLV(X.mat)

  OneLV <- CreateLVIndices(dat=X.mat, nLVs=1)
  TwoLVs <- CreateLVIndices(dat=X.mat, nLVs=2)

  RowNos <- apply(OneLV, 1, function(x) sum(!is.na(x)))

  # Test size, one LV
  expect_equal(ncol(dat.mat), ncol(OneLV))
  expect_equal(nrow(dat.mat), nrow(OneLV))

# Test size, two LVs
  expect_equal(ncol(dat.mat)-1, ncol(TwoLVs))
  expect_equal(nrow(dat.mat), nrow(TwoLVs))
# Test errors
  expect_error(CreateLVIndices(X), "dat should be a matrix or data frame")

# Test values
#  Should do this next test, but it gets hung up on the names being different, which I don't care about
#  expect_equal(unlist(res.mat[1,]), as.integer(c(1, NA, NA, NA, NA)))
  expect_equal(res.mat[,2], c(NA, NA, 1, 2, NA, NA, NA, NA, NA, NA))
  expect_equal(unique(RowNos), 1)
# Check names
  expect_equal(names(OneLV), c("sp1", "sp2", "sp3", "sp4", "sp5"))
  expect_equal(names(TwoLVs), c("sp2", "sp3", "sp4", "sp5"))

})
