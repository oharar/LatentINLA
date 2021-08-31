
test_that("CreateFormulaRHS works correctly", {
  #  skip_on_cran()
  X <- 1:10
  X.mat <- matrix(X, ncol=5)

  dat.mat <- FormatDataFrameForLV(X.mat)
  LVs <- MakeLVsFromDataFrame(dat=X.mat, nLVs=2)
  Formula <- CreateFormulaRHS(LVs)

  # Test size
  expect_equal(length(Formula), 1) # If this fails, we're in trouble
  expect_equal(nchar(Formula), 528)

# Should test more!
})
