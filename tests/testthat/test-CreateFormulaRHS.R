
test_that("CreateFormulaRHS works correctly", {
  #  skip_on_cran()
  X <- 1:10
  X.mat <- matrix(X, ncol=5)

  dat.mat <- FormatDataFrameForLV(X.mat)
  LVs <- MakeLVsFromDataFrame(dat=X.mat, nLVs=2)
  Formula <- CreateFormulaRHS(LVs)
  FormulaC <- CreateFormulaRHS(LVs, constrained=TRUE)

  # Test size
  expect_equal(length(Formula), 1) # If this fails, we're in trouble
  expect_equal(nchar(Formula), 535)
  expect_equal(nchar(FormulaC), 655)
# Should test more!
})
