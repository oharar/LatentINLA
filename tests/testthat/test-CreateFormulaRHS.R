
test_that("CreateFormulaRHS works correctly", {
  #  skip_on_cran()
  X <- 1:10
  X.mat <- matrix(X, ncol=5)

  dat.mat <- FormatDataFrameForLV(X.mat)
  LVs <- MakeLVsFromDataFrame(dat=X.mat, nLVs=2)
  Formula <- CreateFormulaRHS(LVs)
  FormulaB <- CreateFormulaRHS(LVs, prior.beta = 2)
  FormulaC <- CreateFormulaRHS(LVs, constrained=TRUE)
  FormulaD <- CreateFormulaRHS(LVs, constrained=FALSE, hyper = "list(oops)")

  # Test size
  expect_equal(length(Formula), 1) # If this fails, we're in trouble
  expect_equal(nchar(Formula), 731)
  expect_equal(nchar(FormulaB), 724)
  expect_equal(grepl("param=c(0, 0.25)", FormulaB, fixed=TRUE), TRUE)
  expect_equal(nchar(FormulaC), 858)

  expect_equal(length(FormulaD), 1) # If this fails, we're in trouble
  expect_equal(nchar(FormulaD), 793)
  expect_equal(grepl("oops", FormulaD, fixed=TRUE), TRUE)

})
