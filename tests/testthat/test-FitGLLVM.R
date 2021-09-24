
test_that("FitGLLVM works correctly", {
  #  skip_on_cran()
  Y <- 1:50
  Y.mat <- matrix(Y, ncol=5)
  X <- data.frame(x1=rep(1:2, times=5))

  model.OneLV <- FitGLLVM(Y=Y.mat, X=NULL, nLVs=1)
  model.TwoLVs <- FitGLLVM(Y=Y.mat, X=NULL, nLVs=2,
                           RowEff = "random", INLAobj = TRUE)

# Also test that PriorLV works
  model.X <- FitGLLVM(Y=Y.mat, X=X, nLVs=1,
                      PriorLV = list(prior="pc.prec", param=c(1,0.1)))

  # Test errors
  expect_error(FitGLLVM(Y=Y.mat, X=NULL, nLVs=-1),
               "nLVs should be positive")
  expect_error(FitGLLVM(Y=Y.mat, X=NULL, nLVs=6),
               "Must have fewer LVs than columns: reduce nLVs")
  expect_error(FitGLLVM(Y=1:5, X=NULL, nLVs=1),
               "Y should be a matrix or data frame")
  expect_error(FitGLLVM(Y=Y.mat, X=data.frame(x1=1:27), nLVs=1),
               "X and Y should have same number of rows")
  # This next test takes too long to run.
  # expect_warning(FitGLLVM(Y=matrix(1:30, ncol=15), X=NULL, nLVs=11),
  #              "nLVs should be small: do you really want 16 of them?")

    # Test size
  expect_equal(length(model.OneLV), 12)
  expect_equal(length(model.OneLV$roweffs), 1)
  expect_equal(length(model.TwoLVs), 13) # + 1 for INLAobj
  expect_equal(length(model.TwoLVs$roweffs), 2)
  expect_equal(nrow(model.OneLV$fixed), 15)
  expect_equal(nrow(model.OneLV$colscores), 5)
  expect_equal(nrow(model.OneLV$roweffs[[1]]), 10)
  expect_equal(nrow(model.TwoLVs$roweffs[[1]]), 10)
  expect_equal(nrow(model.TwoLVs$roweffs[[2]]), 10)
  expect_equal(class(model.TwoLVs), "iGLLVM")

  expect_equal(nrow(model.X$fixed), 16)

# Test values
# These are currently not implemented
    # expect_equal(model.OneLV$colscores[1,1], 1)
  # expect_equal(model.TwoLVs$colscores[1,1], 0)
  # expect_equal(model.OneLV$colscores[1,2], 1)

# Check names
  expect_equal(rownames(model.OneLV$colscores)[2], "Beta for lv1.col2")

  # Check row & col effects
  expect_equal(nrow(model.OneLV$rowterm), 10)
  expect_equal(nrow(model.OneLV$colterm), 4)

  # Check row & col effects: here RowEff is random
  expect_equal(nrow(model.TwoLVs$rowterm), 10)
  expect_equal(nrow(model.TwoLVs$colterm), 5)

})
