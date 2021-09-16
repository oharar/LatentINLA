
test_that("FitConstrainedGLLVM works correctly", {
  #  skip_on_cran()
  set.seed(2021)
  NRows <- 200;   nLVs <- 1;  NCol <- 20;  NCovs <- 2
  Intercept <- 2;  CovBeta <- c(-0.2, 0.7)
  ColEffs <- matrix(c(1,rnorm(nLVs*(NCol-1), 0, 0.5)), ncol=NCol)
  X <- matrix(10+1:(NCovs*NRows), nrow=NRows,
              dimnames = list(NULL, paste0("X", 1:NCovs)))

  E.Y <- rowSums(apply(X, 2, scale)%*%CovBeta)%*%ColEffs
  Y.mat <- apply(E.Y, 2, function(e) rpois(length(e), exp(e)))
  colnames(Y.mat) <- paste0("Col", 1:ncol(Y.mat))

  # model.OneLV <- FitGLLVM(Y=Y.mat, X=NULL, nLVs=1)
  # model.TwoLVs <- FitGLLVM(Y=Y.mat, X=NULL, nLVs=2)

  model.X <- FitConstrainedGLLVM(Y=Y.mat, X=X, nLVs=1, Family="poisson")

  # Test errors
  expect_error(FitConstrainedGLLVM(Y=Y.mat, X=X, nLVs=-1),
               "nLVs should be positive")
  expect_error(FitConstrainedGLLVM(Y=Y.mat, X=X, nLVs=26),
               "Must have fewer LVs than columns: reduce nLVs")
  expect_error(FitConstrainedGLLVM(Y=1:5, X=X, nLVs=1),
               "Y should be a matrix or data frame")
  expect_error(FitConstrainedGLLVM(Y=Y.mat, X=data.frame(x1=1:27), nLVs=1),
               "X and Y should have same number of rows")
  # This next test takes too long to run.
  # expect_warning(FitGLLVM(Y=matrix(1:30, ncol=15), X=NULL, nLVs=11),
  #              "nLVs should be small: do you really want 16 of them?")

    # Test size
  expect_equal(length(model.X), 9)
#  expect_equal(length(model.X$roweffs), 1)
  expect_equal(nrow(model.X$fixed), 3)
  expect_equal(nrow(model.X$colscores), 20)
#  expect_equal(nrow(model.X$roweffs[[1]]), 10)

  expect_equal(nrow(model.X$fixed), 3)
  expect_equal(class(model.X), "iGLLVM")

# Test values
# These are currently not implemented
  # expect_equal(model.X$colscores[1,1], 1)
  # expect_equal(model.X$colscores[1,2], 1)

# Check names
  expect_equal(rownames(model.X$colscores)[2], "Beta for lv1.col2")
})
