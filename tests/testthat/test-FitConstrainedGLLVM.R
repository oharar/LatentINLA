
test_that("FitConstrainedGLLVM works correctly", {
  #  skip_on_cran()
  set.seed(2021)
  NRows <- 20;   nLVs <- 2;  NCol <- 10;  NCovs <- 2
  Intercept <- 2;  CovBeta <- matrix(c(-0.2, 0.7,0.5,2),ncol=nLVs)
  intercepts <- runif(NCol, -1,1)
  X <- matrix(rnorm(NCovs*NRows),nrow=NRows)
  ColEffs <- matrix(runif(nLVs*NCol, -1, 1), ncol=NCol)
  ColEffs[lower.tri(ColEffs)] <- 0
  diag(ColEffs) <- 1
  e <- matrix(rnorm(NRows*nLVs),ncol=nLVs)
  E.Y <- matrix(intercepts,ncol=NCol,nrow=NRows,byrow=T)+(apply(X, 2, scale)%*%CovBeta + e)%*%ColEffs
  Y.mat <-rpois(NCol*NRows,exp(E.Y))
  Y.mat <- matrix(Y.mat,ncol=NCol,nrow=NRows)
  colnames(Y.mat) <- paste0("Col", 1:ncol(Y.mat))
  colnames(X)<-c("one","two")


  model.X <- FitConstrainedGLLVM(Y=Y.mat, X=X, nLVs=2, Family="poisson",control.inla=list(control.vb=list(emergency=...)))
  model.X1 <- FitConstrainedGLLVM(Y=Y.mat, X=X, nLVs=1, Family="gaussian")#just here because it should be tested as a special case

  # Test errors
  expect_error(FitConstrainedGLLVM(Y=Y.mat, X=X, nLVs=-1),
               "nLVs should be positive")
  expect_error(FitConstrainedGLLVM(Y=Y.mat, X=X, nLVs=26),
               "Must have fewer LVs than columns: reduce nLVs")
  expect_error(FitConstrainedGLLVM(Y=1:5, X=X, nLVs=1),
               "Y should be a matrix or data frame")
  expect_error(FitConstrainedGLLVM(Y=Y.mat, X=data.frame(x1=1:27), nLVs=1),
               "X and Y should have same number of rows")
#   # This next test takes too long to run.
#   # expect_warning(FitGLLVM(Y=matrix(1:30, ncol=15), X=NULL, nLVs=11),
#   #              "nLVs should be small: do you really want 16 of them?")
#
#     # Test size
  expect_equal(length(model.X), 9)
#  expect_equal(length(model.X$roweffs), 1)
  expect_equal(nrow(model.X$fixed), 5)
  expect_equal(nrow(model.X$colscores), 24)
#  expect_equal(nrow(model.X$roweffs[[1]]), 10)

  expect_equal(nrow(model.X$fixed), 5)
  expect_equal(class(model.X), "iGLLVM")

# Test values
# These are currently not implemented
  # expect_equal(model.X$colscores[1,1], 1)
  # expect_equal(model.X$colscores[1,2], 1)

# Check names
  expect_equal(rownames(model.X$colscores)[2], "Beta for lv1.col2")
})
