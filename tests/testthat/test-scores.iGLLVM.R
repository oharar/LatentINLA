
test_that("Can correctly retrieve all scores", {
  #  skip_on_cran()
#  load(file="../testmodel.RData")
  Y <- 1:50
  Y.mat <- matrix(Y, ncol=5)
  X <- data.frame(x1=rep(1:2, times=5))

  model <- FitGLLVM(Y=Y.mat, X=NULL, nLVs=1)

  # Test errors
  expect_error(scores(model,type="whatever"), 'whatever not supported. Must be one of \\{mode,mean\\}.')

  # Test size
  expect_equal(dim(scores(model,"species")),c(ncol(model$Y),model$call$nLVs))
  expect_equal(dim(scores(model,"sites")),c(nrow(model$Y),model$call$nLVs))
  expect_equal(length(scores(model)),2)

})
