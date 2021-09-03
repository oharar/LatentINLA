
test_that("Can correctly retrieve all scores", {
  #  skip_on_cran()
  load(file="testmodel.RData")

  # Test errors
  expect_error(scores(model,type="whatever"), "type should be one of mean, mode.")

  # Test size
  expect_equal(dim(scores(model,"species")),c(ncol(model$Y),model$call$nLVs))
  expect_equal(dim(scores(model,"sites")),c(nrow(model$Y),model$call$nLVs))
  expect_equal(length(scores(model)),2)

})
