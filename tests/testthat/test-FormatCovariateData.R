
test_that("FormatCovariateData works correctly", {
  #  skip_on_cran()
  X <- cbind(X1=1:5, X2=2:6)
  X.for <- FormatCovariateData(X=X, intercept=TRUE, AddTerm = "thing")
  X.for.r <- FormatCovariateData(X=X, intercept=TRUE, AddTerm = "thing",
                                 random="thing")
  X.add <- FormatCovariateData(X=X, intercept=FALSE, AddTerm = letters[1:5])

  # Test size
  expect_equal(nrow(X.for), nrow(X))
  expect_equal(ncol(X.for), 2+ncol(X))

  expect_equal(nrow(X.for), nrow(X))
  expect_equal(all(X.for$Intercept==1), TRUE)
  expect_equal(all(X.for$thing==1:5), TRUE)
  expect_equal(all(X.for$X2==X[,"X2"]), TRUE)
  expect_equal(attr(X.for, "formpart"), "Intercept + X1 + X2 + thing - 1")

  expect_equal(all(X.for.r$thing==1:5), TRUE)
  expect_equal(all(X.for.r$X2==X[,"X2"]), TRUE)
  expect_equal(attr(X.for.r, "formpart"), "Intercept + X1 + X2 + f(thing, model='iid') - 1")

  expect_equal(all(X.add$addedterm==letters[1:5]), TRUE)
  expect_equal(all(X.add$X2==X[,"X2"]), TRUE)
  expect_equal(attr(X.add, "formpart"), "X1 + X2 + AddTerm")
})
