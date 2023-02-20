
test_that("FormatCovariateData works correctly", {
  #  skip_on_cran()
  Xfcd <- cbind(X1=1:5, X2=2:6)
  X.for <- FormatCovariateData(X=Xfcd, intercept=TRUE, AddTerm = "thing",
                               intname = "column")
  X.for.r <- FormatCovariateData(X=Xfcd, intercept=TRUE, AddTerm = "thing",
                                 random="thing")
  X.add <- FormatCovariateData(X=Xfcd, intercept=FALSE, AddTerm = letters[1:5])

  # Test size
  expect_equal(nrow(X.for), nrow(Xfcd))
  expect_equal(ncol(X.for), 2+ncol(Xfcd))

  expect_equal(nrow(X.for), nrow(Xfcd))
  expect_equal(all(X.for$Intercept==1), TRUE)
  expect_equal(all(X.for$thing==1:5), TRUE)
  expect_equal(all(X.for$X2==Xfcd[,"X2"]), TRUE)
  expect_equal(attr(X.for, "formpart"), ". ~ . + Intercept + thing + (X1 + X2)*column - 1")

  expect_equal(all(X.for.r$thing==1:5), TRUE)
  expect_equal(all(X.for.r$X2==Xfcd[,"X2"]), TRUE)
  expect_equal(attr(X.for.r, "formpart"), ". ~ . + X1 + X2 + Intercept + f(thing, model='iid') - 1")

  expect_equal(all(X.add$addedterm==letters[1:5]), TRUE)
  expect_equal(all(X.add$X2==Xfcd[,"X2"]), TRUE)
  expect_equal(attr(X.add, "formpart"), ". ~ . + X1 + X2 + AddTerm")
})
