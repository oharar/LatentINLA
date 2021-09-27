
test_that("Levanger Data is present and correct", {
  #  skip_on_cran()
data(forminifera)

  # Test size
  expect_equal(length(forminifera), 4)
  expect_equal(dim(forminifera$comm), c(31, 24))
  expect_equal(dim(forminifera$traits), c(24, 3))
  expect_equal(dim(forminifera$envir), c(31, 10))
  expect_equal(dim(forminifera$coord), c(31, 2))

  expect_equal(colnames(forminifera$traits)[1], "Form")
  expect_equal(levels(forminifera$traits$Form),
               c("Flat", "Lenticular", "Spindle", "Spines"))
  expect_equal(levels(forminifera$traits$Symbiont),
               c( "Chlorophytes", "Chloroplasts", "Diatoms", "Dinoflagellates", "Rhodophytes"))
  expect_equal(unique(forminifera$traits$`Skeletal structure`), c(0, 1))

  expect_equal(colnames(forminifera$envir)[1], "MaxDepth")
  expect_equal(unlist(forminifera$envir[1,1:5], use.names = FALSE), c(40, 1.60206, 1, 373, 341))
  expect_equal(forminifera$envir[1:5,1], c(40, 45, 40, 48, 28))

  expect_equal(colnames(forminifera$coord), c("X", "Y"))
  expect_equal(unlist(forminifera$coord[1,], use.names = FALSE), c(753350, 9435250))
  expect_equal(forminifera$coord[1:5,1], c(753350, 733150, 751900, 733500, 758100))
  })
