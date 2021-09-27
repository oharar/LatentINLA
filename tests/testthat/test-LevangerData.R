
test_that("Levanger Data is present and correct", {
  #  skip_on_cran()
data(Levanger)

  # Test size
  expect_equal(length(Levanger), 2)
  expect_equal(dim(Levanger$species), c(132, 90))
  expect_equal(dim(Levanger$site), c(132, 35))
  expect_equal(names(Levanger$species)[1], "Agrostis.capillaris")
  expect_equal(unlist(Levanger$species[1,1:5], use.names = FALSE), c(60, 30, 25, 10, 20))
  expect_equal(Levanger$species[1:5,1], c(60, 50, 40, 50, 50))

  expect_equal(names(Levanger$site)[1], "plotID")
  expect_equal(Levanger$site[1:5,1], c(3131, 5297, 3410, 3468, 5432))
  expect_equal(unique(Levanger$site$management), c("abandonment", "grazing"))
  })
