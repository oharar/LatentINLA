
test_that("MakePriorsInits works correctly", {
  #  skip_on_cran()
  YMPI <- 1:50
  Y.mat <- matrix(YMPI, ncol=5)
  XMPI <- data.frame(x1=rep(1:2, times=5))
  WMPI <- data.frame(x1=rep(1:5, times=2))

  Priors1 <- MakePriorsInits(Y=Y.mat, X=XMPI, W=WMPI, nLVs=2,
                             RowEff = "fixed", ColEff = "fixed",
                             RowEffPriorsd=100, ColEffPriorsd=10, PriorLV = NULL)
  Priors2 <- MakePriorsInits(Y=Y.mat, X=XMPI, W=WMPI, nLVs=2,
                             RowEff = "random", ColEff = "fixed",
                             RowEffPriorsd=100, ColEffPriorsd=10, PriorLV = NULL)

  # These seem trivial, so I expect more to be added...

    # Test Priors1
  expect_equal(length(Priors1), 3)
  expect_equal(length(Priors1$prior.fixed), 2)
  expect_equal(length(Priors1$LVTerms), 2)
  expect_equal(length(Priors1$Betas), 2)
  expect_equal(Priors1$LVTerms$lv1, Priors1$LVTerms$lv2)

  # Test Priors2 (roweff = random)
  expect_equal(length(Priors2), 4)
  expect_equal(length(Priors2$prior.fixed), 2)
  expect_equal(length(Priors2$row.prior), 3)
  expect_equal(length(Priors2$LVTerms), 2)
  expect_equal(length(Priors2$Betas), 2)
  expect_equal(Priors2$LVTerms$lv1, Priors2$LVTerms$lv2)

})
