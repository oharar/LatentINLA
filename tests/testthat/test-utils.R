
test_that("UnSparseMartix works correctly", {
  #  skip_on_cran()
  Y <- 1:50
  Y.mat <- matrix(Y, ncol=5)
  Reps <- 5
  SparseX <- do.call(Matrix::bdiag, replicate(Reps, Y.mat, simplify=FALSE))

  UnSparseX <- UnSparseMartix(SparseX)

    # Test size
  expect_equal(nrow(UnSparseX), nrow(Y.mat)*Reps)
  expect_equal(ncol(UnSparseX), ncol(Y.mat)*Reps)

  expect_equal(UnSparseX[11,6], 1)
  expect_equal(UnSparseX[11,5], as.numeric(NA))

})

test_that("AddFixedColScores works correctly", {
  #  skip_on_cran()
  Y.v <- 1:50
  Ncol <- 5
  Y <- matrix(Y.v, ncol=Ncol)
  nLVs <- 1
  Family="gaussian"

  LVs <- MakeLVsFromDataFrame(Y, nLVs = nLVs)
  LatentVectors <- as.data.frame(LVs)
  attr(LatentVectors, "formpart") <- CreateFormulaRHS(LVs=LVs)

  Data <- as.list(LatentVectors)
  Data$Y <- FormatDataFrameForLV(Y)
  attr(Data, "formpart") <- attr(LatentVectors, "formpart")

  Formula <- paste0("Y ~ ", paste0(unlist(attr(Data, "formpart")), collapse=" + "))
  # fit the model
  Family <- rep(Family, ncol(Data$Y))
  model <- INLA::inla(formula(Formula), data=Data, family = Family)

  ColScores <- AddFixedColScores(model)

  # Test size
  expect_equal(nrow(ColScores), Ncol)
  expect_equal(ncol(ColScores), 6)

  # Check names
  expect_equal(rownames(ColScores)[2], "Beta for lv1.col2")
  expect_equal(ColScores[1, 1], 1)
expect_equal(ColScores[1, 2], 0)
expect_equal(ColScores[1, 3], as.numeric(NA))
expect_equal(ColScores[1, 4], as.numeric(NA))
expect_equal(ColScores[1, 5], as.numeric(NA))
expect_equal(ColScores[1, 6], 1)
})

test_that("ifelseNULL works correctly", {
  #  skip_on_cran()
  expect_equal(ifelseNULL(TRUE, TRUE, NULL), TRUE)
  expect_equal(ifelseNULL(TRUE, TRUE, NULL), TRUE)

})

test_that("MakeCovDataDataframe works correctly", {
  #  skip_on_cran()
  Y <- 1:50
  Y.mat <- matrix(Y, ncol=5)
  Y.mat2 <- Y.mat
  rownames(Y.mat2) <- letters[1:nrow(Y.mat2)]
  X <- data.frame(x1=rep(1:2, times=5), x2=rep(1:2, each=5))

  res1 <- MakeCovDataDataframe(X, Y.mat)
  res2 <- MakeCovDataDataframe(X, Y.mat2, indname="thing")
  res3 <- MakeCovDataDataframe(NULL, Y.mat2, indname="thing")

  # Test size
  expect_equal(nrow(res1), nrow(Y.mat))
  expect_equal(ncol(res1), ncol(X)+1)
  expect_equal(colnames(res1), c(colnames(X), "row"))
  expect_equal(all(res1$row%in%1:nrow(Y.mat)), TRUE)

  expect_equal(nrow(res2), nrow(Y.mat))
  expect_equal(ncol(res2), ncol(X)+1)
  expect_equal(colnames(res2), c(colnames(X), "thing"))

  expect_equal(nrow(res3), nrow(Y.mat))
  expect_equal(ncol(res3), 1)
  expect_equal(colnames(res3), "thing")
  expect_equal(all(res3$row%in%1:nrow(Y.mat)), TRUE)

})

test_that("MakeIDs works correctly", {
  #  skip_on_cran()
  X <- data.frame(x1=rep(1:2, times=5), x2=rep(1:2, each=5),
                  x3=rep(11:12, times=5))

  ID1 <- MakeIDs(wh=1, df=X)
  ID2 <- MakeIDs(wh=2, df=X)

  # Test size
  expect_equal(length(ID1), length(c(unlist(X))))
  expect_equal(ID1[1:nrow(X)], seq_along(X$x1))
  expect_equal(ID1[(1+nrow(X)):length(ID1)], as.integer(rep(NA, (ncol(X)-1)*nrow(X))))

  expect_equal(length(ID2), length(c(unlist(X))))
  expect_equal(ID2[1:nrow(X)], as.integer(rep(NA, nrow(X))))
  expect_equal(ID2[(1+nrow(X)):(10+nrow(X))], seq_along(X$x1))
  expect_equal(ID2[(11+nrow(X)):(20+nrow(X))], as.integer(rep(NA, nrow(X))))

  expect_equal(length(ID2), length(c(unlist(X))))
  expect_equal(ID2[1:nrow(X)], as.integer(rep(NA, nrow(X))))
  expect_equal(ID2[(1+nrow(X)):(10+nrow(X))], seq_along(X$x1))
  expect_equal(ID2[(11+nrow(X)):(20+nrow(X))], as.integer(rep(NA, nrow(X))))

})
