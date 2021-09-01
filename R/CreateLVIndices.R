#' Format a data frame/matrix so that it is in the right format for INLA
#'
#' @param dat data. A data frame or matrix
#' @param nLVs The number of latent variables required
#' @return A data frame with the same number of columns, but each row only has one value in it (the rest are NAs)
#' @examples
#' CreateLVIndices(matrix(1:10, ncol=5), nLVs=1)


CreateLVIndices <- function(dat, nLVs=1) {
  if(!is.data.frame(dat) & !is.matrix(dat)) stop("dat should be a matrix or data frame")
  if(!is.data.frame(dat) & !is.matrix(dat)) stop("dat should be a matrix or data frame")
  MakeIDs <- function(wh, df, nlv) {
    out <- c(rep(NA, nrow(df)*(wh-1)),
             1:nrow(df),
             rep(NA, nrow(df)*(ncol(df)-wh)))
    out
  }
  res <- sapply(nLVs:ncol(dat), MakeIDs, df=dat, nlv=nLVs)
  colnames(res) <- c("L", paste0("col", (1+nLVs):ncol(dat)))
  data.frame(res)
}
