#' Format a data frame/matrix so that it is in the right format for INLA
#'
#' @param dat A data frame or matrix
#' @return A data frame with the same number of columns, but each row only has one value in it (the rest are NAs)
#' @examples
#' FormatDataFrameForLV(matrix(1:10, ncol=5))
#' @export


FormatDataFrameForLV <- function(dat) {
  if(!is.data.frame(dat) & !is.matrix(dat)) stop("dat should be a matrix or data frame")
  res <- sapply(seq_len(ncol(dat)), function(wh, df) {
    out <- c(rep(NA, nrow(df)*(wh-1)),
             df[,wh],
             rep(NA, nrow(df)*(ncol(df)-wh)))
    out
  }, df=dat)

  res <- as.data.frame(res)
  names(res) <- colnames(dat)
  res
}

