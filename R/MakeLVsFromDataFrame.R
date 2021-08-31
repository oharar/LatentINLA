#' Make a list of indices for latent variables so that it is in the right format for INLA
#'
#' @param dat A data frame or matrix
#' @param nLVs Number of latent varaibles required
#' @return list of data frames with indices for teh latent variables.
#' @examples
#' MakeLVsFromDataFrame(matrix(1:10, ncol=5), nLVs=1)

MakeLVsFromDataFrame <- function(dat, nLVs) {
  if(nLVs<1 ) stop("nLVs should be positive")
  if(nLVs>=ncol(dat)) stop(paste0("Must have fewer LVs than columns: reduce nLVs"))
  if(nLVs>10) warning(paste0("nLVs should be small: do you really want ", nLVs, " of them?"))
  if(!is.data.frame(dat) & !is.matrix(dat)) stop("dat should be a matrix or data frame")

  LVs <- lapply(1:nLVs, function(lv, dat) CreateLVIndices(dat=dat, nLVs=lv), dat=dat)
  names(LVs) <- paste0("lv", 1:nLVs)
  LVs
}
