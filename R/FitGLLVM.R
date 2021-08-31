#' Fits a GLLVM to data
#'
#' @param Y A data frame or matrix with the response (assuming counts at the moment)
#' @param X A data frame or matrix of covariates (not used yet)
#' @param nLVs The number of latent variables required
#' @return (at the moment) an INLA object
#' @examples
#' FitGLLVM(matrix(1:10, ncol=5), 1)


FitGLLVM <- function(Y, X=NULL, nLVs=1) {
  if(nLVs<1 ) stop("nLVs should be positive")
  if(nLVs>=ncol(Y)) stop(paste0("Must have fewer LVs than columns: reduce nLVs"))
  if(nLVs>10) warning(paste0("nLVs should be small: do you really want ", nLVs, " of them?"))
  if(!is.data.frame(Y) & !is.matrix(Y)) stop("dat should be a matrix or data frame")

  # create LV vectors
  LVs <- MakeLVsFromDataFrame(Y, nLVs = nLVs)
  Formula <- formula(paste0("Y ~ " , CreateFormulaRHS(LVs=LVs)))
  Data <- as.list(as.data.frame(LVs))
  Data$Y <- FormatDataFrameForLV(Y)

  # fit the model
  model = INLA::inla(Formula, data=Data, family = rep("poisson", ncol(Data$Y)))
  model
}
