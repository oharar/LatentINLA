#' Fits a GLLVM to data
#'
#' @param Y A data frame or matrix with the response (assuming counts at the moment)
#' @param X A data frame or matrix of covariates (not used yet)
#' @param nLVs The number of latent variables required
#' @param family A string indicating the likelihood family. If length 1, it gets repeated with one for each column of the data.
#' @param INLAobj Should the full INLA object be included in the output object?
#' Defaults to FALSE
#' @param ... More arguments to be passed to inla()
#' @return A list with fixed, colscores, and roweffs:
#' the posterior summaries for the fixed effects, the column scores and the row
#' effects respectively

#' @examples
#' FitGLLVM(matrix(1:10, ncol=5), nLVs=1, family="poisson")
#' @export
#'@importFrom stats formula



FitGLLVM <- function(Y, X=NULL, nLVs=1, family="gaussian", INLAobj = FALSE, ...) {
  if(!is.data.frame(Y) & !is.matrix(Y)) stop("Y should be a matrix or data frame")
  if(length(family)!=1 & length(family)!=ncol(Y)) stop("family should be either a single value or a vector the same length as Y has columns")
  if(!is.null(X)) {
    if(nrow(X)!=nrow(Y)) stop("X and Y should have same number of rows")
    if(!is.data.frame(X) & !is.matrix(X)) stop("Y should be a matrix or data frame")
  }
  if(nLVs<1 ) stop("nLVs should be positive")
  if(nLVs>=ncol(Y)) stop(paste0("Must have fewer LVs than columns: reduce nLVs"))
  if(nLVs>10) warning(paste0("nLVs should be small: do you really want ", nLVs, " of them?"))

  # create LV vectors
  LVs <- MakeLVsFromDataFrame(Y, nLVs = nLVs)
  Formula <- paste0("Y ~ " , CreateFormulaRHS(LVs=LVs))
  Data <- as.data.frame(LVs)
  if(!is.null(X)) {
    X.rep <- do.call("rbind", replicate(ncol(Y), X, simplify = FALSE))
    Data <- cbind(Data, X.rep)
    Formula <- paste0(Formula, " + ", paste0(colnames(X), collapse=" + "))
  }
  Data <- as.list(Data)
  Data$Y <- FormatDataFrameForLV(Y)

  # fit the model
  if(length(family)==1) family <- rep(family, ncol(Data$Y))
  model <- INLA::inla(formula(Formula), data=Data, family = family, ...)

# Need to add missing species
# I'm sure there is a more elegant way of doing this...
  CS.tmp <- model$summary.hyperpar[grep("^Beta", rownames(model$summary.hyperpar)),]

  CS.tmp$LV <- as.integer(gsub("\\..*", "", gsub("Beta for lv", "", rownames(CS.tmp))))
  CS.tmp$Col <- as.integer(gsub("^.*col", "", rownames(CS.tmp)))
  AllLevels <- expand.grid(Col=1:max(CS.tmp$Col), LV=1:max(CS.tmp$LV))

  ColScores <- merge(CS.tmp, AllLevels, all=TRUE)

  SetToOne <- sapply(unique(ColScores$LV), function(lv, df) {
    max(which(df$LV==lv & is.na(df$mean)))
  }, df=ColScores)
  ColScores[SetToOne, c("mean", "mode")] <- 1
  ColScores[is.na(ColScores$mean), c("mean", "mode")] <- 0
  ColScores$sd[is.na(ColScores$sd)] <- 0

  rownames(ColScores) <- paste0("Beta for lv", ColScores$LV, ".col", ColScores$Col)
  ColScores[,c("LV", "Col")] <- NULL

  res <- list(
    fixed = model$summary.fixed,
    colscores = ColScores,
    roweffs = model$summary.random[grep("\\.L$", names(model$summary.random))],
    formula = Formula,
    call = match.call(),
    family = table(family),
    LL = model$mlik[2]
   )
  if(INLAobj) res$inla <- model
  class(res) <- "iGLLVM"

  res
}
