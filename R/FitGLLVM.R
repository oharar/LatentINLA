#' Fits a GLLVM to data
#'
#' @param Y A data frame or matrix with the response (assuming counts at the moment)
#' @param X A data frame or matrix of covariates (not used yet)
#' @param nLVs The number of latent variables required
#' @param Family A string indicating the likelihood family. If length 1, it gets repeated with one for each column of the data. For supported distributions see names(inla.models()$likelihood).
#' @param INLAobj Should the full INLA object be included in the output object? Defaults to \code{FALSE}
#' @param ... More arguments to be passed to \code{inla()}
#' @return A list with fixed, colscores, and roweffs, formula, Y, X, family..
#' the posterior summaries for the fixed effects, the column scores and the row
#' effects respectively

#' @examples
#' FitGLLVM(matrix(1:10, ncol=5), nLVs=1, Family="poisson")
#' @export
#'@importFrom stats formula
#'@importFrom INLA inla inla.models
#'@importFrom graphics abline points text


FitGLLVM <- function(Y, X=NULL, nLVs=1, Family="gaussian", INLAobj = FALSE, ...) {
  if(any(!Family%in%names(inla.models()$likelihood))){
    stop(paste(unique(Family)[which(!unique(Family)%in%names(inla.models()$likelihood))],
               "is not a valid INLA family."))
  }
  if(!is.data.frame(Y) & !is.matrix(Y)) stop("Y should be a matrix or data frame")
  if(length(Family)!=1 & length(Family)!=ncol(Y))
    stop("Family should be either a single value or a vector the same length as Y has columns")
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
  if(length(Family)==1) Family <- rep(Family, ncol(Data$Y))
  model <- inla(formula(Formula), data=Data, family = Family, ...)

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
    family = table(Family),
    LL = model$mlik[2],
    Y = Y,
    X = X
   )
  if(INLAobj) res$inla <- model
  class(res) <- "iGLLVM"

  res
}
