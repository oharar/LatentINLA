#' Fits a GLLVM to data
#'
#' @param Y A data frame or matrix with the response (assuming counts at the moment)
#' @param X A data frame or matrix of covariates (strictly row-level covariates). Can be NULL
#' @param W A data frame or matrix of column-level covariates. Can be NULL
#' @param nLVs The number of latent variables required
#' @param Family A string indicating the likelihood family. If length 1, it gets repeated with one for each column of the data. For supported distributions see names(inla.models()$likelihood).
#' @param RowEff String indicating what sort of row effect is required. Either none, fixed or random. Defaults to fixed.
#' @param ColEff String indicating what sort of column effect is required. Either none, fixed or random. Defaults to fixed.
#' @param INLAobj Should the full INLA object be included in the output object? Defaults to \code{FALSE}
#' @param ... More arguments to be passed to \code{inla()}
#' @return A list with fixed, rowterm, colterm, colscores, and roweffs, formula, Y, X, family..
#' the posterior summaries for the fixed effects, row main effects, the column main effect, the column scores and the row
#' effects respectively. rowterm and colterm may be NULL if they were  not set to be random.

#' @examples
#' FitGLLVM(matrix(1:10, ncol=5), nLVs=1, Family="poisson")
#' @export
#'@importFrom stats formula
#'@importFrom graphics abline points text

FitGLLVM <- function(Y, X=NULL, W=NULL, nLVs=1, Family="gaussian",
                     RowEff = "fixed", ColEff = "fixed",
                     INLAobj = FALSE,...) {
  if(any(!Family%in%names(INLA::inla.models()$likelihood))){
    stop(paste(unique(Family)[which(!unique(Family)%in%names(INLA::inla.models()$likelihood))],
               "is not a valid INLA family."))
  }
  if(!is.data.frame(Y) & !is.matrix(Y)) stop("Y should be a matrix or data frame")
  if(length(Family)!=1 & length(Family)!=ncol(Y))
    stop("Family should be either a single value or a vector the same length as Y has columns")
  if(!is.null(X)) {
    if(nrow(X)!=nrow(Y)) stop("X and Y should have same number of rows")
    if(!is.data.frame(X) & !is.matrix(X)) stop("X should be a matrix or data frame")
  }
  if(!is.null(W)) {
    if(nrow(W)!=ncol(Y)) stop("W should have as many rows as Y has columns")
    if(!is.data.frame(W) & !is.matrix(W)) stop("W should be a matrix or data frame")
  }
  if(nLVs<1 ) stop("nLVs should be positive")
  if(nLVs>=ncol(Y)) stop(paste0("Must have fewer LVs than columns: reduce nLVs"))
  if(nLVs>10) warning(paste0("nLVs should be small: do you really want ", nLVs, " of them?"))

  if(!ColEff%in%c("none", "fixed", "random")) stop("ColEff must be either none, fixed or random")


  # create LV vectors
  LVs <- MakeLVsFromDataFrame(Y, nLVs = nLVs)
  LatentVectors <- as.data.frame(LVs)
  attr(LatentVectors, "formpart") <- CreateFormulaRHS(LVs=LVs)

# Create data frame of row covariates,
#  including intercept and (if wanted) row effect
  X.effs <- FormatCovariateData(X=X, intercept=TRUE, nrows=nrow(Y),
                                AddTerm = ifelseNULL(RowEff=="none", NULL, "row"),
                                random = ifelseNULL(RowEff=="random", "row", NULL)
  )

  X.rep <- do.call("rbind", replicate(ncol(Y), X.effs, simplify = FALSE))
  attr(X.rep, "formpart") <- attr(X.effs, "formpart")

  # Create data frame of column covariates,
  #  including (if wanted) column effect, but no intercept
  W.effs <- FormatCovariateData(X=W, intercept=FALSE, nrows=ncol(Y),
                                AddTerm = ifelseNULL(ColEff=="none", NULL, "column"),
                                random = ifelseNULL(ColEff=="random", "column", NULL)
  )

# replicate the column covariates
  W.rep <- apply(W.effs, 2, function(x, nn) rep(x, each=nn),
                 nn=nrow(Y))
  attr(W.rep, "formpart") <- attr(W.effs, "formpart")

  Data <- cbind(LatentVectors, X.rep, W.rep)
  Data <- as.list(Data)
  Data$Y <- FormatDataFrameForLV(Y)
  attr(Data, "formpart") <- lapply(list(LatentVectors, X.rep, W.rep), function(X)
    attr(X, "formpart"))

  Formula <- paste0("Y ~ ", paste0(unlist(attr(Data, "formpart")), collapse=" + "))
  # fit the model
  if(length(Family)==1) Family <- rep(Family, ncol(Data$Y))
  model <- INLA::inla(formula(Formula), data=Data, family = Family, ...)

# Need to add missing species
  ColScores <- AddFixedColScores(model)

  # If row or column effects are random, extract their values
  # ifelse() does not seem to like returning a NULL, hence this construction
  RowTerm <- NULL
  ColTerm <- NULL
  if(exists("row", model$summary.random))
    RowTerm <- model$summary.random$row
  if(exists("column", model$summary.random))
    ColTerm <- model$summary.random$column
  if(RowEff=="fixed")
    RowTerm <- model$summary.fixed[grep("^row", rownames(model$summary.fixed)),]
  if(ColEff=="fixed")
    ColTerm <- model$summary.fixed[grep("^column", rownames(model$summary.fixed)),]


  res <- list(
    fixed = model$summary.fixed,
    rowterm = RowTerm,
    colterm = ColTerm,
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
