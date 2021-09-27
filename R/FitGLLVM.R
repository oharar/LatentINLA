#' Fits a GLLVM to data
#'
#' @param Y A data frame or matrix with the response (assuming counts at the moment)
#' @param X A data frame or matrix of covariates (strictly row-level covariates). Can be NULL
#' @param W A data frame or matrix of column-level covariates. Can be NULL
#' @param nLVs The number of latent variables required
#' @param Family A string indicating the likelihood family. If length 1, it gets repeated with one for each column of the data. For supported distributions see names(inla.models()$likelihood).
#' @param RowEff String indicating what sort of row effect is required. Either none, fixed or random. Defaults to fixed.
#' @param ColEff String indicating what sort of column effect is required. Either none, fixed or random. Defaults to fixed.
#' @param RowPriorsd Prior standard deviation for latent variable, defaults to 100
#' @param ColPriorsd Prior standard deviation for column scores (the betas for INLA insiders), defaults to 10
#' @param PriorLV Hyperprior for the precision of the latent variable, as a list that INLA will understand (sorry). Defaults to NULL, where the default INLA prior will be used
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
                     RowPriorsd=100, ColPriorsd=10, PriorLV = NULL,
                     INLAobj = FALSE, ...) {
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
  if(!is.null(PriorLV) & !is.list(PriorLV)) stop("PriorLV should be alist or NULL")

  if(!ColEff%in%c("none", "fixed", "random")) stop("ColEff must be either none, fixed or random")

# create LV vectors
  LVs <- MakeLVsFromDataFrame(Y, nLVs = nLVs)
  LatentVectors <- as.data.frame(LVs)
  Hyper <- ifelseNULL(is.null(PriorLV), NULL,deparse(PriorLV))
  attr(LatentVectors, "formpart") <- CreateFormulaRHS(LVs=LVs,
                                                      prior.beta=RowPriorsd,
                                                      hyperprior.LV=Hyper)
# Create data frames of row & column covariates,
#  including intercept and (if wanted) row/column effect effect
# we need to do this first to get the X:column and W:row interactions
  XisNULL <- is.null(X) # test before we change X
  if(RowEff!="none" | !is.null(W))  X <- MakeCovDataDataframe(X, Y)
  if(ColEff!="none" | !XisNULL) W <- MakeCovDataDataframe(W, t(Y), indname="column")

  X.effs <- FormatCovariateData(X=X, intercept=TRUE, nrows=nrow(Y), intname = "column",
                                random = ifelseNULL(RowEff=="random", "row", NULL)
  )
  if(RowEff=="random") {
    # spot the over-kill
    r.prior <- paste0("list(prec=list(prior='normal', param=c(0,",
                      RowPriorsd^-2, ")), initial=1, fixed=FALSE)")
    attr(X.effs, "formpart") <-
    gsub("f(row, model='iid')",
         paste0("f(row, model='iid', hyper = ", r.prior, ")"),
         attr(X.effs, "formpart"))
  }

  X.rep <- do.call("rbind", replicate(ncol(Y), X.effs, simplify = FALSE))
  attr(X.rep, "formpart") <- attr(X.effs, "formpart")
  # Priors defined here:
#  "Intercept + soil.dry + ... + f(row, model='iid') - 1"
# Note: row may be fixed or random

  W.effs <- FormatCovariateData(X=W, intercept=FALSE, nrows=ncol(Y), intname = "row",
#                                AddTerm = ifelseNULL(ColEff=="none", NULL, "column"),
                                random = ifelseNULL(ColEff=="random", "column", NULL)
  )
# replicate the column covariates: with 1 column factors become character
  if(ncol(W.effs)>1) {
    W.rep <- apply(W.effs, 2, function(x, nn) {
      rep(x, each=nn)
#      res
    }, nn=nrow(Y))
  } else {
    W.rep <- data.frame(nm = rep(W.effs[,1], each=nrow(Y)))
    names(W.rep) <- names(W.effs)
  }
  attr(W.rep, "formpart") <- attr(W.effs, "formpart")
# Priors defined here:
#  "column"
# See also X.rep


  # Merge data
  dfNames <- c("LatentVectors", "X.rep", "W.rep")
  IsNULL <- sapply(dfNames, function(x) is.null(eval(str2expression(x))))
  Data <- unlist(do.call(cbind, list(sapply(dfNames[!IsNULL],
                                     function(x) eval(str2expression(x))))),
                 recursive=FALSE)

# Add response
#  Data <- as.list(Data)
  Data$Y <- FormatDataFrameForLV(Y)
  attr(Data, "formpart") <- lapply(dfNames[!IsNULL], function(X)
    attr(eval(str2expression(X)), "formpart"))

# Write formula
  Formula <- formula(paste0("Y~", paste0(unlist(attr(Data, "formpart")), collapse="+")))

    # fit the model
  if(length(Family)==1) Family <- rep(Family, ncol(Data$Y))

  model <- INLA::inla(formula(Formula), data=Data, family = Family, ...)

# Add missing species to colscores
  ColScores <- AddFixedColScores(model)

  # If row or column effects are random, extract their values
  # ifelse() does not seem to like returning a NULL, hence this construction
  RowTerm <- NULL
  ColTerm <- NULL
  if(exists("column", model$summary.random))
    ColTerm <- model$summary.random$column
  if(RowEff=="fixed")
    RowTerm <- model$summary.fixed[grep("^row", rownames(model$summary.fixed)),]
  if(RowEff=="random")
    RowTerm <- model$summary.random$row
  if(ColEff=="fixed")
    ColTerm <- model$summary.fixed[grep("^column", rownames(model$summary.fixed)),]


  res <- list(
    fixed = model$summary.fixed,
    rowterm = RowTerm,
    colterm = ColTerm,
    colscores = ColScores,
    roweffs = model$summary.random[grep("\\.L$", names(model$summary.random))],
    formula = Formula,
    nLVs = nLVs,
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
