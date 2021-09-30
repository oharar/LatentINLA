#' Create lists of prior specifications and initial values for GLLVM.
#'
#' @param Y A data frame or matrix with the response (assuming counts at the moment)
#' @param X A data frame or matrix of covariates (strictly row-level covariates). Can be NULL
#' @param W A data frame or matrix of column-level covariates. Can be NULL
#' @param nLVs The number of latent variables required
#' @param RowEff String indicating what sort of row effect is required. Either none, fixed or random. Defaults to fixed.
#' @param ColEff String indicating what sort of column effect is required. Either none, fixed or random. Defaults to fixed.
#' @param RowEffPriorsd Prior standard deviation for latent variable, defaults to 100
#' @param ColEffPriorsd Prior standard deviation for column scores (the betas for INLA insiders), defaults to 10
#' @param PriorLV Hyperprior for the precision of the latent variable, as a list that INLA will understand (sorry). Defaults to NULL, where the default INLA prior will be used
#' @return A data frame with X, an intercept and added term, if desires, and with an attribute that is the part of the formula needed in the model
#' @examples
#' FormatCovariateData(X=cbind(X1=1:5, X2=2:6), intercept=TRUE, AddTerm = "thing")
#' @export
#'@importFrom stats as.formula terms

MakePriorsInits <- function(Y, X=NULL, W=NULL, nLVs=2,
                            RowEff = "fixed", ColEff = "fixed",
                            RowEffPriorsd=100, ColEffPriorsd=10, PriorLV = NULL) {

  LVs <- MakeLVsFromDataFrame(Y, nLVs = nLVs)
  LatentVectors <- as.data.frame(LVs)
  attr(LatentVectors, "formpart") <- CreateFormulaRHS(LVs=LVs,
                                                      prior.beta=ColEffPriorsd,
                                                      hyperprior.LV=NULL)


  # Create data frames of row & column covariates,
  XisNULL <- is.null(X) # test before we change X
  if(RowEff!="none" | !is.null(W))  X <- MakeCovDataDataframe(X, Y)
  if(ColEff!="none" | !XisNULL) W <- MakeCovDataDataframe(W, t(Y), indname="column")

  X.effs <- FormatCovariateData(X=X, intercept=TRUE, nrows=nrow(Y), intname = "column",
                                random = "row")

  W.effs <- FormatCovariateData(X=W, intercept=FALSE, nrows=ncol(Y), intname = "row",
                                random = NULL)
  # Priors defined here:
  #  "column"
  # See also X.rep

  LV.Terms <- terms(as.formula(paste0("Y ~", attr(LatentVectors, "formpart"))))
  X.Terms <- terms(as.formula(paste0("Y ~", attr(X.effs, "formpart"))))
  W.Terms <- terms(as.formula(paste0("Y ~", attr(W.effs, "formpart"))))
  AllTerms <- unique(c(attr(X.Terms, "term.labels"), attr(X.Terms, "term.labels")))

  FixedTerms <- AllTerms[!grepl("f(", AllTerms, fixed=TRUE)]

# Set priors
  Priors <- list(
    prior.fixed = list(mean = sapply(FixedTerms, assign, value=0,
                                      USE.NAMES=TRUE, simplify = FALSE),
                        prec = sapply(FixedTerms, assign, value=1,
                                      USE.NAMES=TRUE, simplify = FALSE))
    )
  if(RowEff=="fixed")  Priors$prior.fixed$prec["row"] <- ColEffPriorsd^-2
  if(ColEff=="fixed")  Priors$prior.fixed$prec["column"] <- RowEffPriorsd^-2

  if(RowEff=="random") Priors$row.prior <- MakePrecPrior('loggamma', c(0.01, 0.01), init = 4)
  if(ColEff=="random") Priors$col.prior <- MakePrecPrior('loggamma', c(0.01, 0.01), init = 4)

  Priors$LVTerms <- sapply(LVs, function(lv) {
    MakePrecPrior('loggamma', c(0.01, 0.01), init = 4)
  }, USE.NAMES = TRUE, simplify=FALSE)

  Priors$Betas <- sapply(LVs, function(lv) {
    list(param=c(0,1), init=0.1)
  }, USE.NAMES = TRUE, simplify=FALSE)


  Priors
}
