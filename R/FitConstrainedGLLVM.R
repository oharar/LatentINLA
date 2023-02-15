#' Fits a GLLVM to data
#'
#' @param Y A data frame or matrix with the response (assuming counts at the moment)
#' @param X A data frame or matrix of covariates (not used yet)
#' @param formula for predictors in the ordination
#' @param nLVs The number of latent variables required
#' @param Family A string indicating the likelihood family. If length 1, it gets repeated with one for each column of the data.
#' @param ColScorePriorsd Prior standard deviation for column scores (the betas for INLA insiders), defaults to 10
#' @param INLAobj Should the full INLA object be included in the output object?
#' Defaults to FALSE
#' @param ... More arguments to be passed to inla()
#' @return A list with fixed, colscores, and roweffs:
#' the posterior summaries for the fixed effects, the column scores and the row
#' effects respectively

#' @examples
#' set.seed(2021)
#' NRows <- 200
#' nLVs <- 1
#' NCol <- 20
#' NCovs <- 2
#' Intercept <- 2
#' CovBeta <- matrix(c(-0.2, 0.7, 0.7, -0.2), nrow=2)
#' ColEffs <- matrix(c(1,rnorm(nLVs*(NCol-1), 0, 0.5)), ncol=NCol)
#' X <- matrix(10+1:(NCovs*NRows), nrow=NRows,
#'             dimnames = list(NULL, paste0("X", 1:NCovs)))
#' LV.true <- rowSums(apply(X, 2, scale)%*%CovBeta)
#' E.Y <- Intercept+LV.true%*%ColEffs

#' TrueFixed <- c(Intercept, CovBeta)
#' Y.mat <- apply(E.Y, 2, function(e) rpois(length(e), exp(e)))
#' colnames(Y.mat) <- paste0("Col", 1:ncol(Y.mat))
#' \dontrun{
#'   Try1 <- FitConstrainedGLLVM(Y=Y.mat, X=X, nLVs=1, Family="poisson", INLAobj = FALSE)
#'   Try2 <- FitConstrainedGLLVM(Y=Y.mat, X=X, nLVs=2, Family="poisson",
#'                               INLAobj = FALSE, verbose=TRUE)
#' }
#' @export

FitConstrainedGLLVM <- function(Y, X, formula = NULL, nLVs=1, Family="gaussian",
                                ColScorePriorsd=10, INLAobj = FALSE, ...) {
  if(!is.null(formula)&!inherits(formula,"formula"))stop("'formula' must be a formula-type object")
  if(any(!Family%in%names(INLA::inla.models()$likelihood))){
    stop(paste(unique(Family)[which(!unique(Family)%in%names(INLA::inla.models()$likelihood))],
               "is not a valid INLA family."))
  }
  if(!is.data.frame(Y) & !is.matrix(Y)) stop("Y should be a matrix or data frame")
  if(length(Family)!=1 & length(Family)!=ncol(Y))
    stop("Family should be either a single value or a vector the same length as Y has columns")
  if(nrow(X)!=nrow(Y)) stop("X and Y should have same number of rows")
  if(!is.data.frame(X) & !is.matrix(X)) stop("Y should be a matrix or data frame")
  if(nLVs<1 ) stop("nLVs should be positive")
  if(nLVs>=ncol(Y)) stop(paste0("Must have fewer LVs than columns: reduce nLVs"))
  if(nLVs>10) warning(paste0("nLVs should be small: do you really want ", nLVs, " of them?"))
  if(nLVs>1) warning("This might not work yet: INLA may crash.")

  #if non-numeric columns turn into a design matrix
  if(any(apply(X,2,typeof)%in%c("character","factor"))){
    if(is.null(formula)){
      X <- model.matrix(~., X)
      }else{
        X <- model.matrix(formula, X)
        if(colnames(X)%in%c("(Intercept)")){
          X <- X[,-which(colnames(X)=="(Intercept)")]
        }
      }
    # check for special symbols in column names that could mess with INLA
    # not the most elegant solution currently, should still improve to pick the ones out that error
    if(length(grep("[[:punct:]]",x))>0){
      warning("Special characters will be removed from column names.\n")
      colnames(X) <- gsub("[[:punct:]]", "", colnames(X))
    }
    
  }
  ########################
  # Format Y, including LVs

  # Format is
  # (  dat.y  LV.NAs  )
  # (  y.NAs dat.LVs  )
  dat.y <- FormatDataFrameForLV(Y)
  # Create 0s first, then format
  LV.Zeroes <- matrix(0, ncol=nLVs, nrow=nrow(Y), dimnames = list(NULL, paste0("L", 1:nLVs)))
  dat.LVs <- FormatDataFrameForLV(LV.Zeroes)

  # make NA rows for data
  y.NAs <- matrix(NA, nrow=nrow(dat.LVs), ncol=ncol(dat.y),
                  dimnames=list(NULL, colnames(dat.y)))
  names(y.NAs) <- names(dat.y)

  # Make Latent Variable data frames
  #  i.e. indices to connect LVs to data

  # make NA rows for LVs
  LV.NAs <- matrix(NA, nrow=nrow(dat.y), ncol=ncol(dat.LVs),
                   dimnames=list(NULL, colnames(dat.LVs)))
  names(LV.NAs) <- names(dat.LVs)

  dat <- cbind(rbind(dat.y, y.NAs),
               rbind(LV.NAs, dat.LVs))
  ############################
  # create LV vectors for data
  dataLVs <- cbind(Y, L=matrix(0, ncol=nLVs, nrow = nrow(Y),
                               dimnames = list(NULL, paste0("L", 1:nLVs))))

  LVs <- MakeLVsFromDataFrame(dat=dataLVs, nLVs=nLVs)
  LatentVectors <- as.data.frame(LVs)

  # lapply(LVs, function(df) apply(df, 2, function(x) which(!is.na(x))[1]))

  ###################
  # Format covariates

  # Repeat X for each latent variable
  # eps is the unexplained variation in each LV
  CovariateNames <- colnames(X)
  X.eps <- as.matrix(cbind(X, eps=1:nrow(X)))
  XToCov <- do.call(Matrix::bdiag, replicate(nLVs, X.eps, simplify=FALSE))

  DatToCov <- list(XToCov,
                   matrix(NA, ncol=ncol(XToCov), nrow=nrow(dat)-2*nrow(XToCov)),
                   XToCov)
  Cov.sparse <- do.call(rbind, DatToCov) # This is a dgCMatrix object

  # Make non-sparse Cov.dat
  Cov.dat <- UnSparseMartix(Cov.sparse)
  colnames(Cov.dat) <- apply(expand.grid(colnames(X.eps), names(LVs)), 1, paste0, collapse=".")

  # set predictor effects to a lower triangular matrix
  Cov.dat <- Cov.dat[,-unlist(sapply(2:nLVs,function(q,p)p*(q-1)+seq(q-1,1)+(q-1),simplify=F, p = ncol(X)))]#+(q-1) for "eps" at end of each predictor sequence

  ################
  # Create weights
  w <- apply(as.matrix(Cov.dat[,grep("eps", colnames(Cov.dat))]), 2,
             function(v) ifelse(v==0, NA, -1))
  colnames(w) <- paste0("w.", names(LVs))


  # Add Column effects
  # Cov.dat <- cbind(Cov.dat, rep(c(1:ncol(Y), NA), each=nrow(Y)))
  # colnames(Cov.dat) <- c("epsilon", "X1", "X2", "Column")

  # Merge all of the data together
  # Covariates, latent variable indices, weights, responses
  # if(nLVs==1) {
  #   Data <- cbind(data.frame(Cov.dat),
  #                 data.frame(LatentVectors[[1]]),
  #                 w)
  #   colnames(Data)[grep("Col", colnames(Data))] <- paste0("lv1.", colnames(Data)[grep("Col", colnames(Data))])
  #   colnames(Data)[grep("L1.", colnames(Data))] <- paste0("lv1.", colnames(Data)[grep("L1", colnames(Data))])
  # } else {
    Data <- cbind(Cov.dat,
                  data.frame(LatentVectors),
                  w)
  # }
  Data$Y <- dat

  #########################
  # Write formula for model
  CovEff <- colnames(Cov.dat)
  Formula <- formula(
    paste0("Y ~ " ,
           paste(colnames(Cov.dat)[!grepl("eps.", colnames(Cov.dat))], collapse  = " + "),
           " + ",
           paste("f(", colnames(Cov.dat)[grepl("eps.", colnames(Cov.dat))], ", model='iid')", collapse  = " + "),
           " + ",
           CreateFormulaRHS(LVs=LVs, constrained = TRUE, prior.beta = ColScorePriorsd))
  )

  # fit the model
  if(length(Family)==1) Family.Y <- rep(Family, ncol(Y))
  Fam <- c(Family.Y, rep("gaussian", nLVs))

  model <- INLA::inla(Formula, data=Data, family = Fam, ...)
  #  model <- INLA::inla(Formula, data=Data, family = Fam)

  # Need to add missing species to output
  ColScores <- AddFixedColScores(model)

  # Need to add missing predictor effects to output
  FixedEffs <- AddFixedPredEffs(model)

  res <- list(
    fixed = FixedEffs,
    colscores = ColScores,
    roweffs = model$summary.random[grep("\\.L", names(model$summary.random))],
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
