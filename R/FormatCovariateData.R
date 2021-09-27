#' Create the right hand side of a formula with the specification for the latent variables.
#'
#' @param X Covariate data
#' @param intercept Logical, should an explicit intercept be added?
#' @param AddTerm Term to be added, either a name or a character vector of same length as X. If a single name, the values will be 1:nrow(result)
#' @param intname Name of interaction covariate (e.g. if X is supplied, X:column). Defaults to NULL
#' @param nrows Number of rows in the result. This gets superceded by anything else (i.e. X or AddTerm) defining the number of rows. Defaults to NULL.
#' @param random Vector of names of variables to be made random effects. NULL (the default) if none.
#' @return A data frame with X, an intercept and added term, if desires, and with an attribute that is the part of the formula needed in the model
#' @examples
#' FormatCovariateData(X=cbind(X1=1:5, X2=2:6), intercept=TRUE, AddTerm = "thing")
#' @export

FormatCovariateData <- function(X=NULL, intercept=FALSE, AddTerm = NULL,
                                intname = NULL,
                                nrows=NULL, random=NULL) {

  # Create name for AddTerm
  if(length(AddTerm)==1) {
    addname <- AddTerm
  } else {
    addname <- "AddTerm"
  }

  if(is.null(X) & !intercept & is.null(AddTerm)) stop("You don't seem to want anything")
  if(!is.null(AddTerm)) {
#    if(length(AddTerm)>1 & length(AddTerm)!=nrow(X)) stop("Only one AddTerm allowed")
    if(length(AddTerm) == 1 & !is.character(AddTerm)) stop("AddTerm must be a character")
    if(length(AddTerm) == 1 & is.null(X) & is.null(nrows)) warning("The output will only have one row.")
  }
# check on random to see if names are valid
  if(!is.null(random)) {
    check <- any(sapply(random, function(str, all) !str%in%all,
              all=c(colnames(X), "Intercept", addname)))
    if(check) stop("terms in random should be names used in model")
  }
  NRows <- ifelse(!is.null(X), nrow(X),
                  ifelse(length(AddTerm)>1, length(AddTerm), nrows))

  Intercept <- NULL
  if(intercept) Intercept <- rep(1, NRows)

  if(!is.null(AddTerm)) {
    if(length(AddTerm)==1) {
      addedterm <- 1:NRows
    } else {
      addedterm <- AddTerm
    }
  } else {
    addedterm <- NULL
  }

  res <- as.data.frame(X)
  if(intercept) res$Intercept <- Intercept
  if(!is.null(addedterm) & nrow(res)>0) res[,addname] <- factor(addedterm)
  if(nrow(res)==0) {
    res <- data.frame( addedterm=factor(addedterm))
  }
  if(length(AddTerm)==1) colnames(res)[colnames(res)=="addedterm"] <- AddTerm

# Crate interaction terms, if needed
  NoIntNames <- c("row", "Intercept", "column",
                  ifelseNULL(length(AddTerm)==1, AddTerm, NULL))
  NoInteraction <- names(res)%in%NoIntNames
  if(!is.null(intname) & !all(NoInteraction)) {
    IntForm <- paste0("(", paste0(names(res)[!NoInteraction], collapse=" + "),
                      ")*", intname)
    ToFormula <- c(names(res)[NoInteraction], IntForm)
  } else {
    ToFormula <- names(res)
  }

  attr(res, "formpart") <- paste0(ToFormula, collapse=" + ")
  if(intercept)  attr(res, "formpart") <- paste0(attr(res, "formpart"), " - 1")

  if(!is.null(random)) {
    for(i in seq_along(random)) {
      attr(res, "formpart") <-
        gsub(paste0(random[i]),
             paste0("f(", random[i], ", model='iid')"),
             attr(res, "formpart"), fixed=TRUE)

    }
  }

  res
}
