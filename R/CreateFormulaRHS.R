#' Create the right hand side of a formula with the specification for the latent variables.
#'
#' @param LVs Output from the CreateLVIndices() function
#' @param constrained Logical: should the formula be for a constrained model? Defaults to FALSE
#' @param prior.beta Prior standard deviation for betas (i.e. column scores). Defaults to 100
#' @return A formula
#' @details
#' The prior for the betas is currently fixed to be N(0,10^2) (i.e. mean 0, variance 100)
#' @examples
#' CreateFormulaRHS(CreateLVIndices(matrix(1:10, ncol=5)))
#' @export

CreateFormulaRHS <- function(LVs, constrained=FALSE, prior.beta=100) {

  if(constrained) {
    Copies <- sapply(names(LVs), function(nm, lvs) {
      LV <- lvs[[nm]]
      Copies <- c(
        paste0("f(", nm, ".", names(LV)[grep("^L", names(LV))],
               ", w.", nm, ", model='iid', hyper = list(prec = list(initial = -6, fixed=TRUE)))"),
        paste0("f(", nm, ".", names(LV)[!grepl("^L", names(LV))],
               ", copy='", nm, ".", names(LV)[grep("^L", names(LV))],
               "', hyper = list(beta = list(param=c(0,0.01), initial=0, fixed = FALSE)))",
               collapse = " + ")
      )
    }, lvs=LVs)
  } else {
    Copies <- sapply(names(LVs), function(nm, lvs) {
      LV <- lvs[[nm]]
      form <-   paste0("f(", nm, ".", names(LV)[-1], ", copy=\"", nm,
                       ".L\", hyper = list(beta=list(param=c(0, ", prior.beta^-2, "), initial=0.1, fixed = FALSE)))",
                       collapse = " + ")

      paste0("f(", nm, ".L, model=\"iid\") + ", form)
      #        form
    }, lvs=LVs)
  }
  res <- paste0(unlist(Copies), collapse =" + ")
  res
}
