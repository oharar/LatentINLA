#' Create the right hand side of a formula with the specification for the latent variables.
#'
#' @param LVs Output from the CreateLVIndices() function
#' @return A formula
#' @examples
#' CreateFormulaRHS(CreateLVIndices(matrix(1:10, ncol=5)))


CreateFormulaRHS <- function(LVs) {
  lvs <- 1:length(LVs)

  Copies <- sapply(names(LVs), function(nm, lvs) {
    LV <- lvs[[nm]]

    form <-   paste0("f(", nm, ".", names(LV)[-1], ", copy=\"", nm,
                     ".L\", hyper = list(beta = list(fixed = FALSE)))",
                     collapse = " + ")

    paste0("f(", nm, ".L, model=\"iid\") + ", form)
#        form
  }, lvs=LVs)
  paste0(unlist(Copies), collapse =" + ")
}
