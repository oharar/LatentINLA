#' Format a data frame/matrix so that it is in the right format for INLA
#'
#' @param x data. A data frame or matrix
#' @return A data frame with the same number of columns, but each row only has one value in it (the rest are NAs)
#' @examples
#' FormatDataFrameToLV(matrix(1:10, ncol=5))





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
