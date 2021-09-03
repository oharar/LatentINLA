#'@export

print.iGLLVM <- function(x, ...) {
  cat("Call: \n")
  print(x$call)
  cat("\n")
  cat("family:",paste(names(x$family)," (",as.numeric(x$family),")",sep=""))
  cat("\n\n")
  cat("marginal log-likelihood: ", x$LL, "\n")
}


