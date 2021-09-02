#'@export

print.iGLLVM <- function(x, ...) {
  cat("Call: \n")
  print(x$call)
  cat("\n")
  cat("family:",paste(names(mod$family)," (",as.numeric(mod$family),")",sep=""))
  cat("\n\n")
  cat("marginal log-likelihood: ", x$LL, "\n")
}


