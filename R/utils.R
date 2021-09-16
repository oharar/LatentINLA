# Function to make a matrix dense, with NAs instead of zeroes
UnSparseMartix <- function(sparsemat) {
  mat <- matrix(NA, nrow=nrow(sparsemat), ncol=ncol(sparsemat))
  mat.inla <- INLA::inla.as.sparse(sparsemat)
  for(i in 1:length(mat.inla@i)) {
    mat[mat.inla@i[i]+1, mat.inla@j[i]+1] <- mat.inla@x[i]
  }
  mat
}
