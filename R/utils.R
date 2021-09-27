# Function to make a matrix dense, with NAs instead of zeroes
UnSparseMartix <- function(sparsemat) {
  mat <- matrix(NA, nrow=nrow(sparsemat), ncol=ncol(sparsemat))
  mat.inla <- INLA::inla.as.sparse(sparsemat)
  for(i in 1:length(mat.inla@i)) {
    mat[mat.inla@i[i]+1, mat.inla@j[i]+1] <- mat.inla@x[i]
  }
  mat
}

# Function to add rows into summary for fixed column scores
# I'm sure there is a more elegant way of doing this...
AddFixedColScores <- function(mod) {
  CS.tmp <- mod$summary.hyperpar[grep("^Beta", rownames(mod$summary.hyperpar)),]

  CS.tmp$LV <- as.integer(gsub("\\..*", "", gsub("Beta for lv", "", rownames(CS.tmp))))
  CS.tmp$Col <- as.integer(gsub("^.*[Cc]ol", "", rownames(CS.tmp)))
  AllLevels <- expand.grid(Col=1:max(CS.tmp$Col), LV=1:max(CS.tmp$LV))

  ColScores <- merge(CS.tmp, AllLevels, all=TRUE)

  SetToOne <- sapply(unique(ColScores$LV), function(lv, df) {
    max(which(df$LV==lv & is.na(df$mean)))
  }, df=ColScores)

  ColScores[SetToOne, c("mean", "mode")] <- 1
  ColScores[is.na(ColScores$mean), c("mean", "mode")] <- 0
  ColScores$sd[is.na(ColScores$sd)] <- 0

  rownames(ColScores) <- paste0("Beta for lv", ColScores$LV, ".col", ColScores$Col)
  ColScores[,c("LV", "Col")] <- NULL

  ColScores
}


# ifelse() that can return a NULL
#   needed here but probably has horrible side effects if not used with care
ifelseNULL <- function(test, yes, no) {
  if(test) yes else no
}

# format X or W so it will play nicely with FormatCovariateData()
#   y provides the dimensions, or row names
MakeCovDataDataframe <- function(x, y, indname="row") {
  if(is.null(rownames(y))) rownames(y) <- 1:nrow(y)
  if(is.null(x)) {
    x <- data.frame(row = factor(rownames(y)))
  } else {
    x <- as.data.frame(x)
    x$row <- factor(rownames(y))
  }
  if(indname!="row") names(x)[names(x)=="row"] <- indname
  x
}

# Function to make IDs, used in CreateLVIndices
MakeIDs <- function(wh, df) {
  out <- c(rep(NA, nrow(df)*(wh-1)),
           1:nrow(df),
           rep(NA, nrow(df)*(ncol(df)-wh)))
  out
}



