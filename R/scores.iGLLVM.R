#' @title Extract scores from iGLLVM object
#'
#' @param object an object of class 'iGLLVM'
#' @param which which scores to retrieve: all, sites, or species
#' @param type.posterior.stat which statistic to return of the posterior distributions: mean or mode.
#' @param ...	 extra arguments to be passed along.
#'
#' @author Bert vd Veen
#'
#' @examples
#' \dontrun{
#' model <- FitGLLVM(matrix(1:10, ncol=5), nLVs=1, family="poisson")
#' scores(model)
#'}
#'
#'@aliases scores scores.iGLLVM
#'@method scores iGLLVM
#'@export
#'@export scores.iGLLVM

scores.iGLLVM <- function(object,which="all", type.posterior.stat="mean", ...){
  if(!inherits(object,"iGLLVM")){
    stop("Object needs to be of class iGLLVM.")
  }
  if(!type.posterior.stat%in%c("mean","mode")){
    stop(paste(type.posterior.stat, "not supported. Must be one of {mode,mean}."))
  }
  nLVs <- object$nLVs
  if(which=="all"){
    sites <- matrix(unlist(lapply(object$roweffs,function(x,type.posterior.stat)
      x[type.posterior.stat],type.posterior.stat=type.posterior.stat)),ncol=nLVs)
    species <-  matrix(unlist(object$colscores[type.posterior.stat]),ncol=nLVs)
    colnames(species)<-colnames(sites)<-paste("LV",1:nLVs,sep="")
    return(list(species=species,sites=sites))
  }else if(which=="species"){
    species <-  matrix(unlist(object$colscores[type.posterior.stat]),ncol=nLVs)
    colnames(species)<-paste("LV",1:nLVs,sep="")
    return(species)
  }else if(which=="sites"){
    sites <- matrix(unlist(lapply(object$roweffs,function(x,type.posterior.stat)
      x[type.posterior.stat],type.posterior.stat=type.posterior.stat)),ncol=nLVs)
    colnames(sites)<-paste("LV",1:nLVs,sep="")
    return(sites)
  }

}

#'@export scores
scores <- function(object,which,type.posterior.stat, ...)
{
  UseMethod(generic = "scores")
}
