#' @title Create biplot from iGLLVM object
#'
#'
#' @param object an object of class 'iGLLVM'
#' @param type.posterior.stat which statistic to use from the posterior distributions: mean or mode
#' @param which.lvs vector of length two with indexes of LVs to plot
#' @param alpha scaling factor in biplot, defaults to 0.5 for equal species and site scaling
#' @param site.labels should sites be plotted as labels or as symbols? Defaults to \code{FALSE} so symbols are plotted
#' @param site.col color of plotted site scores
#' @param site.cex size of site labels or symbols
#' @param site.pch index for site symbols
#' @param spp.col color for plotted species scores
#' @param spp.cex size of plotted species scores
#' @param ...	 other arguments passed to \link{plot}
#'
#' @author Bert vd Veen
#'
#' @examples
#' \dontrun{
#' model <- FitGLLVM(matrix(1:10, ncol=5), nLVs=1, family="poisson")
#' biplot(model)
#'}
#'@export
#'@export biplot

biplot.iGLLVM <- function(object,type.posterior.stat="mean",which.lvs = c(1,2), alpha = 0.5, site.labels = F, site.col = "black", site.cex = 1, site.pch = 1, spp.col="blue", spp.cex = 1, ...){
  #scaling code used from https://github.com/JenniNiku/gllvm/blob/master/R/ordiplot.gllvm.R
  if(class(object)!="iGLLVM"){
    stop("Object needs to be of class iGLLVM.")
  }
  if(!type.posterior.stat%in%c("mean","mode")){
    stop(paste(type.posterior.stat, "not supported. Must be one of {mode,mean}."))
  }
  if(length(which.lvs)!=2){
    stop("which.lvs must be of length 2.")
  }
  nLVs <- object$call$nLVs
  lvs <- scores(object, which="sites", type.posterior.stat=type.posterior.stat)
  species <- scores(object,which="species",type.posterior.stat=type.posterior.stat)

  rot <- svd(lvs)$v

  scl <- vector("numeric",ncol(lvs))
  for(i in 1:ncol(lvs)){
    scl[i] <- sqrt(sum(lvs[,i]^2)) * sqrt(sum(lvs[,i]^2))
  }


  lvs <- (t(t(lvs) / sqrt(colSums(lvs^2)) * (scl^alpha))%*%rot)[,which.lvs,drop=F]
  species <- ((species / sqrt(sum(species^2)) * (scl^(1-alpha))) %*%rot)[,which.lvs,drop=F]
  plot(rbind(species,lvs),type="n",xlab=paste("LV",which.lvs[1]),ylab=paste("LV",which.lvs[2]),...)

  if(!site.labels){

    points(lvs,col=site.col, cex=site.cex, pch = site.pch)
  }else{
    text(lvs,col=site.col, cex=site.cex)
  }

  text(species, col=spp.col,cex=spp.cex)
  abline(v=0,h=0,lty="dashed",col="red")
}



biplot <- function(object, ...)
{
  UseMethod(generic = "biplot")
}
