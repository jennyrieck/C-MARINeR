#' TODO: TITLE
#'
#' TODO
#'
#' @export
#'
#' @param fi TODO
#' @param fis TODO
#' @param fi.cols TODO. Default is NULL.
#' @param fis.cols TODO. Default is NULL.
#' @param axes TODO. Default is 1 and 2.
#' @param display.fi.names TODO. Default is TRUE.
#' @param fis.names TODO. Default is FALSE.
#' @param display.fis.names TODO.
#'
#' @return TODO
#'
#' @examples
#' \dontrun{
#' ## hello
#' }

### this is temporary...

plot_covstatis <- function(fi,fis,fi.cols=NULL,fis.cols=NULL,axes=c(1,2),display.fi.names=T,display.fis.names=F){
  if(is.null(fi.cols)){
    fi.cols <- "mediumorchid4"
  }
  if(is.null(fis.cols)){
    fis.cols <- "olivedrab3"
  }
  plot.lims <- apply(apply(fis,axes,function(x){max(abs(x))}),2,max) * 1.1
  plot(0,type="n",xlim=c(-plot.lims[axes[1]],plot.lims[axes[1]]),ylim=c(-plot.lims[axes[2]],plot.lims[axes[2]]), axes=F, asp=1, xlab = paste0("Component ", axes[1]), ylab = paste0("Component ", axes[2]))
  abline(h=0,lty=2,lwd=2, col="grey60")
  abline(v=0,lty=2,lwd=2, col="grey60")

  for(j in 1:dim(fis)[3]){
    for(i in 1:nrow(fi)){
      to.plot <- t(fis[i, axes , j])
      center.point <- fi[i, axes]
      points(rbind(to.plot, center.point), type = "l", lty = 2, lwd = 1, col = "grey80")
    }
    points(fis[,axes,j],col=fis.cols,pch=20)
    if(display.fis.names){
      text(fis[,axes,j],labels=dimnames(fis)[[3]][j],offset=.75,pos=3,col=fis.cols)
    }
  }

  points(fi[,axes],col=fi.cols,pch=20,cex=2)
  if(display.fi.names){
    text(fi[,axes],labels=rownames(fi),offset=.75,pos=3,col=fi.cols)
  }

}
