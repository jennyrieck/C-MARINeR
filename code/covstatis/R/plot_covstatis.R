
### this is temporary...

plot_covstatis <- function(fi,fis,fi.cols=NULL,fis.cols=NULL,axes=c(1,2),display.fi.names=T,display.fis.names=F){
  if(is.null(fi.cols)){
    fi.cols <- "mediumorchid4"
  }
  if(is.null(fis.cols)){
    fis.cols <- "olivedrab3"
  }
  plot.lims <- apply(apply(fis,axes,function(x){max(abs(x))}),2,max) * 1.1
  plot(0,type="n",xlim=c(-plot.lims[axes[1]],plot.lims[axes[1]]),ylim=c(-plot.lims[axes[2]],plot.lims[axes[2]]))
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