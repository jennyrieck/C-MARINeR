plot.statis <- function(fi,w.fis,fi.cols=NULL,w.fis.cols=NULL,axes=c(1,2),display.fi.names=T,display.fis.names=F){
  if(is.null(fi.cols)){
    fi.cols <- "mediumorchid4"
  }
  if(is.null(w.fis.cols)){
    w.fis.cols <- "olivedrab3"
  }
  plot.lims <- apply(apply(w.fis,axes,function(x){max(abs(x))}),2,max) * 1.1
  plot(0,type="n",xlim=c(-plot.lims[axes[1]],plot.lims[axes[1]]),ylim=c(-plot.lims[axes[2]],plot.lims[axes[2]]))
  for(j in 1:dim(w.fis)[3]){
    for(i in 1:nrow(fi)){
      to.plot <- t(w.fis[i, axes , j])
      center.point <- fi[i, axes]
      points(rbind(to.plot, center.point), type = "l", lty = 2, lwd = 1, col = "grey80")
    }
  }
  for(j in 1:dim(w.fis)[3]){
    points(w.fis[,axes,j],col=w.fis.cols[j],pch=20)
    if(display.fis.names){
      text(w.fis[,axes,j],labels=dimnames(w.fis)[[3]][j],offset=.75,pos=3,col=w.fis.cols[j])	
    }
  }
  points(fi[,axes],col=fi.cols,pch=20,cex=2)
  if(display.fi.names){
    text(fi[,axes],labels=rownames(fi),offset=.75,pos=3,col=fi.cols)
  }
}

double.center.cov <- function(cov.mat){
  
  nI <- nrow(cov.mat)
  cent.mat <- matrix(-1/nI,nI,nI)
  diag(cent.mat) <- rep(1 - (1/nI),nI)
  
  return( .5*(cent.mat %*% cov.mat %*% cent.mat) )
  
}

kplus1.covstatis <- function(Sk.array,Sk.target){
  
  dbl.cent.network <- double.center.cov(Sk.target)
  cross.prod.cross.prod <- dbl.cent.X <- array(NA,dim=c(dim(Sk.array)))
  Zmat <- matrix(NA,dim(Sk.array)[1]*dim(Sk.array)[2],dim(Sk.array)[3])
  
    ## could probably benefit from a do.call()
  for(i in 1:dim(Sk.array)[3]){
    
    dbl.cent.X[,,i] <- double.center.cov(Sk.array[,,i])
    cross.prod.cross.prod[,,i] <- crossprod(crossprod(dbl.cent.X[,,i],dbl.cent.network), crossprod(dbl.cent.X[,,i],dbl.cent.network))
    Zmat[,i] <- c(cross.prod.cross.prod[,,i])
  }
  get.alphas <- tolerance_svd(expo.scale(Zmat,scale="SS1"))
  alphas <- get.alphas$v[,1] / sum(get.alphas$v[,1])
  
  compromise.Sk <- matrix(0,nrow(Sk.array),nrow(Sk.array))
  for(i in 1:dim(Sk.array)[3]){
    
    compromise.Sk <- compromise.Sk + (alphas[i] * cross.prod.cross.prod[,,i])
    
  }
  
  kplus1covstats.res <- eigen(compromise.Sk)
  dim.keepers <- which(kplus1covstats.res$values > sqrt(.Machine$double.eps))
  kplus1covstats.res$values <- kplus1covstats.res$values[dim.keepers]
  kplus1covstats.res$vectors <- kplus1covstats.res$vectors[,dim.keepers]
  rownames(kplus1covstats.res$vectors) <- rownames(Sk.target)
  
  fi <- kplus1covstats.res$vectors %*% diag(sqrt(kplus1covstats.res$values))
  ### ok so now figure out projections...
  w.fis <- fis <-  array(NA,dim=c(nrow(kplus1covstats.res$vectors),ncol(kplus1covstats.res$vectors),dim(cross.prod.cross.prod)[3]))
  for(i in 1:dim(cross.prod.cross.prod)[3]){
    
    fis[,,i] <- cross.prod.cross.prod[,,i] %*% kplus1covstats.res$vectors %*% diag(1/sqrt(kplus1covstats.res$values))
    w.fis[,,i] <- fis[,,i] * alphas[i] * dim(fis)[3]
    
  }
  
  return(list(fi=fi,fis=fis,w.fis=w.fis,kplus1covstats.res=kplus1covstats.res,get.alphas=get.alphas,alphas=alphas,compromise.Sk=compromise.Sk))
  
}