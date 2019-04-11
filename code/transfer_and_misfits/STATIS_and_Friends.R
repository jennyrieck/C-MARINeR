

make.crossprod.array <- function(rect.cube){
	Sks <- array(NA,dim=c(nrow(rect.cube),nrow(rect.cube),dim(rect.cube)[3]))
	for(i in 1:dim(Sks)[3]){
		Sks[,,i] <- tcrossprod(rect.cube[,,i])
	}
	return(Sks)
}

compute.alphas <- function(crossprod.cube){
	Zmat <- apply(crossprod.cube,3,c) ## aha, I see the connection back to HA's GetCMat
	    ## I was lazy and took the SVD route. 
	svdz <- svd(Zmat)
	return(list(alphas=svdz$v[,1] / sum(svdz$v[,1]),vectors=svdz$v))
}

compute.compromise <- function(crossprod.cube,alphas){
	Splus <- matrix(0,nrow(crossprod.cube),ncol(crossprod.cube))
	for(i in 1:dim(crossprod.cube)[3]){
		Splus <- (alphas[i] * crossprod.cube[,,i]) + Splus
	}
	return(Splus)
}

compute.fis <- function(fi,Sks,eigen.res){
	fis <- array(NA,dim=c(nrow(fi),ncol(fi),dim(Sks)[3]))
	for(i in 1:dim(Sks)[3]){
		fis[,,i] <- Sks[,,i] %*% eigen.res$vectors %*% diag(1/sqrt(eigen.res$values))	
	}
	return(fis)
}

compute.weighted.fis <- function(fis,alphas){
	w.fis <- fis
	for(i in 1:dim(fis)[3]){
		w.fis[,,i] <- fis[,,i] * alphas[i] * dim(fis)[3]
	}
	return(w.fis)
}

statis <- function(cube.data,rect=T){

	## around here I can replace some of this with the Rv approach.
	if(rect){
	  ## if I MFA norm, it could be here... but it has to be applied to cube.data before the crossprods come back.
		Sks <- make.crossprod.array(cube.data)
	}
	alpha.res <- compute.alphas(Sks)
	alphas <- alpha.res$alphas
	Splus <- compute.compromise(Sks,alphas)

	eigen.res <- eigen(Splus)
	if(any(is.complex(eigen.res$values))){
		stop("Imaginary eigenvalues.")
	}
	keep <- which(eigen.res$values > sqrt(.Machine$double.eps))
	eigen.res$values <- eigen.res$values[keep]
	eigen.res$vectors <- eigen.res$vectors[,keep]
	fi <- eigen.res$vectors %*% diag(sqrt(eigen.res$values))	
	
	fis <- compute.fis(fi,Sks,eigen.res)
	w.fis <- compute.weighted.fis(fis,alphas)
	
	rownames(fi) <- rownames(fis) <- rownames(w.fis) <- rownames(cube.data)
	dimnames(fis)[[3]] <- dimnames(w.fis)[[3]] <- dimnames(cube.data)[[3]]

	return(list(fi=fi,fis=fis,w.fis=w.fis,alphas=alphas,eigen.res=eigen.res, Splus= Splus,alpha.res=alpha.res))
}

double.center.cov <- function(cov.mat){
  
  nI <- nrow(cov.mat)
  cent.mat <- matrix(-1/nI,nI,nI)
  diag(cent.mat) <- rep(1 - (1/nI),nI)
  
  return( .5*(cent.mat %*% cov.mat %*% cent.mat) )
  
}

## clearly I need a "core" statis
  ## I can also quickly adapt this to DiSTATIS

## ALSO
  ## I need MFA norm option
  ## for later: a companion version that is "Rv-STATIS"
covstatis <- function(cov.cube,norm.covs = F){
  
  # if(norm.covs){
  #   
  # }
  
  #Sks <- apply(cov.cube,3,double.center.cov)
  Sks <- array(NA,dim=c(nrow(cov.cube),nrow(cov.cube),dim(cov.cube)[3]))
  for(i in 1:dim(Sks)[3]){
      ## if I MFA norm, it could be here... but it has to be applied to cov.cube before the crossprods come back.
    Sks[,,i] <- double.center.cov(cov.cube[,,i])
  }
  
    ## is it really this easy?
  alpha.res <- compute.alphas(Sks)
  alphas <- alpha.res$alphas
  Splus <- compute.compromise(Sks,alphas)
  
  eigen.res <- eigen(Splus)
  if(any(is.complex(eigen.res$values))){
    stop("Imaginary eigenvalues.")
  }
  keep <- which(eigen.res$values > sqrt(.Machine$double.eps))
  eigen.res$values <- eigen.res$values[keep]
  eigen.res$vectors <- eigen.res$vectors[,keep]
  fi <- eigen.res$vectors %*% diag(sqrt(eigen.res$values))	
  
  fis <- compute.fis(fi,Sks,eigen.res)
  w.fis <- compute.weighted.fis(fis,alphas)
  
  rownames(fi) <- rownames(fis) <- rownames(w.fis) <- rownames(cov.cube)
  dimnames(fis)[[3]] <- dimnames(w.fis)[[3]] <- dimnames(cov.cube)[[3]]
  
  return(list(fi=fi,fis=fis,w.fis=w.fis,alphas=alphas,eigen.res=eigen.res, Splus= Splus,alpha.res=alpha.res))
  
}


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
		points(w.fis[,axes,j],col=w.fis.cols,pch=20)
		if(display.fis.names){
			text(w.fis[,axes,j],labels=dimnames(w.fis)[[3]][j],offset=.75,pos=3,col=w.fis.cols)	
		}
	}
	points(fi[,axes],col=fi.cols,pch=20,cex=2)
	if(display.fi.names){
		text(fi[,axes],labels=rownames(fi),offset=.75,pos=3,col=fi.cols)
	}
}
