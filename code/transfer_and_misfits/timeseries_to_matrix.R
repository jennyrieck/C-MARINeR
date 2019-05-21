## this function is a slight update to/rewrite of ANTsR::timeseries2matrix
## it accomodates NAs in the means and also directly takes in labels.
timeseries_to_matrix <- function (img, mask, roi_labels=NULL) 
{
  m = as.array(mask)
  
  if(any(is.na(m))){
    stop("Mask contains NAs. Mask image must be binary or integer labels")
  }
  
  labs <- sort(unique(m[m > 0.001]))
  if (!all(labs == round(labs))) 
    stop("Mask image must be binary or integer labels")
  if (length(labs) == 1) {
    logmask <- (m == 1)
  }
  else {
    logmask <- (m > 0)
  }
  
  mat <- img[logmask]
  dim(mat) <- c(sum(logmask), dim(img)[length(dim(img))])
  mat <- t(mat)
  if (length(labs) == 1) 
    return(mat)
  maskvec <- m[logmask]
  mmat <- matrix(rowMeans(mat[, maskvec == labs[1], drop = FALSE], na.rm = TRUE), ncol = 1)
  for (i in 2:length(labs)) {
    newmat <- matrix(rowMeans(mat[, maskvec == labs[i], drop = FALSE], na.rm = TRUE), ncol = 1)
    mmat <- cbind(mmat, newmat)
  }
  
  
  if(is.null(roi_labels) | length(roi_labels)!=ncol(mmat)){
    colnames(mmat) <- paste("L", labs)
  }else{
    colnames(mmat) <- roi_labels
  }
  
  return(mmat)
}
