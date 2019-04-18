double_center_sspsd <- function(sspsd_mat, is_squared_distance=FALSE){
  
  nI <- nrow(sspsd_mat)
  centering_mat <- matrix(-1/nI, nI, nI)
  diag(centering_mat) <- rep(1 - (1/nI), nI)
  
  if(is_squared_distance){
    return( (-1/2)*(centering_mat %*% sspsd_mat %*% centering_mat) )
  }else{
    return( (1/2)*(centering_mat %*% sspsd_mat %*% centering_mat) )
  }
  
}