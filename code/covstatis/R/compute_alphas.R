compute_alphas <- function(Z_matrix){
  # Zmat <- apply(crossprod.cube,3,c) ## aha, I see the connection back to HA's GetCMat
  # Z_matrix <- do.call(cbind,lapply(cov_matrices, c))
  ## I was lazy and took the SVD route.
  svd_of_Z_matrix <- svd(Z_matrix)
  #return(list(alphas=svdz$v[,1] / sum(svdz$v[,1]),vectors=svdz$v))
  (svd_of_Z_matrix$v[,1] / sum(svd_of_Z_matrix$v[,1]))
}
