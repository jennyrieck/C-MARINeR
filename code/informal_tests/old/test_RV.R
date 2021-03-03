
require(ExPosition)
require(devtools)
require(magrittr)

data("jocn.2005.fmri")
# load_all("../covstatis/")
load_all(".")

cov_matrices <- list(images=-as.matrix(jocn.2005.fmri$images$data),
                     scans=-as.matrix(jocn.2005.fmri$scans$data))

## from DistatisR
GetCmat <- function(CubeCP, RV = TRUE) {
  nI = dim(CubeCP)[1]
  nJ = dim(CubeCP)[3]
  CP2 = array(CubeCP, dim = c(nI * nI, nJ))
  C = t(CP2) %*% CP2
  if (RV) {
    laNorm = sqrt(apply(CP2^2, 2, sum))
    C = C/(t(t(laNorm)) %*% laNorm)
  }
  rownames(C) <- colnames(C) <- dimnames(CubeCP)[[3]]
  return(C)
}


cov_matrices %>%
  double_center_tables() %>%
  normalize_tables(., table_norm_type = "MFA") ->
  cov_matrices

d_Rv.matrix <- GetCmat(simplify2array(cov_matrices), RV = T)
d_Rveigen <- eigen(d_Rv.matrix, symmetric = TRUE)

d_not_Rv.matrix <- GetCmat(simplify2array(cov_matrices), RV = F)
d_not_Rveigen <- eigen(d_not_Rv.matrix, symmetric = TRUE)

Rv.matrix <- matrix(0, length(cov_matrices), length(cov_matrices))
for(i in 1:(length(cov_matrices)-1)){
  for(j in (i+1):length(cov_matrices)){
    Rv.matrix[i,j] <- rvCoeff(cov_matrices[[i]], cov_matrices[[j]]) -> Rv.matrix[j,i]
  }
}
diag(Rv.matrix) <- rep(1,ncol(Rv.matrix))
Rveigen <- eigen(Rv.matrix, symmetric = TRUE)

cov_matrices %>%
  lapply(., c) %>%
  do.call(cbind, .) ->
  Z_matrix


svd_res_scale <- svd(scale(Z_matrix, center=F, scale=T),nu=0,nv=1)
svd_res_noscale <- svd(scale(Z_matrix, center=F, scale=F),nu=0,nv=1)


sqrt(d_Rveigen$values) / sqrt(Rveigen$values)
sqrt(d_Rveigen$values) / svd_res_scale$d

sqrt(d_not_Rveigen$values) / svd_res_noscale$d

sqrt(d_Rveigen$values) / svd_res_noscale$d
sqrt(d_not_Rveigen$values) / svd_res_scale$d




# cov_matrices %>%
#   double_center_tables() %>%
#   normalize_tables(., table_norm_type = "SS1") %>%
#   lapply(., c) %>%
#   do.call(cbind, .) ->
#   Z_matrix
#
#
#
# cov_matrices %>%
#   double_center_tables() %>%
#   normalize_tables(., table_norm_type = "MFA") %>%
#   lapply(., c) %>%
#   do.call(cbind, .) ->
#   Z_matrix


