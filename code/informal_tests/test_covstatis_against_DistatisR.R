# DblCenterDist <- function(Y) {
#   nI = nrow(Y)
#   CentMat = diag(nI) - (1/nI) * matrix(1, nI, nI)
#   S = -(1/2) * (CentMat %*% Y %*% CentMat)
#   return(S)
# }
# Dist2CP <- function(D3) {
#   CP3 <- (array(apply(D3, 3, DblCenterDist), dim = c(dim(D3)[1],
#                                                      dim(D3)[2], dim(D3)[3])))
#   dimnames(CP3) <- dimnames(D3)
#   return(CP3)
# }

dist_matrices <- list(images=as.matrix(jocn.2005.fmri$images$data),
                      scans=as.matrix(jocn.2005.fmri$scans$data))
dist_cube <- simplify2array(dist_matrices)

# Splus <- apply(apply(dsr_res$res4Splus$SCP, c(1, 2), "*", t(dsr_res$res4Cmat$alpha)), c(2, 3), sum)
# ((dsr_res$res4Splus$SCP[,,1] * dsr_res$res4Cmat$alpha[1]) + (dsr_res$res4Splus$SCP[,,2] * dsr_res$res4Cmat$alpha[2])) / Splus


dsr_res <- DistatisR::distatis(dist_cube, Norm="NONE")

dsr_res2 <- DistatisR::distatis(simplify2array(cov_matrices), Distance = FALSE, Norm="NONE")

dsr_res3 <- DistatisR::distatis(simplify2array(cov_matrices), Distance = FALSE, Norm="NONE")



