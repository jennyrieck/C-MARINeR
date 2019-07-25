library(tidyverse)
library(GSVD)
library(devtools)
library(ExPosition)
load_all(".")


data("jocn.2005.fmri")


dist_matrices <- list(images=as.matrix(jocn.2005.fmri$images$data),
                      scans=as.matrix(jocn.2005.fmri$scans$data))
dist_cube <- simplify2array(dist_matrices)

# Splus <- apply(apply(dsr_res$res4Splus$SCP, c(1, 2), "*", t(dsr_res$res4Cmat$alpha)), c(2, 3), sum)
# ((dsr_res$res4Splus$SCP[,,1] * dsr_res$res4Cmat$alpha[1]) + (dsr_res$res4Splus$SCP[,,2] * dsr_res$res4Cmat$alpha[2])) / Splus


dsr_res <- DistatisR::distatis(dist_cube, Norm="MFA")
dsr_res_1 <- DistatisR::distatis(dist_cube, Norm="NONE")

dsr_res_2 <- DistatisR::distatis(dist_cube, Norm="MFA", RV = FALSE)
dsr_res_3 <- DistatisR::distatis(dist_cube, Norm="NONE", RV = FALSE)

# dsr_res2 <- DistatisR::distatis(simplify2array(cov_matrices), Distance = FALSE, Norm="NONE") 
# dsr_res3 <- DistatisR::distatis(simplify2array(cov_matrices), Distance = FALSE, Norm="NONE")

cs_res <- covstatis::distatis(dist_cube, matrix_norm_type = "MFA")
cs_res_1 <- covstatis::distatis(dist_cube, matrix_norm_type = "none")

cs_res_2 <- covstatis::distatis(dist_cube, matrix_norm_type = "MFA", alpha_from_RV = FALSE)
cs_res_3 <- covstatis::distatis(dist_cube, matrix_norm_type = "none", alpha_from_RV = FALSE)




### all of these are equal hooray.
cs_res$compromise_matrix / dsr_res$res4Splus$Splus
cs_res_1$compromise_matrix / dsr_res_1$res4Splus$Splus
cs_res_2$compromise_matrix / dsr_res_2$res4Splus$Splus
cs_res_3$compromise_matrix / dsr_res_3$res4Splus$Splus


cs_res$compromise_component_scores[,1:3] / dsr_res$res4Splus$F[,1:3]
cs_res_1$compromise_component_scores[,1:3] / dsr_res_1$res4Splus$F[,1:3]
cs_res_2$compromise_component_scores[,1:3] / dsr_res_2$res4Splus$F[,1:3]
cs_res_3$compromise_component_scores[,1:3] / dsr_res_3$res4Splus$F[,1:3]
