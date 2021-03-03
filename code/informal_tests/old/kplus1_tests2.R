## load in hypothetical kplus1 set.

library(tidyverse)
library(devtools)
library(GSVD)
load_all()

# source('../transfer_and_misfits/kplus1.covstatis.R')


load('/Data/TinyNKI/nki_data_design.rda')

target_matrix <- tcrossprod(network7.design.dummy)
colnames(target_matrix) <- rownames(target_matrix) <- paste0("TARGET_",rownames(target_matrix))


kp1cs_res <- kplus1_covstatis(target_cov_matrix = target_matrix, cov_matrices = conn.cube.tog, matrix_norm_type = "none")
# old_kp1cs_res <- kplus1.covstatis(Sk.array = conn.cube.tog, Sk.target = target_matrix)

### this relationship is importantt o be able to recover.
barys <- compute_barycentric_partial_component_scores(kp1cs_res$partial_cross_matrices_component_scores, alpha_weights = kp1cs_res$alpha_weights)

kp1cs_res$compromise_cross_matrices_component_scores / apply(simplify2array(barys),c(1,2),mean)

Reduce("+",barys)