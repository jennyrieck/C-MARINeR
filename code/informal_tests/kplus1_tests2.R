## load in hypothetical kplus1 set.

library(tidyverse)
library(devtools)
library(GSVD)
load_all()


load('/Data/TinyNKI/nki_data_design.rda')

target_matrix <- tcrossprod(network7.design.dummy)
colnames(target_matrix) <- rownames(target_matrix) <- paste0("TARGET_",rownames(target_matrix))


kplus1_covstatis(target_cov_matrix = target_matrix, cov_matrices = conn.cube.tog)
