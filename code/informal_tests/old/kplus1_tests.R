## load in hypothetical kplus1 set.

library(tidyverse)
library(devtools)
library(GSVD)
load_all()


load('/Data/TinyNKI/nki_data_design.rda')

target_matrix <- tcrossprod(network7.design.dummy)
  colnames(target_matrix) <- rownames(target_matrix) <- paste0("TARGET_",rownames(target_matrix))

## need to double center the target & one of other data matrices
  ## then find the fastest way to get tcrossprod(tcrossprod(), tcrossprod())

data_1 <- conn.cube.OA[,,1]


dbcnt_target <- double_center_a_matrix(target_matrix)
dbcnt_data <- double_center_a_matrix(data_1)

  ## these are all identical
crosscov <- crossprod(dbcnt_data,dbcnt_target)
one_cp <- dbcnt_data %*% dbcnt_target
two_cp <- dbcnt_data %*% t(dbcnt_target)
three_cp <- t(dbcnt_data) %*% dbcnt_target


cov_crosscov <- crossprod(crosscov, crosscov)
  crossprod(crosscov)


tcov_crosscov <- tcrossprod(crosscov, crosscov)
  tcrossprod(crosscov)


eig_cov_crosscov <- eigen(cov_crosscov)
eig_tcov_crosscov <- eigen(tcov_crosscov)
svd_crosscov <- svd(crosscov)


svd_crosscov$v[,1:6] / eig_cov_crosscov$vectors[,1:6]
svd_crosscov$u[,1:6] / eig_tcov_crosscov$vectors[,1:6]

  ## same obviously
eig_cov_crosscov$values[1:6]
eig_tcov_crosscov$values[1:6]

  ## not the same, obviously
eig_cov_crosscov$vectors[,1:6]
eig_tcov_crosscov$vectors[,1:6]

  ## but we can recover each other via...
eig_cov_crosscov$vectors[,1:6] / (cov_crosscov %*% eig_cov_crosscov$vectors %*% diag(1/eig_cov_crosscov$values))[,1:6]
eig_tcov_crosscov$vectors[,1:6] / (tcov_crosscov %*% eig_cov_crosscov$vectors %*% diag(1/eig_cov_crosscov$values))[,1:6]



## what this needs to be is the crossprod() of the data matrix i.e., the inner dimensions are the sides of the target

