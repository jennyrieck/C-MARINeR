### split this into just testing the steps and a test against DistatisR.

## a general test of covstatis steps
  ## this is to just ensure that the steps generally work as they are supposed to.
  ## here: for a list of matrices

require(ExPosition)
require(devtools)
require(magrittr)

data("jocn.2005.fmri")
# load_all("../covstatis/")
load_all(".")

cov_matrices <- list(images=-as.matrix(jocn.2005.fmri$images$data),
                     scans=-as.matrix(jocn.2005.fmri$scans$data))

table_norm_type <- "none"
alpha_from_RV <- TRUE



## alt:
cov_matrices %>%
  double_center_tables(.) %>%
  normalize_tables(., table_norm_type = table_norm_type) ->
  cov_matrices

if(table_norm_type != "MFA"){ ## this will have to expand to variance conditions

    stopifnot(all(!unlist(lapply(cov_matrices, is.sspsd.matrix))))

}

cov_matrices %>%
  lapply(., c) %>%
  do.call(cbind, .) %>%
  compute_alphas(., alpha_from_RV = alpha_from_RV) ->
  alpha_weights


tab_1 <- cov_matrices[[1]] * alpha_weights[1]
tab_2 <- cov_matrices[[2]] * alpha_weights[2]

mapply("*", cov_matrices, alpha_weights, SIMPLIFY = FALSE, USE.NAMES = TRUE)[[1]]  / tab_1
mapply("*", cov_matrices, alpha_weights, SIMPLIFY = FALSE, USE.NAMES = TRUE)[[2]]  / tab_2


comp_1 <- tab_1 + tab_2
comp_2 <- Reduce("+",mapply("*", cov_matrices, alpha_weights, SIMPLIFY = FALSE, USE.NAMES = TRUE))

make_compromise_matrix(cov_matrices, alpha_weights) ->
  compromise_matrix

comp_1 / compromise_matrix



eigen(compromise_matrix, symmetric = TRUE) %>%
  eigen_tolerance(.) ->
  compromise_eigen_results

  ## should perform checks & cleaning here...

# compromise_eigen_results$vectors %>%
#   sweep(., 2, sqrt(compromise_eigen_results$values), "*") ->
#   compromise_component_scores
    ## this is faster...
(compromise_eigen_results$vectors %*% diag(sqrt(compromise_eigen_results$values))) ->
  compromise_component_scores


compute_partial_component_scores(cov_matrices, compromise_eigen_results) ->
  partial_component_scores

partial_component_scores[[1]] / (cov_matrices[[1]] %*% compromise_eigen_results$vectors %*% diag(1/sqrt(compromise_eigen_results$values)))
partial_component_scores[[2]] / (cov_matrices[[2]] %*% compromise_eigen_results$vectors %*% diag(1/sqrt(compromise_eigen_results$values)))


compute_weighted_partial_component_scores(partial_component_scores, alpha_weights) ->
  weighted_partial_component_scores

weighted_partial_component_scores[[1]] / (partial_component_scores[[1]] * alpha_weights[1] * 2)
weighted_partial_component_scores[[2]] / (partial_component_scores[[2]] * alpha_weights[1] * 2)

Reduce("+",weighted_partial_component_scores) / compromise_component_scores

(Reduce("+",weighted_partial_component_scores) / length(weighted_partial_component_scores)) / compromise_component_scores