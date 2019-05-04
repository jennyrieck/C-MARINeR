## a general test of covstatis.
  ## here: for an array

require(ExPosition)
require(devtools)
require(magrittr)

data("jocn.2005.fmri")
# load_all("../covstatis/")
load_all(".")



cov_matrices <- list(images=-as.matrix(jocn.2005.fmri$images$data),
                     scans=-as.matrix(jocn.2005.fmri$scans$data))
table_norm_type <- "SS1"

## alt:
cov_matrices %>%
  double_center_tables() %>%
  normalize_tables(., table_norm_type = table_norm_type) ->
  cov_matrices

if(table_norm_type != "MFA"){ ## this will have to expand to variance conditions

    stopifnot(all(!unlist(lapply(cov_matrices, is.sspsd.matrix))))

}

cov_matrices %>%
  lapply(., c) %>%
  do.call(cbind, .) ->
  Z_matrix


Z_matrix %>%
  compute_alphas(.) ->
  alpha_weights

# make_compromise_matrix(cov_matrices, alpha_weights) ->
#   compromise_matrix

tab_1 <- cov_matrices[[1]] * alpha_weights[1]
tab_2 <- cov_matrices[[2]] * alpha_weights[2]

mapply("*", cov_matrices, alpha_wights)

# sapply(cov_matrices,
#        function(table, weight){table*weight},
#        weight=alpha_weights,
#        simplify = FALSE,
#        USE.NAMES = TRUE)


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

compute_weighted_partial_component_scores(partial_component_scores, alpha_weights) ->
  weighted_partial_component_scores
