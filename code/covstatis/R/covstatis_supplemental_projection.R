covstatis_supplemental_projection <- function(cov_matrices, covstatis_results){

  ## by now this set of checks should be a function...

  if(is.array(cov_matrices)){
    if(nrow(cov_matrices)!=ncol(cov_matrices)){
      stop("covstatis: The number of rows does not equal the number of columns. Each matrix must be square, symmetric, and positive semi-definite.")
    }

    cov_matrices %>%
      array2list(.) ->
      cov_matrices
  }

  if(any(!sapply(cov_matrices, is_ss_matrix))){
    stop("covstatis: At least one matrix in the 'cov_matrices' list was not a square and symmetric matrix.")
  }

  # (0) double center each R table as S
  cov_matrices %>%
    double_center_matrices(.) ->
    cov_matrices

  ## a *strict* enforcement of PSD/PD (which may not be necessary)
  if(covstatis_results$input_parameters$strictly_enforce_psd){
    stopifnot(all(sapply(cov_matrices, is_sspsd_matrix, tol = tolerance)))
  }

  ## (1) normalize
  cov_matrices %>%
    normalize_matrices(., matrix_norm_type = covstatis_results$input_parameters$matrix_norm_type) ->
    cov_matrices

  ## (2) make the alphas
  cov_matrices %>%
    lapply(., c) %>%
    do.call(cbind, .) %>%
    compute_alphas(., alpha_from_RV = covstatis_results$input_parameters$alpha_from_RV) ->
    alpha_weights

  ## (3 & 4) make compromise & project to get scores
  supplemental_
  make_compromise_matrix(cov_matrices, alpha_weights) %*% (covstatis_results$compromise_decomposition_results$vectors %*% diag(1/sqrt(covstatis_results$compromise_decomposition_results$values))) ->
    compromise_component_scores_list[[i]]


}
