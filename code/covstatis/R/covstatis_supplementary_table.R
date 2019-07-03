covstatis_supplementary_table <- function(cov_matrix, covstatis_results){


  if(!is_ss_matrix(cov_matrix)){
    stop("covstatis: 'cov_matrix' was not a square and symmetric matrix.")
  }
  if(nrow(cov_matrix) != nrow(covstatis_results$compromise_matrix)){
    stop("covstatis: 'cov_matrix' not the same size as active matrix (covstatis_results$compromise_matrix)")
  }

  ## also check the size...

  # (0) double center cov_matrix
  cov_matrix %>%
    double_center_a_matrix(.)

  ## a *strict* enforcement of PSD/PD (which may not be necessary)
  if(covstatis_results$input_parameters$strictly_enforce_psd){
    stopifnot(is_sspsd_matrix(cov_matrix, tol = tolerance))
  }

  ## (1) normalize
  cov_matrix %>%
    normalize_matrices(., matrix_norm_type = covstatis_results$input_parameters$matrix_norm_type) ->
    cov_matrix


  ## (2) project to get scores
  (cov_matrix %*% (covstatis_results$compromise_decomposition_results$vectors %*% diag(1/sqrt(covstatis_results$compromise_decomposition_results$values))))

}
