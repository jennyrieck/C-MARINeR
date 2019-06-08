kplus1_covstatis <- function(cov_matrices, target_cov_matrix, matrix_norm_type = "MFA", alpha_from_RV = TRUE, compact = TRUE, tolerance = sqrt(.Machine$double.eps), strictly_enforce_psd = FALSE){

  if(missing(target_cov_matrix)){
    stop("kplus1_covstatis: target_cov_matrix matrix not provided.")
  }

  ## everything is list based for now
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

  # (0.a) double center each R table as S
  cov_matrices %>%
    double_center_matrices(.) ->
    cov_matrices

  # (0.b) double center the target
  target_cov_matrix %>%
    double_center_a_matrix(.) ->
    target_cov_matrix


  ## a *strict* enforcement of PSD/PD
  if(strictly_enforce_psd){
    stopifnot(all(sapply(cov_matrices, is_sspsd_matrix, tol = tolerance)))
    stopifnot(is_sspsd_matrix(target_cov_matrix, tol = tolerance))
  }

  # (1.a) Normalize each table
  cov_matrices %>%
    normalize_matrices(., matrix_norm_type = matrix_norm_type) ->
    cov_matrices

  # (1.b) Normalize the target (though, generally this should be unnecessary)
  target_cov_matrix %>%
    normalize_a_matrix(., matrix_norm_type = matrix_norm_type) ->
    target_cov_matrix


  # (2) multiplication
  ## yuck
  ## cross.prod.cross.prod[,,i] <- crossprod(crossprod(dbl.cent.X[,,i],dbl.cent.network), crossprod(dbl.cent.X[,,i],dbl.cent.network))




  # (3) alphas


  ### this requires the SVD and will have two sets of component scores...
    ### but they can be plotted together and I can compute distances between them
    ### and project data onto the components I guess?


}
