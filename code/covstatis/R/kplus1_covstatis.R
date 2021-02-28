kplus1_covstatis <- function(cov_matrices, target_cov_matrix, matrix_norm_type = "MFA", alpha_from_RV = TRUE, compact = TRUE, tolerance = sqrt(.Machine$double.eps), strictly_enforce_psd = FALSE){

  if(missing(target_cov_matrix)){
    stop("kplus1_covstatis: target_cov_matrix matrix not provided.")
  }

  ## everything is list based for now
  if(is.array(cov_matrices)){
    if(nrow(cov_matrices)!=ncol(cov_matrices)){
      stop("kplus1_covstatis: The number of rows does not equal the number of columns. Each matrix must be square, symmetric, and positive semi-definite.")
    }

    cov_matrices %>%
      array2list(.) ->
      cov_matrices
  }

  if(any(!sapply(cov_matrices, is_ss_matrix))){
    stop("kplus1_covstatis: At least one matrix in the 'cov_matrices' list was not a square and symmetric matrix.")
  }
  if(!is_ss_matrix(target_cov_matrix)){
    stop("kplus1_covstatis: The 'target_cov_matrix' was not a square and symmetric matrix.")
  }
    ### IMPORTANT
  ## check that the target is the same size as just the first from cov_matrices
    ### IMPORTANT

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

    if(tolerance < 0){
      tolerance <- sqrt(.Machine$double.eps)
    }

    stopifnot(all(sapply(cov_matrices, is_sspsd_matrix, tol = tolerance)))
    stopifnot(is_sspsd_matrix(target_cov_matrix, tol = tolerance))
  }else{
    tolerance <- NA ## so that we can get negative eigens, if that's what you're into.
  }

  # (1.a) Normalize each table
  cov_matrices %>%
    normalize_matrices(., matrix_norm_type = matrix_norm_type) ->
    cov_matrices

  # (1.b) Normalize the target (though, I suppose sometimes this is unnecessary?)
  target_cov_matrix %>%
    normalize_a_matrix(., matrix_norm_type = matrix_norm_type) ->
    target_cov_matrix


  # (2) multiplication
    ## just use a loop for now.
  cross_matrices <- list()
  for(i in 1:length(cov_matrices)){
    cross_matrices[[i]] <- crossprod(cov_matrices[[i]], target_cov_matrix)
  }

  # (3) alphas


  ### this requires the SVD and will have two sets of component scores...
    ### but they can be plotted together and I can compute distances between them
    ### and project data onto the components I guess?


}
