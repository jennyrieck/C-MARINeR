kplusk_covstatis <- function(cov_matrices, target_cov_matrices, matrix_norm_type = "MFA", alpha_from_RV = TRUE, compact = TRUE, tolerance = sqrt(.Machine$double.eps), strictly_enforce_psd = FALSE){

  if(is.array(cov_matrices)){
    if(nrow(cov_matrices)!=ncol(cov_matrices)){
      stop("kplusk_covstatis: The number of rows does not equal the number of columns for cov_matrices. Each matrix must be square, symmetric, and positive semi-definite.")
    }

    cov_matrices %>%
      array2list(.) ->
      cov_matrices
  }

  if(is.array(target_cov_matrices)){
    if(nrow(target_cov_matrices)!=ncol(target_cov_matrices)){
      stop("kplusk_covstatis: The number of rows does not equal the number of columns for target_cov_matrices. Each matrix must be square, symmetric, and positive semi-definite.")
    }

    target_cov_matrices %>%
      array2list(.) ->
      target_cov_matrices
  }

  if(any(!sapply(cov_matrices, is_ss_matrix))){
    stop("kplusk_covstatis: At least one matrix in the 'cov_matrices' list was not a square and symmetric matrix.")
  }

  if(any(!sapply(target_cov_matrices, is_ss_matrix))){
    stop("kplusk_covstatis: At least one matrix in the 'target_cov_matrices' list was not a square and symmetric matrix.")
  }


}
