kplus1_covstatis <- function(cov_matrices, target_cov_matrix, matrix_norm_type = "MFA", alpha_from_RV = TRUE, compact = TRUE, tolerance = sqrt(.Machine$double.eps), strictly_enforce_psd = FALSE){

  if(missing(target_cov_matrix)){
    stop("kplus1_covstatis: target_cov_matrix matrix not provided.")
  }



  ## an eventual call off to covstatis here...
    ## nope. can't call off to covstatis() because covstatis() performs the centering step...
  # covstatis(cov_matrices, matrix_norm_type = matrix_norm_type, alpha_from_RV = alpha_from_RV, compact = compact, tolerance = tolerance, strictly_enforce_psd = strictly_enforce_psd)

}
