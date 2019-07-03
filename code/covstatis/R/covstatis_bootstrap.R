covstatis_bootstrap <- function(cov_matrices, covstatis_results, iterations = 100){

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

  ## we need the matrices & fixed results
  # boot_compromise_component_scores <- vector(length(cov_matrices), "list")
    ## can I make this more optimized?
  boot_compromise_component_scores_list <- list()

  for(i in 1:iterations){

    ## bootstrap
    bootstrap_sample <- sample(1:length(cov_matrices), replace = T)

    # (0) double center each R table as S
    cov_matrices[bootstrap_sample] %>%
      double_center_matrices(.) ->
      boot_cov_matrices

    ## a *strict* enforcement of PSD/PD (which may not be necessary)
    if(covstatis_results$input_parameters$strictly_enforce_psd){
      stopifnot(all(sapply(boot_cov_matrices, is_sspsd_matrix, tol = tolerance)))
    }

    ## (1) make the alphas
    boot_cov_matrices %>%
      normalize_matrices(., matrix_norm_type = covstatis_results$input_parameters$matrix_norm_type) ->
      boot_cov_matrices


    boot_cov_matrices %>%
      lapply(., c) %>%
      do.call(cbind, .) %>%
      compute_alphas(., alpha_from_RV = covstatis_results$input_parameters$alpha_from_RV) ->
      boot_alpha_weights

    ## (2 & 3) make compromise & project
    make_compromise_matrix(boot_cov_matrices, boot_alpha_weights) %*% (covstatis_results$compromise_decomposition_results$vectors %*% diag(1/sqrt(covstatis_results$compromise_decomposition_results$values))) ->
      boot_compromise_component_scores_list[[i]]


  }

  ## this could benefit from less ugliness and more efficiency.
  ## probably also for memory and speed
  boot.cube.mean <- apply(simplify2array(boot_compromise_component_scores_list), c(1, 2), mean)
  boot.cube.dev <- sapply(boot_compromise_component_scores_list,
                    function(this_slice, mean_slice){(this_slice - mean_slice)^2},
                    mean_slice = boot.cube.mean,
                    simplify = FALSE,
                    USE.NAMES = TRUE)
  boot.ratios <- boot.cube.mean / (sqrt(apply(simplify2array(boot.cube.dev), c(1, 2), mean)))
  boot.ratios[which(is.infinite(boot.ratios))] <- 0

  return(
    list(boot_compromise_component_scores_list = boot_compromise_component_scores_list,
         boot.ratios = boot.ratios)
  )
}
