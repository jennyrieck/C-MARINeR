

#### I am missing something here...

covstatis_inference <- function(cov_matrices, covstatis_results, iterations = 100){

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
  splithalf_cors <- boot_compromise_component_scores_list <- list()

  for(i in 1:iterations){

    ## bootstrap
    bootstrap_sample <- sample(1:length(cov_matrices), replace = T)
    # bootstrap_sample <- 1:length(cov_matrices)
    ## we want to generate a new compromise then project it onto te exist compromise

    # (0) double center each R table as S
    cov_matrices[bootstrap_sample] %>%
      double_center_matrices(.) ->
      boot_cov_matrices

    ## a *strict* enforcement of PSD/PD
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


    # ## split half
    sh1_sample <- sample(1:length(cov_matrices), floor(length(cov_matrices)/2), replace = T)
    sh2_sample <- setdiff(1:length(cov_matrices), sh1_sample)


    cov_matrices[sh1_sample] %>%
      double_center_matrices(.) ->
      sh1_cov_matrices

    cov_matrices[sh2_sample] %>%
      double_center_matrices(.) ->
      sh2_cov_matrices

    if(covstatis_results$input_parameters$strictly_enforce_psd){
      stopifnot(all(sapply(sh1_cov_matrices, is_sspsd_matrix, tol = tolerance)))
    }
    if(covstatis_results$input_parameters$strictly_enforce_psd){
      stopifnot(all(sapply(sh2_cov_matrices, is_sspsd_matrix, tol = tolerance)))
    }

    ## (1) make the alphas
    sh1_cov_matrices %>%
      normalize_matrices(., matrix_norm_type = covstatis_results$input_parameters$matrix_norm_type) ->
      sh1_cov_matrices

    sh2_cov_matrices %>%
      normalize_matrices(., matrix_norm_type = covstatis_results$input_parameters$matrix_norm_type) ->
      sh2_cov_matrices

    sh1_cov_matrices %>%
      lapply(., c) %>%
      do.call(cbind, .) %>%
      compute_alphas(., alpha_from_RV = covstatis_results$input_parameters$alpha_from_RV) ->
      sh1_alpha_weights

    sh2_cov_matrices %>%
      lapply(., c) %>%
      do.call(cbind, .) %>%
      compute_alphas(., alpha_from_RV = covstatis_results$input_parameters$alpha_from_RV) ->
      sh2_alpha_weights

    ## (2 & 3) make compromise & project
    sh1_decomposition_results <- tolerance.eigen(make_compromise_matrix(sh1_cov_matrices, sh1_alpha_weights), symmetric = TRUE, tol = covstatis_results$input_parameters$tolerance)
    sh2_decomposition_results <- tolerance.eigen(make_compromise_matrix(sh2_cov_matrices, sh2_alpha_weights), symmetric = TRUE, tol = covstatis_results$input_parameters$tolerance)

    ## ok now project each split onto the results of the other...
    # the correlate the split with the predict & add the abs(cor())

  }

  ### this is ugly and needs substantial re-writing
    ## probably also for memory and speed
  boot.cube <- simplify2array(boot_compromise_component_scores_list)
  boot.cube.mean <- apply(boot.cube, c(1, 2), mean)
  boot.cube.mean_repeat <- array(boot.cube.mean, dim = c(dim(boot.cube)))
  boot.cube.dev <- (boot.cube - boot.cube.mean_repeat)^2
  s.boot <- sqrt(apply(boot.cube.dev, c(1, 2), mean))
  boot.ratios <- boot.cube.mean/s.boot
  boot.ratios[which(is.infinite(boot.ratios))] <- 0

  return(
    list(
      covstatis_bootstrap_results = list(boot_compromise_component_scores_list = boot_compromise_component_scores_list,
                                         boot.ratios = boot.ratios),
      covstatis_splithalf_results = list()
      )
    )
}
