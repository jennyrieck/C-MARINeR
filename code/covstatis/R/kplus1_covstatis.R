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
  }
  ## because this is the SVD, we can do both, instead of making a choice like with the eigen.
  # else{
  #   tolerance <- NA ## so that we can get negative eigens, if that's what you're into.
  # }

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

  ###### LIKE IN COVSTATIS, THE FOLLOWING STEPS HELP SHOW A "CORE" FUNCTION COULD BE CREATED
    ## alphas, compromise, decompose, (compromise) scores

  # (3) alphas
  cross_matrices %>%
    compute_alphas(., alpha_from_RV = alpha_from_RV) ->
    alpha_weights

  # (4) Make the compromise matrix
  make_compromise_matrix(cross_matrices, alpha_weights) ->
    compromise_matrix

  ### NOTE: Unlike covSTATIS, we no longer have square & symmetric matrices
    ### they are square, but the triangles are different because we've got the cross product between each matrix and the target.

  ### this requires the SVD and will have two sets of component scores...
    ### but they can be plotted together and I can compute distances between them
    ### and project data onto the components I guess?

  # (5) SVD of compromise
  compromise_matrix %>%
    tolerance_svd(., tol = tolerance) ->
    compromise_decomposition_results

  # (6a) compute compromise component scores
  (compromise_decomposition_results$u %*% diag(compromise_decomposition_results$d)) ->
    compromise_cross_matrices_component_scores
  # (6b) compute compromise component scores
  (compromise_decomposition_results$v %*% diag(compromise_decomposition_results$d)) ->
    compromise_target_matrix_component_scores



  ###### i believe the projection matrix/partial scores are wrong.
  ### NEED TO STEP BACK AND TRY THIS OUT FROM THE DECOMP RESULTS.

  #### these are not sufficient anymore, we need both sides.
  ### make own functions here.

  # (7) compute partial (table) component scores
    ### what if this is supposed to be the cov_matrices?
  compute_kplus1covstatis_partial_component_scores(cross_matrices, compromise_decomposition_results) ->
    partial_cross_matrices_component_scores

  ## I might be able to use this actually... so it will get renamed again.
  # # (7) compute weighted partial (table) component scores
  # compute_barycentric_partial_component_scores(partial_component_scores, alpha_weights) ->
  #   barycentric_partial_component_scores

  ## perhaps the barycentric ones are to be dropped, as they sort of help illustrate the properties.

  # # (8a) compute weighted partial (table) component scores
  # compute_barycentric_partial_component_scores(partial_component_scores, alpha_weights) ->
  #   barycentric_partial_cross_matrices_component_scores
  # # (8b) compute weighted target component scores
  # compute_barycentric_partial_component_scores(rep(list(compromise_target_matrix_component_scores), length(alpha_weights)), alpha_weights) ->
  #   barycentric_partial_target_component_scores


  rownames(compromise_cross_matrices_component_scores) <- rownames(target_cov_matrix) -> rownames(compromise_decomposition_results$v)

  rownames(compromise_cross_matrices_component_scores) <- rownames(cov_matrices[[1]]) -> rownames(compromise_decomposition_results$u)
  partial_cross_matrices_component_scores <- mapply(function(scores,covs){rownames(scores) <- rownames(covs); scores}, partial_cross_matrices_component_scores, cross_matrices, SIMPLIFY = FALSE, USE.NAMES = TRUE)
  # barycentric_partial_cross_matrices_component_scores <- mapply(function(scores,covs){rownames(scores) <- rownames(covs); scores}, barycentric_partial_cross_matrices_component_scores, cross_matrices, SIMPLIFY = FALSE, USE.NAMES = TRUE)


  ## should use a class here for the prints.
  return(list(
    compromise_cross_matrices_component_scores = compromise_cross_matrices_component_scores,
    compromise_target_matrix_component_scores = compromise_target_matrix_component_scores,
    partial_cross_matrices_component_scores = partial_cross_matrices_component_scores,
    # barycentric_partial_component_scores = barycentric_partial_component_scores,
    compromise_decomposition_results = compromise_decomposition_results,
    compromise_matrix = compromise_matrix,
    alpha_weights = alpha_weights,
    input_parameters = list(matrix_norm_type = matrix_norm_type, alpha_from_RV = alpha_from_RV, tolerance = tolerance, strictly_enforce_psd = strictly_enforce_psd)
  ))



}
