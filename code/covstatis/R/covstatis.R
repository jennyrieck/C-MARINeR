#' TODO: TITLE
#'
#' TODO
#'
#' @export
#'
#' @param cov_matrices TODO
#' @param matrix_norm_type TODO. Default is "MFA".
#' @param alpha_from_RV TODO. Default is TRUE.
#' @param compact TODO. Default is TRUE.
#' @param tolerance TODO. Default \code{sqrt(.Machine$double.eps)}.
#' @param strictly_enforce_psd TODO. Default is FALSE.
#'
#' @return TODO
#'
#' @examples
#' TODO

## we are requiring that these are cov/cor matrices.

### each of these functions needs a more coherent name...
  ### cov_matrices only applies here...


covstatis <- function(cov_matrices, matrix_norm_type = "MFA", alpha_from_RV = TRUE, tolerance = sqrt(.Machine$double.eps), strictly_enforce_psd = FALSE){

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


  ## we are going to recycle the name cov_matrices and transform them each step along the way, as to not create new large lists.

  # (0) double center each R table as S
    cov_matrices %>%
      double_center_matrices(.) ->
      cov_matrices

  ## a *strict* enforcement of PSD/PD
  if(strictly_enforce_psd){
    stopifnot(all(sapply(cov_matrices, is_sspsd_matrix, tol = tolerance)))
  }

  # (1) Normalize each table
  cov_matrices %>%
    normalize_matrices(., matrix_norm_type = matrix_norm_type) ->
    cov_matrices


  ####
  ##    BETWEEN HERE AND BELOW IS EFFECTIVELY A "CORE" CROSS-PRODUCT STATIS
  ##      At least until (5) but also possibly through all steps.
  ####

  # (2) Get the alpha weights
  cov_matrices %>%
    compute_alphas(., alpha_from_RV = alpha_from_RV) ->
    alpha_weights


  # (3) Make the compromise matrix
  make_compromise_matrix(cov_matrices, alpha_weights) ->
    compromise_matrix


  ## a *strict* enforcement of PSD/PD
  if(!is_sspsd_matrix(compromise_matrix, tol=tolerance)){
    if(strictly_enforce_psd){
      stop("covstatis: compromise_matrix is not positive semi-definite within `tolerance`")
    }
    warning("covstatis: compromise_matrix is not positive semi-definite within `tolerance`")
  }


  # (4) eigen of compromise
    ## this will need to become geigen() and then compute_partial_component_scores() will change
  compromise_matrix %>%
    tolerance.eigen(., symmetric = TRUE, tol = tolerance) ->
    compromise_decomposition_results

  ## here I need to ensure that tolerance.eigen or whatever is used actually sends back a class type
    ## I should probably switch to geigen() and rely on that class.

  # (5) compute compromise component scores
  (compromise_decomposition_results$vectors %*% diag(sqrt(compromise_decomposition_results$values))) ->
    compromise_component_scores

  ####
  ##    BETWEEN HERE AND ABOVE IS EFFECTIVELY A "CORE" CROSS-PRODUCT STATIS
  ####

  # (6) compute partial (table) component scores
  compute_partial_component_scores(cov_matrices, compromise_decomposition_results) ->
    partial_component_scores

  # (7) compute weighted partial (table) component scores
    ### removed for now.
  compute_barycentric_partial_component_scores(partial_component_scores, alpha_weights) ->
    barycentric_partial_component_scores

  ####
  ##    BETWEEN HERE AND ABOVE IS EFFECTIVELY A "CORE" CROSS-PRODUCT STATIS
  ####


  rownames(compromise_component_scores) <- rownames(cov_matrices[[1]]) -> rownames(compromise_decomposition_results$vectors)
  partial_component_scores <- mapply(function(scores,covs){rownames(scores) <- rownames(covs); scores}, partial_component_scores, cov_matrices, SIMPLIFY = FALSE, USE.NAMES = TRUE)
  barycentric_partial_component_scores <- mapply(function(scores,covs){rownames(scores) <- rownames(covs); scores}, barycentric_partial_component_scores, cov_matrices, SIMPLIFY = FALSE, USE.NAMES = TRUE)

  return(list(
    alpha_weights = alpha_weights,
    compromise_matrix = compromise_matrix,
    compromise_decomposition_results = compromise_decomposition_results,
    compromise_component_scores = compromise_component_scores,
    partial_component_scores = partial_component_scores,
    barycentric_partial_component_scores = barycentric_partial_component_scores,
    input_parameters = list(matrix_norm_type = matrix_norm_type, alpha_from_RV = alpha_from_RV, tolerance = tolerance, strictly_enforce_psd = strictly_enforce_psd)
  ))

}
