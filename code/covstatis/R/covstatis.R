#' TODO: TITLE
#'
#' TODO
#'
#' @export
#'
#' @param cov_matrices TODO
#' @param table_norm_type TODO. Default is MFA.
#' @param alpha_from_RV TODO. Default is TRUE.
#' @param compact TODO. Default is TRUE.
#'
#' @return TODO
#'
#' @examples
#' TODO

covstatis <- function(cov_matrices, table_norm_type = "MFA", alpha_from_RV = TRUE, compact = TRUE, tolerance = sqrt(.Machine$double.eps), strictly_enforce_psd = FALSE){

  ## everything is list based for now

  if(is.array(cov_matrices)){
    if(nrow(cov_matrices)!=ncol(cov_matrices)){
      stop("covstatis: The number of rows does not equal the number of columns. Each matrix must be square, symmetric, and positive semi-definite.")
    }

    cov_matrices %>%
      array2list(.) ->
      cov_matrices
  }

  if(is.list(cov_matrices)){
    if(any(!sapply(cov_matrices, is_ss_matrix))){
      stop("covstatis: At least one matrix in the 'cov_matrices' list was not a square and symmetric matrix.")
    }
  }

  # (0) double center each R table as S
  cov_matrices %>%
    double_center_tables(.) ->
    cov_matrices

  ## a *strict* enforcement of PSD/PD
  if(strictly_enforce_psd){
    stopifnot(all(!sapply(cov_matrices, is_sspsd_matrix)))
  }

  # (1) Normalize each table
  cov_matrices %>%
    normalize_tables(., table_norm_type = table_norm_type) ->
    cov_matrices


  # (2) Get the alpha weights
  cov_matrices %>%
    lapply(., c) %>%
    do.call(cbind, .) %>%
    compute_alphas(., alpha_from_RV = alpha_from_RV) ->
    alpha_weights


  # (3) Make the compromise matrix
  make_compromise_matrix(cov_matrices, alpha_weights) ->
    compromise_matrix


  ## a *strict* enforcement of PSD/PD
  if(strictly_enforce_psd & !is_sspsd_matrix(compromise_matrix)){
    stop("covstatis: compromise_matrix is not positive semi-definite")
  }


  # (4) eigen of compromise
  compromise_matrix %>%
    tolerance.eigen(., tol=tolerance, symmetric = TRUE) ->
    compromise_eigen


  # (5) compute compromise component scores
  (compromise_eigen$vectors %*% diag(sqrt(compromise_eigen$values))) ->
    compromise_component_scores

  # (6) compute partial (table) component scores
  compute_partial_component_scores(cov_matrices, compromise_eigen) ->
    partial_component_scores

  # (7) compute weighted partial (table) component scores
    ### removed for now.
  # compute_weighted_partial_component_scores(partial_component_scores, alpha_weights) ->
  #   weighted_partial_component_scores

  ## major question left:
  ## which is more appropriate to visualize: partial or (correctly) weighted partials?
  ## to think about; requires going back to compute_weighted_partial_component_scores and making a decision


  rownames(compromise_component_scores) <- rownames(cov_matrices[[1]])

  ## considering renaming these to match the STATIS paper.
  if(compact){
    return(list(
      compromise_matrix = compromise_matrix,
      compromise_eigen = compromise_eigen,
      compromise_component_scores = compromise_component_scores,
      partial_component_scores = partial_component_scores
      # weighted_partial_component_scores = weighted_partial_component_scores
    ))
  }else{
    return(list(
      cov_matrices = cov_matrices,
      compromise_matrix = compromise_matrix,
      compromise_eigen = compromise_eigen,
      compromise_component_scores = compromise_component_scores,
      partial_component_scores = partial_component_scores
      # weighted_partial_component_scores = weighted_partial_component_scores
    ))
  }

}
