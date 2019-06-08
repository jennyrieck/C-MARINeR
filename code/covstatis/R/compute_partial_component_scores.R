#' Compute partial component scores
#'
#' TODO
#'
#' @export
#'
#' @param cov_matrices TODO
#' @param compromise_decomposition_results TODO
#'
#' @return TODO
#'
#' @examples
#' TODO

compute_partial_component_scores <- function(cov_matrices, compromise_decomposition_results){

  ## check if eigen or svd here
    ## eigen
  (compromise_decomposition_results$vectors %*% diag(1/sqrt(compromise_decomposition_results$values))) ->
    projection_matrix
    ## svd

    ## quit!

  sapply(cov_matrices,
         function(cov_matrix, projection){cov_matrix %*% projection_matrix},
         projection=projection_matrix,
         simplify = FALSE,
         USE.NAMES = TRUE)

}
