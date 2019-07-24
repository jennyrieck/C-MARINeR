#' Compute partial component scores
#'
#' TODO
#'
#' @export
#'
#' @param centered_normed_matrices TODO
#' @param compromise_decomposition_results TODO
#'
#' @return TODO
#'
#' @examples
#' TODO

compute_partial_component_scores <- function(centered_normed_matrices, compromise_decomposition_results){

  ## check if eigen or svd here
    ## eigen
  (compromise_decomposition_results$vectors %*% diag(1/sqrt(compromise_decomposition_results$values))) ->
    projection_matrix
    ## svd

      ## do we want to pass in a class here, or just make this a method for computing covstatis scores?
        ### let's finish covstatis first.


    ## quit!

  sapply(centered_normed_matrices,
         function(cov_matrix, projection){cov_matrix %*% projection},
         projection=projection_matrix,
         simplify = FALSE,
         USE.NAMES = TRUE)

}
