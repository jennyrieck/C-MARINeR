### actually, this would work for k+1 and covstatis if they both used the SVD and encforced $u as the cov_matrices
### that being said, we break that pattern if we expand to K+KcovSTATIS.
### which is actually fixed if we directly pass in vectors and values...


##
#' @export
#'
#' @title compute_covstatis_partial_component_scores: compute the partial component scores (per matrix) for the compromise space.
#'
#' @param centered_normed_matrices list. These must be the normalized cross-product matrices, i.e., they have already been double centered then normalzied.
#' @param compromise_decomposition_results list. These must be the results from the decomposition of the compromise matrix (e.g., \code{$vectors} and \code{$values}).
#'
#' @return list. Each element in the list is a matrix of partial component scores.

compute_covstatis_partial_component_scores <- function(centered_normed_matrices, compromise_decomposition_results){

  ## I should set up some safetys here, but later.

  (compromise_decomposition_results$vectors %*% diag(1/sqrt(compromise_decomposition_results$values))) ->
    projection_matrix


  sapply(centered_normed_matrices,
         function(cov_matrix, projection){cov_matrix %*% projection},
         projection=projection_matrix,
         simplify = FALSE,
         USE.NAMES = TRUE)

}
