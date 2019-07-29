##
#' @export
#'
#' @title make_compromise_matrix: make the compromise (weighted average) matrix
#'
#' @param centered_normed_matrices list. A list of covariance or correlation matrices that have been double centered and normalized
#' @param alpha_weights vector. A vector of weights to use for the weighted average to produce the compromise matrix
#'
#' @return matrix. The weighted average of all matrices in \code{centered_normed_matrices} with the weights of \code{alpha_weights}

make_compromise_matrix <- function(centered_normed_matrices, alpha_weights){

  ## previous note said this didn't work but I'm certain it does... need to check.
  compromise_matrix <- Reduce("+",
    mapply("*", centered_normed_matrices, alpha_weights, SIMPLIFY = FALSE, USE.NAMES = TRUE)
  )
  rownames(compromise_matrix) <- rownames(centered_normed_matrices[[1]])

  compromise_matrix
}
