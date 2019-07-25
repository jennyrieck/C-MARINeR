##
#' @export
#'
#' @title double_center_matrices: double center a list of matrices
#'
#' @param cov_matrices list. Covariance or correlation matrices to be double centered
#'
#' @return list. Double centered covariance or correlation matrices

double_center_matrices <- function(cov_matrices){

  ### sapply is not safe; change over to vapply
  sapply(cov_matrices, double_center_a_matrix, simplify = FALSE, USE.NAMES = TRUE)

}


##
#' @export
#'
#' @title double_center_a_matrix: double center a matrix
#'
#' @param sspsd_matrix matrix. A square, symmetric, and (presumed) positive semi-definite---i.e., covariance or correlation---matrix to be centered
#'
#' @return matrix. A double centered covariance or correlation matrix

double_center_a_matrix <- function(sspsd_matrix){

  sspsd_matrix %>%
    is_ss_matrix %>%
    stopifnot

  nI <- nrow(sspsd_matrix)
  centering_mat <- matrix(-1/nI, nI, nI)
  rownames(centering_mat) <- rownames(sspsd_matrix) -> colnames(centering_mat)

  diag(centering_mat) <- rep(1 - (1/nI), nI)
  (centering_mat %*% sspsd_matrix %*% centering_mat)/2

}
