#' Double center a matrix
#'
#' TODO
#'
#' @export
#'
#' @param sspsd_matrix A square symmetric matrix.
#'
#' @return TODO A centered matrix?
#'
#' @examples
#' TODO

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
