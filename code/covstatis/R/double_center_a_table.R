#' Double center a table
#'
#' TODO
#'
#' @export
#'
#' @param sspsd_mat A square symmetric matrix.
#'
#' @return TODO A centered table?
#'
#' @examples
#' TODO

double_center_a_table <- function(sspsd_mat){

  ## on entry we need to guarantee we have a square symmetric matrix

  nI <- nrow(sspsd_mat)
  centering_mat <- matrix(-1/nI, nI, nI)
  diag(centering_mat) <- rep(1 - (1/nI), nI)
  (centering_mat %*% sspsd_mat %*% centering_mat)/2

}
