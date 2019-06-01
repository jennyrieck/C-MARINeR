#' Double center matrices
#'
#' TODO
#'
#' @export
#'
#' @param cov_matrices TODO
#'
#' @return TODO A list of double centered matrices?
#'
#' @examples
#' TODO

double_center_matrices <- function(cov_matrices){

  ### sapply is not safe; change over to vapply
  sapply(cov_matrices, double_center_a_matrix, simplify = FALSE, USE.NAMES = TRUE)

}
