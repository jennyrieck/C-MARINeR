#' Make a compromise matrix
#'
#' TODO
#'
#' @export
#'
#' @param cov_matrices TODO
#' @param alpha_weighted TODO
#'
#' @return TODO
#'
#' @examples
#' TODO

make_compromise_matrix <- function(cov_matrices, alpha_weights){

  ## previous note said this didn't work but I'm certain it does... need to check.
  compromise_matrix <- Reduce("+",
    mapply("*", cov_matrices, alpha_weights, SIMPLIFY = FALSE, USE.NAMES = TRUE)
  )
  rownames(compromise_matrix) <- rownames(cov_matrices[[1]])

  compromise_matrix
}
