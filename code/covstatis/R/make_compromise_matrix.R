#' Make a compromise matrix
#'
#' TODO
#'
#' @export
#'
#' @param centered_normed_matrices TODO
#' @param alpha_weighted TODO
#'
#' @return TODO
#'
#' @examples
#' TODO

make_compromise_matrix <- function(centered_normed_matrices, alpha_weights){

  ## previous note said this didn't work but I'm certain it does... need to check.
  compromise_matrix <- Reduce("+",
    mapply("*", centered_normed_matrices, alpha_weights, SIMPLIFY = FALSE, USE.NAMES = TRUE)
  )
  rownames(compromise_matrix) <- rownames(centered_normed_matrices[[1]])

  compromise_matrix
}
