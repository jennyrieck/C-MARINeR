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

  ## a check to ensure that length(alpha_weights) == length(cov_matrices)
      ## or if array... length(alpha_weights) == dim(cov_matrices)[3]

    ## I think this could be made simpler (specifically, the sapply part)

  ### this obviously does not work... I need to multiple each cov by each alpha
  Reduce("+",
    mapply("*", cov_matrices, alpha_weights, SIMPLIFY = FALSE, USE.NAMES = TRUE)
  )

}
