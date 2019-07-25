##
#' @export
#'
#' @title compute_barycentric_partial_component_scores: compute re-weighted versions of \code{partial_component_scores} which are barycentric (i.e., equal the compromise scores)
#'
#' @param partial_component_scores list. A list of the partial component scores as computed from \code{compute_partial_component_scores}
#' @param alpha_weights vector. A vector that contains the alpha weights as used in the computation of the compromise.
#'
#' @return list. A list of the barycentric version of the partial component scores.
#'
#' @details The name "barycentric partial component scores" means that the partial component scores (\code{partial_component_scores}) are re-weighted by
#' the number of tables (\code{length(alpha_weights)}) and the alpha weights (\code{alpha_weights}). The mean of the barycentric partial component scores
#' is equal to the compromise component scores.
#'
#' @examples
#' TODO

compute_barycentric_partial_component_scores <- function(partial_component_scores, alpha_weights){

  # mapply("*", partial_component_scores, (alpha_weights * length(alpha_weights)), SIMPLIFY = FALSE, USE.NAMES = TRUE)
  mapply("*", partial_component_scores, alpha_weights * length(partial_component_scores), SIMPLIFY = FALSE, USE.NAMES = TRUE)

}
