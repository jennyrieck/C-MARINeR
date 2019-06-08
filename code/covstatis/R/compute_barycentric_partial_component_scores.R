#' Compute weighted partial component scores
#'
#' TODO
#'
#' @param partial_component_scores TODO
#' @param alpha_weights TODO
#'
#' @return TODO
#'
#' @examples
#' TODO

compute_barycentric_partial_component_scores <- function(partial_component_scores, alpha_weights){

  # mapply("*", partial_component_scores, (alpha_weights * length(alpha_weights)), SIMPLIFY = FALSE, USE.NAMES = TRUE)
  mapply("*", partial_component_scores, alpha_weights * length(partial_component_scores), SIMPLIFY = FALSE, USE.NAMES = TRUE)

}
