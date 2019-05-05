compute_weighted_partial_component_scores <- function(partial_component_scores, alpha_weights){

  ## major question left:
    ## which is more appropriate to return?

  # mapply("*", partial_component_scores, (alpha_weights * length(alpha_weights)), SIMPLIFY = FALSE, USE.NAMES = TRUE)
  mapply("*", partial_component_scores, alpha_weights, SIMPLIFY = FALSE, USE.NAMES = TRUE)

}
