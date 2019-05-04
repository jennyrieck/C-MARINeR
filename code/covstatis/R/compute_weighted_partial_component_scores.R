compute_weighted_partial_component_scores <- function(partial_component_scores, alpha_weights){

  sapply(cov_matrices,
         function(table, weight){table*weight},
         weight=alpha_weights,
         simplify = FALSE,
         USE.NAMES = TRUE)

}
