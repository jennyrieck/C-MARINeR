## DB: I don't recall what this was for.

#' Compute individual component scores relative to compromise component score.
#'
#' TODO
#'
#' @export
#'
#' @param covstatis_results TODO
#'
#' @return TODO


compute_relative_distance<-function(covstatis_results){

  sapply(covstatis_results$partial_component_scores,
         function(partial_scores, compromise){partial_scores - compromise},
         compromise=covstatis_results$compromise_component_scores,
         simplify = FALSE,
         USE.NAMES = TRUE)

}
