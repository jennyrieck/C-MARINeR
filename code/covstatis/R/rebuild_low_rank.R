#' Rebuild low-rank matrices for compromise and each individual matrix
#'
#' TODO
#'
#' @export
#'
#' @param covstatis_results TODO
#' @param components_to_rebuild TODO
#'
#' @return TODO
#'
#' @examples
#' TODO


rebuild_low_rank<-function(covstatis_results, components_to_rebuild){

  if(components_to_rebuild > dim(covstatis_results$compromise_component_scores)[2]){
    warning('# of components to rebuild is greater than # of components returned')
  }

  rebuilt_compromise<-tcrossprod(covstatis_results$compromise_component_scores[,seq(1,components_to_rebuild)])

  rebuilt_individuals<-sapply(covstatis_results$partial_component_scores,
                              function(partial_scores){tcrossprod(partial_scores[,seq(1,components_to_rebuild)])},
                              simplify = FALSE,
                              USE.NAMES = TRUE)

  rebuilt_barycentric_individuals<-sapply(covstatis_results$barycentric_partial_component_scores,
                                          function(bary_partial_scores){tcrossprod(bary_partial_scores[,seq(1,components_to_rebuild)])},
                                          simplify = FALSE,
                                          USE.NAMES = TRUE)
  return(list(
    rebuilt_compromise = rebuilt_compromise,
    rebuilt_individuals = rebuilt_individuals,
    rebuilt_barycentric_individuals = rebuilt_barycentric_individuals,
    components_to_rebuild = components_to_rebuild
  ))
}
