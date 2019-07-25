#' Compute alphas
#'
#' TODO
#'
#' @export
#'
#' @param Z_matrix TODO
#' @param alpha_from_RV TODO. Default is TRUE.
#'
#' @return TODO
#'
#' @examples
#' TODO

compute_alphas <- function(centered_normed_matrices, alpha_from_RV = TRUE){

  centered_normed_matrices %>%
    lapply(., c) %>%
    do.call(cbind, .) %>%
    scale(., center = F, scale = alpha_from_RV) %>%
    svd(. , nu=0, nv=1 ) ->
    svd_Z_matrix

  (svd_Z_matrix$v / sum(svd_Z_matrix$v))

}
