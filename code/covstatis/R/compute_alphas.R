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

compute_alphas <- function(Z_matrix, alpha_from_RV = TRUE){
  ## we only need the one vector, so only get that.
  svd_of_Z_matrix <- svd( scale(Z_matrix, center = F, scale = alpha_from_RV), nu=0, nv=1 )
  (svd_of_Z_matrix$v / sum(svd_of_Z_matrix$v))
}
