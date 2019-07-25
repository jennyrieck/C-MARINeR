##
#' @export
#'
#' @title compute_alphas: compute alpha weights per matrix
#'
#' @param centered_normed_matrices list. These must be the normalized cross-product matrices, i.e., they have already been double centered then normalzied.
#' @param alpha_from_RV boolean. Default is TRUE. If TRUE, this scales the vectorized \code{centered_normed_matrices} and provides weights based on the Rv-coefficient. Else (FALSE) the vectorized matrices are only centered.
#'
#' @return vector. These are the weights per matrix to make the compromise.

compute_alphas <- function(centered_normed_matrices, alpha_from_RV = TRUE){

  centered_normed_matrices %>%
    lapply(., c) %>%
    do.call(cbind, .) %>%
    scale(., center = F, scale = alpha_from_RV) %>%
    svd(. , nu=0, nv=1 ) ->
    svd_Z_matrix

  (svd_Z_matrix$v / sum(svd_Z_matrix$v))

}
