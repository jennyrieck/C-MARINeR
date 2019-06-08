#' TODO: TITLE
#'
#' TODO
#'
#' @export
#'
#' @param squared_dist_matrices TODO
#' @param matrix_norm_type TODO. Default is MFA.
#' @param alpha_from_RV TODO. Default is TRUE.
#' @param compact TODO. Default is TRUE.
#'
#' @return TODO
#'
#' @examples
#' TODO

distatis <- function(squared_dist_matrices, matrix_norm_type = "MFA", alpha_from_RV = TRUE, tolerance = sqrt(.Machine$double.eps), strictly_enforce_psd = FALSE){

  if(is.array(squared_dist_matrices)){
    if(nrow(squared_dist_matrices)!=ncol(squared_dist_matrices)){
      stop("distatis: The number of rows does not equal the number of columns. Each matrix must be square, symmetric, and positive semi-definite.")
    }

    squared_dist_matrices %>%
      array2list(.) ->
      squared_dist_matrices
  }

  if(is.list(squared_dist_matrices)){
    if(any(!sapply(squared_dist_matrices, is_ss_dist_matrix))){
      stop("distatis: At least one matrix in the 'squared_dist_matrices' list was not a square and symmetric matrix with 0s on the diagonal.")
    }
  }

  covstatis(sapply(squared_dist_matrices,"*", -1, simplify = FALSE, USE.NAMES = TRUE),
            matrix_norm_type = matrix_norm_type, alpha_from_RV = alpha_from_RV, tolerance = tolerance, strictly_enforce_psd = strictly_enforce_psd)
}
