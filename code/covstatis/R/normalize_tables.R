#' Normalize matrices
#'
#' TODO
#'
#' @export
#'
#' @param cov_matrices TODO
#' @param matrix_norm_type TODO. Defaults is SS1.
#'
#' @return TODO
#'
#' @examples
#' TODO

normalize_matrices <- function(cov_matrices, matrix_norm_type = c("SS1")){

  valid_matrix_norm_types <- c("SS1","MFA","none")

  if(length(matrix_norm_type) > 1){
    stop("Please select only one 'matrix_norm_type'")
  }
  if(!(matrix_norm_type %in% valid_matrix_norm_types)){
    warning("Choice of 'matrix_norm_type' not recognized. Returning 'cov_matrices' as is")
  }

  ### sapply is not safe; change over to vapply
  sapply(cov_matrices, normalize_a_table, matrix_norm_type = matrix_norm_type, simplify = FALSE, USE.NAMES = TRUE)
}
