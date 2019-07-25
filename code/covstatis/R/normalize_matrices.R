##
#' @export
#'
#' @title normalize_matrices: normalize a list of matrices
#'
#' @param centered_matrices list. A list of double centered matrices to be normalized.
#' @param matrix_norm_type character string. Normalization type for each \code{centered_matrices}. Options are "MFA", "SS1", and "none"
#'
#' @return list. A list of normalized matrices

normalize_matrices <- function(centered_matrices, matrix_norm_type = "MFA"){

  valid_matrix_norm_types <- c("SS1","MFA","none")

  if(length(matrix_norm_type) > 1){
    stop("Please select only one 'matrix_norm_type'")
  }
  if(!(matrix_norm_type %in% valid_matrix_norm_types)){
    warning("Choice of 'matrix_norm_type' not recognized. Returning 'centered_matrices' as is")
  }

  sapply(centered_matrices, normalize_a_matrix, matrix_norm_type = matrix_norm_type, simplify = FALSE, USE.NAMES = TRUE)
}



##
#' @export
#'
#' @title normalize_a_matrix: normalize a double centered matrix
#'
#' @param centered_matrix matrix. A double centered matrix to be normalized
#' @param matrix_norm_type character string. Normalization type for \code{centered_matrix}. Options are "MFA", "SS1", and "none"
#'
#' @return list. A list of normalized matrices

normalize_a_matrix <- function(centered_matrix, matrix_norm_type = "MFA"){

  centered_matrix %>%
    is_ss_matrix %>%
    stopifnot

  valid_matrix_norm_types <- c("SS1","MFA","none")

  if(length(matrix_norm_type) > 1){
    stop("Please select only one 'matrix_norm_type'")
  }
  if(!(matrix_norm_type %in% valid_matrix_norm_types)){
    warning("Choice of 'matrix_norm_type' not recognized. Returning 'centered_matrix' as is")
  }


  ## so now I'll do the if-else myself...
  if(matrix_norm_type=="SS1"){
    return(centered_matrix / sqrt(sum(centered_matrix^2)))
  }
  if(matrix_norm_type=="MFA"){
    ### this will not perform the tolerance checks. that is a job for other places in the pipeline.
    eigen_results <- tolerance.eigen(centered_matrix, symmetric = TRUE, only.values = TRUE, tol = NA)
    return(centered_matrix / eigen_results$values[1])
  }
  centered_matrix

}
