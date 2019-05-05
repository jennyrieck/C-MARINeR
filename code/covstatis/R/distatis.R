distatis <- function(squared_dist_matrices, table_norm_type = "MFA", alpha_from_RV = TRUE, compact = TRUE){

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

  covstatis(sapply(squared_dist_matrices,"*", -1, simplify = FALSE, USE.NAMES = TRUE))
}
