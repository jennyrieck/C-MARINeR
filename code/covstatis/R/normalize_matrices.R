#' Normalize matrices
#'
#' TODO
#'
#' @export
#'
#' @param centered_matrices TODO
#' @param matrix_norm_type TODO. Defaults is SS1.
#'
#' @return TODO
#'
#' @examples
#' TODO

normalize_matrices <- function(centered_matrices, matrix_norm_type = c("SS1")){

  valid_matrix_norm_types <- c("SS1","MFA","none")

  if(length(matrix_norm_type) > 1){
    stop("Please select only one 'matrix_norm_type'")
  }
  if(!(matrix_norm_type %in% valid_matrix_norm_types)){
    warning("Choice of 'matrix_norm_type' not recognized. Returning 'centered_matrices' as is")
  }

  ### sapply is not safe; change over to vapply
    ## correction: vapply is hard to use in this case. consider lapply or ensure checks before this.
    ## the function downstream is actually checking. so that's OK.
  sapply(centered_matrices, normalize_a_matrix, matrix_norm_type = matrix_norm_type, simplify = FALSE, USE.NAMES = TRUE)
}



#' Normalize a matrix
#'
#' TODO
#'
#' @export
#'
#' @param sspsd_matrix TODO
#' @param matrix_norm_type TODO. Default is SS1.
#'
#' @return TODO. A normalized matrix?
#'
#' @examples
#' TODO


## tol not currently used.
normalize_a_matrix <- function(sspsd_matrix, matrix_norm_type = "SS1"){

  sspsd_matrix %>%
    is_ss_matrix %>%
    stopifnot

  valid_matrix_norm_types <- c("SS1","MFA","none")

  if(length(matrix_norm_type) > 1){
    stop("Please select only one 'matrix_norm_type'")
  }
  if(!(matrix_norm_type %in% valid_matrix_norm_types)){
    warning("Choice of 'matrix_norm_type' not recognized. Returning 'sspsd_matrix' as is")
  }


  ## so now I'll do the if-else myself...
  if(matrix_norm_type=="SS1"){
    return(sspsd_matrix / sqrt(sum(sspsd_matrix^2)))
  }
  if(matrix_norm_type=="MFA"){
    ### this will not perform the tolerance checks. that is a job for other places in the pipeline.
    eigen_results <- tolerance.eigen(sspsd_matrix, symmetric = TRUE, only.values = TRUE, tol = NA)
    return(sspsd_matrix / eigen_results$values[1])
  }
  sspsd_matrix

}
