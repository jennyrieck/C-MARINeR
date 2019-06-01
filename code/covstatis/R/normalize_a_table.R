#' Normalize a matrix
#'
#' TODO
#'
#' @export
#'
#' @param sspsd_matrix TODO
#' @param matrix_norm_type TODO. Default is SS1.
#' @param tol TODO. Default is 1e-12.
#'
#' @return TODO. A normalized matrix?
#'
#' @examples
#' TODO


## tol not currently used.
normalize_a_matrix <- function(sspsd_matrix, matrix_norm_type = "SS1", tol=1e-12){

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
    ## consider switching to geigen() or tolerance.eigen()
    eigen_results <- eigen(sspsd_matrix, symmetric = TRUE, only.values = TRUE)
    return(sspsd_matrix / eigen_results$values[1])
  }
  sspsd_matrix

}
