## utils and stolen utils from other places, documented as necessary.


##
#' @export
#'
#' @title \code{is_ss_matrix}: test if a matrix is a square and symmetric matrix
#'
#' @description \code{is_ss_matrix} takes a matrix and tests if it is a square and symmetric matrix.
#'
#' @param x A matrix to test.
#' @param tol Tolerance precision to eliminate all abs(x) values below \code{tol}. Default is \code{.Machine$double.eps}.
#'
#' @return A boolean. TRUE if the matrix is a square and symmetric matrix, FALSE if the matrix is not.

is_ss_matrix <- function(x, tol = sqrt(.Machine$double.eps)){

  if(is.null(dim(x)) | !is.matrix(x)){
    stop("is.sspsd.matrix: X is not a matrix.")
  }
  # x[ x^2 < tol ] <- 0

  ## square
  if(nrow(x)!=ncol(x)){
    return(FALSE)
  }
  ## symmetric
  if(!isSymmetric.matrix(x, tol=tol)){
    return(FALSE)
  }

  return(TRUE)

}

##
#' @export
#'
#' @title \code{is_ss_dist_matrix}: test if a matrix is a square and symmetric distance matrix
#'
#' @description \code{is_ss_dist_matrix} takes a matrix and tests if it is a square and symmetric distance matrix.
#'
#' @param x A matrix to test.
#' @param tol Tolerance precision to eliminate all abs(x) values below \code{tol}. Default is \code{.Machine$double.eps}.
#'
#' @return A boolean. TRUE if the matrix is a square and symmetric distance matrix, FALSE if the matrix is not.

is_ss_dist_matrix <- function(x, tol = sqrt(.Machine$double.eps)){

  if(is.null(dim(x)) | !is.matrix(x)){
    stop("is.sspsd.matrix: X is not a matrix.")
  }
  # x[ x^2 < tol ] <- 0

  ## square
  if(nrow(x)!=ncol(x)){
    return(FALSE)
  }
  ## symmetric
  if(!isSymmetric.matrix(x, tol=tol)){
    return(FALSE)
  }
  ## diagonals are 0s
  # if( !identical(diag(x),rep(0,ncol(x))) ){
  if( abs(sum(diag(x)-rep(0,ncol(x)))) > tol){
    return(FALSE)
  }
  return(TRUE)

}

##
#' @export
#'
#' @title \code{is_sspsd_matrix}: test if a matrix square, symmetric, and positive semi-definite (sspsd) matrix
#'
#' @description \code{is_sspsd_matrix} takes a matrix and tests if it is a square, symmetric, and positive semi-definite (sspsd) matrix.
#'
#' @param x A matrix to test.
#' @param tol Tolerance precision to eliminate all abs(x) values below \code{tol}. Default is \code{.Machine$double.eps}.
#'
#' @return A boolean. TRUE if the matrix is a square, symmetric, and positive semi-definite (sspsd) matrix, FALSE if the matrix is not.

is_sspsd_matrix <- function(x, tol = sqrt(.Machine$double.eps)){

  if(is.null(dim(x)) | !is.matrix(x)){
    stop("is.sspsd.matrix: X is not a matrix.")
  }
  # x[ x^2 < tol ] <- 0

    ## square
  if(nrow(x)!=ncol(x)){
    return(FALSE)
  }
    ## symmetric
  if(!isSymmetric.matrix(x, tol=tol)){
    return(FALSE)
  }
    ## positive semi definite
  eigen.values <- eigen(x, symmetric = T, only.values = T)$values
  if(any(eigen.values < 0 & abs(eigen.values) > tol)){
    return(FALSE)
  }

  return(TRUE)

}

# Consider bringing this one back later.
#
# make_psd_matrix <- function(x, tol = sqrt(.Machine$double.eps)){
#
#   if(!is_ss_matrix(x)){
#     stop("make_psd_matrix: x is not square and symmetric")
#   }
#
#   # eigen_results <- eigen(x)
#   # if(any(is.complex(eigen_results$values))){
#   #   stop("make_psd_matrix: eigenvalues are complex")
#   # }
#   #
#   # if(all(abs(eigen_results$values)) < tol ){
#   #
#   # }
#
#   eigen_results <- tolerance.eigen(eigen_results, tol=tol)
#
#   ## I can make this faster.
#   (eigen_results$vectors %*% diag(eigen_results$values) %*% t(eigen_results$vectors))
# }


##
#' @export
#'
#' @title \code{array2list}: converts three dimensional arrays to a list of matrices
#'
#' @description \code{array2list} takes a three dimensional array and converts it to a list of length \code{dim(x)[[3]]},
#' where each matrix in the list has \code{dim(x)[[1]]} rows and  \code{dim(x)[[1]]} columns.
#'
#' @param x An array to convert to a list of matrices
#'
#' @note This function is copied directly from https://stackoverflow.com/questions/20198751/three-dimensional-array-to-list
#'
#' @return A boolean. TRUE if the matrix is a square and symmetric matrix, FALSE if the matrix is not.
array2list <- function(x){
  setNames(lapply(split(x, arrayInd(seq_along(x), dim(x))[, 3]),
                  array, dim = dim(x)[-3], dimnames(x)[-3]),dimnames(x)[[3]])
}


#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL
