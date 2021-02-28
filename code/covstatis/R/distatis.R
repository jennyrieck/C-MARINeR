##
#' @export
#'
#' @title DiSTATIS: STATIS for distance matrices (i.e., multi-table MDS)
#'
#' @description \code{distatis} performs DiSTATIS: a multi-table MDS for multiple distance matrices.
#' \code{distatis} also allows for some variations in optimizations (per table, and similarity estimates). \code{squared_dist_matrices} are assumed to be *squared* distances in the matrices.
#'
#' @param squared_dist_matrices array or list of matrices; each matrix (or slice of an array) must be a squared distance matrix
#' @param matrix_norm_type character string. Normalization to perform per matrix (after double centering). Default is "MFA" (multiple factor analysis); other options include "SS1" (sums of squares 1), and "none".
#' @param alpha_from_RV boolean. Compute alpha weights from the Rv-coefficient matrix or not. Default is TRUE (Rv).
#' @param tolerance numeric >= 0. Tolerance value to use as an effective zero; any values below \code{tolerance} are considered 0. Default \code{sqrt(.Machine$double.eps)}. Multiple checks use this parameter.
#' @param strictly_enforce_psd boolean. Strictly enforce that all matrices conform to positive semi-definite (non-negative eigenvalues). Default is FALSE.
#'
#' @return List.
#' \item{compromise_component_scores}{a matrix. Contains the component scores (from the decomposition) of the compromise matrix.}
#' \item{partial_component_scores}{list (of matrices). Contains the partial component scores per matrix (which was projected onto the compromise space)}
#' \item{barycentric_partial_component_scores}{list (of matrices). Contains "barycentric" partial component scores. These are the \code{partial_component_scores} multipled by their respective weights (\code{alpha_weights} and the total number of tables. The mean of \code{barycentric_partial_component_scores} equals compromise_component_scores (hence, barycentric)}
#' \item{compromise_decomposition_results}{list. Contains results from \code{\link[GSVD]{tolerance_eigen}} which has \code{$vectors} and \code{$values} from an eigen-decomposition.}
#' \item{compromise_matrix}{matrix. The compromise (weighted average) matrix of all matrices for decomposition.}
#' \item{alpha_weights}{vector. The weights per matrix for use to create the compromise}
#' \item{input_parameters}{list. This list contains the other input parameters for easier subsequent use: \code{matrix_norm_type}, \code{alpha_from_RV}, \code{tolerance}, \code{strictly_enforce_psd}}
#'
#' @details
#'
#' Users should take note that \code{squared_dist_matrices} are assumed to be distances matrices with squared distances: square, symmetric, zeros on the diagonals, and squared distances off-diagonals.
#' Please check and ensure you have squared distance matrices. There are a number of utilities in this package to help (e.g., \code{\link{is_ss_dist_matrix}}).
#'
#' Additionally, \code{distatis} is just a specific implementation of \code{covstatis}, where the distance matrices are preprocessed in a way to make them akin to covariance matrices.
#' Thus, what is returned from \code{distatis} is identical to \code{covstatis}.
#'
#' @references
#' Abdi, H., Williams, L.J., Valentin, D., & Bennani-Dosse, M. (2012). STATIS and DISTATIS: Optimum multi-table principal component analysis and three way metric multidimensional scaling. \emph{Wiley Interdisciplinary Reviews: Computational Statistics}, 4, 124-167.
#'
#' @seealso
#' \code{\link{distatis}}, \code{\link{GSVD}}
#'
#' @examples
#' \dontrun{
#' ## hello
#' }
#'
#' @keywords multivariate, diagonalization, decomposition

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
