#' CovSTATIS
#'
#' @description Perhaps the statement here is not malformed anymore.
#' @details not malformed anymored.
#' @seealso \code{\link{GSVD}}
#' @examples
#' \dontrun{
#' ## hello
#' }
#'
#' @references
#' Abdi, H., Williams, L.J., Valentin, D., & Bennani-Dosse, M. (2012). STATIS and DISTATIS: Optimum multi-table principal component analysis and three way metric multidimensional scaling. \emph{Wiley Interdisciplinary Reviews: Computational Statistics}, 4, 124-167.
#'
#' @keywords multivariate svd generalized matrix decomposition variance component orthogonal diagonalization eigen
#'
#' @importFrom magrittr "%>%"
#' @importFrom stats sd setNames
#' @importFrom graphics abline plot points text
#' @import GSVD
"_PACKAGE"
## Stolen from https://github.com/jennybc/googlesheets/blob/master/R/googlesheets.R
if(getRversion() >= "2.15.1") utils::globalVariables(c("."))


##
#' @export
#'
#' @title CovSTATIS: STATIS for covariance matrices
#'
#' @description \code{covstatis} performs CovSTATIS: a multi-table PCA for multiple covariance (or correlation) matrices.
#' \code{covstatis} also allows for some variations in optimizations (per table, and similarity estimates). \code{cov_matrices} are assumed to be *proper* covariance or correlation matrices.
#'
#' @param cov_matrices array or list of matrices; each matrix (or slice of an array) must be a covariance or correlation matrix
#' @param matrix_norm_type character string. Normalization to perform per matrix (after double centering). Default is "MFA" (multiple factor analysis); other options include "SS1" (sums of squares 1), and "none".
#' @param alpha_from_RV boolean. Compute alpha weights from the Rv-coefficient matrix or not. Default is TRUE (Rv).
#' @param tolerance numeric >= 0. Tolerance value to use as an effective zero; any values below \code{tolerance} are considered 0. Default \code{sqrt(.Machine$double.eps)}. Multiple checks use this parameter.
#' @param strictly_enforce_psd boolean. Strictly enforce that all matrices conform to positive semi-definite (non-negative eigenvalues). Default is FALSE.
#'
#' @return List.
#' \item{compromise_component_scores}{a matrix. Contains the component scores (from the decomposition) of the compromise matrix.}
#' \item{partial_component_scores}{list (of matrices). Contains the partial component scores per matrix (which was projected onto the compromise space)}
#' \item{barycentric_partial_component_scores}{list (of matrices). Contains "barycentric" partial component scores. These are the \code{partial_component_scores} multipled by their respective weights (\code{alpha_weights} and the total number of tables. The mean of \code{barycentric_partial_component_scores} equals compromise_component_scores (hence, barycentric)}
#' \item{compromise_decomposition_results}{list. Contains results from \code{GSVD::\link[GSVD]{tolerance_eigen}} which has \code{$vectors} and \code{$values} from an eigen-decomposition.}
#' \item{compromise_matrix}{matrix. The compromise (weighted average) matrix of all matrices for decomposition.}
#' \item{alpha_weights}{vector. The weights per matrix for use to create the compromise}
#' \item{input_parameters}{list. This list contains the other input parameters for easier subsequent use: \code{matrix_norm_type}, \code{alpha_from_RV}, \code{tolerance}, \code{strictly_enforce_psd}}
#'
#' @details
#'
#' Users should take note that \code{cov_matrices} are assumed to be *proper* covariance or correlation matrices: square, symmetric, and positive semi-definite (i.e., non-negative eigenvalues).
#' However those assumptions of proper covariance or correlation matrices can be circumvented with \code{strictly_enforce_psd = FALSE}; even though it is the default, we do not recommend that.
#' Please check and ensure you have proper covariance matrices. There are a number of utilities in this package to help (e.g., \code{\link{is_sspsd_matrix}}).
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
#' @keywords multivariate diagonalization decomposition

covstatis <- function(cov_matrices, matrix_norm_type = "MFA", alpha_from_RV = TRUE, tolerance = sqrt(.Machine$double.eps), strictly_enforce_psd = FALSE){

  ## everything is list based for now
  if(is.array(cov_matrices)){
    if(nrow(cov_matrices)!=ncol(cov_matrices)){
      stop("covstatis: The number of rows does not equal the number of columns. Each matrix must be square, symmetric, and positive semi-definite.")
    }

    cov_matrices %>%
      array2list(.) ->
      cov_matrices
  }

  if(any(!sapply(cov_matrices, is_ss_matrix))){
    stop("covstatis: At least one matrix in the 'cov_matrices' list was not a square and symmetric matrix.")
  }

  # (0) double center each R table as S
    cov_matrices %>%
      double_center_matrices(.) ->
      cov_matrices

  ## a *strict* enforcement of PSD/PD
  if(strictly_enforce_psd){

    if(tolerance < 0){
      tolerance <- sqrt(.Machine$double.eps)
    }

    stopifnot(all(sapply(cov_matrices, is_sspsd_matrix, tol = tolerance)))
  }else{
    tolerance <- NA ## so that we can get negative eigens, if that's what you're into.
  }

  # (1) Normalize each table
  cov_matrices %>%
    normalize_matrices(., matrix_norm_type = matrix_norm_type) ->
    cov_matrices


  ####
  ##    BETWEEN HERE AND BELOW IS EFFECTIVELY A "CORE" CROSS-PRODUCT STATIS
  ##      At least until (5) but also possibly through all steps.
  ####

  # (2) Get the alpha weights
  cov_matrices %>%
    compute_alphas(., alpha_from_RV = alpha_from_RV) ->
    alpha_weights


  # (3) Make the compromise matrix
  make_compromise_matrix(cov_matrices, alpha_weights) ->
    compromise_matrix


  ## a *strict* enforcement of PSD/PD
  if(!is_sspsd_matrix(compromise_matrix, tol=1e-13)){
    if(strictly_enforce_psd){
      stop("covstatis: compromise_matrix is not positive semi-definite within `tolerance`")
    }
    warning("covstatis: compromise_matrix is not positive semi-definite within `tolerance`")
  }


  # (4) eigen of compromise
    ## this will need to become geigen() someday and then compute_covstatis_partial_component_scores() will change
    ## the alternative here, too, is to use svd and ignore $v (so only use $u and $d)
    ## however, I'm not a fan of that quite yet, as this currently helps illustrate the idea.
  compromise_matrix %>%
    tolerance_eigen(., symmetric = TRUE, tol = tolerance) ->
    compromise_decomposition_results

  ## here I need to ensure that tolerance_eigen or whatever is used actually sends back a class type
    ## I should probably switch to geigen() and rely on that class.

  stop()

  ## quick rethink: the scores should come from a function
    ## so the partial scores should too, and all of it should come from the decoposition results
  ### also make this as dead simple as possible.
  ### this is striking a balance between illustrative and functional, not end goal.

  # (5) compute compromise component scores
  (compromise_decomposition_results$vectors %*% diag(sqrt(compromise_decomposition_results$values))) ->
    compromise_component_scores



  # (6) compute partial (table) component scores
    ### break this out, even if it uses a loop (for clarity)
    ### that projection_matrix is actually improtant for subsequent use.
  # compute_covstatis_partial_component_scores(cov_matrices, compromise_decomposition_results) ->
  #   partial_component_scores

  # (6) compute the projection matrix
  (compromise_decomposition_results$vectors %*% diag(1/sqrt(compromise_decomposition_results$values))) ->
    projection_matrix

  # (7) compute partial (table) component scores


  ## perhaps the barycentric ones are to be dropped, as they sort of help illustrate the properties.

  # # (7) compute weighted partial (table) component scores
  # compute_covstatis_barycentric_partial_component_scores(partial_component_scores, alpha_weights) ->
  #   barycentric_partial_component_scores

  ####
  ##    BETWEEN HERE AND ABOVE IS EFFECTIVELY A "CORE" CROSS-PRODUCT STATIS
  ####


  rownames(compromise_component_scores) <- rownames(cov_matrices[[1]]) -> rownames(compromise_decomposition_results$vectors)
  partial_component_scores <- mapply(function(scores,covs){rownames(scores) <- rownames(covs); scores}, partial_component_scores, cov_matrices, SIMPLIFY = FALSE, USE.NAMES = TRUE)
  # barycentric_partial_component_scores <- mapply(function(scores,covs){rownames(scores) <- rownames(covs); scores}, barycentric_partial_component_scores, cov_matrices, SIMPLIFY = FALSE, USE.NAMES = TRUE)


  ## should use a class here for the prints.
  return(list(
    compromise_component_scores = compromise_component_scores,
    partial_component_scores = partial_component_scores,
    # barycentric_partial_component_scores = barycentric_partial_component_scores,
    compromise_decomposition_results = compromise_decomposition_results,
    compromise_matrix = compromise_matrix,
    alpha_weights = alpha_weights,
    input_parameters = list(matrix_norm_type = matrix_norm_type, alpha_from_RV = alpha_from_RV, tolerance = tolerance, strictly_enforce_psd = strictly_enforce_psd)
  ))

}
