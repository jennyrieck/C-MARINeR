covstatis <- function(cov_matrices, table_norm_type = "SS1"){

  ## on entry of the function, everything in cov_matrices must be square symmetric
      ## this is so we can rely on eigen(..., symmetric=TRUE)
  ## actually I think I'd prefer to keep these as a list for now...

  ## preliminary checks.
  if(is.list(cov_matrices)){
    # check sizes & properties
    if(any(!unlist(lapply(cov_matrices, is.ss.matrix)))){
      stop("At least one item in the 'cov_matrices' list was not a square & symmetric matrix.")
    }else{
      #cov_matrices_is_list <- TRUE
      cov_matrices <- simplify2array(cov_matrices)
    }

  }
  if(is.array(cov_matrices)){
    if(nrow(cov_matrices)!=ncol(cov_matrices)){
      stop("The number of rows does not equal the number of columns. Each matrix must be square, symmetric, and positive semi-definite.")
    }
  }

  ## note: cmdscale does this:
    ## part of this is useful for covstatis, all of it for distatis
  # x <- as.matrix(d^2)
  # x <- .Call(C_DoubleCentre, x)
  # e <- eigen(-x/2, symmetric = TRUE)

  # a large question: should the masses apply to each table, or only as consideration for the compromise?
    ## I believe for now it's best for just the compromise, as the masses are intended to constrain the model
    ## *if* there is some normalization needed per table, the user must handle that on their own.

  # steps (after checks):
  # (0) double center each R table as S
  # (1) Normalize each table (consider inclusion of masses)
    # (a) MFA or
    # (b) sqrt SS of each element (default)
  # (2) Compute C / alpha weights (consider inclusion of masses)
    # (a) C = ZZt = [vec{S}...vec{S}][vec{S}...vec{S}]t
    # (b) eigen(C) (return & visualize C?)
    # (c) alpha = u / sum(u)
  # (3) Compute STATIS
    # (a) S+ = sum a*S
    # (b) eigen(S+) or geigen(S,M)


  ## I should overload the methods... i.e., double_center(list, ) or double_center(matrix, )
  ## I should actually just finish the core of this then deal with that.

  # double center all matrices...
  #cov_matrices <- lapply(cov_matrices, double_center_sspsd)
  # cov_matrices <- double_center_tables(cov_matrices)
    ## alt:
     # cov_matrices <- sapply(cov_matrices, double_center_sspsd, simplify = FALSE, USE.NAMES = TRUE)

  ## then test for psd? can that be integrated with the norm?
  # norm all matrices
  # cov_matrices <- norm_tables(cov_matrices, table_norm_type)
  # cov_matrices <- normalize_tables(cov_matrices, table_norm_type = table_norm_type)

  ## alt:
  cov_matrices %>%
    double_center_tables() %>%
    normalize_tables(., table_norm_type = table_norm_type) ->
    cov_matrices

  ## test PSD here, *if* we use non-eigen normalizations
    ## else, use the eige-norms to also test.

  if(table_norm_type != "MFA"){ ## this will have to expand to variance conditions
    cov_matrices %>%
      lapply(., is.sspsd.matrix)
        ## the is.sspsd.matrix() function should stop for us.
  }

  cov_matrices %>%
    lapply(., c) %>%
    do.call(cbind, .) ->
    Z_matrix
      ## I want to keep Z around because the similarity matrix can be visualized, and more things could be done with Z later.

  Z_matrix %>%
    compute_alphas() ->
    alpha_weights

  make_compromise_matrix(cov_matrices, alpha_weights) ->
    compromise_matrix

  eigen(compromise_matrix, symmetric = TRUE) %>%
    eigen_tolerance(.) ->
    compromise_eigen_results

  (compromise_eigen_results$vectors %*% diag(sqrt(compromise_eigen_results$values))) ->
    compromise_component_scores

  compute_partial_component_scores(cov_matrices, compromise_eigen_results) ->
    partial_component_scores

  compute_weighted_partial_component_scores(partial_component_scores, alpha_weights) ->
    weighted_partial_component_scores



  # ## I could just do:
      ## but shouldn't because I should test the PSD-ness
  # cov_matrices %>%
  #   double_center_tables() %>%
  #   normalize_tables(., table_norm_type = table_norm_type) %>%
  #   lapply(., c) %>%
  #   do.call(cbind, .) ->
  # Z_matrix

  ## unsure whether to do this here or call off to a function.
    # I want to preserve Z or C
  ## compute C
  # Z_matrix <- do.call(cbind,lapply(cov_matrices, c))
  # Z_matrix_svd <- eigen(C_matrix)
  # Z_matrix_svd$v[,1] / sum(Z_matrix_svd$v[,1])

}
