covstatis <- function(cov_matrices, table_norm_type = c("SS1")){

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


  # double center all matrices...

  ## then test for psd? can that be integrated with the norm?
  # norm all matrices
  norm_tables(cov_matrices, table_norm_type)


}
