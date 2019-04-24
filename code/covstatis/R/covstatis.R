covstatis <- function(cov_matrices, are_distances=FALSE){

  ## preliminary checks.
  if(is.list(cov_matrices)){
    # check sizes & properties
    if(any(!unlist(lapply(cov_matrices, is.ss.matrix)))){
      stop("At least one item in the 'cov_matrices' list was not a square & symmetric matrix.")
    }else{
      cov_matrices_is_list <- TRUE
      # cov_matrices <- simplify2array(cov_matrices)
    }

  }
  if(is.array(cov_matrices)){
    if(nrow(cov_matrices)!=ncol(cov_matrices)){
      stop("The number of rows does not equal the number of columns. Each matrix must be square, symmetric, and positive semi-definite.")
    }
  }

    ## we won't deal with distances at this time... only cov/cors
  if(are_distances){
    # diagonal must be 0
  }
  if(!are_distances){
    # diagonal must not be 0
  }
  ## note: cmdscale does this:
  # x <- as.matrix(d^2)
  # x <- .Call(C_DoubleCentre, x)
  # e <- eigen(-x/2, symmetric = TRUE)

  ## steps:
  # (1) take in correlation, covariance, or distance matrices
    # (1a) convert distance to covariance
    # (1b) convert cov/cor to crossproduct matrices
  # (2) perform sspsd tests
  # (3) normalize each matrix
  # (4) run cross-product STATIS
    # lots of steps here.

  # note: steps 1 and 3 could possibly be switched in the case of covariance matrices
    ## but I think it's better to norm after we've done all of our steps.


  ## go back and walk through all the statis steps...


  ## cross product STATIS is:
    # (1) we need a bunch of cross-product matrices (Sk)
    # (2) we need the alphas of the Sks
    # (3) compromise
    # (4) (generalized?) eigen


}
