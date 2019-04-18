covstatis <- function(){
  
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
  
}