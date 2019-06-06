## here I want to test sapply vs. vapply so that I can understand the differences
  ## and go with the safer vapply option

require(ExPosition)
require(devtools)
require(magrittr)

data("jocn.2005.fmri")
# load_all("../covstatis/")
load_all(".")

cov_matrices <- list(images=-as.matrix(jocn.2005.fmri$images$data),
                     scans=-as.matrix(jocn.2005.fmri$scans$data))




# sspsd_matrix / sqrt(sum(sspsd_matrix^2))

sapply_res <- sapply(cov_matrices, function(x){ x / sqrt(sum(x^2)) }, simplify = FALSE, USE.NAMES = TRUE)
norm_mat_res <- normalize_matrices(cov_matrices)

  ## nah. we'll go with sapply for now and use checks before we actually use that function.
vapply_res <- vapply(cov_matrices, function(x){ x / sqrt(sum(x^2)) }, FUN.VALUE = matrix(0,nrow(cov_matrices[[1]]),ncol(cov_matrices[[1]])))