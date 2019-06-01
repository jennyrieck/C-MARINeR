## double center tests
  ### need to figure out the best way to do this that is also the fastest.

require(rbenchmark)
require(microbenchmark)
require(ggplot2)
require(ExPosition)

data("jocn.2005.fmri")
sspsd_mat <- (-1) * as.matrix(jocn.2005.fmri$images$data)


nI <- nrow(sspsd_mat)
centering_mat <- matrix(-1/nI, nI, nI)
diag(centering_mat) <- rep(1 - (1/nI), nI)

db.mat <- (1/2)*(centering_mat %*% sspsd_mat %*% centering_mat)
db.mat2 <- (centering_mat %*% sspsd_mat %*% centering_mat)/2


## from here: https://stackoverflow.com/questions/43639063/double-centering-in-r
R <- sspsd_mat*0 + rowMeans(sspsd_mat)
C <- t(sspsd_mat*0 + colMeans(sspsd_mat))
M_double_centered <- (sspsd_mat - R - C + mean(sspsd_mat[]))/2


## from multivariance:::double.center
another_dc_mat <- (sspsd_mat - outer(rowMeans(sspsd_mat),colMeans(sspsd_mat), FUN ="+") + mean(sspsd_mat))/2

NOT_ALLOWED_dc_mat <- (.Call(stats:::C_DoubleCentre, sspsd_mat)/2)
