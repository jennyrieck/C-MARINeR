## eigen-y performance tests.

require(rbenchmark)
require(microbenchmark)
require(GSVD)
require(ggplot2)


benchmark(
  "eigen" = {
    eigen(cor(matrix(rnorm(1000), 100, 10)))$values
  },
  "eigen_symmetric" = {
    eigen(cor(matrix(rnorm(1000), 100, 10)), symmetric = TRUE)$values
  },
  "svd" = {
    svd(cor(matrix(rnorm(1000), 100, 10)))$d
  },
  "gsvd" = {
    gsvd(cor(matrix(rnorm(1000), 100, 10)))$d
  },
  "tolerance.svd" = {
    tolerance.svd(cor(matrix(rnorm(1000), 100, 10)))$d
  },
  replications = 1000,
  columns = c("test", "replications", "elapsed",
              "relative", "user.self", "sys.self")
)




mbm <- microbenchmark(
  "eigen" = {
    eigen(cor(matrix(rnorm(1000), 100, 10)))$values
  },
  "eigen_symmetric" = {
    eigen(cor(matrix(rnorm(1000), 100, 10)), symmetric = TRUE)$values
  },
  "svd" = {
    svd(cor(matrix(rnorm(1000), 100, 10)))$d
  },
  "gsvd" = {
    gsvd(cor(matrix(rnorm(1000), 100, 10)))$d
  },
  "tolerance.svd" = {
    tolerance.svd(cor(matrix(rnorm(1000), 100, 10)))$d
  }, times = 1000
)

mbm

# autoplot(mbm)



mbm <- microbenchmark(
  "scores1" = {
    eigen_res <- eigen(cor(matrix(rnorm(5000), 100, 50)), symmetric = TRUE)
    (eigen_res$vectors %*% diag(sqrt(eigen_res$values)))
  },
  "scores2" = {
    eigen_res <- eigen(cor(matrix(rnorm(5000), 100, 50)), symmetric = TRUE)
    (eigen_res$vectors %>% sweep(., 2, sqrt(eigen_res$values),"*"))
  }, times = 1000
)
