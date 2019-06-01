load('/Professional/Data/TinyNKI/connectivity_cubes.rda')


eig_vals <- apply(conn.cube.tog,3,eigen, only.values=TRUE)
# (eig.res$vectors %*% diag(eig.res$values) %*% t(eig.res$vectors)) / (conn.cube.tog[,,1])

eig.res <- eigen(conn.cube.tog[,,1])
tolerance.eig.res <- tolerance.eigen(conn.cube.tog[,,1], tol = sqrt(.Machine$double.eps))
tolerance.eig.res_2 <- tolerance.eigen(conn.cube.tog[,,1], tol = 1e-5)

cor_covstatis_results <- covstatis(conn.cube.tog)
## and now a visual
# plot_covstatis(cor_covstatis_results$compromise_component_scores, simplify2array(cor_covstatis_results$partial_component_scores), display.fis.names = T)
