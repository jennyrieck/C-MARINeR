

require(ExPosition)
require(devtools)
require(magrittr)

data("jocn.2005.fmri")
# load_all("../covstatis/")
load_all(".")

# dist_matrices <- list(images=as.matrix(jocn.2005.fmri$images$data),
#                      scans=as.matrix(jocn.2005.fmri$scans$data))
# 
# my_res <- distatis(dist_matrices)
# ha_res <- DistatisR::distatis(simplify2array(dist_matrices))
## and now a visual
# plot_covstatis(my_res$compromise_component_scores, simplify2array(my_res$partial_component_scores), display.fis.names = T)


cov_matrices <- list(images=-as.matrix(jocn.2005.fmri$images$data),
                     scans=-as.matrix(jocn.2005.fmri$scans$data))

covstatis_results <- covstatis(cov_matrices)
## covstatis has multiple options (the top is the default):
# covstatis_results <- covstatis(cov_matrices, table_norm_type = "MFA", alpha_from_RV = TRUE)
# covstatis_results <- covstatis(cov_matrices, table_norm_type = "MFA", alpha_from_RV = FALSE)
# covstatis_results <- covstatis(cov_matrices, table_norm_type = "SS1", alpha_from_RV = TRUE)
# covstatis_results <- covstatis(cov_matrices, table_norm_type = "SS1", alpha_from_RV = FALSE)
# covstatis_results <- covstatis(cov_matrices, table_norm_type = "none", alpha_from_RV = TRUE)
# covstatis_results <- covstatis(cov_matrices, table_norm_type = "none", alpha_from_RV = FALSE)



# barys <- compute_barycentric_partial_component_scores(covstatis_results$partial_component_scores, covstatis_results$alpha_weights)


## and now a visual
plot_covstatis(covstatis_results$compromise_component_scores, simplify2array(covstatis_results$partial_component_scores), display.fis.names = T)



data("ep.iris")
setosa_correlation_matrix <- cor(ep.iris$data[which(ep.iris$design[,1]==1),])
versicolor_correlation_matrix <- cor(ep.iris$data[which(ep.iris$design[,2]==1),])
virginica_correlation_matrix <- cor(ep.iris$data[which(ep.iris$design[,3]==1),])


correlation_matrix_list <- list(
  setosa = setosa_correlation_matrix,
  versicolor = versicolor_correlation_matrix,
  virginica = virginica_correlation_matrix
)


cor_covstatis_results <- covstatis(correlation_matrix_list)

barys <- compute_barycentric_partial_component_scores(cor_covstatis_results$partial_component_scores, cor_covstatis_results$alpha_weights)


## and now a visual
plot_covstatis(cor_covstatis_results$compromise_component_scores, simplify2array(cor_covstatis_results$partial_component_scores), display.fis.names = T)
