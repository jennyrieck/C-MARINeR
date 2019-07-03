
library(tidyverse)
library(devtools)
library(GSVD)

load_all()

# load('/Professional/Data/TinyNKI/connectivity_cubes.rda')
load('/Data/TinyNKI/connectivity_cubes.rda')


cor_covstatis_results <- covstatis(conn.cube.tog, strictly_enforce_psd = FALSE)

# ### this is way slower than it should be...
covstatis_bootstrap_results <- covstatis_bootstrap(conn.cube.tog, cor_covstatis_results, iterations = 100)

plot(cor_covstatis_results$compromise_component_scores)
points(apply(simplify2array(covstatis_bootstrap_results$boot_compromise_component_scores),c(1,2), mean), col="red", pch=20)


# 
# conn.cube.tog <- covstatis::array2list(conn.cube.tog)
# 
# # cor_covstatis_results
# ### test it here...
# bootstrap_sample <- sample(1:length(conn.cube.tog), replace = T)
# 
# conn.cube.tog[bootstrap_sample] %>%
#   double_center_matrices(.) ->
#   boot_cov_matrices
# 
# 
# ## (1) make the alphas
# boot_cov_matrices %>%
#   normalize_matrices(., matrix_norm_type = cor_covstatis_results$input_parameters$matrix_norm_type) ->
#   boot_cov_matrices
# 
# boot_cov_matrices %>%
#   lapply(., c) %>%
#   do.call(cbind, .) %>%
#   compute_alphas(., alpha_from_RV = cor_covstatis_results$input_parameters$alpha_from_RV) ->
#   boot_alpha_weights
# 
# 
# boot_compromise <- make_compromise_matrix(boot_cov_matrices, boot_alpha_weights)
# 
# ## (2 & 3) make compromise & project
# make_compromise_matrix(boot_cov_matrices, boot_alpha_weights) %*% (cor_covstatis_results$compromise_decomposition_results$vectors %*% diag(1/sqrt(cor_covstatis_results$compromise_decomposition_results$values))) ->
#   boot_compromise_component_scores
# 
# 
# 
# ### this is way slower than it should be...
covstatis_inference_results <- covstatis_inference(conn.cube.tog, cor_covstatis_results)
# 
# 
# apply(simplify2array(covstatis_inference_results$covstatis_bootstrap_results$boot_compromise_component_scores),c(1,2), mean)

plot(cor_covstatis_results$compromise_component_scores)
points(apply(simplify2array(covstatis_inference_results$covstatis_bootstrap_results$boot_compromise_component_scores),c(1,2), mean), col="red", pch=20)






