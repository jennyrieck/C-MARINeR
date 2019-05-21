

require(ExPosition)
require(devtools)
require(magrittr)

data("jocn.2005.fmri")
# load_all("../covstatis/")
load_all(".")

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


## and now a visual
plot_covstatis(covstatis_results$compromise_component_scores, simplify2array(covstatis_results$partial_component_scores), display.fis.names = T)
