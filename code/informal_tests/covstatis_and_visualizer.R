

require(ExPosition)
require(devtools)
require(magrittr)

data("jocn.2005.fmri")
# load_all("../covstatis/")
load_all(".")

cov_matrices <- list(images=-as.matrix(jocn.2005.fmri$images$data),
                     scans=-as.matrix(jocn.2005.fmri$scans$data))


covstatis_results <- covstatis(cov_matrices)

## and now a visual
plot_covstatis(covstatis_results$compromise_component_scores, simplify2array(covstatis_results$partial_component_scores), display.fis.names = T)
