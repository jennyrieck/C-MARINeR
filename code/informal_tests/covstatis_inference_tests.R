
library(tidyverse)
library(devtools)
library(GSVD)

load_all()

# load('/Professional/Data/TinyNKI/connectivity_cubes.rda')
load('/Data/TinyNKI/connectivity_cubes.rda')


cor_covstatis_results <- covstatis(conn.cube.tog, strictly_enforce_psd = FALSE)

  ### this is way slower than it should be...
covstatis_inference_results <- covstatis_inference(conn.cube.tog, cor_covstatis_results)



apply(simplify2array(covstatis_inference_results$covstatis_bootstrap_results$boot_compromise_component_scores),c(1,2), mean)

# plot(cor_covstatis_results$compromise_component_scores)
# points(apply(simplify2array(covstatis_inference_results$covstatis_bootstrap_results$boot_compromise_component_scores),c(1,2), mean), col="red", pch=20)
