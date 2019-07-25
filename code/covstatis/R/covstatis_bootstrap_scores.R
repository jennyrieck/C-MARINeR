### this is slower than I expected it to be.
### this may only apply to covstatis/distatis for now.

covstatis_bootstrap_scores <- function(covstatis_results, iterations = 100){

  boot_compromise_component_scores_list <- list()

  for(i in 1:iterations){

    bootstrap_sample <- sample(1:length(covstatis_results$barycentric_partial_component_scores), replace = T)

    apply(simplify2array(covstatis_results$partial_component_scores[bootstrap_sample]),c(1,2),mean) ->
        boot_compromise_component_scores_list[[i]]

  }


  ### there has to be a way to make this faster/more efficient...

  boot.cube.mean <- apply(simplify2array(boot_compromise_component_scores_list), c(1, 2), mean)
  boot.cube.dev <- sapply(boot_compromise_component_scores_list,
                          function(this_slice, mean_slice){(this_slice - mean_slice)^2},
                          mean_slice = boot.cube.mean,
                          simplify = FALSE,
                          USE.NAMES = TRUE)
  boot.ratios <- boot.cube.mean / (sqrt(apply(simplify2array(boot.cube.dev), c(1, 2), mean)))
  boot.ratios[which(is.infinite(boot.ratios))] <- 0

  return(
    list(boot_compromise_component_scores_list = boot_compromise_component_scores_list,
         boot.ratios = boot.ratios)
  )}
