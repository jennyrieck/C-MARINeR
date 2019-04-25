double_center_tables <- function(cov_matrices){


  sapply(cov_matrices, double_center_a_table, simplify = FALSE, USE.NAMES = TRUE)
}
