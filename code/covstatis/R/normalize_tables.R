normalize_tables <- function(cov_matrices, table_norm_type = c("SS1")){

  valid_table_norm_types <- c("SS1","MFA","none")

  if(length(table_norm_type) > 1){
    stop("Please select only one 'table_norm_type'")
  }
  if(!(table_norm_type %in% valid_table_norm_types)){
    warning("Choice of 'table_norm_type' not recognized. Returning 'cov_matrices' as is")
  }

  # names_of_cov_matrices <- names(cov_matrices)
  # cov_matrices <- dplyr::case_when(
  #   table_norm_type == "SS1" ~ lapply(cov_matrices, function(x) { x / sqrt(sum(x^2)) } ),
  #   table_norm_type == "MFA" ~ lapply(cov_matrices, function(x) { x / eigen(x)$values[1] } ),
  #   # table_norm_type == "none" ~ cov_matrices,
  #   TRUE ~ cov_matrices
  # )

  # names(cov_matrices) <- names_of_cov_matrices
  # cov_matrices

    ## I wish something like this would work...
  sapply(cov_matrices, normalize_a_table, table_norm_type = table_norm_type, simplify = FALSE, USE.NAMES = TRUE)
}
