normalize_tables <- function(cov_matrices, table_norm_type = c("SS1")){

  valid_table_norm_types <- c("SS1","MFA","none")

  if(length(table_norm_type) > 1){
    stop("Please select only one 'table_norm_type'")
  }
  if(!(table_norm_type %in% valid_table_norm_types)){
    warning("Choice of 'table_norm_type' not recognized. Returning 'cov_matrices' as is")
  }

  sapply(cov_matrices, normalize_a_table, table_norm_type = table_norm_type, simplify = FALSE, USE.NAMES = TRUE)
}
