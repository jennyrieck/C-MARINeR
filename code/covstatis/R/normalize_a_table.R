normalize_a_table <- function(sspsd_table, table_norm_type = c("SS1")){

  ## on entry, need to ensure we have a square symmetric matrix

  valid_table_norm_types <- c("SS1","MFA","none")

  if(length(table_norm_type) > 1){
    stop("Please select only one 'table_norm_type'")
  }
  if(!(table_norm_type %in% valid_table_norm_types)){
    warning("Choice of 'table_norm_type' not recognized. Returning 'sspsd_table' as is")
  }

  ## this returns a numeric vector from a matrix... I don't want it to do that.
  #names_of_sspsd_table <- names(sspsd_table)
  # sspsd_table <- dplyr::case_when(
  #   table_norm_type == "SS1" ~ as.matrix( sspsd_table / sqrt(sum(sspsd_table^2)) ),
  #   table_norm_type == "MFA" ~ as.matrix( sspsd_table / eigen(sspsd_table)$values[1] ),
  #   # table_norm_type == "none" ~ sspsd_table,
  #   TRUE ~ as.matrix(sspsd_table)
  # )
  #names(sspsd_table) <- names_of_sspsd_table

    ## so now I'll do the if-else myself...
  if(table_norm_type=="SS1"){
    return(sspsd_table / sqrt(sum(sspsd_table^2)))
  }
  if(table_norm_type=="MFA"){
    eigen_results <- eigen(sspsd_table, symmetric = TRUE)
    if(any(eigen_results$values < 0 & abs(eigen_results$values) > tol)){
      stop("normalize_a_table (MFA normalization): matrix is not positive-semi definite (i.e., strictly non-negative eigenvalues).")
    }
    return(sspsd_table / eigen_results$values[1])
  }
  sspsd_table

}
