require(ExPosition)

data("ep.iris")
setosa_correlation_matrix <- cor(ep.iris$data[which(ep.iris$design[,1]==1),])
versicolor_correlation_matrix <- cor(ep.iris$data[which(ep.iris$design[,2]==1),])
virginica_correlation_matrix <- cor(ep.iris$data[which(ep.iris$design[,3]==1),])


correlation_matrix_list <- list(
  setosa = setosa_correlation_matrix,
  versicolor = versicolor_correlation_matrix,
  virginica = virginica_correlation_matrix
)

  ## super lazy way
correlation_matrix_cube <- array(NA, dim=c(dim(setosa_correlation_matrix), 3))
correlation_matrix_cube[,,1] <- setosa_correlation_matrix
correlation_matrix_cube[,,2] <- versicolor_correlation_matrix
correlation_matrix_cube[,,3] <- virginica_correlation_matrix
