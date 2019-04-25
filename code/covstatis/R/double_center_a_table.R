  ## by way of distatis, the sign can be flipped before it gets here...
# double_center_sspsd <- function(sspsd_mat, is_squared_distance=FALSE){
# double_center_a_sspsd_matrix <- function(sspsd_mat){
double_center_a_table <- function(sspsd_mat){

  # nI <- nrow(sspsd_mat)
  # centering_mat <- matrix(-1/nI, nI, nI)
  # diag(centering_mat) <- rep(1 - (1/nI), nI)


  ## need to test this against: x <- .Call(C_DoubleCentre, x)

  # if(is_squared_distance){
  #   return( (-1/2)*(centering_mat %*% sspsd_mat %*% centering_mat) )
  # }else{
  #   return( (1/2)*(centering_mat %*% sspsd_mat %*% centering_mat) )
  # }

  # if(is_squared_distance){
  #   sspsd_mat <- (-sspsd_mat)
  # }
  (.Call(stats:::C_DoubleCentre, sspsd_mat)/2)
}
