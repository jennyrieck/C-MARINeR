
double_center_a_table <- function(sspsd_mat){

  ## on entry we need to guarantee we have a square symmetric matrix

  (.Call(stats:::C_DoubleCentre, sspsd_mat)/2)

}
