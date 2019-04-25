
double_center_a_table <- function(sspsd_mat){

  (.Call(stats:::C_DoubleCentre, sspsd_mat)/2)

}
