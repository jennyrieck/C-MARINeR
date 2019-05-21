
double_center_a_table <- function(sspsd_mat){

  ## on entry we need to guarantee we have a square symmetric matrix

    ## package build is yelling about this. apparently I'm not allowed to use stats:::C_DoubleCentre?
    ## that's nonsense.
  (.Call(stats:::C_DoubleCentre, sspsd_mat)/2)

}
