#' Double center a table
#'
#' TODO
#'
#' @export
#'
#' @param sspsd_mat A square symmetric matrix.
#'
#' @return TODO A centered table?
#'
#' @examples
#' TODO

double_center_a_table <- function(sspsd_mat){

  ## on entry we need to guarantee we have a square symmetric matrix

    ## package build is yelling about this. apparently I'm not allowed to use stats:::C_DoubleCentre?
    ## that's nonsense.
   (.Call(stats:::C_DoubleCentre, sspsd_mat)/2)

}
