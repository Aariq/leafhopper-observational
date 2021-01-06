#' Which element(s) of a vector is (are) nearest to some value?
#' 
#' For use in dplyr::filter() to get all the rows with the value closest to some target value.
#'
#' @param x a numeric vector
#' @param val a numeric scalar
#' @seealso dplyr::near()
#'
#' @return a logical vector
#' @export
#'
#' @examples
#' x <- seq(1, 10, 0.3)
#' nearest(x, 5)
nearest <- function(x, val) {
  abs(x - val) == min(abs(x - val))
}
