###########################################################################################################################################################
#' Add leading zeroes to strings
#'
#' @param target string or vector of strings to add leading zero (zeros) to.
#' @param n number of characters the returned strings should be.
#'
#' @return This function returns a string or vector of strings with leading zeros
#'
#' @author Bjorn Kallerud
#'
#' @import dplyr
#'
#' @export
#'

add_leading_zero <- function(target, n) {
  
  if (max(nchar(target)) > n) {
    stop("One or more strings have more than the maximum number of characters.")
  }
  
  case_when(
    nchar(target) == n ~ target,
    nchar(target) == (n - 1) ~ paste0("0", target),
    nchar(target) == (n - 2) ~ paste0("00", target),
    nchar(target) == (n - 3) ~ paste0("000", target),
    nchar(target) == (n - 4) ~ paste0("0000", target),
    nchar(target) == (n - 5) ~ paste0("00000", target)
  )
  
}



