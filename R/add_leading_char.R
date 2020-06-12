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

add_leading_char <- function(target, n, char = "0") {

  if (max(nchar(target)) > n) {
    stop("One or more strings have more than the maximum number of characters.")
  }

  case_when(
    nchar(target) == n ~ as.character(target),
    nchar(target) == (n - 1) ~ paste0(rep(char, 1), target),
    nchar(target) == (n - 2) ~ paste0(rep(char, 2), target),
    nchar(target) == (n - 3) ~ paste0(rep(char, 3), target),
    nchar(target) == (n - 4) ~ paste0(rep(char, 4), target),
    nchar(target) == (n - 5) ~ paste0(rep(char, 5), target)
  )

}



