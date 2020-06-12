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
    nchar(target) == (n - 1) ~ paste0(paste(rep(char, 1), collapse = ""), target),
    nchar(target) == (n - 2) ~ paste0(paste(rep(char, 2), collapse = ""), target),
    nchar(target) == (n - 3) ~ paste0(paste(rep(char, 3), collapse = ""), target),
    nchar(target) == (n - 4) ~ paste0(paste(rep(char, 4), collapse = ""), target),
    nchar(target) == (n - 5) ~ paste0(paste(rep(char, 5), collapse = ""), target),
    nchar(target) == (n - 6) ~ paste0(paste(rep(char, 6), collapse = ""), target),
    nchar(target) == (n - 7) ~ paste0(paste(rep(char, 7), collapse = ""), target),
    nchar(target) == (n - 8) ~ paste0(paste(rep(char, 8), collapse = ""), target),
  )

}



