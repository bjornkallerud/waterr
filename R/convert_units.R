###########################################################################################################################################################
#' Convert Water Units
#'
#' Converts between gallons, hcf, and acre-feet
#'
#' @param xx numeric value or vector to be converted
#' @param from unit to convert from, either "gallons", "hcf", or "af" (acre-feet)
#' @param to unit to convert to, either "gallons", "hcf", or "af" (acre-feet)
#'
#' @return This function returns a numeric value or vector
#'
#' @author Bjorn Kallerud
#'
#' @import dplyr
#'
#' @export
#'
convert_units <- function(xx, from = "gallons", to = "hcf") {

  if (from == "gallons" & to == "hcf")     {xx / 748}     else
    if (from == "hcf"     & to == "gallons") {xx * 748}     else
      if (from == "hcf"     & to == "af")      {xx / 435.599} else
        if (from == "af"      & to == "hcf")     {xx * 435.599} else
          if (from == "gallons" & to == "af")      {xx / 325851}  else
            if (from == "af"      & to == "gallons") {xx / 325851}  else
              stop("Measurement units invalid. Use either `gallons`, `hcf`, or `af`. ")

}
