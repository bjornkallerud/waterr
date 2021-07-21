###########################################################################################################################################################
#' Convert Meter Sizes
#'
#' Standardizes meter size formatting
#'
#' @param meter_col vector of meter sizes to convert
#' @param fraction if TRUE, convert meter sizes to fraction format
#'
#' @return This function returns a vector of correctly formatted meter sizes
#'
#' @author Zach Van Dinther
#'
#' @import dplyr
#'
#' @export
#' 
#' @example convert_meters(c("0.75", '0.75"', '3/4'))

convert_meters <- function(meter_col, fraction = TRUE) {
  
  # lists of possible formats
  fe <- list( "5/8", "0.625", ".625")
  tq <- list( "3/4", "0.75", ".75")                 
  one <- list("1", "1.00",  "1.0")
  opf <- list( "11/2", "1.5")
  two <- list("2",  "2.00", "2.0")
  tpf <- list("21/2",  "2.50",   "2.5")
  three <- list("3", "3.00",  "3.0")
  four <- list("4",  "4.00", "4.0")
  six <- list("6",  "6.00", "6.0")
  eight <- list("8", "8.00", "8.0")
  ten <- list("10", "10.00", "10.0")
  twelve <- list("12", "12.00", "12.0")
  fteen <- list("14", "14.00",  "14.0")
  steen <- list("16",  "16.00", "16.0")
  eteen <- list("18", "18.00", "18.0")
  
  # drop all spaces
  xx <- gsub(" ", "", meter_col)
  xx <- gsub('"', "", xx)
  
  if (fraction == TRUE) {
    
    
    xx <- ifelse(xx %in% fe, '5/8"',
                 ifelse(xx %in% tq, '3/4"', 
                        ifelse(xx %in% one, '1"',
                               ifelse(xx %in% opf, '1 1/2"',
                                      ifelse(xx %in% two, '2"',
                                             ifelse(xx %in% tpf, '2 1/2"',
                                                    ifelse(xx %in% three, '3"',
                                                           ifelse(xx %in% four, '4"',
                                                                  ifelse(xx %in% six, '6"',
                                                                         ifelse(xx %in% eight, '8"',
                                                                                ifelse(xx %in% ten, '10"',
                                                                                       ifelse(xx %in% twelve, '12"',
                                                                                              ifelse(xx %in% fteen, '14"',
                                                                                                     ifelse(xx %in% steen, '16"',
                                                                                                            ifelse(xx %in% eteen, '18"',xx
                                                                                                            )))))))))))))))
  } else {
    
    xx <- ifelse(xx %in% fe, '0.625"',
                 ifelse(xx %in% tq, '0.75"', 
                        ifelse(xx %in% one, '1.00"',
                               ifelse(xx %in% opf, '1.50"',
                                      ifelse(xx %in% two, '2.00"',
                                             ifelse(xx %in% tpf, '2.50"',
                                                    ifelse(xx %in% three, '3.00"',
                                                           ifelse(xx %in% four, '4.00"',
                                                                  ifelse(xx %in% six, '6.00"',
                                                                         ifelse(xx %in% eight, '8.00"',
                                                                                ifelse(xx %in% ten, '10.00"',
                                                                                       ifelse(xx %in% twelve, '12.00"',
                                                                                              ifelse(xx %in% fteen, '14.00"',
                                                                                                     ifelse(xx %in% steen, '16.00"',
                                                                                                            ifelse(xx %in% eteen, '18.00"',xx
                                                                                                            )))))))))))))))
  }
  
  return(xx)
  

  }
