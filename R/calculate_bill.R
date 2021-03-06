###########################################################################################################################################################
#' Calculate Water Bill
#'
#' Calculates customers' water bills based on fixed and volumetric rates. There is an option to include both current
#' and proposed rate structures to assess bill impacts.
#'
#' calculate_bill <- function(df_use, df_rates, rate_group = c("class", "meter_size"), suffix = "current")
#'
#' @param df_use \code{dataframe} that contains customer usage - var must be titled \code{usage} or \code{use}.
#' @param df_rates \code{dataframe} that contains fixed and volumetric rates with suffix "_current". Set to \code{NULL} if widths and rates are already in \code{df_use}.
#' @param rate_group \code{vector} of strings for the group that defines unique rate structure.
#' @param suffix calculate bill based on \code{current} or \code{proposed} rate structure.
#' @return This function returns a \code{dataframe} that includes the final bills under the current rate
#' structure as well as the bill's respective tier components.
#'
#' @author Bjorn Kallerud
#'
#' @import dplyr
#'
#' @export
#'

calculate_bill <- function(df_use, df_rates, rate_group = c("class", "meter_size"), suffix = "current") {

  # Throw error if suffix is not current or proposed
  if (!suffix %in% c("current", "proposed")) {
    stop("Invalid suffix.")
  }

  # Store number of tiers for current and proposed rates
  nt <- ifelse(is.null(df_rates), length(grep(if (suffix == "current") "rate_c" else if (suffix == "proposed") "rate_p", colnames(df_use))),
               length(grep(if (suffix == "current") "rate_c" else if (suffix == "proposed") "rate_p", colnames(df_rates))))

  # Join use and rate dataframes
  if (!is.null(df_rates)) {
    df <- left_join(df_use, df_rates, by = rate_group)
  } else {df <- df_use}
  #TODO include warning if not perfect match

  ## Group columns for calculations
  # Current
  use_cols    <- paste0("t", 1:nt, if (suffix == "current") "_use_current" else if (suffix == "proposed") "_use_proposed")
  width_cols  <- paste0("t", 1:(nt - 1), if (suffix == "current") "_width_current" else if (suffix == "proposed") "_width_proposed")
  rate_cols   <- paste0("t", 1:nt, if (suffix == "current") "_rate_current" else if (suffix == "proposed") "_rate_proposed")
  charge_cols <- paste0("t", 1:nt, if (suffix == "current") "_charges_current" else if (suffix == "proposed") "_charges_proposed")

  # Allocate consumption
  df <- df %>%
    allocate_consumption(suffix = paste0("_", suffix))

  # Calculate charges
  df[charge_cols]  <- df[use_cols] * df[rate_cols]

  # Calculate total bill
  df <- df %>%
    {if (suffix == "current") mutate(., bill_current = round(fixed_current + rowSums(df[charge_cols]), 2)) else
      if (suffix == "proposed") mutate(., bill_proposed = round(fixed_proposed + rowSums(df[charge_cols]), 2))}

  return(df)

}
