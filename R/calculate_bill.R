###########################################################################################################################################################
#' Calculate Water Bill
#'
#' Calculates customers' water bills based on fixed and volumetric rates. There is an option to include both current
#' and proposed rate structures to assess bill impacts.
#'
#' BillCalculation <- function(df, elasticity = 0.0, nt_c, nt_p = NULL, uniform_rates = TRUE, proposed_rates = TRUE,
#wdths_c = NULL, wdths_p = NULL, fxd_c = NULL, fxd_p = NULL, vol_c = NULL, vol_p = NULL)
#'
#' @param df_use \code{dataframe} that contains customer usage - var must be titled \code{usage} or \code{use}.
#' @param df_rates \code{dataframe} that contains fixed and volumetric rates with suffix "_current". Set to \code{NULL} if widths and rates are already in \code{df_use}.
#' @param rate_group \code{vector} of strings for the group that defines unique rate structure.
#'
#' @return This function returns a \code{dataframe} that includes the final bills under the current rate
#' structure as well as the bill's respective tier components.
#'
#' @author Bjorn Kallerud
#'
#' @import dplyr
#'
#' @export
#'

calculate_bill <- function(df_use, df_rates, rate_group = c("class", "meter_size")) {

  # Store number of tiers for current and proposed rates
  nt_c <- ifelse(is.null(df_rates), length(grep("rate_c", colnames(df_use))), length(grep("rate_c", colnames(df_rates))))

  # Join use and rate dataframes
  if (!is.null(df_rates)) {
    df <- left_join(df_use, df_rates, by = rate_group)
  } else {df <- df_use}
  #TODO include warning if not perfect match

  ## Group columns for calculations
  # Current
  use_cols_current    <- paste0("t", 1:nt_c, "_use_current")
  width_cols_current  <- paste0("t", 1:(nt_c - 1), "_width_current")
  rate_cols_current   <- paste0("t", 1:nt_c, "_rate_current")
  charge_cols_current <- paste0("t", 1:nt_c, "_charges_current")

  # Allocate consumption
  df <- df %>%
    allocate_consumption(suffix = "_current")

  # Calculate charges
  df[charge_cols_current]  <- df[use_cols_current] * df[rate_cols_current]

  # Calculate total bill
  df <- df %>%
    mutate(bill_current  = round(fixed_current  + rowSums(df[charge_cols_current ]), 2))

  return(df)

}
