###########################################################################################################################################################
#' Compare Water Bill
#'
#' Calculates customers' water bills based on fixed and volumetric rates. There is an option to include both current
#' and proposed rate structures to assess bill impacts.
#'
#' BillCalculation <- function(df, elasticity = 0.0, nt_c, nt_p = NULL, uniform_rates = TRUE, proposed_rates = TRUE,
#wdths_c = NULL, wdths_p = NULL, fxd_c = NULL, fxd_p = NULL, vol_c = NULL, vol_p = NULL)
#'
#' @param df_use \code{dataframe} that contains customer usage - var must be titled \code{usage} or \code{use}.
#' @param df_rates \code{dataframe} that contains current and proposed fixed and volumetric rates.
#' @param elasticity_par the elasticity parameter, default is set to zero which excludes elasticity from the analysis.
#'
#' @return This function returns a \code{dataframe} that includes all final bills under the current and proposed rate
#' structures as well as their respective tier components.
#'
#' @author Bjorn Kallerud
#'
#' @import dplyr
#'
#' @export
#'

# set.seed(8)
# df_use <- data.frame(account_number = 1:10, meter_size = c("0.75\"", "1\""), use = round(runif(10, 10, 30), 1), class = "sfr")
# df_rates <- data.frame(meter_size = c("0.75\"", "1\""), class = "sfr",
#                        t1_width_current = c(10, 11),
#                        t1_width_proposed = c(10, 11), t2_width_proposed = c(5, 6),
#                        fixed_current = c(20, 30), fixed_proposed = c(21, 32),
#                        t1_rate_current = 2, t2_rate_current = 3, t1_rate_proposed = 1.9, t2_rate_proposed = 3, t3_rate_proposed = 5)

compare_bills <- function(df_use, df_rates, rate_group = c("class", "meter_size"), elasticity_par = 0.0, elasticity_response = "average") {

  # Store number of tiers for current and proposed rates
  nt_c <- ifelse(is.null(df_rates), length(grep("rate_c", colnames(df_use))), length(grep("rate_c", colnames(df_rates))))
  nt_p <- ifelse(is.null(df_rates), length(grep("rate_p", colnames(df_use))), length(grep("rate_p", colnames(df_rates))))

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
  # Proposed
  use_cols_proposed    <- paste0("t", 1:nt_p, "_use_proposed")
  width_cols_proposed  <- paste0("t", 1:(nt_p - 1), "_width_proposed")
  rate_cols_proposed   <- paste0("t", 1:nt_p, "_rate_proposed")
  charge_cols_proposed <- paste0("t", 1:nt_p, "_charges_proposed")
  # Prime - accounts for elasticity
  use_cols_prime    <- paste0("t", 1:nt_p, "_use_prime")
  charge_cols_prime <- paste0("t", 1:nt_p, "charges_prime")

  # Allocate consumption
  df <- df %>%
    allocate_consumption(suffix = "_current") %>%
    allocate_consumption(suffix = "_proposed")

  # Calculate charges
  df[charge_cols_current]  <- df[use_cols_current] * df[rate_cols_current]
  df[charge_cols_proposed] <- df[use_cols_proposed] * df[rate_cols_proposed]

  # Calculate total bill
  df <- df %>%
    mutate(bill_current  = fixed_current  + rowSums(df[charge_cols_current ])) %>%
    mutate(bill_proposed = fixed_proposed + rowSums(df[charge_cols_proposed])) %>%
    mutate(avg_price_current  = ifelse(is.infinite(bill_current  / use), 99999999, bill_current  / use)) %>%
    mutate(avg_price_proposed = ifelse(is.infinite(bill_proposed / use), 99999999, bill_proposed / use)) %>%
    # Calculate use that incorporates elasticity
    mutate(use_prime = use + (elasticity_par * (avg_price_proposed - avg_price_current) / avg_price_current) * use) %>%
    # Allocate usage prime into tiers and calculate bill
    allocate_consumption(suffix = "_proposed", use.prime = T)

  df[charge_cols_prime] <- df[use_cols_prime] * df[rate_cols_proposed]

  df <- df %>%
    mutate(bill_prime = fixed_proposed + rowSums(df[charge_cols_prime]),
           perc_change_bill = (bill_prime - bill_current) / bill_current)

  return(df)

}
