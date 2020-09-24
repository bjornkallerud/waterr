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
#' @param rate_group \code{vector} of strings for the group that defines unique rate structure.
#' @param skip_allocation set to TRUE if customer usage has already been allocated among tiers.
#' @param use.essential SFR usage (in same units as use) that is deemed essential. All usage for classes other than SFR is categorized as essential.
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

# # Sample data for troubleshooting/feature additions
# set.seed(8)
# df_use <- data.frame(account_number = 1:10, meter_size = c("0.75\"", "1\""), use = round(runif(10, 10, 30), 1), class = "SFR")
# df_rates <- data.frame(meter_size = c("0.75\"", "1\""), class = "SFR",
#                        t1_width_current = c(10, 11),
#                        t1_width_proposed = c(10, 11), t2_width_proposed = c(5, 6),
#                        fixed_current = c(20, 30), fixed_proposed = c(21, 32),
#                        t1_rate_current = 2, t2_rate_current = 3, t1_rate_proposed = 1.9, t2_rate_proposed = 3, t3_rate_proposed = 5)

compare_bills <- function(df_use, df_rates, rate_group = c("class", "meter_size"), skip_allocation = FALSE, use.essential = 10) {

  # Define price elasticity according to customer class
  pel_params <- data.frame(class = c("SFR", "MFR", "IRR", "CNS", "CI"),
                           price_elast_essential = c(-0.05, -0.05, -0.15, -0.15, -0.15),
                           price_elast_discretionary = c(-0.15, -0.05, -0.15, -0.15, -0.15))

  # Store number of tiers for current and proposed rates
  nt_c <- ifelse(is.null(df_rates), length(grep("rate_c", colnames(df_use))), length(grep("rate_c", colnames(df_rates))))
  nt_p <- ifelse(is.null(df_rates), length(grep("rate_p", colnames(df_use))), length(grep("rate_p", colnames(df_rates))))

  # Join use and rate dataframes
  df <- left_join(df_use, df_rates, by = rate_group)
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
  if (skip_allocation == F) {

    df <- df %>%
      allocate_consumption(suffix = "_current") %>%
      allocate_consumption(suffix = "_proposed")

  }


  # Calculate charges
  df[charge_cols_current]  <- df[use_cols_current]  * df[rate_cols_current]
  df[charge_cols_proposed] <- df[use_cols_proposed] * df[rate_cols_proposed]

  # Calculate total bill
  df <- df %>%
    mutate(bill_current  = round(fixed_current  + rowSums(df[charge_cols_current ]), 2)) %>%
    mutate(bill_proposed = round(fixed_proposed + rowSums(df[charge_cols_proposed]), 2)) %>%
    mutate(avg_price_current  = ifelse(is.infinite(bill_current  / use), 99999999, bill_current  / use)) %>%
    mutate(avg_price_proposed = ifelse(is.infinite(bill_proposed / use), 99999999, bill_proposed / use)) %>%
    ## Elasticity
    # Break usage into essential // discretionary
    mutate(use_essential = case_when(
      class == "SFR" & use >= use.essential ~ use.essential,
      class == "SFR" & use <  use.essential ~ use,
      class != "SFR" ~ use)) %>%
    mutate(use_discretionary = ifelse(use >= use.essential, use - use_essential, 0)) %>%
    # Conduct elasticity calculations on essential and discretionary use
    left_join(pel_params, by = "class") %>%
    mutate(price_elast_essential = ifelse(is.na(price_elast_essential), -0.15, price_elast_essential),
           price_elast_discretionary = ifelse(is.na(price_elast_discretionary), -0.15, price_elast_discretionary)) %>%
    mutate(use_prime_essential     = use_essential     + (price_elast_essential     * (avg_price_proposed - avg_price_current) / avg_price_current) * use_essential,
           use_prime_discretionary = use_discretionary + (price_elast_discretionary * (avg_price_proposed - avg_price_current) / avg_price_current) * use_discretionary,
           use_prime = use_prime_essential + use_prime_discretionary) %>%
    select(-c(use_prime_essential, use_prime_discretionary, use_essential, use_discretionary)) %>%
    # Allocate usage prime into tiers and calculate bill
    allocate_consumption(suffix = "_proposed", use.prime = T)

  df[charge_cols_prime] <- df[use_cols_prime] * df[rate_cols_proposed]

  df <- df %>%
    mutate(bill_prime = round(fixed_proposed + rowSums(df[charge_cols_prime]), 2),
           perc_change_bill = (bill_prime - bill_current) / bill_current)

  return(df)

}

