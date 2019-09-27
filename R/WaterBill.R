###########################################################################################################################################################
#' Water Bill Calculation
#'
#' Calculates customers' water bills based on fixed and volumetric rates. There is an option to include both current
#' and proposed rate structures to assess bill impacts.
#'
#' BillCalculation <- function(df, elasticity = 0.0, nt_c, nt_p = NULL, uniform_rates = TRUE, proposed_rates = TRUE,
#wdths_c = NULL, wdths_p = NULL, fxd_c = NULL, fxd_p = NULL, vol_c = NULL, vol_p = NULL)
#'
#' @param df \code{dataframe} that contains customer usage - var must be titled \code{usage}. If rates are not uniform across
#' customers, fixed rate columns must be titled \code{fixed_c} and \code{fixed_p}
#' @param elasticity the elasticity parameter, default is set to zero which excludes elasticity from the analysis
#' @param nt_c number of tiers in the current rate structure
#' @param nt_p number of tiers in the proposed rate structure
#' @param uniform_rates \code{logical} if \code{TRUE} the rates are constant across all customers in dataframe. If \code{TRUE}
#' inputs for the rate structure are required in the function call
#' @param proposed_rates \code{logical} if \code{TRUE} two rate structures are included in the calculation
#' @param wdths_c \code{vector} of tier widths under the current rate structure - not necessary if rate structure is not uniform
#' @param wdths_p \code{vector} of tier widths under the proposed rate structure - not necessary if rate structure is not uniform
#' @param fxd_c \code{double} of fixed charge under current rate structure - not necessary if rate structure is not uniform
#' @param fxd_p \code{double} of fixed charge under proposed rate structure - not necessary if rate structure is not uniform
#' @param vol_c \code{vector} of volumetric rates under the current rate structure - not necessary if rate structure is not uniform
#' @param vol_p \code{vector} of volumetric rates under the proposed rate structure - not necessary if rate structure is not uniform
#'
#' @return This function returns a \code{dataframe} that includes all final bills under the current and proposed rate
#' structures as well as their respective components
#'
#' @author Bjorn Kallerud
#'
#' @import dplyr
#'
#' @export
WaterBill <- function(df, uniform_rates = TRUE, nt_c, wdths_c = NULL, fxd_c = NULL, vol_c = NULL,
                      proposed_rates = TRUE, elasticity = 0.0, nt_p = NULL, wdths_p = NULL, fxd_p = NULL,  vol_p = NULL) {

  # Define columns dynamically
  use_cols_current <- paste0("t", 1:nt_c, "_use_current")
  width_cols_current <- paste0("t", 1:nt_c - 1, "_width_current")
  rate_cols_current <- paste0("t", 1:nt_c, "rate_current")
  charge_cols_current <- paste0("t", 1:nt_c, "charges_current")

  if (proposed_rates == TRUE) {

    use_cols_proposed <- paste0("t", 1:nt_p, "_use_proposed")
    width_cols_proposed <- paste0("t", 1:nt_p - 1, "_width_proposed")
    rate_cols_proposed <- paste0("t", 1:nt_p, "rate_proposed")
    charge_cols_proposed <- paste0("t", 1:nt_p, "charges_proposed")

    use_cols_prime <- paste0("t", 1:nt_p, "_use_prime")
    charge_cols_prime <- paste0("t", 1:nt_p, "charges_prime")


  }

  # Calculate usage by tier using TierConsumption()
  df[use_cols_current] <- do.call("TierConsumption",
                                  c(list(nt_c), list(df$usage),
                                    if (uniform_rates == T) {unname(wdths_c)} else {
                                      unname(df[width_cols_current])}))

  if (proposed_rates == TRUE) {

    df[use_cols_proposed] <- do.call("TierConsumption",
                                     c(list(nt_p), list(df$usage),
                                       if (uniform_rates == T) {unname(wdths_p)} else {
                                         unname(df[width_cols_proposed])}))

  }

  # Calculate current charges based on tiered use
  df[charge_cols_current] <- if (uniform_rates == TRUE) {df[use_cols_current] * vol_c} else {
    df[use_cols_current] * df[rate_cols_current]}

  if (proposed_rates == TRUE) {

    df[charge_cols_proposed] <- if (uniform_rates == TRUE) {df[use_cols_proposed] * vol_p} else {
      df[use_cols_proposed] * df[rate_cols_proposed]}

  }

  # Calculate total bill
  df <- mutate(df, bill_current = if (uniform_rates == TRUE) {fxd_c + rowSums(df[charge_cols_current])} else {
    fixed_c + rowSums(df[charge_cols_current])})

  if (proposed_rates == TRUE) {

    df <- df %>%
      mutate(bill_proposed = if (uniform_rates == T) {fxd_c + rowSums(df[charge_cols_proposed])} else {
        fixed_p + rowSums(df[charge_cols_proposed])},
        avg_price_current = ifelse(is.infinite(bill_current / usage), 99999999, bill_current / usage),
        avg_price_proposed = ifelse(is.infinite(bill_proposed / usage), 99999999, bill_proposed / usage),

        usage_prime = usage + elasticity * (avg_price_proposed - avg_price_current) / avg_price_current * usage)

    # Allocate usage prime into tiers
    df[use_cols_prime] <- do.call("TierConsumption", c(list(nt_p), list(df$usage_prime),
                                                       if (uniform_rates == T) {unname(wdths_p)} else {
                                                         unname(df[width_cols_proposed])}))

    df[charge_cols_prime] <- if (uniform_rates == TRUE) {df[use_cols_prime] * vol_p} else {
      df[use_cols_prime] * df[rate_cols_proposed]}

    df <- df %>%
      mutate(bill_prime = if (uniform_rates == TRUE) {fxd_p + rowSums(df[charge_cols_prime])} else {
        fixed_p + rowSums(df[charge_cols_prime])},

        perc_change_bill = (bill_prime - bill_current) / bill_current
      )

  }

  return(df)

}

