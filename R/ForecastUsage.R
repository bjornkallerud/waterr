#' Forecast Meters
#'
#' \code{dplyr}-style function that forecasts the number of meters by group (typically meter size)
#'
#' @param df dataframe that contains annual observations of per-meter usage
#' @param t_0 integer for final year for which data is available
#' @param t_final integer for the final year of forecasting period
#'
#' @return This function returns a \code{dataframe} of forecasted per-meter usage
#'
#' @author Bjorn Kallerud
#'
#' @export
ForecastUsage <- function(df, t_0 = 2018, t_final = 2024) {

  # Regress use on date
  model_use <- lm(use_pm ~ date, data = df)

  fc_use <- bind_rows(df, data.frame(date = seq(t_0 + 1, t_final), use_pm = NA)) %>%
    mutate(use_pm = ifelse(date > t_0,
                           use_pm[date == t_0] + model_use$coefficients[2] * (date - 2018),
                           use_pm),
           fc = ifelse(date > t_0, "Forecasted", "Actual"))

  return(fc_use)

}
