#' Forecast Meters
#'
#' \code{dplyr}-style function that forecasts the number of meters by group (typically meter size)
#'
#' @param df dataframe that contains annual observations of the number of meters by size
#' @param t_0 integer for final year for which data is available
#' @param t_final integer for the final year of forecasting period
#'
#' @return This function returns a \code{dataframe} of forecasted meters by group
#'
#' @author Bjorn Kallerud
#'
#' @export
ForecastMeters <- function(df, t_0 = 2018, t_final = 2024) {

  # Take structure of inputted data frame
  fc_meters <- df[NULL, 0]

  # Forecast each meter size/agency combination
  for (xx in unique(df$meter_size)) {

    tmp <- df %>%
      filter(meter_size == xx) %>%
      select(date, meter_size, meters)

    model_meters <- lm(meters ~ date, data = tmp)

    tmp_fc <- tmp %>%
      bind_rows(
        data.frame(date = seq(t_0 + 1, t_final, 1), meter_size = tmp$meter_size[1], meters = NA)) %>%
      mutate(yrs_forward = date - t_0,
             meters = ifelse(yrs_forward >= 1,
                             tmp$meters[tmp$date == t_0] + yrs_forward * model_meters$coefficients[2],
                             meters)) %>%
      select(-yrs_forward)

    fc_meters <- bind_rows(fc_meters, tmp_fc)

  }

  # Replace if less than 0
  fc_meters <- mutate(fc_meters, meters = ifelse(meters < 0 | is.na(meters), 0, round(meters, 0))) %>%
    mutate(fc = ifelse(date > t_0, "Forecasted", "Actual"))

  return(fc_meters)

}
