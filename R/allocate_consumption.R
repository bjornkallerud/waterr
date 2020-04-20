###########################################################################################################################################################
#' Allocate Tiered Water Consumption
#'
#' Distributes a customers total water consumption over various tiers.
#'
#' @param df dataframe that contains customer usage and tier widths. usage column can be titled either "use" or "usage". width columns titles must take the form of "tX_width".
#' @param suffix width column suffix. we often define widths in terms of current/proposed rates (ie tX_width_current), so defining the suffix enables use of this function for current and proposed widths.
#' @param use.prime boolean - set to TRUE if you want to use `use_prime` usage column instead of standard use column.
#'
#' @return This function returns a \code{dataframe} of numeric columns for tiered usage
#'
#' @author Bjorn Kallerud
#'
#' @import dplyr
#'
#' @export
#'

allocate_consumption <- function(df, suffix = NULL, use.prime = FALSE) {

  # If there is a suffix (ie proposed or current widths) then we want to drop the suffix, conduct the manipulations and then paste in the suffix
  # if (! is.null(suffix)) {names(df) <- gsub(suffix, "", names(df))}

  # Attach dataframe so we can utilize exists()
  attach(df)

  # Allow df usage column to be originally named usage, store so we can replace name at the end
  kUsage <- (exists("usage"))
  df <- df %>% {if(exists("usage")) rename(., use = usage) else .}

  # We calculate use_prime when incorporating elasticity, need a way to have two use columns
  # Method - rename actual use as temporary name and then change prime to use, run calculations, change names back
  if (use.prime){
    df <- df %>%
      rename(use_tmp = use,
             use = use_prime)
  }

  # Define tier maximums
  df <- df %>%
    {if(exists(paste0("t1_width", suffix))) mutate(., t1_max = get(paste0("t1_width", suffix)))           else mutate(., t1_max = 9999999999)} %>%
    {if(exists(paste0("t2_width", suffix))) mutate(., t2_max = t1_max + get(paste0("t2_width", suffix)))  else mutate(., t2_max = 9999999999)} %>%
    {if(exists(paste0("t3_width", suffix))) mutate(., t3_max = t2_max + get(paste0("t3_width", suffix)))  else mutate(., t3_max = 9999999999)} %>%
    {if(exists(paste0("t4_width", suffix))) mutate(., t4_max = t3_max + get(paste0("t4_width", suffix)))  else mutate(., t4_max = 9999999999)} %>%
    {if(exists(paste0("t5_width", suffix))) mutate(., t5_max = t4_max + get(paste0("t5_width", suffix)))  else mutate(., t5_max = 9999999999)}

  # Allocate usage
  df <- df %>%
    {if(exists(paste0("t1_width", suffix))) mutate(., t1_use = ifelse(use > t1_max, t1_max, use),
                                   t2_use = ifelse(use > t1_max, ifelse(use >= t2_max, t2_max - t1_max, use - t1_max), 0)) else
                                     mutate(., t1_use = use)} %>%
    {if(exists(paste0("t2_width", suffix))) mutate(., t3_use = ifelse(use > t2_max, ifelse(use > t3_max, t3_max - t2_max, use - t2_max), 0)) else .} %>%
    {if(exists(paste0("t3_width", suffix))) mutate(., t4_use = ifelse(use > t3_max, ifelse(use > t4_max, t4_max - t3_max, use - t3_max), 0)) else .} %>%
    {if(exists(paste0("t4_width", suffix))) mutate(., t5_use = ifelse(use > t4_max, ifelse(use > t5_max, t5_max - t4_max, use - t4_max), 0)) else .} %>%
    {if(exists(paste0("t5_width", suffix))) mutate(., t6_use = ifelse(use > t5_max, use - t5_max, 0)) else .}

  # Drop maximums
  df <- df %>% select(., - contains("max"))

  # Tweak names - change use to usage and add suffix if need be
  if (! is.null(suffix)) {names(df) <- gsub("_use$", paste0("_use", ifelse(use.prime == F, suffix, "_prime")), names(df))}
  if (kUsage) {df <- rename(df, usage = use )}

  # Switch names back if prime
  if (use.prime){
    df <- df %>%
      rename(use_prime = use,
             use = use_tmp)
  }

  detach(df)
  return(df)

}



