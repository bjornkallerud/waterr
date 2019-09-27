###########################################################################################################################################################
#' Calculate Tier Use
#'
#' Distributes a customers total water consumption over various tiers
#'
#' @param nt integer that contains the number of tiers of the rate structure
#' @param usage numeric vector containing customer's water usage
#' @param widths integer that contains the width of each tier or a numeric vector that contains individual tier widths in the case of water budget rates
#'
#' @return This function returns a \code{list} of numeric vectors of water usage within each tier
#'
#' @author Bjorn Kallerud
#'
#' @export
#'
#' @example
#' calcTierUse(nt = 3, usage = 1:30, t1width = 5, t2width = 7)
TierConsumption <-
  function(nt, usage,
           t1width, t2width = NULL, t3width = NULL, t4width = NULL, t5width = NULL) {

    # Calculate max and mins from widths
    t1max <- t1width
    t2min <- t1width + 0.01
    t2max <- t1width + t2width
    t3min <- t2max + 0.01
    t3max <- t2max + t3width
    t4min <- t3max + 0.01
    t4max <- t3max + t4width
    t5min <- t4max + 0.01
    t5max <- t4max + t5width
    t6min <- t5max + 0.01

    # Calculate
    t1use <- if (nt == 1) usage else if (nt >= 2) ifelse(usage < t1max, usage, t1max-0)

    t2use <- if (nt == 2) ifelse(usage >= t2min, usage - t1use, 0) else
      if (nt >= 3) ifelse(t2min <= usage & usage <= t2max, usage - t1max, ifelse(usage > t2max, t2max-t1max,0))

    t3use <- if (nt == 3) ifelse(usage >= t3min, usage - t2max, 0) else
      if (nt >= 4) ifelse(t3min <= usage & usage <= t3max, usage - t2max, ifelse(usage > t3max, t3max-t2max,0))

    t4use <- if (nt == 4) ifelse(usage >= t4min, usage - t3max, 0) else
      if (nt >= 5) ifelse(t4min <= usage & usage <= t4max, usage - t3max, ifelse(usage > t4max, t4max-t3max,0))

    t5use <- if (nt == 5) ifelse(usage >= t5min, usage - t4max, 0) else
      if (nt >=5 ) ifelse(t5min <= usage & usage <= t5max, usage - t4max, ifelse(usage > t5max, t5max-t4max,0))

    t6use <- if (nt >= 6) ifelse(usage >= t6min, usage - t5max, 0)

    # Error if more than 6 tiers
    if(nt > 6) {stop("This function only supports 6 or fewer tiers")}

    return(if (nt == 1) list(t1use = t1use) else
      if (nt == 2) list(t1use = t1use,t2use = t2use) else
        if (nt == 3) list(t1use = t1use,t2use = t2use,t3use = t3use) else
          if (nt == 4) list(t1use = t1use,t2use = t2use,t3use = t3use,t4use = t4use) else
            if (nt == 5) list(t1use = t1use,t2use = t2use,t3use = t3use,t4use = t4use,t5use = t5use) else
              if (nt == 6) list(t1use = t1use,t2use = t2use,t3use = t3use,t4use = t4use,t5use = t5use,t6use = t6use)
    )

  }
