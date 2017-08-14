##############################################################################################
#' @title Definition function: indices for difference calculation, e.g. time rate of change of temperature

#' @author 
#' Ke Xu \email{kxu@battelleecology.org}

#' @description 
#' Definition function to produce a dataframe of indices and corresponding times for difference calculation.
#'
#' @param PrdWndwAgr the width of time period to aggregate to averaging in seconds (4 min = 240 s) [s]
#' @param PrdIncrAgr the incremental time period to calculate the difference in seconds (30 min = 1800 s) [s]
#' @param numDate the number of days in the dataset [-]

#' 
#' @return A dataframe of indices for difference calculation.

#' @references 
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007.

#' @keywords NEON, aggregation, averaging intervals, difference

#' @examples 
#' FreqLoca <- 20
#' timeMeas <- base::as.POSIXlt(seq.POSIXt(
#'   from = base::as.POSIXlt("2016-01-01 00:00:00.001", format="%Y-%m-%d %H:%M:%OS", tz="UTC"),
#'   to = base::as.POSIXlt("2016-01-01 04:59:59.952", format="%Y-%m-%d %H:%M:%OS", tz="UTC"),
#'   by = 1/FreqLoca), tz = "UTC")
#' PrdAgr <- 1800
#' def.idx.agr(time = timeMeas, PrdAgr = PrdAgr, FreqLoca = FreqLoca)

#' @seealso Currently none

#' @export

# changelog and author contributions / copyrights
#   Ke Xu (2017-08-12)
#     original creation
##############################################################################################

def.idx.diff <- function(
  PrdWndwAgr,
  PrdIncrAgr,
  numDate
){
  
  rpt <- list()
  
  rpt <- data.frame(
    Bgn = seq(from = (- (PrdWndwAgr/60)/2 + 1), to = 24 * 60 * numDate / (numDate * 24*60/(PrdIncrAgr/60)) * (numDate * 24*60/(PrdIncrAgr/60)) + (- (PrdWndwAgr/60)/2 + 1), length.out = 24*60/(PrdIncrAgr/60) * numDate+1),
    End = seq(from = ((PrdWndwAgr/60)/2), to = 24 * 60 * numDate + ((PrdWndwAgr/60)/2), length.out = 24*60/(PrdIncrAgr/60) * numDate+1)
  )
  
  rpt$Bgn[which(rpt$Bgn < 1)] <- 1
  rpt$End[which(rpt$End > (24 * 60 * numDate))] <- 24 * 60 * numDate
  
  #Returning output dataframe
  return(rpt)
  
}
