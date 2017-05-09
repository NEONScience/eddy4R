##############################################################################################
#' @title Definition function: indices for aggregation periods

#' @author 
#' Dave Durden \email{ddurden@battelleecology.org}

#' @description 
#' Definition function to produce a dataframe of indices and corresponding times for aggregation periods.
#'
#' @param time a vector of timestamps 
#' @param PrdAgr the time period to aggregate to averaging in seconds (30 min = 1800 s)
#' @param FreqLoca the frequency of the measurements that are being aggregated in hertz (Hz)
#' 
#' @return A dataframe of indices and corresponding times for aggregation periods.

#' @references 
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007.

#' @keywords NEON, aggregation, averaging intervals

#' @examples 
#' FreqLoca <- 20
#' timeMeas <- base::as.POSIXlt(seq.POSIXt(
#'   from = base::as.POSIXlt("2016-01-01 00:00:00.001", format="%Y-%m-%d %H:%M:%OS", tz="UTC"),
#'   to = base::as.POSIXlt("2016-01-01 05:00:00.002", format="%Y-%m-%d %H:%M:%OS", tz="UTC"),
#'   by = 1/FreqLoca), tz = "UTC")
#' PrdAgr <- 1800
#' def.idx.agr(time = timeMeas, PrdAgr = PrdAgr, FreqLoca = FreqLoca)

#' @seealso Currently none

#' @export

# changelog and author contributions / copyrights
#   Dave Durden (2017-04-30)
#     original creation
##############################################################################################

def.idx.agr <- function(
  time,
  PrdAgr,
  FreqLoca
){
    #Beginning indices
    idxBgn <- seq(from = 1, to = length(time), by = PrdAgr * FreqLoca)
    
    #Beginning time based on indices
    timeBgn <- time[idxBgn]
    
    #Ending indices
    idxEnd <- seq(from = PrdAgr*FreqLoca, to = length(time), by = PrdAgr * FreqLoca)
    
    #Ending time based on indices
    timeEnd <- time[idxEnd]
    
    #Packaging for output in dataframe
    rpt <- data.frame(idxBgn,idxEnd,timeBgn,timeEnd)

    #Returning output dataframe
    return(rpt)

}