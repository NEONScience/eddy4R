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
#' PrdWndwAgr <- 240
#' PrdIncrAgr <- 1800
#' numDate <- 1
#' out <- eddy4R.base::def.idx.diff(
#'  PrdWndwAgr=PrdWndwAgr,
#'  PrdIncrAgr=PrdIncrAgr,
#'  numDate=numDate
#' )

#' @seealso Currently none

#' @export

# changelog and author contributions / copyrights
#   Ke Xu (2017-08-12)
#     original creation
#   Natchaya P-Durden (2018-03-28)
#     revised examples
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
