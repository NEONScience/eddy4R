##############################################################################################
#' @title Resampling irregular data to strictly regular / equidistant data

#' @author Stefan Metzger \email{eddy4R.info@gmail.com}

#' @description Function defintion. 
#' Takes a (potentially) irregularly spaced timeseries \code{timeMeas} of data \code{dataMeas} and returns a strictuly regularly spaced timeseries \code{timeRegl} of data \code{dataRegl}. \strong{ATTENTION}: \code{MethRglr = "zoo"} uses the zoo:na.approx() function, which does not currently abide by its \code{maxgap} argument. In result, where gaps exist currently the last known value is repeated instead of NAs being inserted. An Email with a request for bugfixing has been sent to \email{Achim.Zeileis@R-project.org} (2016-05-08).

#' @param \code{timeMeas} A vector containing the observation times. Of class "POSIXlt" including timezone attribute, and of the same length as \code{dataMeas}. [-]
#' @param \code{dataMeas} A named data.frame containing the observations. Columns may be of class "numeric" or "integer", and of the same length as \code{timeMeas}. Columns of classes other than "numeric" or "integer" are removed and not included in the returned \code{dataRegl}. [user-defined]
#' @param \code{unitMeas} A vector containing the unit of each column in \code{dataMeas}. Of class "character".
#' @param \code{BgnRglr} Desired begin time for the regularized dataset. Of class "POSIXlt" including timezone attribute, and \code{length(BgnRglr) = 1}. [-]
#' @param \code{EndRglr} Desired end time for the regularized dataset. Of class "POSIXlt" including timezone attribute, and \code{length(EndRglr) = 1}. [-]
#' @param \code{TzRglr} Desired timezone for the regularized dataset. Of class "character" and \code{length(TzRglr) = 1}, defaults to the same timezone as \code{BgnRglr}. [-]
#' @param \code{FreqRglr} Desired frequency of  the regularized dataset. Of class "numeric" or "integer" and \code{length(FreqRglr) = 1}. [Hz]
#' @param \code{MethRglr} Switch for different regularization methods. Of class "character", currently defaults to "zoo". [-]

#' @return Returns a list with elements \code{TzRglr}, \code{FreqRglr}, \code{MethRglr}, \code{timeRglr}, and \code{dataRglr}.

#' @references
#' License: Terms of use of the NEON FIU algorithm repository dated 2015-01-16. \cr

#' @keywords regularization, equidistant

#' @examples
#' # make sure that fractional seconds can be seen from the console
#' options(digits.secs=3)
#' # assign measured time vector
#' timeMeas <- base::as.POSIXlt(seq.POSIXt(
#'   from = base::as.POSIXlt("2016-01-01 00:00:00.001", format="%Y-%m-%d %H:%M:%OS", tz="UTC"),
#'   to = base::as.POSIXlt("2016-01-01 00:00:01.002", format="%Y-%m-%d %H:%M:%OS", tz="UTC"),
#'   by = 1/10), tz = "UTC")[-c(5,6,8)]
#' # assign fake observations
#' dataMeas <- base::data.frame("wind01" = rnorm(base::length(timeMeas)), "wind02" = rnorm(base::length(timeMeas))),
#' # regularize
#' def.rglr(
#'   timeMeas = timeMeas,
#'   dataMeas = dataMeas,
#'   unitMeas = c("metersPerSecond", "metersPerSecond"),
#'   BgnRglr = base::as.POSIXlt("2016-01-01 00:00:00.000", format="%Y-%m-%d %H:%M:%OS", tz="UTC"),
#'   EndRglr = base::as.POSIXlt("2016-01-01 00:00:01.000", format="%Y-%m-%d %H:%M:%OS", tz="UTC"),
#'   FreqRglr = 10,
#'   MethRglr = "zoo"
#' )

#' @seealso ?zoo:na.approx, ?stats::approx

#' @export

# changelog and author contributions / copyrights
#   Stefan Metzger (2016-05-08)
#     original creation
##############################################################################################

# start function for regularization
def.rglr <- function(
  timeMeas,
  dataMeas,
  unitMeas,
  BgnRglr,
  EndRglr,
  TzRglr = attributes(BgnRglr)$tzone,
  FreqRglr,
  MethRglr
){
  
  # assign list for storing the results
  rpt <- list()
  rpt$TzRglr <- TzRglr
  rpt$FreqRglr <- FreqRglr
  rpt$MethRglr <- MethRglr
  
  # default: using the zoo::na.approx() function
  # takes 3 s for 1,728,000 observations, i.e. one day of one 20 Hz variable
  # tested to work with types "double" and "integer"; definitly does not work with type "character"
  if(MethRglr == "zoo") {
    
    # add a small amount of time to avoid "down-rounding" by R-internal POSIX
    timeMeas$sec <- timeMeas$sec + 0.0001
    BgnRglr$sec <- BgnRglr$sec + 0.0001
    EndRglr$sec <- EndRglr$sec + 0.0002
    
    # create equidistant reference time vector
    rpt$timeRglr <- base::as.POSIXlt(seq.POSIXt(from = BgnRglr, to = EndRglr, by = 1/FreqRglr), tz=TzRglr)
    
    # delete rows with times that are duplicates of rows with smaller indices
    whr01 <- which(base::duplicated(timeMeas))
    if(base::length(whr01) != 0) {
      dataMeas <- dataMeas[-whr01,]
      timeMeas <- timeMeas[-whr01]
    }; base::rm(whr01)
    
    # reduce dataMeas to variables that are of type double or integer (not character!)
    whr02 <- base::sapply(1:base::ncol(dataMeas), function(x) base::typeof(dataMeas[[x]]))
    whr02 <- which((whr02 %in% c("double", "integer")))
    dataMeas <- base::subset(dataMeas, select = whr02)
    unitMeas <- unitMeas[whr02]
    base::rm(whr02)
    
    # start loop around variables
    rpt$dataRglr <- base::data.frame(tmp = rpt$timeRglr, stringsAsFactors = FALSE)
    for(idx in base::names(dataMeas)) {
      
      # determine number of non-NAs in averaging period
      whr03 <- base::length(base::which(!base::is.na(dataMeas[,idx])))
      
      # if less than 2 values (minimum required by na.approx() function)
      if(whr03 < 2) {
        
        rpt$dataRglr[,idx] <- base::rep(NaN, base::length(rpt$timeRglr))
        
        #else interpolate dataMeas
      } else {
        
        rpt$dataRglr[,idx] <- zoo::na.approx(object = dataMeas[,idx], x = timeMeas, xout = rpt$timeRglr,
                                             method = "constant", maxgap = 0, na.rm = FALSE, rule = 1, f = 0)
        # example for current maxgap bug
        # xout <- 1:10
        # x = xout[-c(4:8)]
        # object = rnorm(length(x))
        # zoo::na.approx(object = object, x = x, xout = xout, method = "constant", maxgap = 0, na.rm = FALSE, rule = 1, f = 0)
        
      }
      
      # end loop around variables
    }; base::rm(idx, whr03)
    
    #remove temporary variable from data.frame
    rpt$dataRglr <- subset(rpt$dataRglr, select = -tmp)
    
    # assign unit attributes
    attributes(rpt$dataRglr)$unit <- unitMeas
    
    # end MethRglr == zoo
  }
  
  # return results
  return(rpt)
  
  # end function 
}
