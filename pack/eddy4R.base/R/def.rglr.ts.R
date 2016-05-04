##############################################################################################
#' @title Regularize time series to a fixed exact frequency according to NEON Cyber Infrastructure protocol

#' @author 
#' Cove Sturtevant \email{csturtevant@neoninc.org}

#' @description 
#' Function definition. Regularize L0 time series according to default NEON cyber infrastructure (CI) procedure. 
#' NEON CI transforms raw L0 data into a regularized time series according to the expected data frequency.
#' Namely, a new time series is created from the first measurement time, rounded toward zero, using the 
#' expected data frequency. The first measurement falling in between one time stamp and the next is assigned
#' to the first of these, and all other measurements falling in this range are ignored. 
#' This code replicates this procedure in order to compare expected output to that produced by CI.

#' @param \code{data} Required input. A data frame containing the data to be evaluated (do not include the 
#' time stamp vector here). 
#' @param \code{ts} Required input. A time vector of class POSIXlt of times corresponding with each row in data. Defaults to an evenly spaced time vector starting from system time of execution by seconds. 
#' @param \code{Freq} Required input. A numeric value of length one indicating the expected data frequency [Hz].

#' @return A list of the following: \cr
#' \code{tsRglr}. A vector of type POSIXlt of regularized times corresponding to the rows in \code{dataRglr}
#' \code{dataRglr}. A data frame of the regularized time series data 
#' 
#' @references Currently none.
#' license: Terms of use of the NEON FIU algorithm repository dated 2015-01-16

#' @keywords NEON, QAQC, quality flags and metrics, raw data

#' @examples Currently none

#' @seealso Currently none

#' @export

# changelog and author contributions / copyrights
#   Cove Sturtevant (2016-05-03)
#     original creation
##############################################################################################

def.rglr.ts <- function(data,ts,Freq) {
  
  # Check data
  if(base::missing("data") | !base::is.data.frame(data)) {
    stop("Required input 'data' must be a data frame")
  }
  if(base::missing("ts")) {
    stop("Input 'ts' is required")
  }
  if(base::missing("Freq")) {
    stop("Input 'Freq' is required")
  }
  numVar <- base::length(data[1,])
  
  # Check ts
  ts <- try(base::as.POSIXlt(ts),silent=TRUE)
  numData <- base::length(data[,1])
  if(base::class(ts)[1] == "try-error"){
    stop("Input variable ts must be of class POSIXlt")
  } else if (base::length(ts) != numData) {
    stop("Length of input variable ts must be equal to the sample size of data.")
  } 
  
  # Check Freq
  if(!base::is.numeric(Freq) || (base::length(Freq) != 1)) {
    stop("Input parameter Freq must be single number.")
  }
  
  # CI uses the first value as the starting point for the regularization, rounding down to the nearest second
  # Note: the rounding down aspect is a change implemented week of 1 May 2016. Previously the starting point was
  # the exact time (to the decimal second).
  tsRglr <- base::as.POSIXlt(base::seq.POSIXt(from=base::trunc.POSIXt(ts[1],units="secs"),
                                              to=ts[length(ts)],by=1/Freq))
  
  # Pull the first value that falls within each bin
  dataRglr <- base::vapply(base::seq(from=1,to=length(tsRglr),by=1),FUN=function(x){
    idx <- base::which((ts >= tsRglr[x]) & (ts < (tsRglr[x]+1/Freq)))
    if(base::length(idx)>0){
      return(base::as.double(data[idx[1],]))
    } else {
      return(base::rep(NA,numVar))
    }
    },FUN.VALUE=base::numeric(length=numVar))
  dataRglr <- base::as.data.frame(base::matrix(dataRglr,ncol=numVar,byrow=TRUE)) # Need to transpose
  base::names(dataRglr) <- base::names(data) # Assign names same as data
  
  # Report output
  rpt <- base::list(tsRglr=tsRglr,dataRglr=dataRglr)
  base::return(rpt)
}
