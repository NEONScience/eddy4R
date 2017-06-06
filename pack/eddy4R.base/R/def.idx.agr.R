##############################################################################################
#' @title Definition function: indices for aggregation periods

#' @author 
#' Dave Durden \email{ddurden@battelleecology.org}
#' Natchaya Pingintha-Durden \email{ndurden@battelleecology.org}

#' @description 
#' Definition function to produce a dataframe of indices and corresponding times for aggregation periods.
#'
#' @param time a vector of timestamps 
#' @param PrdAgr the time period to aggregate to averaging in seconds (30 min = 1800 s) [s]
#' @param FreqLoca the frequency of the measurements that are being aggregated in hertz [Hz]
#' @param MethIdx a vector of class "character" containing the name of method used to determine the beginning and ending indicies. MethIdx = c("rglr", "specBgn", "specEnd"), where \cr
#' "rglr" is the regular method, e.g. for FreqLoca = 1 and PrdAgr = 1800, the first result of idxBgn and idxEnd are 1 and 1800, respectively. \cr
#' "specBgn" is the specific method to determine the beginning and ending indicies using the first indency when data is available. \cr
#' "specEnd" is the specific method to determine the beginning and ending indicies using the last indency when data is available. \cr
#' Defaults to "rglr". [-]
#' @param data a vector of input data which will be used to determine when data is available when "specBgn" or "specEnd" is selected. Defaults to NULL. [User-defined]
#' @param CritTime the critcal time to include before determine the beginning and ending indicies, e.g. CritTime = 60 for aggregation only the last 2 min from 3 min measurement time. Defaults to 0. [s]
#' 
#' @return A dataframe of indices and corresponding times for aggregation periods.

#' @references 
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007.

#' @keywords NEON, aggregation, averaging intervals

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
#   Dave Durden (2017-04-30)
#     original creation
#   Natchaya Pingintha-Durden (2017-05-26)
#     added functionality to determine PrdAgr
##############################################################################################

def.idx.agr <- function(
  time,
  PrdAgr,
  FreqLoca,
  MethIdx = c("rglr", "specBgn", "specEnd")[1],
  data = NULL,
  CritTime = 0
){
  if (MethIdx == "rglr"){
    #Beginning indices
    idxBgn <- seq(from = 1, to = length(time), by = PrdAgr * FreqLoca)
    
    #Beginning time based on indices
    timeBgn <- time[idxBgn]
    
    #Ending indices
    idxEnd <- seq(from = PrdAgr*FreqLoca, to = length(time), by = PrdAgr * FreqLoca)
    
    #Ending time based on indices
    timeEnd <- time[idxEnd]
    }#close if statement of MethIdx %in% "rglr"
  
  if (MethIdx %in% "specBgn"){
    if (is.null(data)){base::stop("Missing input data")}
    
    if(length(which(!is.na(data))) > 0){
      #determine the indices which have data  
      whrMsm <- which(!is.na(data))
      #assign the begin indicy
      whrBgn <- whrMsm[1]
      #calculate the difference between indices
      whrMsmDif <- sapply(1:(length(whrMsm)-1), function(xx) whrMsm[(xx + 1)] - whrMsm[xx])
      
      #determine the rest of beginning indicies
      if(length(which(whrMsmDif > 2)) > 0){
        whrBgn <- c(whrBgn, whrMsm[which(whrMsmDif > 2) + 1])
      }
      #determine idxBgn
      #cut off some of the last data point
      #CritTime <- 0
      #PrdAgr <- 120 
      idxBgn <- whrBgn + (CritTime*FreqLoca)
      #Beginning time based on indices
      timeBgn <- time[idxBgn]
      #determine the Ending indices
      idxEnd <- idxBgn + PrdAgr - 1
      #Ending time based on indices
      timeEnd <- time[idxEnd]
    }#close if statement of length(which(!is.na(data))) > 0 
  }#close if statement of MethIdx %in% "specBgn"
  
  if (MethIdx %in% "specEnd"){
    if (is.null(data)){base::stop("Missing input data")}
        
        if(length(which(!is.na(data))) > 0){
          #determine the indices which have data  
          whrMsm <- which(!is.na(data))
          #assign the last ending indicy
          whrEnd <- whrMsm[length(whrMsm)]
          #calculate the difference between indices
          whrMsmDif <- sapply(1:(length(whrMsm)-1), function(xx) whrMsm[(xx + 1)] - whrMsm[xx])
          
          #determine the rest of Ending indicies
          if(length(which(whrMsmDif > 2)) > 0){
            whrEnd <- c(whrEnd, whrMsm[which(whrMsmDif > 2)])
            #whrEnd <- data.frame(whrEnd = c(whrEnd[2:length(whrEnd)], whrEnd[1]))
            whrEnd <- c(whrEnd[2:length(whrEnd)], whrEnd[1])
          }
          #determine idxBgn
          #cut off some of the last data point
          #CritTime <- 0
          #PrdAgr <- 120 
          idxBgn <- whrEnd - (CritTime*FreqLoca) - PrdAgr + 1
          #Beginning time based on indices
          timeBgn <- time[idxBgn]
          #determine the Ending indices
          idxEnd <- whrEnd - (CritTime*FreqLoca)
          #Ending time based on indices
          timeEnd <- time[idxEnd]
          }#close if statement of length(which(!is.na(data))) > 0 
        }#close if statement of MethIdx %in% "specEnd"
    
    #Packaging for output in dataframe
    rpt <- data.frame(idxBgn,idxEnd,timeBgn,timeEnd)

    #Returning output dataframe
    return(rpt)

}
