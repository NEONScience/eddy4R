##############################################################################################
#' @title Definition function: calculate time rate of change for ecse dp02 data and qfqm

#' @author 
#' Ke Xu \email{kxu@battelleecology.org}

#' @description 
#' Definition function. Calculate time rate of change for ecse dp02

#' @param \code{dataInp} Input data. 
#' @param \code{numDate} number of dates of the input data
#' @param \code{PrdWndwAgr} window period, 240 s as the default
#' @param \code{PrdIncrAgr} incremental period, 30 min, 1800 s as the default
#' @param \code{Date} Date of the dataInp
#' @param \code{qfqmFlag} whether the input data is actual data or qfqm, FALSE as default

#' @return \code{rpt} is list returned that consists of the time rate of change of data and qfqm. 

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007. \cr
#' NEON Algorithm Theoretical Basis Document:Eddy Covariance Turbulent Exchange Subsystem Level 0 to Level 0’ data product conversions and calculations (NEON.DOC.000823) \cr
#' NEON Algorithm Theoretical Basis Document:Eddy Covariance Turbulent Exchange Subsystem Level 1 data product calculations (NEON.DOC.000807)

#' @keywords time, intepolate, ECSE

#' @examples 
#' Currently none

#' @seealso Currently none

#' @export

# changelog and author contributions / copyrights
#   Ke Xu (2018-06-21)
#     original creation
##############################################################################################################
#Start of function call
##############################################################################################################

def.time.rate.diff <- function(
  dataInp,
  numDate,
  PrdWndwAgr, 
  PrdIncrAgr,
  Date,
  qfqmFlag = FALSE
){
  
  
  #identify idx for 4 minute average---------------------------------------------------------------------------------------------------------------------------------------
  
  wrk <- list()
  wrk$whrData <- list()
  
  
  wrk$whrData <- eddy4R.base::def.idx.diff(
    PrdWndwAgr=PrdWndwAgr,
    PrdIncrAgr=PrdIncrAgr,
    numDate=numDate
  )
  
  
  
  rpt <- list()
 
  for(idxAgr in c(1:(length(wrk$whrData$Bgn) - 1))) {
    #idxAgr <- 1
    rpt[[idxAgr]] <- list()
    
    
    if(qfqmFlag){
      #determine qfFinl
      rpt[[idxAgr]]$qfFinl[[paste0("rate", capitalize(idxVar))]] <- as.integer(ifelse((any(dataInp[wrk$whrData$Bgn[idxAgr + 1]:wrk$whrData$End[idxAgr + 1]] == 1) |
                                                                                              any(dataInp[wrk$whrData$Bgn[idxAgr]:wrk$whrData$End[idxAgr]] == 1)), 1, 0))
    } else {
      rpt[[idxAgr]]$mean[[paste0("rate", capitalize(idxVar))]] <- (mean(dataInp[wrk$whrData$Bgn[idxAgr + 1]:wrk$whrData$End[idxAgr + 1]], na.rm=T) - 
                                                                          mean(dataInp[wrk$whrData$Bgn[idxAgr]:wrk$whrData$End[idxAgr]], na.rm=T))/PrdIncrAgr
    }
    
    
    #grab and add both time begin and time end to rpt
    rpt[[idxAgr]]$timeBgn <- list()
    rpt[[idxAgr]]$timeEnd <- list()
    
    #assign output standard time
    timeOut <- as.POSIXlt(seq.POSIXt(
      from = as.POSIXlt(paste(Date, " 00:00:00", sep=""), format="%Y-%m-%d %H:%M:%OS", tz="UTC"),
      to = as.POSIXlt(paste(Date, " 23:59:00", sep=""), format="%Y-%m-%d %H:%M:%OS", tz="UTC"),
      by = 60
    ), tz="UTC")
    
    rpt[[idxAgr]]$timeBgn[[paste0("rate", capitalize(idxVar))]] <- format(timeOut[wrk$whrData$End[idxAgr] - 1], format = "%Y-%m-%d %H:%M:%S")
    rpt[[idxAgr]]$timeEnd[[paste0("rate", capitalize(idxVar))]] <- timeOut[(wrk$whrData$End[idxAgr] - 1) + (PrdIncrAgr / 60 - 1)] + 59
     
  }
  
  return(rpt)
  
}
