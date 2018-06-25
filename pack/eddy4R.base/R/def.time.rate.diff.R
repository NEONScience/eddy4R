##############################################################################################
#' @title Definition function: calculate time rate of change for ecse dp02

#' @author 
#' Ke Xu \email{kxu@battelleecology.org}

#' @description 
#' Definition function. Calculate time rate of change for ecse dp02

#' @param \code{dataInp} Input data. 
#' @param \code{numDate} number of dates of the input data
#' @param \code{PrdWndwAgr} window period, 240 s as the default
#' @param \code{PrdIncrAgr} incremental period, 30 min, 1800 s as the default

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
#   Ke Xu (2018-06-20)
#     original creation
##############################################################################################################
#Start of function call
##############################################################################################################

def.time.rate.diff <- function(
  dataInp,
  numDate,
  PrdWndwAgr, 
  PrdIncrAgr
){
  
  
  #identify idx for 4 minute average---------------------------------------------------------------------------------------------------------------------------------------
  
  wrk <- list()
  wrk$whrData <- list()
  
  #unit [s]
  PrdWndwAgr <- 4 * 60
  PrdIncrAgr <- resoTimeDp02[[idxDp]] * 60
  
  wrk$whrData <- eddy4R.base::def.idx.diff(
    PrdWndwAgr=PrdWndwAgr,
    PrdIncrAgr=PrdIncrAgr,
    numDate=numDate
  )
  
  
  
  rpt <- list()
  rpt$data <- list()
  rpt$qfqm <- ist()
  
  for(idxAgr in c(1:(length(wrk$whrData$Bgn) - 1))) {
    #idxAgr <- 1
    rpt$data[[idxAgr]] <- list()
    rpt$qfqm[[idxAgr]] <- list()
    
    rpt$data[[idxAgr]]$mean[[paste0("rate", capitalize(idxVar))]] <- (mean(tmpItpl[wrk$whrData$Bgn[idxAgr + 1]:wrk$whrData$End[idxAgr + 1]], na.rm=T) - 
                                                                                               mean(tmpItpl[wrk$whrData$Bgn[idxAgr]:wrk$whrData$End[idxAgr]], na.rm=T))/(resoTimeDp02[[idxDp]]*60)
    
    #determine qfFinl
    rpt$qfqm[[idxAgr]]$qfFinl[[paste0("rate", capitalize(idxVar))]] <- as.integer(ifelse((any(tmpQfqmItpl[wrk$whrData$Bgn[idxAgr + 1]:wrk$whrData$End[idxAgr + 1]] == 1) |
                                                                                                                       any(tmpQfqmItpl[wrk$whrData$Bgn[idxAgr]:wrk$whrData$End[idxAgr]] == 1)), 1, 0))
    
    #grab and add both time begin and time end to rpt
    rpt$data[[idxAgr]]$timeBgn <- list()
    rpt$data[[idxAgr]]$timeEnd <- list()
    rpt$qfqm[[idxAgr]]$timeBgn <- list()
    rpt$qfqm[[idxAgr]]$timeEnd <- list()
    
    rpt$data[[idxAgr]]$timeBgn[[paste0("rate", capitalize(idxVar))]] <- format(timeDp02[wrk$whrData$End[idxAgr] - 1], format = "%Y-%m-%d %H:%M:%S")
    rpt$data[[idxAgr]]$timeEnd[[paste0("rate", capitalize(idxVar))]] <- timeDp02[(wrk$whrData$End[idxAgr] - 1) + (resoTimeDp02[[idxDp]] - 1)] + 59
    rpt$qfqm[[idxAgr]]$timeBgn[[paste0("rate", capitalize(idxVar))]] <- format(timeDp02[wrk$whrData$End[idxAgr] - 1], format = "%Y-%m-%d %H:%M:%S")
    rpt$qfqm[[idxAgr]]$timeEnd[[paste0("rate", capitalize(idxVar))]] <- timeDp02[(wrk$whrData$End[idxAgr] - 1) + (resoTimeDp02[[idxDp]] - 1)] + 59
    
  
  return(rpt)
  
}
