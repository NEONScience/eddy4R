##############################################################################################
#' @title Definition function: temporal intepolation from discontinous data points into temporally continous data 

#' @author 
#' Ke Xu \email{kxu@battelleecology.org}

#' @description 
#' Definition function. Function temporal intepolation from discontinous data points into temporally continous data 

#' @param \code{dataInp} Input data. 
#' @param \code{methItpl} intepolation method: linear intepolation is the default method. 
#' @param \code{WndwMax} maximum gap for intepolation. Gaps exist this parameter won't be intepolated due to long missing data [s]. 

#' @return \code{rpt} is list returned that consists of the intepolated data. 

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
#   Ke Xu (2018-07-01)
#     apply eddy4R terms: from gap to Wndw
#   remove na value before applying linear interpolation if like that maxgap will not work
##############################################################################################################
#Start of function call
##############################################################################################################

def.itpl.time <- function(
  dataInp,
  methItpl,
  WndwMax
){
     
  
  #assign output standard time
  Date <- substring(dataInp$timeBgn[1], 1, 10)
  timeOut <- as.POSIXlt(seq.POSIXt(
    from = as.POSIXlt(paste(Date, " 00:00:00", sep=""), format="%Y-%m-%d %H:%M:%OS", tz="UTC"),
    to = as.POSIXlt(paste(Date, " 23:59:00", sep=""), format="%Y-%m-%d %H:%M:%OS", tz="UTC"),
    by = 60
  ), tz="UTC")
  
  
  #fractional UTC time [h UTC]
  timeFracOut <- timeOut$hour + timeOut$min / 60 + timeOut$sec / 3600
  #fractional day of year [DOY]
  timeDoy <- timeOut$yday + 1 +  timeFracOut / 24
  #     #make sure that all entries are sorted ascending  
  #       DATA$tmpOut <- DATA$tmpOut[order(DATA$tmpOut$UTC, decreasing=FALSE),]
  #clean up  
  #rm(timeOut)
  
    
  #assign actual time
  #convert to POSIXct, so the full date and time can be stored in as accessed as a single vector
  timeInp <- as.POSIXlt(dataInp$timeBgn, format="%Y-%m-%dT%H:%M:%OSZ", tz="UTC")
  
  #if(idxDp == "co2Stor" | idxDp == "h2oStor"){
    #timeBgn + numSamp/2/* 1/1Hz
    timeInp <- as.POSIXlt(timeInp + dataInp$numSamp/2*1/1, format="%Y-%m-%d %H:%M:%OS", tz="UTC")
  #}
  
  #fractional UTC time [h UTC]
  dataInp$timeFrac <- timeInp$hour + timeInp$min / 60 + timeInp$sec / 3600
 
  #fractional day of year [DOY]
  dataInp$DOYFrac <- timeInp$yday + 1 +  dataInp$timeFrac / 24
  #     #make sure that all entries are sorted ascending  
  #       rpt$dp01$data$tmpOut <- rpt$dp01$data$tmpOut[order(rpt$dp01$data$tmpOut$UTC, decreasing=FALSE),]
  #clean up  
  rm(timeInp)
  
  
  
  
  #data: determine which datapoints to assess            
  setLgth <- length(which(!is.na(dataInp$mean)))
  
  
  #less than 2 values (minimum required by approx() function)
  if(setLgth < 2) {
    
    rpt <- as.numeric(rep(NA, length(timeOut)))
    
    #interpolate actual data
  } else {
    
    if(methItpl == "linear"){
      #remove na value if like that maxgap will not work
      dataInp <- na.omit(dataInp)
      rpt <- zoo::na.approx(object=as.vector(dataInp$mean), x=#dataInp$timeFrac
                                  as.integer(dataInp$timeFrac * 60)
                                , xout=as.integer(timeFracOut * 60)
                                , method = "linear", maxgap=(WndwMax/60), na.rm=FALSE, rule=1, f=0)
      
    }
    
  }
  
  return(rpt)
    
}
