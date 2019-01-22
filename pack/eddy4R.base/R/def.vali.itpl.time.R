##############################################################################################
#' @title definition function: Determine time-series of slope and offset based on linear interpolation method

#' @author
#' Natchaya P-Durden \email{eddy4R.info@gmail.com}

#' @description Definition function to calculate time-series of slope and offset based on linear interpolation method.

#' @param data List consisting of \code{ff::ffdf} file-backed objects containing the dp0p input IRGA.
#' @param DateProc A vector of class "character" containing the processing date.
#' @param timeBgn A time vector of class POSIXlt containing the time when determination of time series is beginning.
#' @param timeEnd A time vector of class POSIXlt containing the time when determination of time series is ending.
#' @param coef List consists of linear regression coefficients (slope and offset) for DateProc - 1, DateProc, and DateProc + 1. 
#' @param valiCrit A logical stating if there are more than one validation occurred within DateProc. Defaut to FALSE.
#' @return 
#' The returned object consists of offset \code{data00$coef[1,1]}, slope \code{data00$coef[2,1]}and its standard error.

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007.

#' @keywords derived, irgaTurb, post-processing, pre-processing, validation

#' @examples
#' Currently none.

#' @seealso Currently none.

#' @export

# changelog and author contributions / copyrights
#   Natchaya P-Durden (2019-01-21)
#     original creation
##############################################################################################
def.vali.itpl.time <- function(
 data,
 DateProc,
 coef,
 timeBgn,
 timeEnd,
 valiCrit = FALSE
){
  #adding library
  library(deming)

  #assign list
  rpt <- list()
  
  #dates that will be used in determination of slope and offset
  Date <- c(base::as.Date(DateProc) - 1, base::as.Date(DateProc), base::as.Date(DateProc) + 1)
  Date <- as.character(Date)
  Freq <- 20  #measurement frequency (20 Hz)
  
  #inpDate <- c(base::as.Date(DateProc) - 1, base::as.Date(DateProc), base::as.Date(DateProc) + 1)
  #output time
  timeOut <- as.POSIXlt(seq.POSIXt(
    from = as.POSIXlt(timeBgn, format="%Y-%m-%d %H:%M:%OS", tz="UTC"),
    to = as.POSIXlt(timeEnd, format="%Y-%m-%d %H:%M:%OS", tz="UTC"),
    by = 1/Freq
  ), tz="UTC")
  
  #fractional
  timeFracOut <- timeOut$hour + timeOut$min / 60 + timeOut$sec / 3600
  #calculate doy
  timeDoy <- timeOut$yday + 1 +  timeFracOut / 24
  
  if (valiCrit == TRUE){
    dateBgn <- c(base::as.Date(DateProc) - 1, base::as.Date(DateProc), base::as.Date(DateProc))
    dateBgn <- as.character(dateBgn)
    dateEnd <- c(base::as.Date(DateProc), base::as.Date(DateProc), base::as.Date(DateProc) + 1)
    dateEnd <- as.character(dateEnd)
    coefBgn <- c("data01", "data00", "data01")
    coefEnd <- c("data00", "data01", "data00")
  }
  for (idx in 1:length(dateBgn)){
    #when coefficients are not NAs
    if (!is.na(coef[[dateBgn[idx]]][[coefBgn[idx]]]$coef[1]) & !is.na(coef[[dateBgn[idx]]][[coefBgn[idx]]]$coef[2]) &
        !is.na(coef[[dateEnd[idx]]][[coefEnd[idx]]]$coef[1]) & !is.na(coef[[dateEnd[idx]]][[coefEnd[idx]]]$coef[2])){
      #create object for reference values
      #offset
      ofst <- zoo::zoo(c(rpt[[Date[idx]]]$rtioMoleDryCo2Mlf$coef[1], rpt[[Date[idx+1]]]$rtioMoleDryCo2Mlf$coef[1]), 
                       c(timeDoy[1], timeDoy[length(timeDoy)]))
      #slope
      slp <- zoo::zoo(c(rpt[[Date[idx]]]$rtioMoleDryCo2Mlf$coef[2], rpt[[Date[idx+1]]]$rtioMoleDryCo2Mlf$coef[2]), 
                      c(timeDoy[1], timeDoy[length(timeDoy)]))
      #interpolation
      ofstLin <- zoo::na.approx(object = ofst, xout = timeDoy, na.rm=FALSE)
      slpLin <- zoo::na.approx(object = slp, xout = timeDoy, na.rm=FALSE)
    } # ending the logic when coefficients are not NAs
    
    #when coefficients in Date[idx] are not NAs but Date[idx+1] are NAs
    if ((!is.na(rpt[[Date[idx]]]$rtioMoleDryCo2Mlf$coef[1]) & !is.na(rpt[[Date[idx]]]$rtioMoleDryCo2Mlf$coef[2])) &
        (is.na(rpt[[Date[idx+1]]]$rtioMoleDryCo2Mlf$coef[1]) | is.na(rpt[[Date[idx+1]]]$rtioMoleDryCo2Mlf$coef[2]))){
      ofstLin <- rpt[[Date[idx]]]$rtioMoleDryCo2Mlf$coef[1]
      slpLin <- rpt[[Date[idx]]]$rtioMoleDryCo2Mlf$coef[2]
    } # ending the logic when coefficients in Date[idx] are not NAs but Date[idx+1] are not NAs
    
    #when coefficients in Date[idx] are NAs
    if (is.na(rpt[[Date[idx]]]$rtioMoleDryCo2Mlf$coef[1]) & is.na(rpt[[Date[idx]]]$rtioMoleDryCo2Mlf$coef[2])){
      ofstLin <- NA
      slpLin <- NA
    }
    #get subset data from timeBgn to timeEnd
    #get indecies
    idxSub <- which(as.POSIXlt(data$irgaTurb$time[]) >= timeBgn &  as.POSIXlt(data$irgaTurb$time[]) < timeEnd)
    #subset data
    subData <- data$irgaTurb[][min(idxSub):max(idxSub),]
    #applying the interpolated coefficients to measured data
    if (all(is.na(ofstLin)) & all(is.na(slpLin))){
      #subData$rtioMoleDryCo2Cor <- as.numeric(subData$rtioMoleDryCo2)
      subData$rtioMoleDryCo2Cor <- NA
    } else {
      subData$rtioMoleDryCo2Cor <- as.numeric(ofstLin + subData$rtioMoleDryCo2*slpLin)
    }
    
    outSub[[idx]] <- subData
}
#append dataframe
outTmp02 <- do.call(rbind,outSub)


}#end function

