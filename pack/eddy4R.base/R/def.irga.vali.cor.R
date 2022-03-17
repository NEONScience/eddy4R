##############################################################################################
#' @title definition function: Calculation of time-series of the correction IRGA sub data products.

#' @author
#' Natchaya P-Durden \email{eddy4R.info@gmail.com}

#' @description Definition function to calculate time-series of the correction IRGA sub data products based on linear interpolation method.

#' @param data List consisting of \code{ff::ffdf} file-backed objects containing the dp0p input IRGA.
#' @param DateProc A vector of class "character" containing the processing date.
#' @param valiData List consisting of descriptive statistics (mean, min, max, vari, numSamp, se) of CO2 dry mole concentration during performing validation and CO2 dry mole concentration of reference gases.
#' @param coef List consists of linear regression coefficients (slope and offset) for DateProc - 1, DateProc, and DateProc + 1. 
#' @param valiCrit A logical stating if there are more than one validation occurred within DateProc. 
#' @param ScalMax Maximum scale value. The validation correction will not apply if scale (resulted from maximum-likelihood fitting of a functional relationship (MLFR)) is greater than ScalMax or ScalMax = FALSE. Defaults to FALSE.
#' @param FracSlp Upper and lower bounds of slope values. The validation correction will not apply if slope (resulted from regression fitting) is greater/lower than the FracSlp maximum or minimum value or FracSlp = FALSE. Defaults to FALSE.
#' @param OfstMax Maximum offset value. The validation correction will not apply if slope (resulted from regression fitting) is greater than the OfstMax (unit in mol mol-1) or OfstMax = FALSE. Defaults to FALSE.
#' @param Freq Measurement frequency. Defaults to 20. [Hz]

#' @return 
#' The returned dataframe consists of the correction IRGA sub data products.

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
#   Natchaya P-Durden (2019-01-31)
#     bugs fix to add all NA data of rtioMoleDryCo2Cor in rpt when no validation occurred
#   Natchaya P-Durden (2019-02-04)
#     bugs fix on typo of rtioMoleDryCo2Cor
#   Natchaya P-Durden (2019-02-19)
#     not apply the correction when slope and scale greater than thresholds
#   Natchaya P-Durden (2019-03-05)
#     apply ff object to dataframe to save the memory
#   Natchaya P-Durden (2019-03-15)
#     added ScalMax, FracSlpMax, and Freq into input function parameters
#   Natchaya P-Durden (2020-01-14)
#     added time when the real validation begin
#   Natchaya P-Durden (2020-01-16)
#     generated NA for rtioMoleDryH2oCor
#   Natchaya P-Durden (2020-01-14)
#     added 5 min after the validation end
#   Natchaya P-Durden (2020-02-28)
#     added logical statement to not apply filters (slope, offset, and scale) if they are equal to FALSE
#   Natchaya P-Durden (2020-04-14)
#     update the way to determine time begin and end to be able to 
#     work when the validation do not have a full set of gas tanks
#   Natchaya P-Durden (2020-04-15)
#     adding logical to handle the period that falling into the last day and first day of year
#   David Durden (2020-05-26)
#     Failsafe for when the valve is switched, but no validation occurs
#   Chris Florian (2022-03-02)
#     Updating the slope filter to allow for values not evenly centered around 1
##############################################################################################
def.irga.vali.cor <- function(
 data,
 DateProc,
 coef,
 valiData,
 valiCrit = FALSE,
 ScalMax = FALSE,
 FracSlp = FALSE,
 OfstMax = FALSE,
 Freq = 20
){
  #adding library
  #library(deming)

  #assign list
  rpt <- list()
  outSub <- list()
  outTmp00 <- list()
  #dates that will be used in determination of slope and offset
  Date <- c(base::as.Date(DateProc) - 1, base::as.Date(DateProc), base::as.Date(DateProc) + 1)
  Date <- as.character(Date)
  Freq <- Freq  #measurement frequency (20 Hz)
  
  #create a list to keep all filters
  filt <- list()
  filt$ScalMax <- ScalMax
  filt$FracSlp <- FracSlp
  filt$OfstMax <- OfstMax
  
  
  #check if filter will apply.
  for (idxDate in Date){
    #idxDate <- Date[1]
    for (idxData in names(coef[[idxDate]])){
      #idxData <- names(coef[[idxDate]])[1]
      for (idxFilt in names(filt)){
        #idxFilt <- names(filt)[1]
        if (filt[[idxFilt]] == FALSE){
          coef[[idxDate]][[idxData]] <- coef[[idxDate]][[idxData]]
        } else{#testing the filters
          if (idxFilt == "ScalMax"){
            if (!is.na(coef[[idxDate]][[idxData]]$scal[1]) & coef[[idxDate]][[idxData]]$scal[1] <= ScalMax){
              coef[[idxDate]][[idxData]] <- coef[[idxDate]][[idxData]] 
            } else {
              coef[[idxDate]][[idxData]]$coef <- NA
              coef[[idxDate]][[idxData]]$se <- NA
              coef[[idxDate]][[idxData]]$scal <- NA
            }
          }#end ScalMax
          
          if (idxFilt == "FracSlp"){
            #calculate minimum and maximum slope
            minSlp <- base::min(FracSlp)
            maxSlp <- base::max(FracSlp)
            if (!is.na(coef[[idxDate]][[idxData]]$coef[2]) & (coef[[idxDate]][[idxData]]$coef[2] >= minSlp & coef[[idxDate]][[idxData]]$coef[2] <= maxSlp)){
              coef[[idxDate]][[idxData]] <- coef[[idxDate]][[idxData]] 
            } else {
              coef[[idxDate]][[idxData]]$coef <- NA
              coef[[idxDate]][[idxData]]$se <- NA
              coef[[idxDate]][[idxData]]$scal <- NA
            }
          }#end of FracSlp
          
          if (idxFilt == "OfstMax"){
            if (!is.na(coef[[idxDate]][[idxData]]$coef[1]) & (abs(coef[[idxDate]][[idxData]]$coef[1]) <= OfstMax)){
              coef[[idxDate]][[idxData]] <- coef[[idxDate]][[idxData]] 
            }
            else {
              coef[[idxDate]][[idxData]]$coef <- NA
              coef[[idxDate]][[idxData]]$se <- NA
              coef[[idxDate]][[idxData]]$scal <- NA
            }
          }#end OfstMax
        }#end of testing filter
        
      }#end of idxFilt
      
    
  }#end of idxData
  }#end idxDate
  
  #organize input coefficient table 
  if (valiCrit == TRUE){
    dateBgn <- c(base::as.Date(DateProc) - 1, base::as.Date(DateProc), base::as.Date(DateProc))
    dateBgn <- as.character(dateBgn)
    dateEnd <- c(base::as.Date(DateProc), base::as.Date(DateProc), base::as.Date(DateProc) + 1)
    dateEnd <- as.character(dateEnd)
    coefBgn <- c("data01", "data00", "data01")
    coefEnd <- c("data00", "data01", "data00")
  }else{
    dateBgn <- c(base::as.Date(DateProc) - 1, base::as.Date(DateProc))
    dateBgn <- as.character(dateBgn)
    dateEnd <- c(base::as.Date(DateProc), base::as.Date(DateProc) + 1)
    dateEnd <- as.character(dateEnd)
    coefBgn <- c("data01", "data01")
    coefEnd <- c("data00", "data00")
  }
  numDate <- 0
  for (idx in 1:length(dateBgn)){
    numDate <- numDate + 1
    tmpTimeBgn <- as.POSIXlt(paste(dateBgn[idx], " ", "23:59:59.950", sep=""), format="%Y-%m-%d %H:%M:%OS", tz="UTC")
    tmpTimeEnd <- as.POSIXlt(paste(dateEnd[idx], " ", "", sep="00:00:00.000"), format="%Y-%m-%d %H:%M:%OS", tz="UTC")
    #time begin and time End to apply coefficient
    #time when performing of last gas is done
    if (all(valiData[[dateBgn[idx]]][[coefBgn[idx]]]$timeEnd == tmpTimeBgn) &
        all(is.na(valiData[[dateBgn[idx]]][[coefBgn[idx]]]$numSamp))){
      timeBgn <- as.POSIXlt(paste(dateBgn[idx], " ", "23:59:59.950", sep=""), format="%Y-%m-%d %H:%M:%OS", tz="UTC")
    } else {
      #identify which row that timeEnd not = "23:59:59.950"
      tmpEndRow <- which(valiData[[dateBgn[idx]]][[coefBgn[idx]]]$timeEnd != tmpTimeBgn & !is.na(valiData[[dateBgn[idx]]][[coefBgn[idx]]]$numSamp))
      timeBgn <- as.POSIXlt(valiData[[dateBgn[idx]]][[coefBgn[idx]]]$timeEnd[tmpEndRow[length(tmpEndRow)]]+(60*5.0))
    }
    
    #time when performing of first gas is started
    if (all(valiData[[dateEnd[idx]]][[coefEnd[idx]]]$timeBgn == tmpTimeEnd) &
        all(is.na(valiData[[dateEnd[idx]]][[coefEnd[idx]]]$numSamp))){
      timeEnd <- as.POSIXlt(paste(dateEnd[idx], " ", "", sep="00:00:00.000"), format="%Y-%m-%d %H:%M:%OS", tz="UTC")
    } else {
      #identify which row that timeEnd not = "00:00:00.000"
      tmpBgnRow <- which(valiData[[dateEnd[idx]]][[coefEnd[idx]]]$timeBgn != tmpTimeEnd & !is.na(valiData[[dateEnd[idx]]][[coefEnd[idx]]]$numSamp))
      timeEnd <- as.POSIXlt(valiData[[dateEnd[idx]]][[coefEnd[idx]]]$timeBgn[tmpBgnRow[1]]-(60*3.5))
    }
    
    # #fail safe to make sure timeBgn less than timeEnd
    # if (difftime(timeBgn, timeEnd) >= 0){
    #   timeBgn <- as.POSIXlt(paste(dateBgn[idx], " ", "23:59:59.950", sep=""), format="%Y-%m-%d %H:%M:%OS", tz="UTC")
    # } else {
    #   timeBgn <- timeBgn
    # }
    
    #output time
    timeOut <- as.POSIXlt(seq.POSIXt(
      from = as.POSIXlt(timeBgn, format="%Y-%m-%d %H:%M:%OS", tz="UTC"),
      to = as.POSIXlt(timeEnd, format="%Y-%m-%d %H:%M:%OS", tz="UTC"),
      by = 1/Freq
    ), tz="UTC")
    
    #fractional
    timeFracOut <- timeOut$hour + timeOut$min / 60 + timeOut$sec / 3600
    #adding logical to handle the period that falling into the last day and first day of year
    if (format(as.Date(timeBgn, format="%d/%m/%Y"),"%Y") != format(as.Date(timeEnd, format="%d/%m/%Y"),"%Y")){
      #replace those fist day (equal to 0 to max(timeOut$yday)+1)
      timeOut$yday[timeOut$yday != max(timeOut$yday)] <- max(timeOut$yday)+1
    }

    #calculate doy
    timeDoy <- timeOut$yday + 1 +  timeFracOut / 24
    
    #Calculate time-series (20Hz) of slope and zero offset
    #case1: when coefficients are not NAs
    if (!is.na(coef[[dateBgn[idx]]][[coefBgn[idx]]]$coef[1]) & !is.na(coef[[dateBgn[idx]]][[coefBgn[idx]]]$coef[2]) &
        !is.na(coef[[dateEnd[idx]]][[coefEnd[idx]]]$coef[1]) & !is.na(coef[[dateEnd[idx]]][[coefEnd[idx]]]$coef[2])){
      #create object for reference values
      #offset
      ofst <- zoo::zoo(c(coef[[dateBgn[idx]]][[coefBgn[idx]]]$coef[1], coef[[dateEnd[idx]]][[coefEnd[idx]]]$coef[1]), 
                       c(timeDoy[1], timeDoy[length(timeDoy)]))
      #slope
      slp <- zoo::zoo(c(coef[[dateBgn[idx]]][[coefBgn[idx]]]$coef[2], coef[[dateEnd[idx]]][[coefEnd[idx]]]$coef[2]), 
                      c(timeDoy[1], timeDoy[length(timeDoy)]))
      #interpolation
      ofstLin <- zoo::na.approx(object = ofst, xout = timeDoy, na.rm=FALSE)
      slpLin <- zoo::na.approx(object = slp, xout = timeDoy, na.rm=FALSE)
    } # ending the logic when coefficients are not NAs
    
    #case2: when coefficients in Date[idx] are not NAs but Date[idx+1] are NAs
    if ((!is.na(coef[[dateBgn[idx]]][[coefBgn[idx]]]$coef[1]) & !is.na(coef[[dateBgn[idx]]][[coefBgn[idx]]]$coef[2])) &
        (is.na(coef[[dateEnd[idx]]][[coefEnd[idx]]]$coef[1]) | is.na(coef[[dateEnd[idx]]][[coefEnd[idx]]]$coef[2]))){
      ofstLin <- coef[[dateBgn[idx]]][[coefBgn[idx]]]$coef[1]
      slpLin <- coef[[dateBgn[idx]]][[coefBgn[idx]]]$coef[2]
    } # ending the logic when coefficients in Date[idx] are not NAs but Date[idx+1] are not NAs
    
    #case3: when coefficients in Date[idx] are NAs
    if (is.na(coef[[dateBgn[idx]]][[coefBgn[idx]]]$coef[1]) & is.na(coef[[dateBgn[idx]]][[coefBgn[idx]]]$coef[2])){
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
      subData$rtioMoleDryH2oCor <- NA
    } else {
      subData$rtioMoleDryCo2Cor <- as.numeric(ofstLin + subData$rtioMoleDryCo2*slpLin)
      #place holder for future h2o correction
      subData$rtioMoleDryH2oCor <- NA
    }
    
    #outSub[[idx]] <- subData
    # case #1: first day (creation)
    if(numDate == 1) {
      allSubData <- as.ffdf(data.frame(subData))
    }else{
      # case #2: subsequent day (appending)
      allSubData <- ffbase::ffdfappend(allSubData, subData)
    }
}
#append dataframe
#outTmp00 <- do.call(rbind,outSub)
#change row.name in allSubData
  row.names(allSubData) <-  make.names(1:length(allSubData$time), unique = TRUE)
#return data only the processing date
#report time
options(digits.secs=3) 
rptTimeBgn <- base::as.POSIXlt(paste(DateProc, " ", "00:00:00.0001", sep=""), format="%Y-%m-%d %H:%M:%OS", tz="UTC")
rptTimeEnd <- base::as.POSIXlt(paste(DateProc, " ", "23:59:59.9502", sep=""), format="%Y-%m-%d %H:%M:%OS", tz="UTC")
outTmp01<- data.frame(allSubData[][which(allSubData$time[] >= rptTimeBgn & allSubData$time[] < rptTimeEnd),])
#generate time according to frequency
timeRglr <- seq.POSIXt(
  from = base::as.POSIXlt(rptTimeBgn, format="%Y-%m-%d %H:%M:%OS", tz="UTC"),
  to = base::as.POSIXlt(rptTimeEnd, format="%Y-%m-%d %H:%M:%OS", tz="UTC"),
  by = 1/Freq
)

#regularize the data
rpt <- eddy4R.base::def.rglr(timeMeas = base::as.POSIXlt(outTmp01$time, format="%Y-%m-%d %H:%M:%OS", tz="UTC"),
                                                           dataMeas = outTmp01,
                                                           BgnRglr = as.POSIXlt(min(timeRglr)),
                                                           EndRglr = as.POSIXlt(max(timeRglr)+0.0002),
                                                           FreqRglr = Freq,
                                                           MethRglr = "CybiEc"
)$dataRglr

#replace time to regularize time
rpt$time <- timeRglr
#check if rtioMoleDryCo2Cor in rpt if not add them with all NA
if (length(rpt$rtioMoleDryCo2Cor) == 0) {rpt$rtioMoleDryCo2Cor <- NA}
if (length(rpt$rtioMoleDryH2oCor) == 0) {rpt$rtioMoleDryH2oCor <- NA}
#Creating the index to organize the variables in alphabetical order
idxIrga <- order(names(rpt))
#Changing the order of the variables to alphabetical order using the index
rpt <- rpt[,idxIrga]
#adding unit attributes
attrUnit <- c("-", "-", "molCo2 m-3", "molH2o m-3", "NA", "V", "W", "W", "W", "W", "Pa", "Pa", "Pa", "molCo2 mol-1Dry", "molCo2 mol-1Dry", "molH2o mol-1Dry",
              "molH2o mol-1Dry", "-", "-", "K", "K", "K", "K", "NA")

for(idxVar in 1:length(attrUnit)) {
  
  base::attr(x = rpt[[idxVar]], which = "unit") <-
    attrUnit[idxVar]
  
}; rm(idxVar, outSub, outTmp00, outTmp01)

invisible(gc())
#return results
return(rpt)

}#end function

