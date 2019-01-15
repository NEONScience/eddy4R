##############################################################################################
#' @title Wrapper function: Validation processing for IRGA

#' @author
#' Natchaya P-Durden \email{eddy4R.info@gmail.com}

#' @description Wrapper function to apply IRGA validation.

#' @param data List consisting of \code{ff::ffdf} file-backed objects containing the dp0p input IRGA.
#' @param qfqmFlag List consisting of \code{ff::ffdf} file-backed objects containing the IRGA quality flags.
#' @param gasRefe List containing the values of the reference gases. [mol mol-1] 
#' @param DateProc A vector of class "character" containing the processing date.

#' @return 
#' The returned object consistes of descriptive statistics (mean, min, max, vari, numSamp, se) of CO2 dry mole concentration during performing validation and CO2 dry mole concentration of reference gases.

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007.

#' @keywords derived, irgaTurb, post-processing, pre-processing, validation

#' @examples
#' Currently none.

#' @seealso Currently none.

#' @export

# changelog and author contributions / copyrights
#   Natchaya P-Durden (2018-11-14)
#     original creation
#   Natchaya P-Durden (2018-11-27)  
#     including the reference gases into the output 
#   Natchaya P-Durden (2018-11-28)  
#     removing standard error from report value
#     adding unit attributes
#   Natchaya P-Durden (2018-12-04)
#     adding standard deviation calculation
#     adding maximum-likelihood fitting of a functional relationship (MLFR) 
#   Natchaya P-Durden (2019-01-03)
#     adding logic when coefficients are NAs
#   Natchaya P-Durden (2019-01-04)
#     adding unit attributes to the reported outputs
#   Natchaya P-Durden (2019-01-10)
#     adding logic to handle when there is only one day of input data
##############################################################################################

wrap.irga.vali <- function(
  data,
  qfqmFlag,
  gasRefe,
  DateProc
) {
  #adding library
  library(deming)
  library(zoo)
  
  #assign list
  rpt <- list()
  
  #dates that will be used in determination of slope and offset (DatePro and DateProc+1)
  Date <- c(base::as.Date(DateProc) - 1, base::as.Date(DateProc), base::as.Date(DateProc) + 1)
  Date <- as.character(Date)
  Freq <- 20  #measurement frequency (20 Hz)
  
  #calculation for each date in Date
  for (idxDate in Date){
    #idxDate <- Date[1]
    #processing date
    DateBgn <- base::as.Date(idxDate)
    #pre-processing date
    DatePre <- base::as.Date(idxDate) - 1
    #post-processing date
    DatePost <- base::as.Date(idxDate) + 1
    #grab 3 days window of irga data and qfqmFlag (pre-processing, processing, and post-processing date)
    #check if there are data and qfqmFlag
    allDate <- c(base::as.Date(DatePre), base::as.Date(DateBgn), base::as.Date(DatePost))
    subDataList <- list()
    subQfqmList <- list()
    
    for (idxAllDate in allDate){
      idxAllDate <- as.Date(idxAllDate, origin = "1970-01-01")
      locDate <- which(as.Date(data$irgaTurb$time[]) == as.Date(idxAllDate, origin = "1970-01-01"))
      #locDate <- which(as.Date(data$irgaTurb$time[]) == as.Date(as.POSIXlt(idxAllDate, " ", "00:00:00.0001", sep=""), format="%Y-%m-%d %H:%M:%OS", tz="UTC"))
      if(length(locDate) == 0){
        #create the empty dataframe
        subData <- data.frame(matrix(ncol = length(data$irgaTurb), nrow = length(data$irgaTurb[[1]])))
        subQfqmFlag <- data.frame(matrix(ncol = length(qfqmFlag$irgaTurb), nrow = length(qfqmFlag$irgaTurb[[1]])))
        colnames(subData) <- names(data$irgaTurb)
        colnames(subQfqmFlag) <- names(qfqmFlag$irgaTurb)
        #add time
        #output time
        options(digits.secs=3) 
        subTimeBgn <- base::as.POSIXlt(paste(base::as.Date(idxAllDate, origin = "1970-01-01"), " ", "00:00:00.0001", sep=""), format="%Y-%m-%d %H:%M:%OS", tz="UTC")
        subTimeEnd <- base::as.POSIXlt(paste(base::as.Date(idxAllDate, origin = "1970-01-01"), " ", "23:59:59.9502", sep=""), format="%Y-%m-%d %H:%M:%OS", tz="UTC")
        time <- as.POSIXlt(seq.POSIXt(
          from = as.POSIXlt(subTimeBgn, format="%Y-%m-%d %H:%M:%OS", tz="UTC"),
          to = as.POSIXlt(subTimeEnd, format="%Y-%m-%d %H:%M:%OS", tz="UTC"),
          by = 1/Freq), tz="UTC")
        subData$time <- time
      }else{
        subData <- data$irgaTurb[][min(locDate):max(locDate),]
        subQfqmFlag <- qfqmFlag$irgaTurb[][min(locDate):max(locDate),]
      }
      subDataList[[idxAllDate]] <- subData
      subQfqmList[[idxAllDate]] <- subQfqmFlag
      }#end loop for of idxAllDate
    #combine dataframe from each loop
    allSubData <- do.call(rbind, subDataList)
    allSubQfqm <- do.call(rbind, subQfqmList)
    # #get indecies
    # idxSub <- which(as.Date(data$irgaTurb$time[]) == DatePre |
    #                   as.Date(data$irgaTurb$time[]) == DateBgn |
    #                   as.Date(data$irgaTurb$time[]) == DatePost)
    #subset data
    #subData <- data$irgaTurb[][min(idxSub):max(idxSub),]
    #subset qfqmFlag
    #subQfqmFlag <- qfqmFlag$irgaTurb[][min(idxSub):max(idxSub),]
    
    nameQf <- c("qfIrgaTurbValiGas01", "qfIrgaTurbValiGas02", "qfIrgaTurbValiGas03", "qfIrgaTurbValiGas04", "qfIrgaTurbValiGas05")
    
    
    #statistical names (will be used when no validation occured at all)
    NameStat <- c("mean", "min", "max", "vari", "numSamp", "se")
    
    #assign list
    tmp <- list()
    rptTmp <- list()
    
    for (idxNameQf in nameQf){
      #idxNameQf <- nameQf[2]
      print(idxNameQf)
      #preparing the qfIrgaTurbValiGas01 to 05 data for def.idx.agr()
      #replace NA to the qf which are not equal to 1
      allSubQfqm[[idxNameQf]] <- ifelse(allSubQfqm[[idxNameQf]] != 1, NA, allSubQfqm[[idxNameQf]])
      
      #determine when validation occur
      #if there is at least one measurement
      if(length(which(!is.na(allSubQfqm[[idxNameQf]]))) > 0){
        #determine the beginning and ending indicies of each validation
        idxVali <- eddy4R.base::def.idx.agr(time = allSubData$time, PrdAgr = 90, FreqLoca = 20, MethIdx = "specEnd", data = allSubQfqm[[idxNameQf]], CritTime = 0)
        #delete row if last timeBgn and timeEnd is NA
        idxVali <- idxVali[rowSums(is.na(idxVali)) != 2,]
        #if last timeEnd is NA, replce that time to the last time value in data$time
        idxVali$timeEnd <- as.POSIXct(ifelse(is.na(idxVali$timeEnd), allSubData$time[length(allSubData$time)], idxVali$timeEnd), origin = "1970-01-01", tz = "UTC")
        
        for (idxAgr in 1:length(idxVali$timeBgn)){
          #idxAgr <- 1
          inpTmp <- data.frame(rtioMoleDryCo2 = allSubData$rtioMoleDryCo2[idxVali$idxBgn[idxAgr]:idxVali$idxEnd[idxAgr]]) 
          
          #dp01 processing
          tmp[[idxNameQf]][[idxAgr]] <- eddy4R.base::wrap.dp01(
            # assign data: data.frame or list of type numeric or integer
            data = inpTmp
          )
        }#end for each idxAgr
        #report only validation which occured on DateBgn
        #assign time window 
        timeMin <- base::as.POSIXlt(paste(DateBgn, " ", "00:01:29.950", sep=""), format="%Y-%m-%d %H:%M:%OS", tz="UTC")
        timeMax <- base::as.POSIXlt(paste(DatePost, " ", "00:01:29.950", sep=""), format="%Y-%m-%d %H:%M:%OS", tz="UTC")
        #determine index when timeEnd fall in Date Proc
        idxTime <- which(idxVali$timeEnd >= timeMin &  idxVali$timeEnd < timeMax)
        if (length(idxTime) != 0){
          #report data
          rptTmp[[idxNameQf]] <- tmp[[idxNameQf]][[idxTime]]
          #report time
          rptTmp[[idxNameQf]]$timeBgn <- data.frame(rtioMoleDryCo2 = idxVali$timeBgn[idxTime])
          rptTmp[[idxNameQf]]$timeEnd <- data.frame(rtioMoleDryCo2 = idxVali$timeEnd[idxTime])
          #units:
          #attributes(rptTmp[[idxNameQf]]$mean$rtioMoleDryCo2)$unit <- attributes(data$irgaTurb$rtioMoleDryCo2)$unit
        } else {
          for(idxStat in NameStat){
            #report data
            rptTmp[[idxNameQf]][[idxStat]] <- data.frame(rtioMoleDryCo2 = NaN)
            #report time
            rptTmp[[idxNameQf]]$timeBgn <- data.frame(rtioMoleDryCo2 = base::as.POSIXlt(paste(DateBgn, " ", "00:00:00.000", sep=""), format="%Y-%m-%d %H:%M:%OS", tz="UTC"))
            rptTmp[[idxNameQf]]$timeEnd <- data.frame(rtioMoleDryCo2 = base::as.POSIXlt(paste(DateBgn, " ", "23:59:59.950", sep=""), format="%Y-%m-%d %H:%M:%OS", tz="UTC"))
            #units:
            #attributes(rptTmp[[idxNameQf]]$mean$rtioMoleDryCo2)$unit <- attributes(data$irgaTurb$rtioMoleDryCo2)$unit
          }#end idxStat
          
        }#end idxTime
        #end for at least there is one measurement
      } else {
        for(idxStat in NameStat){
          #report data
          rptTmp[[idxNameQf]][[idxStat]] <- data.frame(rtioMoleDryCo2 = NaN)
        }#end idxStat
        #report time
        rptTmp[[idxNameQf]]$timeBgn <- data.frame(rtioMoleDryCo2 = base::as.POSIXlt(paste(DateBgn, " ", "00:00:00.000", sep=""), format="%Y-%m-%d %H:%M:%OS", tz="UTC"))
        rptTmp[[idxNameQf]]$timeEnd <- data.frame(rtioMoleDryCo2 = base::as.POSIXlt(paste(DateBgn, " ", "23:59:59.950", sep=""), format="%Y-%m-%d %H:%M:%OS", tz="UTC"))
      }
    }#end of each qf in nameQf
    
    #return results as dataframe
    outTmp00 <-list()
    outTmp01 <- list()
    
    for (idxGas in names(rptTmp)){
      #for (idxGas in c("qfIrgaTurbValiGas02", "qfIrgaTurbValiGas03", "qfIrgaTurbValiGas04")){
      #idxGas <- names(rptTmp)[2]
      for (idxStat in names(rptTmp[[idxGas]])){
        #idxStat <- names(rptTmp[[idxGas]])[1]
        outTmp00[[idxStat]] <- data.frame(rptTmp[[idxGas]][[idxStat]]$rtioMoleDryCo2)
      }
      outTmp01[[idxGas]] <- do.call(cbind, outTmp00)
    }
    
    #convert idxDate to character
    #idxDate <- as.character(idxDate)
    #combine row
    rpt[[idxDate]]$rtioMoleDryCo2Vali <- do.call(rbind, outTmp01)
    
    #remove
    rm(outTmp00, outTmp01, rptTmp)
    
    #assign column names
    colnames(rpt[[idxDate]]$rtioMoleDryCo2Vali) <- c("mean", "min", "max", "vari", "numSamp", "se", "timeBgn", "timeEnd")
    
    #remove row names
    rownames(rpt[[idxDate]]$rtioMoleDryCo2Vali) <- NULL
    
    #add gasRefe values into rpt
    #create temporary dataframe
    tmpGasRefe <- data.frame(matrix(ncol = 2, nrow = 5))
    #assign column name
    colnames(tmpGasRefe) <- c("rtioMoleDryCo2Refe", "rtioMoleDryCo2RefeSd")
    #add values of gasRefe and their sd to tmpGasRefe
    for (idxRow in 1:nrow(tmpGasRefe)){
      if (idxRow == 2){
        #add zero gas
        tmpGasRefe[idxRow,1] <- 0
        tmpGasRefe[idxRow,2] <- NA
      }else{
        #get location in gasRefe
        if (idxRow == 1){
          loc <- idxRow
        } else{
          loc <- idxRow-1
        }
        #if no gasRefe and sd for idxDate
        if (is.null(gasRefe$rtioMoleDryCo2Refe01[[idxDate]])){
          tmpGasRefe <- tmpGasRefe
        } else {
        #test time condition for picking the right value
        if (gasRefe$rtioMoleDryCo2RefeTime01[[idxDate]][[loc]] == gasRefe$rtioMoleDryCo2RefeTime02[[idxDate]][[loc]]){
          tmpGasRefe[idxRow,1] <- gasRefe$rtioMoleDryCo2Refe01[[idxDate]][[loc]]
          tmpGasRefe[idxRow,2] <- eddy4R.base::def.unit.conv(data = gasRefe$rtioMoleDryCo2RefeSd01[[idxDate]][[loc]],
                                                             unitFrom = "umol mol-1",
                                                             unitTo = "intl")
        } else {
          if (rpt[[idxDate]]$rtioMoleDryCo2Vali$timeBgn[idxRow] >= gasRefe$rtioMoleDryCo2RefeTime02[[idxDate]][[loc]]){
            tmpGasRefe[idxRow,1] <- gasRefe$rtioMoleDryCo2Refe02[[idxDate]][[loc]]
            tmpGasRefe[idxRow,2] <- eddy4R.base::def.unit.conv(data = gasRefe$rtioMoleDryCo2RefeSd02[[idxDate]][[loc]],
                                                               unitFrom = "umol mol-1",
                                                               unitTo = "intl")
          } else {
            tmpGasRefe[idxRow,1] <- gasRefe$rtioMoleDryCo2Refe01[[idxDate]][[loc]]
            tmpGasRefe[idxRow,2] <- eddy4R.base::def.unit.conv(data = gasRefe$rtioMoleDryCo2RefeSd01[[idxDate]][[loc]],
                                                               unitFrom = "umol mol-1",
                                                               unitTo = "intl")
          }
        }
        }
      }
    }# end for loop
    
    #add gasRefe values into rpt
    rpt[[idxDate]]$rtioMoleDryCo2Vali <- cbind(rpt[[idxDate]]$rtioMoleDryCo2Vali, tmpGasRefe)
    
    #calculate standard deviation from se
    rpt[[idxDate]]$rtioMoleDryCo2Vali$sd <- (rpt[[idxDate]]$rtioMoleDryCo2Vali$se)*(sqrt(rpt[[idxDate]]$rtioMoleDryCo2Vali$numSamp))
    
    #calculate linear regression between validation gas standard and sensor reading values
    #using maximum-likelihood fitting of a functional relationship (MLFR)
    #create empty dataframe to keep intercept and slope output from MLFR
    rpt[[idxDate]]$rtioMoleDryCo2Mlf <- data.frame(matrix(ncol = 2, nrow = 2))
    #assign column name
    colnames(rpt[[idxDate]]$rtioMoleDryCo2Mlf) <- c("coef", "se")
    
    #test if all inputs are NA
    if (all(is.na(rpt[[idxDate]]$rtioMoleDryCo2Vali$mean)) | all(is.na(rpt[[idxDate]]$rtioMoleDryCo2Vali$se)) |
        all(is.na(rpt[[idxDate]]$rtioMoleDryCo2Vali$rtioMoleDryCo2Refe)) | all(is.na(rpt[[idxDate]]$rtioMoleDryCo2Vali$rtioMoleDryCo2RefeSd))){
      rpt[[idxDate]]$rtioMoleDryCo2Mlf[,] <- NA
    } else{
      rtioMoleDryCo2Mlfr <- deming::deming(mean[2:5]~rtioMoleDryCo2Refe[2:5], data = rpt[[idxDate]]$rtioMoleDryCo2Vali,
                                           xstd = rtioMoleDryCo2RefeSd[2:5], ystd = sd[2:5]) 
      #write output to table
      #intercept
      rpt[[idxDate]]$rtioMoleDryCo2Mlf[1,1] <- rtioMoleDryCo2Mlfr$coefficients[[1]]
      #slope
      rpt[[idxDate]]$rtioMoleDryCo2Mlf[2,1] <- rtioMoleDryCo2Mlfr$coefficients[[2]]
      #se
      rpt[[idxDate]]$rtioMoleDryCo2Mlf[,2] <- rtioMoleDryCo2Mlfr$se
    }
    #reorder column
    rpt[[idxDate]]$rtioMoleDryCo2Vali <- rpt[[idxDate]]$rtioMoleDryCo2Vali[,c(1:5, 9, 7, 8)]
    #unit attributes
    unitRtioMoleDryCo2Vali <- attributes(data$irgaTurb$rtioMoleDryCo2)$unit
    
    attributes(rpt[[idxDate]]$rtioMoleDryCo2Vali)$unit <- c(unitRtioMoleDryCo2Vali, #"mean"
                                                            unitRtioMoleDryCo2Vali, #"min"
                                                            unitRtioMoleDryCo2Vali, #"max" 
                                                            unitRtioMoleDryCo2Vali,#"vari"
                                                            "NA", #"numSamp"
                                                            attributes(gasRefe$rtioMoleDryCo2Refe01[[idxDate]]$`702_000`)$unit,#"rtioMoleDryCo2Refe"
                                                            "NA", #"timeBgn"
                                                            "NA")#"timeEnd"
  }#end of idxDate
                                                
  #applying the calculated coefficients to measured data
  #Calculate time-series (20Hz) of slope and zero offset 
  outSub <- list()
  for (idx in 1:2){
    #idx <- 2
    #time begin and time End to apply coefficient
    #time when performing of high gas is done
    timeBgn <- as.POSIXlt(rpt[[Date[idx]]]$rtioMoleDryCo2Vali$timeEnd[5])
    #time when performing of zero gas is started
    timeEnd <- as.POSIXlt(rpt[[Date[idx+1]]]$rtioMoleDryCo2Vali$timeBgn[2])
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
    
    #when coefficients are not NAs
    if (!is.na(rpt[[Date[idx]]]$rtioMoleDryCo2Mlf$coef[1]) & !is.na(rpt[[Date[idx]]]$rtioMoleDryCo2Mlf$coef[2]) &
        !is.na(rpt[[Date[idx+1]]]$rtioMoleDryCo2Mlf$coef[1]) & !is.na(rpt[[Date[idx+1]]]$rtioMoleDryCo2Mlf$coef[2])){
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
  #return data only the processing date
  #report time
  options(digits.secs=3) 
  rptTimeBgn <- base::as.POSIXlt(paste(DateProc, " ", "00:00:00.0001", sep=""), format="%Y-%m-%d %H:%M:%OS", tz="UTC")
  rptTimeEnd <- base::as.POSIXlt(paste(DateProc, " ", "23:59:59.9502", sep=""), format="%Y-%m-%d %H:%M:%OS", tz="UTC")
  outTmp03<- data.frame(outTmp02[which(outTmp02$time >= rptTimeBgn & outTmp02$time < rptTimeEnd),])
  #generate time according to frequency
  timeRglr <- seq.POSIXt(
    from = base::as.POSIXlt(rptTimeBgn, format="%Y-%m-%d %H:%M:%OS", tz="UTC"),
    to = base::as.POSIXlt(rptTimeEnd, format="%Y-%m-%d %H:%M:%OS", tz="UTC"),
    by = 1/Freq
  )
  
  rpt[[DateProc]]$rtioMoleDryCo2Cor <- eddy4R.base::def.rglr(timeMeas = base::as.POSIXlt(outTmp03$time, format="%Y-%m-%d %H:%M:%OS", tz="UTC"),
                                dataMeas = outTmp03,
                                BgnRglr = as.POSIXlt(min(timeRglr)),
                                EndRglr = as.POSIXlt(max(timeRglr)+0.0002),
                                FreqRglr = Freq,
                                MethRglr = "CybiEc"
  )$dataRglr
  
  #replace time to regularize time
  rpt[[DateProc]]$rtioMoleDryCo2Cor$time <- timeRglr
  #Creating the index to organize the variables in alphabetical order
  idxIrga <- order(names(rpt[[DateProc]]$rtioMoleDryCo2Cor))
  #Changing the order of the variables to alphabetical order using the index
  rpt[[DateProc]]$rtioMoleDryCo2Cor <- rpt[[DateProc]]$rtioMoleDryCo2Cor[,idxIrga]
  
  attrUnit <- c("-", "-", "molCo2 m-3", "molH2o m-3", "NA", "V", "W", "W", "W", "W", "Pa", "Pa", "Pa", "molCo2 mol-1Dry", "molCo2 mol-1Dry",
                                                                "molH2o mol-1Dry", "-", "-", "K", "K", "K", "K", "NA")
  
  for(idxVar in 1:length(attrUnit)) {
    
    base::attr(x = rpt[[DateProc]]$rtioMoleDryCo2Cor[[idxVar]], which = "unit") <-
      attrUnit[idxVar]
    
  }; rm(idxVar)
  
#return results
  return(rpt)
}
