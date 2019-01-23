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
#' The returned object consists of:\cr
#' \code{rtioMoleDryCo2Vali} scriptive statistics (mean, min, max, vari, numSamp, se) of CO2 dry mole concentration during performing validation and CO2 dry mole concentration of reference gases.\cr
#' \code{rtioMoleDryCo2Mlf} linear regression coefficients resulted from the maximum-likelihood fitting of a functional relationship. \cr
#' \code{rtioMoleDryCo2Cor} dataframe consists of the correction IRGA sub data products.

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
#   Natchaya P-Durden (2019-01-17)
#     adding the logic to handle when there are more than one validation occurred within one day
#   Natchaya P-Durden (2019-01-22)
#     replace command lines to apply the correction value by the definition function
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
  valiData <- list()
  #create temporary place to host coeficience values
  tmpCoef <- list()
  
  #dates that will be used in determination of slope and offset
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
          #report data
          rptTmp[[idxNameQf]][[idxAgr]] <- tmp[[idxNameQf]][[idxAgr]]
          #report time
          rptTmp[[idxNameQf]][[idxAgr]]$timeBgn <- data.frame(rtioMoleDryCo2 = idxVali$timeBgn[[idxAgr]])
          rptTmp[[idxNameQf]][[idxAgr]]$timeEnd <- data.frame(rtioMoleDryCo2 = idxVali$timeEnd[[idxAgr]])
        }#end for each idxAgr
        #end for at least there is one measurement
      } else {
        for(idxStat in NameStat){
          #report data
          rptTmp[[idxNameQf]][[1]][[idxStat]] <- data.frame(rtioMoleDryCo2 = NaN)
        }#end idxStat
        #report time
        rptTmp[[idxNameQf]][[1]]$timeBgn <- data.frame(rtioMoleDryCo2 = base::as.POSIXlt(paste(DateBgn, " ", "00:00:00.000", sep=""), format="%Y-%m-%d %H:%M:%OS", tz="UTC"))
        rptTmp[[idxNameQf]][[1]]$timeEnd <- data.frame(rtioMoleDryCo2 = base::as.POSIXlt(paste(DateBgn, " ", "23:59:59.950", sep=""), format="%Y-%m-%d %H:%M:%OS", tz="UTC"))
      }
    }#end of each qf in nameQf
    
    #return results as dataframe
    outTmp00 <-list()
    outTmp01 <- list()
    outTmp02 <- list()
    
    for (idxGas in names(rptTmp)){
      #for (idxGas in c("qfIrgaTurbValiGas02", "qfIrgaTurbValiGas03", "qfIrgaTurbValiGas04")){
      #idxGas <- names(rptTmp)[2]
      for (idxLoc in 1:length(rptTmp[[idxGas]])){
      for (idxStat in names(rptTmp[[idxGas]][[idxLoc]])){
        #idxStat <- names(rptTmp[[idxGas]][[idxLoc]])[1]
        outTmp00[[idxStat]] <- data.frame(rptTmp[[idxGas]][[idxLoc]][[idxStat]]$rtioMoleDryCo2)
      }
       outTmp01[[idxLoc]] <- do.call(cbind, outTmp00)
      }
       outTmp02[[idxGas]] <- do.call(rbind, outTmp01)
       outTmp02[[idxGas]]$gasType <- idxGas
       
       #empty lists
       outTmp00 <-list()
       outTmp01 <- list()
       
    }
    
    
    #convert idxDate to character
    #idxDate <- as.character(idxDate)
    #combine row
    rpt[[idxDate]]$rtioMoleDryCo2Vali <- do.call(rbind, outTmp02)
    
    #remove
    rm(outTmp00, outTmp01, outTmp02, rptTmp, idxGas)
    
    #assign column names
    colnames(rpt[[idxDate]]$rtioMoleDryCo2Vali) <- c("mean", "min", "max", "vari", "numSamp", "se", "timeBgn", "timeEnd", "gasType")
    
    #remove row names
    rownames(rpt[[idxDate]]$rtioMoleDryCo2Vali) <- NULL
    
    #assign time window 
    timeMin <- base::as.POSIXlt(paste(DateBgn, " ", "00:01:29.950", sep=""), format="%Y-%m-%d %H:%M:%OS", tz="UTC")
    timeMax <- base::as.POSIXlt(paste(DatePost, " ", "00:01:29.950", sep=""), format="%Y-%m-%d %H:%M:%OS", tz="UTC")
    #determine index when timeEnd fall in Date Proc
    rpt[[idxDate]]$rtioMoleDryCo2Vali <- rpt[[idxDate]]$rtioMoleDryCo2Vali[which(rpt[[idxDate]]$rtioMoleDryCo2Vali$timeEnd >= timeMin &  rpt[[idxDate]]$rtioMoleDryCo2Vali$timeBgn < timeMax),]
    
    #fail safe: fill in dataframe with NaN values when there is only qfIrgaTurbValiGas01 
    if (length(rpt[[idxDate]]$rtioMoleDryCo2Vali$mean) == 1){
      rpt[[idxDate]]$rtioMoleDryCo2Vali[2:5,] <-  rpt[[idxDate]]$rtioMoleDryCo2Vali[1,]
      #replace gasType
      rpt[[idxDate]]$rtioMoleDryCo2Vali$gasType <- nameQf
    }
    
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
    }; rm(idxRow)# end for loop
    #add gas type
    tmpGasRefe$gasType <- nameQf
    #add gasRefe values into rpt
    for (idxRow in 1:nrow(rpt[[idxDate]]$rtioMoleDryCo2Vali)){
      locGas <- which(tmpGasRefe$gasType == rpt[[idxDate]]$rtioMoleDryCo2Vali$gasType[idxRow])
      rpt[[idxDate]]$rtioMoleDryCo2Vali$rtioMoleDryCo2Refe[idxRow] <- tmpGasRefe$rtioMoleDryCo2Refe[locGas]
      rpt[[idxDate]]$rtioMoleDryCo2Vali$rtioMoleDryCo2RefeSd[idxRow] <- tmpGasRefe$rtioMoleDryCo2RefeSd[locGas]
    }
    #rpt[[idxDate]]$rtioMoleDryCo2Vali$rtioMoleDryCo2Refe <- cbind(rpt[[idxDate]]$rtioMoleDryCo2Vali, tmpGasRefe)
    
    #calculate standard deviation from se
    rpt[[idxDate]]$rtioMoleDryCo2Vali$sd <- (rpt[[idxDate]]$rtioMoleDryCo2Vali$se)*(sqrt(rpt[[idxDate]]$rtioMoleDryCo2Vali$numSamp))
    
    #check if there are more than one validation occurred within one day
    if (length(which(rpt[[idxDate]]$rtioMoleDryCo2Vali$gasType == "qfIrgaTurbValiGas02")) == 2 &
        length(which(rpt[[idxDate]]$rtioMoleDryCo2Vali$gasType == "qfIrgaTurbValiGas03")) == 2&
        length(which(rpt[[idxDate]]$rtioMoleDryCo2Vali$gasType == "qfIrgaTurbValiGas04")) == 2&
        length(which(rpt[[idxDate]]$rtioMoleDryCo2Vali$gasType == "qfIrgaTurbValiGas05"))== 2){
      valiCrit <- TRUE
    } else{
      valiCrit <- FALSE
    }
    
    #if valiCrit = TRUE, separate the data into 2 table
    if (valiCrit == TRUE){
      locGas <- which(rpt[[idxDate]]$rtioMoleDryCo2Vali$gasType == "qfIrgaTurbValiGas02")
      #defined the critical time by adding 30 min after the end of running zero gas
      timeCrit00 <- as.POSIXlt(rpt[[idxDate]]$rtioMoleDryCo2Vali$timeEnd[locGas[1]] + 30*60,format="%Y-%m-%d %H:%M:%OS", tz="UTC") 
      timeCrit01 <- as.POSIXlt(rpt[[idxDate]]$rtioMoleDryCo2Vali$timeEnd[locGas[2]] + 30*60,format="%Y-%m-%d %H:%M:%OS", tz="UTC")
      #get rid of archive gas
      valiData[[idxDate]]$data00 <- rpt[[idxDate]]$rtioMoleDryCo2Vali[-which(rpt[[idxDate]]$rtioMoleDryCo2Vali$gasType == "qfIrgaTurbValiGas01"),]
      valiData[[idxDate]]$data01 <- rpt[[idxDate]]$rtioMoleDryCo2Vali[-which(rpt[[idxDate]]$rtioMoleDryCo2Vali$gasType == "qfIrgaTurbValiGas01"),]
      #select data within timeCrit
      valiData[[idxDate]]$data00 <- valiData[[idxDate]]$data00[which(valiData[[idxDate]]$data00$timeEnd < timeCrit00),]
      valiData[[idxDate]]$data01 <- valiData[[idxDate]]$data01[which(valiData[[idxDate]]$data01$timeEnd < timeCrit01),]
    }; rm (locGas, timeCrit00, timeCrit01)
    
    if (valiCrit == FALSE){
      #get rid of archive gas
      valiData[[idxDate]]$data00 <- rpt[[idxDate]]$rtioMoleDryCo2Vali[-which(rpt[[idxDate]]$rtioMoleDryCo2Vali$gasType == "qfIrgaTurbValiGas01"),]
      if (length(valiData[[idxDate]]$data00) == 4){
        valiData[[idxDate]]$data00 <- valiData[[idxDate]]$data00
      }else{
      #incase of more data than expected; due to valves problem
        locGas00 <- which(valiData[[idxDate]]$data00$gasType == "qfIrgaTurbValiGas02")
        #defined the critical time by adding 30 min after the end of running zero gas
        timeCrit00 <- as.POSIXlt(valiData[[idxDate]]$data00$timeEnd[locGas00[1]] + 30*60,format="%Y-%m-%d %H:%M:%OS", tz="UTC") 
        #select data within timeCrit
        valiData[[idxDate]]$data00 <- valiData[[idxDate]]$data00[which(valiData[[idxDate]]$data00$timeEnd < timeCrit00),]
        #check if there are all data as expected
        if (length(valiData[[idxDate]]$data00) == 4){
          valiData[[idxDate]]$data00 <- valiData[[idxDate]]$data00
        }else{
          #incase of valves malfunction
          for (idxGas in c("qfIrgaTurbValiGas02", "qfIrgaTurbValiGas03", "qfIrgaTurbValiGas04", "qfIrgaTurbValiGas05")){
            locGas01 <- which(valiData[[idxDate]]$data00$gasType == idxGas)
            if (length(locGas01) == 1){
              valiData[[idxDate]]$data00 <- valiData[[idxDate]]$data00
            }else{
              #keep the last value
              valiData[[idxDate]]$data00 <- valiData[[idxDate]]$data00[-c(1:length(locGas01)-1),]
            }#end else
          }#end for
        }#end else
        }#end else
      valiData[[idxDate]]$data01 <- valiData[[idxDate]]$data00
    }
    
    #calculate linear regression between validation gas standard and sensor reading values
    #using maximum-likelihood fitting of a functional relationship (MLFR)
    #calculate linear regression for each of valiData[[idxDate]]$data01 and valiData[[idxDate]]$data00
    #test if all inputs are NA
    for (idxData in names(valiData[[idxDate]])){
      #create empty dataframe to keep intercept and slope output from MLFR
      tmpCoef[[idxDate]][[idxData]] <- data.frame(matrix(ncol = 2, nrow = 2)) 
      #assign column name
      colnames(tmpCoef[[idxDate]][[idxData]]) <- c("coef", "se")
      
    if (all(is.na(valiData[[idxDate]][[idxData]]$mean)) | all(is.na(valiData[[idxDate]][[idxData]]$se)) |
        all(is.na(valiData[[idxDate]][[idxData]]$rtioMoleDryCo2Refe)) | all(is.na(valiData[[idxDate]][[idxData]]$rtioMoleDryCo2RefeSd))){
      tmpCoef[[idxDate]][[idxData]][,] <- NA
    } else{
      rtioMoleDryCo2Mlfr <- deming::deming(mean[1:4]~rtioMoleDryCo2Refe[1:4], data = valiData[[idxDate]][[idxData]],
                                           xstd = rtioMoleDryCo2RefeSd[1:4], ystd = sd[1:4]) 
      #write output to table
      #intercept
      tmpCoef[[idxDate]][[idxData]][1,1] <- rtioMoleDryCo2Mlfr$coefficients[[1]]
      #slope
      tmpCoef[[idxDate]][[idxData]][2,1] <- rtioMoleDryCo2Mlfr$coefficients[[2]]
      #se
      tmpCoef[[idxDate]][[idxData]][,2] <- rtioMoleDryCo2Mlfr$se
    }
    }#end of for loop of idxData
    
    #report output
    rpt[[idxDate]]$rtioMoleDryCo2Mlf <- tmpCoef[[idxDate]]$data00
    #reorder column
    rpt[[idxDate]]$rtioMoleDryCo2Vali <- rpt[[idxDate]]$rtioMoleDryCo2Vali[,c(1:5, 10, 7, 8)]
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
    
    }; rm(valiCrit)#end of idxDate
   
  #check if there are more than one validation occurred in DateProc
  if (length(which(rpt[[DateProc]]$rtioMoleDryCo2Vali$gasType == "qfIrgaTurbValiGas02")) == 2 &
      length(which(rpt[[DateProc]]$rtioMoleDryCo2Vali$gasType == "qfIrgaTurbValiGas03")) == 2&
      length(which(rpt[[DateProc]]$rtioMoleDryCo2Vali$gasType == "qfIrgaTurbValiGas04")) == 2&
      length(which(rpt[[DateProc]]$rtioMoleDryCo2Vali$gasType == "qfIrgaTurbValiGas05"))== 2){
    valiCrit <- TRUE
  } else{
    valiCrit <- FALSE
  }
  
  #applying the calculated coefficients to measured data
  #Calculate time-series (20Hz) of slope and zero offset
  rpt[[DateProc]]$rtioMoleDryCo2Cor <- eddy4R.base::def.irga.vali.cor(data = data, DateProc = DateProc, coef = tmpCoef, valiData = valiData, valiCrit = valiCrit)
  
#return results
  return(rpt)
}
