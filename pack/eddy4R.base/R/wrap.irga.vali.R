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
#   Natchaya P-Durden (2019-01-24)
#     revise the MLFR code
#   Natchaya P-Durden (2019-02-04)
#     bugs fixed to retrieve the last value when valve malfunction
#   Natchaya P-Durden (2019-02-13)
#     changed the rtioMoleDryCo2RefeSd of zero gas to 0.1 ppm
#     bugs fixed on the output of standard error of coefficients
#   Natchaya P-Durden (2019-02-14)
#     using standard error in MFL instead of standard deviation
#   Natchaya P-Durden (2019-02-19)
#     output scale from MLFR
#     revise code due to the values in Para$Cal are standard error not standard deviation
#   Natchaya P-Durden (2019-03-05)
#     apply ff object to dataframe to save the memory
#   Natchaya P-Durden (2019-03-15)
#     clean up workflow and updated def.irga.vali.cor()
#   Natchaya P-Durden (2019-03-27)
#     changing the logic to determine the critical time when 2 validation occurred within one day
#   Natchaya P-Durden (2019-05-09)
#     updating logic in fail-safe to fill in dataframe with NaN when there is only archive gas or no validation at all
#     bug fix on selecting the validation gas based on timeCrit
##############################################################################################

wrap.irga.vali <- function(
  data,
  qfqmFlag,
  gasRefe,
  DateProc
) {

  #adding library
  #library(deming)
  #library(zoo)

  #assign list
  rpt <- list()
  valiData <- list()
  #create temporary place to host coeficience values
  tmpCoef <- list()

  #dates that will be used in determination of slope and offset
  Date <- c(base::as.Date(DateProc) - 1, base::as.Date(DateProc), base::as.Date(DateProc) + 1)
  Date <- as.character(Date)
  Freq <- 20  #measurement frequency (20 Hz)
  #standard error of zero gas (unit in mol mol-1)
  zeroRefeSe <- 1*10^(-6)

  #calculation for each date in Date
  for (idxDate in Date){
    #idxDate <- Date[1]
    #processing date
    DateBgn <- base::as.Date(idxDate)
    #pre-processing date
    DatePre <- base::as.Date(idxDate) - 1
    #post-processing date
    DatePost <- base::as.Date(idxDate) + 1
    #define 3 days which data will be used
    allDate <- c(base::as.Date(DatePre), base::as.Date(DateBgn), base::as.Date(DatePost))

    #grab 3 days window of irga data and qfqmFlag (pre-processing, processing, and post-processing date)
    numDate <- 0

    for (idxAllDate in allDate){
      numDate <- numDate + 1
      idxAllDate <- as.Date(idxAllDate, origin = "1970-01-01")
      locDate <- which(as.Date(data$irgaTurb$time[]) == as.Date(idxAllDate, origin = "1970-01-01"))
      #check if there are data and qfqmFlag
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
        subTime <- data.frame(time = as.POSIXlt(time))
      }else{
        subData <- data$irgaTurb[][min(locDate):max(locDate),]
        subQfqmFlag <- qfqmFlag$irgaTurb[][min(locDate):max(locDate),]
        subTime <- data.frame(time = as.POSIXlt(subData$time))
      }

      # case #1: first day (creation)
      if(numDate == 1) {
        allSubData <- as.ffdf(data.frame(subData))
        allSubQfqm <- as.ffdf(data.frame(subQfqmFlag))
        allSubTime <- as.ffdf(data.frame(subTime))
      }else{
        # case #2: subsequent day (appending)
        allSubData <- ffbase::ffdfappend(allSubData, subData)
        allSubQfqm <- ffbase::ffdfappend(allSubQfqm, subQfqmFlag)
        allSubTime <- ffbase::ffdfappend(allSubTime, subTime)
      }
      };rm(subData, subQfqmFlag, subTime)#end loop for of idxAllDate
    #define qf for each gas cylinder
    nameQf <- c("qfIrgaTurbValiGas01", "qfIrgaTurbValiGas02", "qfIrgaTurbValiGas03", "qfIrgaTurbValiGas04", "qfIrgaTurbValiGas05")

    #statistical names (will be used when no validation occured at all)
    NameStat <- c("mean", "min", "max", "vari", "numSamp", "se")

    #assign list
    tmp <- list()
    rptTmp <- list()

    #calculate statistical for each gas
    for (idxNameQf in nameQf){
      #idxNameQf <- nameQf[2]
      tryCatch({log$debug(idxNameQf)}, error=function(cond){print(idxNameQf)})
      #preparing the qfIrgaTurbValiGas01 to 05 data for def.idx.agr()
      #replace NA to the qf which are not equal to 1
      allSubQfqm[[idxNameQf]][] <- ifelse(allSubQfqm[[idxNameQf]][] != 1, NA, allSubQfqm[[idxNameQf]][])

      #determine when validation occur
      #if there is at least one measurement
      if(length(which(!is.na(allSubQfqm[[idxNameQf]][]))) > 0){
        #determine the beginning and ending indicies of each validation
        idxVali <- eddy4R.base::def.idx.agr(time = allSubTime$time[], PrdAgr = 90, FreqLoca = 20, MethIdx = "specEnd", data = allSubQfqm[[idxNameQf]][], CritTime = 0)
        #delete row if last timeBgn and timeEnd is NA
        idxVali <- idxVali[rowSums(is.na(idxVali)) != 2,]
        #if last timeEnd is NA, replce that time to the last time value in data$time
        idxVali$timeEnd <- as.POSIXct(ifelse(is.na(idxVali$timeEnd), allSubData$time[length(allSubData$time)], idxVali$timeEnd), origin = "1970-01-01", tz = "UTC")

        for (idxAgr in 1:length(idxVali$timeBgn)){
          #idxAgr <- 1
          inpTmp <- data.frame(rtioMoleDryCo2 = allSubData$rtioMoleDryCo2[idxVali$idxBgn[idxAgr]:idxVali$idxEnd[idxAgr]])

          #statistical processing
          tmp[[idxNameQf]][[idxAgr]] <- eddy4R.base::wrap.dp01(data = inpTmp)
          #report data
          rptTmp[[idxNameQf]][[idxAgr]] <- tmp[[idxNameQf]][[idxAgr]]
          #report time
          rptTmp[[idxNameQf]][[idxAgr]]$timeBgn <- data.frame(rtioMoleDryCo2 = idxVali$timeBgn[[idxAgr]])
          rptTmp[[idxNameQf]][[idxAgr]]$timeEnd <- data.frame(rtioMoleDryCo2 = idxVali$timeEnd[[idxAgr]])
        }#end for each idxAgr
        #end for at least there is one measurement
      } else {#if there is no measurement
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

    #Transform rptTmp into report dataframe format
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

    #combine row and save statistical outputs into rpt[[idxDate]]$rtioMoleDryCo2Vali
    rpt[[idxDate]]$rtioMoleDryCo2Vali <- do.call(rbind, outTmp02)

    #assign column names
    colnames(rpt[[idxDate]]$rtioMoleDryCo2Vali) <- c("mean", "min", "max", "vari", "numSamp", "se", "timeBgn", "timeEnd", "gasType")

    #remove row names
    rownames(rpt[[idxDate]]$rtioMoleDryCo2Vali) <- NULL

    #remove unuse objects
    rm(outTmp00, outTmp01, outTmp02, rptTmp, idxGas)

    #select only data fall in DateProc
    #assign time window
    timeMin <- base::as.POSIXlt(paste(DateBgn, " ", "00:01:29.950", sep=""), format="%Y-%m-%d %H:%M:%OS", tz="UTC")
    timeMax <- base::as.POSIXlt(paste(DatePost, " ", "00:01:29.950", sep=""), format="%Y-%m-%d %H:%M:%OS", tz="UTC")
    #determine index when timeEnd fall in DateProc
    rpt[[idxDate]]$rtioMoleDryCo2Vali <- rpt[[idxDate]]$rtioMoleDryCo2Vali[which(rpt[[idxDate]]$rtioMoleDryCo2Vali$timeEnd >= timeMin &  rpt[[idxDate]]$rtioMoleDryCo2Vali$timeBgn < timeMax),]

    #fail safe: fill in dataframe with NaN values when there is only qfIrgaTurbValiGas01 or no validation at all
    if (length(rpt[[idxDate]]$rtioMoleDryCo2Vali$mean) <= 1){
      if(length(rpt[[idxDate]]$rtioMoleDryCo2Vali$mean) == 1 & rpt[[idxDate]]$rtioMoleDryCo2Vali$gasType[1] == "qfIrgaTurbValiGas01"){
        rpt[[idxDate]]$rtioMoleDryCo2Vali[2:5,] <-  NA
        rpt[[idxDate]]$rtioMoleDryCo2Vali$numSamp <- NaN
        rpt[[idxDate]]$rtioMoleDryCo2Vali$timeBgn[2:5] <- base::as.POSIXlt(paste(idxDate, " ", "00:00:00.000", sep=""), format="%Y-%m-%d %H:%M:%OS", tz="UTC")
        rpt[[idxDate]]$rtioMoleDryCo2Vali$timeEnd[2:5] <- base::as.POSIXlt(paste(idxDate, " ", "23:59:59.950", sep=""), format="%Y-%m-%d %H:%M:%OS", tz="UTC")
        #replace gasType
        rpt[[idxDate]]$rtioMoleDryCo2Vali$gasType <- nameQf
        }else{
        if(length(rpt[[idxDate]]$rtioMoleDryCo2Vali$mean) == 0){
          rpt[[idxDate]]$rtioMoleDryCo2Vali[1:5,] <-  NA
          rpt[[idxDate]]$rtioMoleDryCo2Vali$numSamp <- NaN
          rpt[[idxDate]]$rtioMoleDryCo2Vali$timeBgn <- base::as.POSIXlt(paste(idxDate, " ", "00:00:00.000", sep=""), format="%Y-%m-%d %H:%M:%OS", tz="UTC")
          rpt[[idxDate]]$rtioMoleDryCo2Vali$timeEnd <- base::as.POSIXlt(paste(idxDate, " ", "23:59:59.950", sep=""), format="%Y-%m-%d %H:%M:%OS", tz="UTC")
          #replace gasType
          rpt[[idxDate]]$rtioMoleDryCo2Vali$gasType <- nameQf
          }else{
          rpt[[idxDate]]$rtioMoleDryCo2Vali <- rpt[[idxDate]]$rtioMoleDryCo2Vali
        }
      }
    }

    #add gasRefe values into rpt
    #create temporary dataframe
    tmpGasRefe <- data.frame(matrix(ncol = 3, nrow = 5))
    #assign column name
    colnames(tmpGasRefe) <- c("rtioMoleDryCo2Refe", "rtioMoleDryCo2RefeSe", "rtioMoleDryCo2RefeDf")
    #add values of gasRefe and their se to tmpGasRefe
    for (idxRow in 1:nrow(tmpGasRefe)){
      if (idxRow == 2){
        #add zero gas
        tmpGasRefe[idxRow,"rtioMoleDryCo2Refe"] <- 0
        tmpGasRefe[idxRow,"rtioMoleDryCo2RefeSe"] <- NA
        tmpGasRefe[idxRow,"rtioMoleDryCo2RefeDf"] <- NA
      }else{
        #get location in gasRefe
        if (idxRow == 1){
          loc <- idxRow
        } else{
          loc <- idxRow-1
        }
        #if no gasRefe and se for idxDate
        if (is.null(gasRefe$rtioMoleDryCo2Refe01[[idxDate]])){
          tmpGasRefe <- tmpGasRefe
        } else {
        #test time condition for picking the right value
        if (gasRefe$rtioMoleDryCo2RefeTime01[[idxDate]][[loc]] == gasRefe$rtioMoleDryCo2RefeTime02[[idxDate]][[loc]]){
          tmpGasRefe[idxRow,"rtioMoleDryCo2Refe"] <- gasRefe$rtioMoleDryCo2Refe01[[idxDate]][[loc]]
          tmpGasRefe[idxRow,"rtioMoleDryCo2RefeSe"] <- eddy4R.base::def.unit.conv(data = gasRefe$rtioMoleDryCo2RefeSe01[[idxDate]][[loc]],
                                                             unitFrom = "umol mol-1",
                                                             unitTo = "intl")
          tmpGasRefe[idxRow,"rtioMoleDryCo2RefeDf"] <- gasRefe$rtioMoleDryCo2RefeDf01[[idxDate]][[loc]]
        } else {
          if (rpt[[idxDate]]$rtioMoleDryCo2Vali$timeBgn[idxRow] >= gasRefe$rtioMoleDryCo2RefeTime02[[idxDate]][[loc]]){
            tmpGasRefe[idxRow,"rtioMoleDryCo2Refe"] <- gasRefe$rtioMoleDryCo2Refe02[[idxDate]][[loc]]
            tmpGasRefe[idxRow,"rtioMoleDryCo2RefeSe"] <- eddy4R.base::def.unit.conv(data = gasRefe$rtioMoleDryCo2RefeSe02[[idxDate]][[loc]],
                                                               unitFrom = "umol mol-1",
                                                               unitTo = "intl")
            tmpGasRefe[idxRow,"rtioMoleDryCo2RefeDf"] <- gasRefe$rtioMoleDryCo2RefeDf02[[idxDate]][[loc]]
          } else {
            tmpGasRefe[idxRow,"rtioMoleDryCo2Refe"] <- gasRefe$rtioMoleDryCo2Refe01[[idxDate]][[loc]]
            tmpGasRefe[idxRow,"rtioMoleDryCo2RefeSe"] <- eddy4R.base::def.unit.conv(data = gasRefe$rtioMoleDryCo2RefeSe01[[idxDate]][[loc]],
                                                               unitFrom = "umol mol-1",
                                                               unitTo = "intl")
            tmpGasRefe[idxRow,"rtioMoleDryCo2RefeDf"] <- gasRefe$rtioMoleDryCo2RefeDf01[[idxDate]][[loc]]
          }
        }
        }
      }
    }; rm(idxRow)# end for loop

    #replace the rtioMoleDryCo2RefeSe of zero gas to 0.1 ppm
    tmpGasRefe$rtioMoleDryCo2RefeSe[2] <- zeroRefeSe
    #add gas type
    tmpGasRefe$gasType <- nameQf
    #add gasRefe values into rpt
    for (idxRow in 1:nrow(rpt[[idxDate]]$rtioMoleDryCo2Vali)){
      locGas <- which(tmpGasRefe$gasType == rpt[[idxDate]]$rtioMoleDryCo2Vali$gasType[idxRow])
      rpt[[idxDate]]$rtioMoleDryCo2Vali$rtioMoleDryCo2Refe[idxRow] <- tmpGasRefe$rtioMoleDryCo2Refe[locGas]
      rpt[[idxDate]]$rtioMoleDryCo2Vali$rtioMoleDryCo2RefeSe[idxRow] <- tmpGasRefe$rtioMoleDryCo2RefeSe[locGas]
    }

    #preparing data tables for calculating the regression
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
      #timeCrit01 <- as.POSIXlt(rpt[[idxDate]]$rtioMoleDryCo2Vali$timeEnd[locGas[2]] + 30*60,format="%Y-%m-%d %H:%M:%OS", tz="UTC")
      #get rid of archive gas
      valiData[[idxDate]]$data00 <- rpt[[idxDate]]$rtioMoleDryCo2Vali[-which(rpt[[idxDate]]$rtioMoleDryCo2Vali$gasType == "qfIrgaTurbValiGas01"),]
      valiData[[idxDate]]$data01 <- rpt[[idxDate]]$rtioMoleDryCo2Vali[-which(rpt[[idxDate]]$rtioMoleDryCo2Vali$gasType == "qfIrgaTurbValiGas01"),]
      #select data within timeCrit
      valiData[[idxDate]]$data00 <- valiData[[idxDate]]$data00[which(valiData[[idxDate]]$data00$timeEnd < timeCrit00),]
      valiData[[idxDate]]$data01 <- valiData[[idxDate]]$data01[which(valiData[[idxDate]]$data01$timeEnd > timeCrit00),]
    }; rm (locGas, timeCrit00, timeCrit01)

    subVali <- list()
    subVali01 <- list()
    if (valiCrit == FALSE){
      #get rid of archive gas
      valiData[[idxDate]]$data00 <- rpt[[idxDate]]$rtioMoleDryCo2Vali[rpt[[idxDate]]$rtioMoleDryCo2Vali$gasType != "qfIrgaTurbValiGas01",]
      if (length(valiData[[idxDate]]$data00$timeBgn) <= 4){
        valiData[[idxDate]]$data00 <- valiData[[idxDate]]$data00
      }else{
      #in case of more data than expected; due to valves problem
        locGas00 <- which(valiData[[idxDate]]$data00$gasType == "qfIrgaTurbValiGas02")
        #in case of more then one location for locGas00, select the last one
        if (length(locGas00) > 1) {
          locGas00 <- locGas00[length(locGas00)]
        }
        #defined the critical time by adding 30 min after the end of running zero gas
        timeCrit00 <- as.POSIXlt(valiData[[idxDate]]$data00$timeEnd[locGas00[1]] + 30*60,format="%Y-%m-%d %H:%M:%OS", tz="UTC")
        #select data within timeCrit
        valiData[[idxDate]]$data00 <- valiData[[idxDate]]$data00[which(valiData[[idxDate]]$data00$timeEnd >= valiData[[idxDate]]$data00$timeEnd[locGas00[1]] &
                                                                         valiData[[idxDate]]$data00$timeEnd < timeCrit00),]
        #check if there are all data as expected
        if (length(valiData[[idxDate]]$data00$timeBgn) <= 4){
          valiData[[idxDate]]$data00 <- valiData[[idxDate]]$data00
        }else{
          #incase of valves malfunction
          for (idxGas in c("qfIrgaTurbValiGas02", "qfIrgaTurbValiGas03", "qfIrgaTurbValiGas04", "qfIrgaTurbValiGas05")){
            locGas01 <- which(valiData[[idxDate]]$data00$gasType == idxGas)
            if (length(locGas01) == 1){
              subVali <- valiData[[idxDate]]$data00[locGas01,]
            }else{
              #keep the last value
              subVali <- valiData[[idxDate]]$data00[locGas01[length(locGas01)],]
            }#end else
            subVali01[[idxGas]] <- subVali
          }#end for
          valiData[[idxDate]]$data00 <- do.call(rbind, subVali01)
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
      tmpCoef[[idxDate]][[idxData]] <- data.frame(matrix(ncol = 3, nrow = 2))
      #assign column name
      colnames(tmpCoef[[idxDate]][[idxData]]) <- c("coef", "se", "scal")

    if (length(valiData[[idxDate]][[idxData]]$mean) < 4 |
        sum(is.na(valiData[[idxDate]][[idxData]]$mean)) > 0 | sum(is.na(valiData[[idxDate]][[idxData]]$se)) >0 |
        sum(is.na(valiData[[idxDate]][[idxData]]$rtioMoleDryCo2Refe)) > 0 | sum(is.na(valiData[[idxDate]][[idxData]]$rtioMoleDryCo2RefeSe)) > 1){
      tmpCoef[[idxDate]][[idxData]][,] <- NA
    } else{
      #x are sensor readings; y are reference gas values
      rtioMoleDryCo2Mlfr <- deming::deming(rtioMoleDryCo2Refe[1:4] ~ mean[1:4], data = valiData[[idxDate]][[idxData]],
                                           xstd = se[1:4], ystd = rtioMoleDryCo2RefeSe[1:4])
      #write output to table
      #intercept
      tmpCoef[[idxDate]][[idxData]][1,1] <- rtioMoleDryCo2Mlfr$coefficients[[1]]
      #slope
      tmpCoef[[idxDate]][[idxData]][2,1] <- rtioMoleDryCo2Mlfr$coefficients[[2]]
      #se
      tmpCoef[[idxDate]][[idxData]][,2] <- sqrt(diag(rtioMoleDryCo2Mlfr$variance))
      #scale
      tmpCoef[[idxDate]][[idxData]][1,3] <- rtioMoleDryCo2Mlfr$sigma
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

    }; rm(valiCrit, allSubData, allSubQfqm, allSubTime)#end of idxDate

  invisible(gc())
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
  rpt[[DateProc]]$rtioMoleDryCo2Cor <- eddy4R.base::def.irga.vali.cor(data = data, DateProc = DateProc, coef = tmpCoef, valiData = valiData, valiCrit = valiCrit, ScalMax = 20, FracSlpMax = 0.1, Freq = 20)

#return results
  return(rpt)
}
