##############################################################################################
#' @title Wrapper function: Validation processing for IRGA

#' @author
#' Natchaya P-Durden \email{eddy4R.info@gmail.com}

#' @description Wrapper function to apply IRGA validation.

#' @param data List consisting of \code{ff::ffdf} file-backed objects containing the dp0p input IRGA.
#' @param qfqmFlag List consisting of \code{ff::ffdf} file-backed objects containing the IRGA quality flags.
#' @param gasRefe List containing the values of the reference gases. [mol mol-1]
#' @param DateProc A vector of class "character" containing the processing date.
#' @param ScalMax Maximum scale value. The validation correction will not apply if scale (resulted from maximum-likelihood fitting of a functional relationship (MLFR)) is greater than ScalMax or ScalMax = FALSE. Defaults to FALSE.
#' @param FracSlp Upper and lower bounds of slope values. The validation correction will not apply if slope (resulted from regression fitting) is greater/lower than the FracSlp maximum or minimum value or FracSlp = FALSE. Defaults to FALSE.
#' @param OfstMax Maximum offset value. The validation correction will not apply if slope (resulted from regression fitting) is greater than the OfstMax (unit in mol mol-1) or OfstMax = FALSE. Defaults to FALSE.


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
#   Natchaya P-Durden (2020-01-15)
#     reporting the rtioMoleDryH2oVali table
#   Natchaya P-Durden (2020-01-31)
#     adjust workflow to run MLFR even missing one gas cylinder
#   Natchaya P-Durden (2020-03-05)
#     Set all thresholds to screen linear coefficients to FALSE.
#   Chris Florian (2021-08-03)
#     add thresholding based on benchmarking regression
#   Chris Florian (2021-08-09)
#     adding -1 flag for missing validations
#   Chris Florian (2021-08-26)
#     adding failsafe for extra validation gas rows
#   Chris Florian (2021-08-27)
#     retaining the rest of the rtioMoleDryCo2Cor for failed validations 
#     adding NaNs for meanCor with failed validations to keep structure the same
#   Chris Florian (2021-08-27)
#     resetting attributes on rtioMoleDryCo2Cor to fix issues when the corrected data was removed
#   Chris Florian (2021-02-15)
#     setting corrected data to NaN if qfEvalThsh is -1 to prevent bad data passing through if the evaluation doesn't run
#   Chris Florian (2022-03-02)
#     Updating the slope filter to allow for values not evenly centered around 1
#   Adam Young (2023-08-10)
#     Adding check on valiThsh for standard error of slope to remove days with large outliers but are not
#     flagged by check on slope coefficient estimate.
#   Adam Young (2023-08-22)
#     Modified to only pass 5 vali rows through the correction and eval scripts. Aimed to solve
#     instances where more than 5 rows (1 for each reference gas) are output.
#   Adam Young (2023-09-05)
#     Added logic to account for instances where there are less than 5 rows in valiData. 
#     Current version now ensures there are 5 rows (one for each refe gas) in each vali table
#     that is exported from function.
#   Adam Young (2023-09-11)
#     Added units attributes back in for H2oVali.
##############################################################################################

wrap.irga.vali <- function(
  data,
  qfqmFlag,
  gasRefe,
  DateProc,
  ScalMax = FALSE,
  FracSlp = FALSE,
  OfstMax = FALSE,
  evalSeMax = 0.015
) {

  #adding library
  #library(deming)
  #library(zoo)

  #dates that will be used in determination of slope and offset
  Date <- c(base::as.Date(DateProc) - 1, base::as.Date(DateProc), base::as.Date(DateProc) + 1)
  Date <- as.character(Date)
  Freq <- 20  #measurement frequency (20 Hz)
  #standard error of zero gas (unit in mol mol-1)
  zeroRefeSe <- 1*10^(-6)
  
  # Set to NULL, will be replaced with name of reference gas comparison that is 
  # producing the biggest error between corrected and reference values, and this
  # observation will be removed from regressions and further evaluation.
  gasRmv <- NULL
  
  # Outer for loop allows for option to reprocess and re-run main components of 
  # wrap.irga.vali.R under the specific conditions that:
  #   1. evalCoef passes test (i.e. > 0.95 & < 1.05) 
  #     AND
  #   2. evalCoefSe > 0.015
  # Under these specific conditions there can be a large difference between 
  # corrected and reference values but still pass the evaluation test. Under
  # the reprocessing in loop 2 the largest outlier value is removed from 
  # the deming regresion and def.irga.vali.thsh.R results. This ultimately 
  # allows for the option to keep these data if the correction is still working
  
  for (idxLoop in c(1, 2)) {
    
  #assign list
  rpt <- list()
  valiData <- list()
  #create temporary place to host coeficience values
  tmpCoef <- list()

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
        subData <- data.frame(matrix(ncol = length(data$irgaTurb), nrow = 24*60*60*Freq))#20Hz data over 1 day
        subQfqmFlag <- data.frame(matrix(ncol = length(qfqmFlag$irgaTurb), nrow = 24*60*60*Freq))
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
        allSubData <- ff::as.ffdf(data.frame(subData))
        allSubQfqm <- ff::as.ffdf(data.frame(subQfqmFlag))
        allSubTime <- ff::as.ffdf(data.frame(subTime))
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
    
    #for each loop of rtioMoleDryCo2 and rtioMoleDryH2o
    for (idxVar in c("rtioMoleDryCo2", "rtioMoleDryH2o")) {
      # idxVar <- "rtioMoleDryCo2"
      #assign list
      tmp <- list()
      rptTmp <- list()
    #calculate statistical for each gas
    for (idxNameQf in nameQf){
      #idxNameQf <- nameQf[2]
      tryCatch({rlog$debug(idxNameQf)}, error=function(cond){print(idxNameQf)})
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
          inpTmp <- data.frame(idxVar = allSubData[[idxVar]][idxVali$idxBgn[idxAgr]:idxVali$idxEnd[idxAgr]])
          colnames(inpTmp) <- idxVar
          
          #statistical processing
          tmp[[idxNameQf]][[idxAgr]] <- eddy4R.base::wrap.dp01(data = inpTmp)
          #report data
          rptTmp[[idxNameQf]][[idxAgr]] <- tmp[[idxNameQf]][[idxAgr]]
          #report time
          rptTmp[[idxNameQf]][[idxAgr]]$timeBgn <- data.frame(idxVar = idxVali$timeBgn[[idxAgr]])
          rptTmp[[idxNameQf]][[idxAgr]]$timeEnd <- data.frame(idxVar = idxVali$timeEnd[[idxAgr]])
          colnames(rptTmp[[idxNameQf]][[idxAgr]]$timeBgn) <- idxVar
          colnames(rptTmp[[idxNameQf]][[idxAgr]]$timeEnd) <- idxVar
        }#end for each idxAgr
        #end for at least there is one measurement
      } else {#if there is no measurement
        for(idxStat in NameStat){
          #report data
          rptTmp[[idxNameQf]][[1]][[idxStat]] <- data.frame(idxVar = NaN)
          colnames(rptTmp[[idxNameQf]][[1]][[idxStat]]) <- idxVar
        }#end idxStat
        #report time
        rptTmp[[idxNameQf]][[1]]$timeBgn <- data.frame(idxVar = base::as.POSIXlt(paste(DateBgn, " ", "00:00:00.000", sep=""), format="%Y-%m-%d %H:%M:%OS", tz="UTC"))
        rptTmp[[idxNameQf]][[1]]$timeEnd <- data.frame(idxVar = base::as.POSIXlt(paste(DateBgn, " ", "23:59:59.950", sep=""), format="%Y-%m-%d %H:%M:%OS", tz="UTC"))
        colnames(rptTmp[[idxNameQf]][[1]]$timeBgn) <- idxVar
        colnames(rptTmp[[idxNameQf]][[1]]$timeEnd) <- idxVar
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
        outTmp00[[idxStat]] <- data.frame(rptTmp[[idxGas]][[idxLoc]][[idxStat]][[idxVar]])
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
    valiTmp <- paste0(idxVar,"Vali")
    rpt[[idxDate]][[valiTmp]] <- do.call(rbind, outTmp02)

    #assign column names
    colnames(rpt[[idxDate]][[valiTmp]]) <- c("mean", "min", "max", "vari", "numSamp", "se", "timeBgn", "timeEnd", "gasType")

    #remove row names
    rownames(rpt[[idxDate]][[valiTmp]]) <- NULL

    #remove unuse objects
    rm(outTmp00, outTmp01, outTmp02, rptTmp, idxGas)

    #select only data fall in DateProc
    #assign time window
    timeMin <- base::as.POSIXlt(paste(DateBgn, " ", "00:01:29.950", sep=""), format="%Y-%m-%d %H:%M:%OS", tz="UTC")
    timeMax <- base::as.POSIXlt(paste(DatePost, " ", "00:01:29.950", sep=""), format="%Y-%m-%d %H:%M:%OS", tz="UTC")
    #determine index when timeEnd fall in DateProc
    rpt[[idxDate]][[valiTmp]] <- rpt[[idxDate]][[valiTmp]][which(rpt[[idxDate]][[valiTmp]]$timeEnd >= timeMin &  rpt[[idxDate]][[valiTmp]]$timeBgn < timeMax),]

    #fail safe: fill in dataframe with NaN values when there is only qfIrgaTurbValiGas01 or no validation at all
    if (length(rpt[[idxDate]][[valiTmp]]$mean) <= 1){
      if(length(rpt[[idxDate]][[valiTmp]]$mean) == 1 & rpt[[idxDate]][[valiTmp]]$gasType[1] == "qfIrgaTurbValiGas01"){
        rpt[[idxDate]][[valiTmp]][2:5,] <-  NA
        rpt[[idxDate]][[valiTmp]]$numSamp <- NaN
        rpt[[idxDate]][[valiTmp]]$timeBgn[2:5] <- base::as.POSIXlt(paste(idxDate, " ", "00:00:00.000", sep=""), format="%Y-%m-%d %H:%M:%OS", tz="UTC")
        rpt[[idxDate]][[valiTmp]]$timeEnd[2:5] <- base::as.POSIXlt(paste(idxDate, " ", "23:59:59.950", sep=""), format="%Y-%m-%d %H:%M:%OS", tz="UTC")
        #replace gasType
        rpt[[idxDate]][[valiTmp]]$gasType <- nameQf
        }else{
        if(length(rpt[[idxDate]][[valiTmp]]$mean) == 0){
          rpt[[idxDate]][[valiTmp]][1:5,] <-  NA
          rpt[[idxDate]][[valiTmp]]$numSamp <- NaN
          rpt[[idxDate]][[valiTmp]]$timeBgn <- base::as.POSIXlt(paste(idxDate, " ", "00:00:00.000", sep=""), format="%Y-%m-%d %H:%M:%OS", tz="UTC")
          rpt[[idxDate]][[valiTmp]]$timeEnd <- base::as.POSIXlt(paste(idxDate, " ", "23:59:59.950", sep=""), format="%Y-%m-%d %H:%M:%OS", tz="UTC")
          #replace gasType
          rpt[[idxDate]][[valiTmp]]$gasType <- nameQf
          }else{
          rpt[[idxDate]][[valiTmp]] <- rpt[[idxDate]][[valiTmp]]
        }
      }
    }

    #add gasRefe values into rpt
    if (idxVar == "rtioMoleDryCo2") {
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
          if (rpt[[idxDate]]$rtioMoleDryCo2Vali$timeBgn[idxRow] >= gasRefe$rtioMoleDryCo2RefeTime02[[idxDate]][[loc]]) {
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
        length(which(rpt[[idxDate]]$rtioMoleDryCo2Vali$gasType == "qfIrgaTurbValiGas03")) == 2 &
        length(which(rpt[[idxDate]]$rtioMoleDryCo2Vali$gasType == "qfIrgaTurbValiGas04")) == 2 &
        length(which(rpt[[idxDate]]$rtioMoleDryCo2Vali$gasType == "qfIrgaTurbValiGas05"))== 2){
      valiCrit <- TRUE
    } else{
      valiCrit <- FALSE
    }

    #if valiCrit = TRUE, separate the data into 2 table
    if (valiCrit == TRUE) {
      locGas <- which(rpt[[idxDate]]$rtioMoleDryCo2Vali$gasType == "qfIrgaTurbValiGas02")
      #defined the critical time by adding 30 min after the end of running zero gas
      timeCrit00 <- as.POSIXlt(rpt[[idxDate]]$rtioMoleDryCo2Vali$timeEnd[locGas[1]] + 30*60,format="%Y-%m-%d %H:%M:%OS", tz="UTC")
      #timeCrit01 <- as.POSIXlt(rpt[[idxDate]]$rtioMoleDryCo2Vali$timeEnd[locGas[2]] + 30*60,format="%Y-%m-%d %H:%M:%OS", tz="UTC")
      #get rid of archive gas
      valiData[[idxDate]]$data00 <- rpt[[idxDate]]$rtioMoleDryCo2Vali #[-which(rpt[[idxDate]]$rtioMoleDryCo2Vali$gasType == "qfIrgaTurbValiGas01"),]
      valiData[[idxDate]]$data01 <- rpt[[idxDate]]$rtioMoleDryCo2Vali #[-which(rpt[[idxDate]]$rtioMoleDryCo2Vali$gasType == "qfIrgaTurbValiGas01"),]
      #select data within timeCrit
      valiData[[idxDate]]$data00 <- valiData[[idxDate]]$data00[which(valiData[[idxDate]]$data00$timeEnd < timeCrit00),]
      valiData[[idxDate]]$data01 <- valiData[[idxDate]]$data01[which(valiData[[idxDate]]$data01$timeEnd > timeCrit00),]
    }; rm (locGas, timeCrit00, timeCrit01)

    subVali <- list()
    subVali01 <- list()
    if (valiCrit == FALSE) {
      # Keep all rows to start
      valiData[[idxDate]]$data00 <- rpt[[idxDate]]$rtioMoleDryCo2Vali #[rpt[[idxDate]]$rtioMoleDryCo2Vali$gasType != "qfIrgaTurbValiGas01",]
      
      gasTypeNum <- table(valiData[[idxDate]]$data00$gasType)
      
      if (sum(gasTypeNum) == 5 & all(gasTypeNum == 1)) {
        
        valiData[[idxDate]]$data01 <- valiData[[idxDate]]$data00
        
      } else {
        
        # First, split table into two
        tmpVali00 <- valiData[[idxDate]]$data00[valiData[[idxDate]]$data00$gasType %in% names(gasTypeNum[gasTypeNum > 1]), ]
        tmpVali01 <- valiData[[idxDate]]$data00[valiData[[idxDate]]$data00$gasType %in% names(gasTypeNum[gasTypeNum <= 1]), ]
        
        #in case of more data than expected; due to valves problem
        locGas00 <- which(tmpVali00$gasType == "qfIrgaTurbValiGas02")
        
        #in case of more then one location for locGas00, select the last one
        if (length(locGas00) > 1) {
          
          locGas00 <- locGas00[length(locGas00)]
          
          #defined the critical time by adding 30 min after the end of running zero gas
          timeCrit00 <- as.POSIXlt(tmpVali00$timeEnd[locGas00[1]] + 30*60, format="%Y-%m-%d %H:%M:%OS", tz="UTC")
          
          idxKeep <- which(tmpVali00$timeEnd >= tmpVali00$timeEnd[locGas00[1]] & tmpVali00$timeEnd < timeCrit00)
          tmpVali00 <- tmpVali00[idxKeep, ]
          
        }
        
        # Second check and solution if there are still too many rows -------------
        if (any(table(tmpVali00$gasType) > 1)) {
          #incase of valves malfunction
          for (idxGas in c("qfIrgaTurbValiGas01", "qfIrgaTurbValiGas02", "qfIrgaTurbValiGas03", "qfIrgaTurbValiGas04", "qfIrgaTurbValiGas05")){
            locGas01 <- which(tmpVali00$gasType == idxGas)
            if (length(locGas01) == 1){
              subVali <- tmpVali00[locGas01, ]
            }else{
              #keep the last value
              subVali <- tmpVali00[locGas01[length(locGas01)],]
            }#end else
            subVali01[[idxGas]] <- subVali
          }#end for
          tmpVali00 <- do.call(rbind, subVali01)
        }
        
        # Find which gasType is missing
        idxMiss <- which(!(nameQf %in% c(tmpVali00$gasType, tmpVali01$gasType)))
        
        # Add row for missing gasType
        for (tmpIdxMiss in idxMiss) {
          tmpVali01[nrow(tmpVali01) + 1, ] <- NaN
          tmpVali01$gasType[nrow(tmpVali01)] <- nameQf[tmpIdxMiss]
          tmpVali01$timeBgn[nrow(tmpVali01)] <- base::as.POSIXlt(paste(idxDate, " ", "00:00:00.000", sep=""), format="%Y-%m-%d %H:%M:%OS", tz="UTC")
          tmpVali01$timeEnd[nrow(tmpVali01)] <- base::as.POSIXlt(paste(idxDate, " ", "23:59:59.950", sep=""), format="%Y-%m-%d %H:%M:%OS", tz="UTC")
          if (nameQf[idxMiss] == "qfIrgaTurbValiGas02") {
            tmpVali01$rtioMoleDryCo2Refe[nrow(tmpVali01)] <- 0
            tmpVali01$rtioMoleDryCo2RefeSe[nrow(tmpVali01)] <- NA
          }
        }        
        
        valiData[[idxDate]]$data00 <- do.call(rbind, list(tmpVali00 = tmpVali00, tmpVali01 = tmpVali01))
        rownames(valiData[[idxDate]]$data00) <- NULL
        
        valiData[[idxDate]]$data01 <- valiData[[idxDate]]$data00
        
      }
      
      
    }; rm(tmpVali00, tmpVali01)
  
    #calculate linear regression between validation gas standard and sensor reading values
    #using maximum-likelihood fitting of a functional relationship (MLFR)
    #calculate linear regression for each of valiData[[idxDate]]$data01 and valiData[[idxDate]]$data00
    #test if all inputs are NA
    for (idxData in names(valiData[[idxDate]])){
      # idxData <- "data00"
      
      # If this is the second reprocessing loop (i.e. idxReprLoop = 2) then 
      # remove the reference and mean values from the following regression to 
      # see if this improves fit and allows us to keep this day of data.
      if (!is.null(gasRmv) & idxDate == DateProc) {
        
        valiData[[idxDate]][[idxData]][valiData[[idxDate]][[idxData]]$gasType == gasRmv, c("mean", "min", "max", "vari", "numSamp", "se")] <- NaN
        
      }
      
      #create empty dataframe to keep intercept and slope output from MLFR
      tmpCoef[[idxDate]][[idxData]] <- data.frame(matrix(ncol = 3, nrow = 2))
      #assign column name
      colnames(tmpCoef[[idxDate]][[idxData]]) <- c("coef", "se", "scal")
      
      # Remove archive gas from regression analysis
      tmpValiData <- valiData[[idxDate]][[idxData]][valiData[[idxDate]][[idxData]]$gasType != "qfIrgaTurbValiGas01", ]
      
      #get the temporary valiData table without NA
      tmpValiData <- na.omit(valiData[[idxDate]][[idxData]])
      
      # AY (2023-08-22): This looks redundant relative to the initialization of the tmpCoef matrix above
      # report NA for regression coefficients if input validation data less than 2 values
      # if (nrow(tmpValiData) < 2){
      #   tmpCoef[[idxDate]][[idxData]][,] <- NA
      #   }
      
      #do simple linear regression when there are only 2 input data
      if (nrow(tmpValiData) == 2){
        rtioMoleDryCo2Mlfr <- stats::lm(rtioMoleDryCo2Refe ~ mean, data = tmpValiData)
        
        # if (rtioMoleDryCo2Mlfr$coefficients[2] >= min(FracSlp) & rtioMoleDryCo2Mlfr$coefficients[2] <= max(FracSlp)) {
          #write output to table
          #intercept
          tmpCoef[[idxDate]][[idxData]][1,1] <- rtioMoleDryCo2Mlfr$coefficients[[1]]
          #slope
          tmpCoef[[idxDate]][[idxData]][2,1] <- rtioMoleDryCo2Mlfr$coefficients[[2]]
          #se
          tmpCoef[[idxDate]][[idxData]][,2] <- NA
          #scale
          tmpCoef[[idxDate]][[idxData]][1,3] <- NA
        # }
      }
      
      #do MLFR if more than 2 input data avaliable
      if (nrow(tmpValiData) > 2) {
        
        #x are sensor readings; y are reference gas values
        rtioMoleDryCo2Mlfr <- deming::deming(rtioMoleDryCo2Refe[1:nrow(tmpValiData)] ~ mean[1:nrow(tmpValiData)], data = tmpValiData,
                                             xstd = se[1:nrow(tmpValiData)], ystd = rtioMoleDryCo2RefeSe[1:nrow(tmpValiData)])
        
        # if (rtioMoleDryCo2Mlfr$coefficients[2] >= min(FracSlp) & rtioMoleDryCo2Mlfr$coefficients[2] <= max(FracSlp)) {
          
          #write output to table
          #intercept
          tmpCoef[[idxDate]][[idxData]][1,1] <- rtioMoleDryCo2Mlfr$coefficients[[1]]
          #slope
          tmpCoef[[idxDate]][[idxData]][2,1] <- rtioMoleDryCo2Mlfr$coefficients[[2]]
          #se
          tmpCoef[[idxDate]][[idxData]][,2] <- sqrt(diag(rtioMoleDryCo2Mlfr$variance))
          #scale
          tmpCoef[[idxDate]][[idxData]][1,3] <- rtioMoleDryCo2Mlfr$sigma
          
        # }
        
    }
    }#end of for loop of idxData
    #report output
    rpt[[idxDate]]$rtioMoleDryCo2Mlf <- tmpCoef[[idxDate]]$data00
    
    # Pass on valiData frames that have been filtered and run through the validation
    # regressions, fixes previous issue where unfiltered data frames were passed on 
    # with output and could contain more than 5 rows (1 for each reference gas). Also
    # re-order rows to correspond to qfIrgaTurbValiGas01-05 order.
    if (valiCrit) {
      rpt[[idxDate]][[valiTmp]] <- rbind(valiData[[idxDate]]$data00, valiData[[idxDate]]$data01)
      rpt[[idxDate]][[valiTmp]] <- rpt[[idxDate]][[valiTmp]][order(rpt[[idxDate]][[valiTmp]]$gasType), ]
    } else {
      rpt[[idxDate]][[valiTmp]] <- valiData[[idxDate]]$data00[order(valiData[[idxDate]]$data00$gasType), ]
    }
    
    #close if idxVar == rtioMoleDryCo2
    
    } else {
      
      rpt[[idxDate]]$rtioMoleDryH2oVali$rtioMoleDryH2oRefe <- ifelse(rpt[[idxDate]]$rtioMoleDryH2oVali$gasType == "qfIrgaTurbValiGas02", 0, NA)
      
    }
    
    # Reset row indice/names to 1:nrow
    rownames(rpt[[idxDate]][[valiTmp]]) <- NULL
    
    #reorder column - use column names to re-order instead of numeric indices. Also now keeps "gasType" in this output.
    rpt[[idxDate]][[valiTmp]] <- rpt[[idxDate]][[valiTmp]][, c("mean", "min", "max", "vari", "numSamp", paste0(idxVar, "Refe"), "timeBgn", "timeEnd", "gasType")]

    #unit attributes
    unitVali <- attributes(data$irgaTurb[[idxVar]])$unit
    #unit attributes for gasRefe
    if (idxVar == "rtioMoleDryCo2") {
      unitRefe <- attributes(gasRefe$rtioMoleDryCo2Refe[[idxDate]]$`702_000`)$unit #"rtioMoleDryCo2Refe"
    } else {
      unitRefe <- "molH2o mol-1"
    }

    attributes(rpt[[idxDate]][[valiTmp]])$unit <- c(unitVali, #"mean"
                                                   unitVali,  #"min"
                                                   unitVali,  #"max"
                                                   unitVali,  #"vari"
                                                   "NA",      #"numSamp"
                                                   unitRefe,  #"gasRefe"
                                                   "NA",      #"timeBgn"
                                                   "NA",      #"timeEnd"
                                                   "NA")      #"gasType"

    }#end of for loop of idxVar
    }; rm(valiCrit, allSubData, allSubQfqm, allSubTime)#end of idxDate

  invisible(gc())
  #check if there are more than one validation occurred in DateProc
  if (length(which(rpt[[DateProc]]$rtioMoleDryCo2Vali$gasType == "qfIrgaTurbValiGas02")) == 2 &
      length(which(rpt[[DateProc]]$rtioMoleDryCo2Vali$gasType == "qfIrgaTurbValiGas03")) == 2 &
      length(which(rpt[[DateProc]]$rtioMoleDryCo2Vali$gasType == "qfIrgaTurbValiGas04")) == 2 &
      length(which(rpt[[DateProc]]$rtioMoleDryCo2Vali$gasType == "qfIrgaTurbValiGas05")) == 2) {
    valiCrit <- TRUE
  } else{
    valiCrit <- FALSE
  }

  #applying the calculated coefficients to measured data
  #Calculate time-series (20Hz) of slope and zero offset
  rpt[[DateProc]]$rtioMoleDryCo2Cor <- eddy4R.base::def.irga.vali.cor(data = data, DateProc = DateProc, coef = tmpCoef, valiData = valiData, valiCrit = valiCrit, ScalMax = ScalMax, FracSlp = FracSlp, OfstMax = OfstMax, Freq = 20)

  #run the benchmarking regression to determine if the validation was good
  valiEval <- eddy4R.base::def.irga.vali.thsh(data = rpt[[DateProc]], DateProc = DateProc, evalSlpMax = 1.05, evalSlpMin = 0.95, evalOfstMax = 100, evalOfstMin = -100)
  
  # Check for the rare occurrence that the valiEval passed but large outliers still exist between 
  # reference and corrected values. Do this by looking at standard error estimate of slope parameter.
  if (idxLoop == 1) {
    
    # If this is the first reprocessing loop, then check to see which reference gas comparison is 
    # causing the biggest difference between corrected and reference values. Pass this information
    # onto the second reprocessing loop so this reference comparison can be removed and fit can 
    # be re-evaluated.
    if (!is.na(valiEval$evalCoefSe[2]) & valiEval$evalCoefSe[2] > evalSeMax & valiEval$valiEvalPass == TRUE) {
      
      valiEval$valiEvalPass <- FALSE
      
      valiData00 <- rpt[[DateProc]]$rtioMoleDryCo2Vali
      rtioMoleDryCo2Cor00 <- rpt[[DateProc]]$rtioMoleDryCo2Cor
      rtioMoleDryCo2Mlfr00 <- rpt[[DateProc]]$rtioMoleDryCo2Mlf
      valiEval00 <- valiEval
      
      msg <- paste0("dataset ", DateProc, ": valiEvalPass == TRUE and evalCoefSe > evalSeMax, running second iteration after removing largest outlier.")
      tryCatch({rlog$info(msg)}, error=function(cond){print(msg)})
      
      absErrCor <- abs(valiEval$meanCor - rpt[[DateProc]]$rtioMoleDryCo2Vali$rtioMoleDryCo2Refe[rpt[[DateProc]]$rtioMoleDryCo2Vali$gasType != "qfIrgaTurbValiGas01"])
      idxMaxErr <- which(absErrCor == max(absErrCor, na.rm = TRUE))[1]
      
      gasRmv <- rpt[[DateProc]]$rtioMoleDryCo2Vali$gasType[idxMaxErr]
      
      msg <- paste0("dataset ", DateProc, ": ", gasRmv, " is being set to NaN in next iteration of irga correction and validation.")
      tryCatch({rlog$debug(msg)}, error=function(cond){print(msg)})
      
    } else {
      
      break
      
    }
    
  } else if (idxLoop == 2) {
    
    # If the specific conditions regarding evalCoefSe above are not met then 
    # exit reprocessing loop using 'break' command and do not perform second iteration.
    
    if ((!is.na(valiEval$evalCoefSe[2]) & valiEval$evalCoefSe[2] > evalSeMax & valiEval$valiEvalPass == TRUE) | valiEval$valiEvalPass == FALSE) {
      
      rpt[[DateProc]]$rtioMoleDryCo2Vali <- valiData00
      rpt[[DateProc]]$rtioMoleDryCo2Mlf <- rtioMoleDryCo2Mlfr00
      rpt[[DateProc]]$rtioMoleDryCo2Cor <- rtioMoleDryCo2Cor00
      valiEval <- valiEval00
      
      msg <- paste0("dataset ", DateProc, ": Second iteration of valiEval also failed, setting output back to original correction.")
      tryCatch({rlog$debug(msg)}, error=function(cond){print(msg)})
      
    } else {
     
      msg <- paste0("dataset ", DateProc, ": Second iteration succeeded, using updated correction and evaluation output.")
      tryCatch({rlog$debug(msg)}, error=function(cond){print(msg)})
      
    }
    
    rm(valiData00, rtioMoleDryCo2Mlfr00, rtioMoleDryCo2Cor00, valiEval00)
    
  }
  
  }
  
   #add corrected reference gas values to vali table 
  
  if(base::nrow(rpt[[DateProc]]$rtioMoleDryCo2Vali) == base::length(valiEval$meanCor)+1){ # failsafe for row mismatches, valiEval$meanCor will always be one short because the archive gas is not included
    
    rpt[[DateProc]]$rtioMoleDryCo2Vali$meanCor <- c(NaN, valiEval$meanCor) # need to add the NaN to account for the archive gas in the first position of the vali table
    rpt[[DateProc]]$rtioMoleDryCo2Vali$meanCor[is.na(rpt[[DateProc]]$rtioMoleDryCo2Vali$meanCor)] <- NaN
    
    
  } else {
    
    # Final check, if the rare occurrence happens when there are more or less than 5 rows in vali table than the evaluation fails
    valiEval$valiEvalPass <- FALSE
    
    #fill meanCor with NaN if there were extra validation gas rows
    rpt[[DateProc]]$rtioMoleDryCo2Vali$meanCor <- NaN
    
  }
  
  #remove corrected data if validation fails benchmarking test
  if (valiEval$valiEvalPass == FALSE){
    rpt[[DateProc]]$rtioMoleDryCo2Cor$rtioMoleDryCo2Cor <- NaN #data are removed if the validation does not pass the thresholds set for evaluation slope and offset
    #raise quality flag in validation table to indicate validation status
    rpt[[DateProc]]$rtioMoleDryCo2Mlf$qfEvalThsh <-  c(NA, 1)
    msg <- paste0("validation did not pass evaluation threshold, corrected data were set to NaN")
    tryCatch({rlog$debug(msg)}, error=function(cond){print(msg)})
  } else if (valiEval$valiEvalPass == TRUE) {
    rpt[[DateProc]]$rtioMoleDryCo2Mlf$qfEvalThsh <- c(NA, 0) #corrected data will be included in the processed file in this case
  } else {
    rpt[[DateProc]]$rtioMoleDryCo2Mlf$qfEvalThsh <- c(NA, -1)
    rpt[[DateProc]]$rtioMoleDryCo2Cor$rtioMoleDryCo2Cor <- NaN #also remove data in the -1 missing validation case, prevents unexpected inclusion of questionable validations and also removes data if the eval regression can't run due to lack of span gasses
  }
  
  #force qfValiEval to -1 if slope is outside the threshold because this validation can't be applied
  if(!is.na(rpt[[DateProc]]$rtioMoleDryCo2Mlf$coef[2])){ # only run if there are coefficients to check 
    
    if (rpt[[DateProc]]$rtioMoleDryCo2Mlf$coef[2] < base::min(FracSlp) | rpt[[DateProc]]$rtioMoleDryCo2Mlf$coef[2] > base::max(FracSlp)){
      rpt[[DateProc]]$rtioMoleDryCo2Mlf$qfEvalThsh <- c(NA, -1)
    }
  }
  
  #add additional coefficients to mlf table
  rpt[[DateProc]]$rtioMoleDryCo2Mlf$evalCoef <- valiEval$evalCoef
  rpt[[DateProc]]$rtioMoleDryCo2Mlf$evalCoefSe <- valiEval$evalCoefSe
  rpt[[DateProc]]$rtioMoleDryCo2Mlf$evalSlpThsh <- valiEval$evalSlpThsh
  rpt[[DateProc]]$rtioMoleDryCo2Mlf$evalOfstThsh <- valiEval$evalOfstThsh
  
  for (idxVar in c("rtioMoleDryH2oVali", "rtioMoleDryCo2Vali")) {
    
    # Remove "gasType" column from final output and rename reference column to "refe" for Co2 and H2o
    rpt[[DateProc]][[idxVar]] <- rpt[[DateProc]][[idxVar]][, colnames(rpt[[DateProc]][[idxVar]]) != "gasType"]
    colnames(rpt[[DateProc]][[idxVar]])[grep("Refe", colnames(rpt[[DateProc]][[idxVar]]))] <- "refe"  
    
  }
  
  #reorder to place the corrected reference values next to the original reference values
  rpt[[DateProc]]$rtioMoleDryCo2Vali <- rpt[[DateProc]]$rtioMoleDryCo2Vali[, c("mean", "min", "max", "vari", "numSamp", "refe", "meanCor", "timeBgn", "timeEnd")]
  rpt[[DateProc]]$rtioMoleDryH2oVali <- rpt[[DateProc]]$rtioMoleDryH2oVali[, c("mean", "min", "max", "vari", "numSamp", "refe", "timeBgn", "timeEnd")]
  
  
  #reset attributes
  
  attributes(rpt[[DateProc]]$rtioMoleDryCo2Vali)$unit <- c("molCo2 mol-1Dry", #"mean"
                                                           "molCo2 mol-1Dry", #"min"
                                                           "molCo2 mol-1Dry", #"max"
                                                           "molCo2 mol-1Dry", #"vari"
                                                           "NA", #"numSamp"
                                                           "molCo2 mol-1Dry",#gasRefe
                                                           "molCo2 mol-1Dry",#gasRefeCor
                                                           "NA", #"timeBgn"
                                                           "NA") #"timeEnd"
  
  attributes(rpt[[DateProc]]$rtioMoleDryH2oVali)$unit <- c("molH2o mol-1Dry", #"mean"
                                                           "molH2o mol-1Dry", #"min"
                                                           "molH2o mol-1Dry", #"max"
                                                           "molH2o mol-1Dry", #"vari"
                                                           "NA", #"numSamp"
                                                           "molH2o mol-1",#gasRefe
                                                           "NA", #"timeBgn"
                                                           "NA") #"timeEnd"  

  attributes(rpt[[DateProc]]$rtioMoleDryCo2Cor$rtioMoleDryCo2Cor)$unit <- "molCo2 mol-1Dry"
  
  
   
#return results
  return(rpt)
  
}

