##############################################################################################
#' @title Wrapper function: Preprocessing and computing NEON eddy-covariance stroage exchange L1 data product descriptive statistics

#' @author
#' Natchaya Pingintha-Durden \email{eddy4R.info@gmail.com}

#' @description 
#' Wrapper function. Preprocessing and computing NEON eddy-covariance stroage exchange (ECSE) Level 1 data product (dp01) descriptive statistics (mean, minimum, maximum, variance, number of non-NA points). 

#' @param dp01 A vector of class "character" containing the name of NEON ECSE dp01 which descriptive statistics are being calculated, \cr
#' c("co2Stor", "h2oStor", "tempAirLvl", "tempAirTop", "isoCo2", "isoH2o"). Defaults to "co2Stor". [-] 
#' @param lvl  Measurement level of dp01 which descriptive statistics are being calculated. Of type character. [-]
#' @param lvlMfcSampStor Measurement level of mfcSampStor which apply to only  dp01 equal to "co2Stor" or "h2oStor". Defaults to NULL. Of type character. [-]
#' @param lvlEnvHut Measurement level of envHut. Defaults to NULL. Of type character. [-]
#' @param lvlValv Measurement level of irgaValvLvl, crdCo2ValvLvl, or crdH2oValvLvl. Defaults to NULL. Of type character. [-]
#' @param lvlCrdH2oValvVali Measurement level of crdH2oValvVali which apply to only  dp01 equal to "isoH2o". Defaults to NULL. Of type character. [-]
#' @param lvlCrdCo2Valv Horizontal and vertical location for crdCo2 valve. Defaults to NULL. Of type character. [-]
#' @param data A list of data frame containing the input dp0p data that related to dp01 which descriptive statistics are being calculated. Of class integer". [User defined] 
#' @param qfInp A list of data frame containing the input quality flag data that related to dp01 are being grouped. Of class integer". [-] 
#' @param TypeMeas A vector of class "character" containing the name of measurement type (sampling or validation), TypeMeas = c("samp", "vali"). Defaults to "samp". [-]
#' @param PrdMeas The measurement time period in minute.  [min]
#' @param PrdAgr The time period to aggregate to averaging in minute. [min]
#' @param idxTime A list of data frame containing the indices and corresponding times for aggregation periods. [-]

#' @return A list of dp01 descriptive statistics. \cr 
#' \code{mean} The mean of non-NA values in \code{data}
#' \code{min} The minimum value of non-NA values in \code{data}
#' \code{max} The maximum value of non-NA values in \code{data}
#' \code{vari} The variance of non-NA values in \code{data}
#' \code{numSamp} The number of non-NA values in \code{data}
#' \code{se} The standard error of the mean of non-NA values in \code{data}
#' \code{timeBgn} The beginning time to determine descriptive statistics.
#' \code{timeEnd} The ending time to determine descriptive statistics.

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007.

#' @keywords aggregate, average, descriptive statistics, dp01, ECSE

#' @examples Currently none.

#' @seealso Currently none.

#' @export

# changelog and author contributions / copyrights 
#   Ke Xu (2017-03-21)
#     original creation in flow.stor.towr.neon.R
#   Natchaya Pingintha-Durden (2017-07-26)
#     generate wrapper function
#   Natchaya Pingintha-Durden (2017-08-01)
#     added unit attributes
#   Natchaya Pingintha-Durden (2017-09-19)
#     added envHut data
#   Natchaya Pingintha-Durden (2017-12-04)
#     modified the logic to not output the empty list when there is no data for a whole day
#   Natchaya Pingintha-Durden (2018-01-24)
#     use quality flag to determine indices
#   Natchaya P-Durden (2018-04-03)
#     update @param format
#   Ke Xu (2018-04-19)
#     applied term name convention; replaced qfInput by qfInp
#   Natchaya P-Durden (2018-05-22)
#     rename function from wrap.neon.dp01() to wrap.dp01()
#     rename function from wrap.neon.dp01.ecse() to wrap.dp01.ecse()
#   Natchaya P-Durden (2019-01-31)
#     using injNum instate of qfRngTmp to determine missing data
#   Natchaya P-Durden (2020-03-12)
#     In irgaCo2 an irgaH2o, not include the period when crdCo2 take over to measure at that level 
#     and irga have to move to measure next level
#   Natchaya P-Durden (2020-03-25)
#     added lvlCrdCo2Valv to the function's parameter
#   David Durden (2020-05-15)
#     failsafe for crd kickoff removal causing no data for entire day
#   David Durden (2020-07-23)
#     bug fix for valve issues where looks like consistently Stor data thrown off by Crd
##############################################################################################
wrap.dp01.ecse <- function(
  dp01 = c("co2Stor", "h2oStor", "tempAirLvl", "tempAirTop", "isoCo2", "isoH2o")[1],
  lvl,
  lvlMfcSampStor = NULL,
  lvlEnvHut = NULL,
  lvlValv = NULL,
  lvlCrdH2oValvVali = NULL,
  lvlCrdCo2Valv = NULL,
  data = list(),
  qfInp = list(),
  TypeMeas = c("samp", "vali")[1],
  PrdMeas,
  PrdAgr,
  idxTime = list() 
) {
  #assign list
  wrk <- list()
  rpt <- list()
  #statistical names
  NameStat <- c("mean", "min", "max", "vari", "numSamp", "se")
  #calculate dp01 for co2Stor and h2oStor ########################################################################################
  if (dp01 %in% c("co2Stor", "h2oStor")){
    #during sampling period 
    if (TypeMeas %in% "samp"){
      #assign lvlIrga for each measurement level
      if (lvl == "000_010") {lvlIrga <- "lvl01"}
      if (lvl == "000_020") {lvlIrga <- "lvl02"}
      if (lvl == "000_030") {lvlIrga <- "lvl03"}
      if (lvl == "000_040") {lvlIrga <- "lvl04"}
      if (lvl == "000_050") {lvlIrga <- "lvl05"}
      if (lvl == "000_060") {lvlIrga <- "lvl06"}
      if (lvl == "000_070") {lvlIrga <- "lvl07"}
      if (lvl == "000_080") {lvlIrga <- "lvl08"}
      
      #input the whole day data
      if(dp01 == "co2Stor"){
        wrk$data <- data.frame(stringsAsFactors = FALSE,
                               "frt00" = data$mfcSampStor[[lvlMfcSampStor]]$frt00,
                               #wrk$data$mfcSampStor[[paste0(Para$Flow$LevlTowr$mfcSampStor, "_", sprintf("%02d", idxPrdAgr), "m")]]$frt00,
                               "pres" = data$irgaStor[[lvl]]$pres,
                               "presEnvHut" = data$envHut[[lvlEnvHut]]$pres,
                               "rhEnvHut" = data$envHut[[lvlEnvHut]]$rh,
                               "rtioMoleDryCo2" = data$irgaStor[[lvl]]$rtioMoleDryCo2,
                               "rtioMoleWetCo2" = data$irgaStor[[lvl]]$rtioMoleWetCo2,
                               "rtioMoleWetH2oEnvHut" = data$envHut[[lvlEnvHut]]$rtioMoleWetH2o,
                               "temp" = data$irgaStor[[lvl]]$temp,
                               "tempEnvHut" = data$envHut[[lvlEnvHut]]$temp,
                               "lvlIrga" = data$irgaValvLvl[[lvlValv]]$lvlIrga
                               
        )
      }
      
      if(dp01 == "h2oStor"){
        wrk$data <- data.frame(stringsAsFactors = FALSE,
                               "frt00" = data$mfcSampStor[[lvlMfcSampStor]]$frt00,
                               #wrk$data$mfcSampStor[[paste0(Para$Flow$LevlTowr$mfcSampStor, "_", sprintf("%02d", idxPrdAgr), "m")]]$frt00,
                               "pres" = data$irgaStor[[lvl]]$pres,
                               "presEnvHut" = data$envHut[[lvlEnvHut]]$pres,
                               "rhEnvHut" = data$envHut[[lvlEnvHut]]$rh,
                               "rtioMoleDryH2o" = data$irgaStor[[lvl]]$rtioMoleDryH2o,
                               "rtioMoleWetH2o" = data$irgaStor[[lvl]]$rtioMoleWetH2o,
                               "rtioMoleWetH2oEnvHut" = data$envHut[[lvlEnvHut]]$rtioMoleWetH2o,
                               "temp" = data$irgaStor[[lvl]]$temp,
                               "tempEnvHut" = data$envHut[[lvlEnvHut]]$temp,
                               "lvlIrga" = data$irgaValvLvl[[lvlValv]]$lvlIrga
                               
        )
      }
      
      #input the whole day qfqm 
      wrk$qfqm <- list()
      wrk$qfqm$irgaStor <- qfInp$irga[[lvl]]
      
      if (PrdMeas == PrdAgr) {
        #PrdAgr <- 2
        #2 minutely sampling data
        #idxLvLPrdAgr <- paste0(lvl, "_", sprintf("%02d", PrdAgr), "m")
        #rpt[[dp01]][[idxLvLPrdAgr]] <- list()
        
        #if there is at least one measurement
        if(length(which(!is.na(wrk$qfqm$irgaStor$qfRngTemp))) > 0){
          #determine the index of each measurement  
          wrk$idx <- eddy4R.base::def.idx.agr(time = data$time, PrdAgr = (PrdMeas*60), FreqLoca = 1, MethIdx = "specBgn", data = wrk$qfqm$irgaStor$qfRngTemp, CritTime = 60)
          #delete row if last timeBgn and timeEnd is NA
          wrk$idx <- wrk$idx[rowSums(is.na(wrk$idx)) != 2,]
          #replace last idxEnd > 86400 by 86400
          wrk$idx$idxEnd <- ifelse(wrk$idx$idxEnd > 86400, 86400, wrk$idx$idxEnd)
          #if last timeEnd is NA, replce that time to the last time value in data$time
          wrk$idx$timeEnd <- as.POSIXct(ifelse(is.na(wrk$idx$timeEnd), data$time[length(data$time)], wrk$idx$timeEnd), origin = "1970-01-01", tz = "UTC")
          #idxAgr2 <- 0
          for (idxAgr in 1:length(wrk$idx$idxBgn)){
            #idxAgr <- 3
            
            #only use the middle 2 min after the first 1 min
            wrk$inpMask$data <- list()
            wrk$inpMask$data <- wrk$data[wrk$idx$idxBgn[idxAgr]:wrk$idx$idxEnd[idxAgr],]  
            #replace frt00 data with NaN when irga got kick out to measure the new measurement level
            wrk$inpMask$data$frt00 <- ifelse(wrk$inpMask$data$lvlIrga == lvlIrga, wrk$inpMask$data$frt00, NaN)
            wrk$inpMask$data$presEnvHut <- ifelse(wrk$inpMask$data$lvlIrga == lvlIrga, wrk$inpMask$data$presEnvHut, NaN)
            wrk$inpMask$data$rhEnvHut <- ifelse(wrk$inpMask$data$lvlIrga == lvlIrga, wrk$inpMask$data$rhEnvHut, NaN)
            wrk$inpMask$data$rtioMoleWetH2oEnvHut <- ifelse(wrk$inpMask$data$lvlIrga == lvlIrga, wrk$inpMask$data$rtioMoleWetH2oEnvHut, NaN)
            wrk$inpMask$data$tempEnvHut <- ifelse(wrk$inpMask$data$lvlIrga == lvlIrga, wrk$inpMask$data$tempEnvHut, NaN)
            #get rid of lvlIrga
            wrk$inpMask$data <- wrk$inpMask$data[,-which(names(wrk$inpMask$data) == "lvlIrga")]
            
            rpt[[idxAgr]] <- eddy4R.base::wrap.dp01(
              # assign data: data.frame or list of type numeric or integer
              data = wrk$inpMask$data#,
              # if data is a list, which list entries should be processed into Level 1 data products?
              # defaults to NULL which expects data to be a data.frame
              #idx = 
              #names(wrk$inpMask$data[[dp01]]) #"000_010_02m"
            )
            
            #units:
            for (idxVar in names(rpt[[1]]$mean)){
              #idxVar <- names(rpt[[dp01]][[idxLvLPrdAgr]][[1]]$mean)[1]
              attributes(rpt[[1]]$mean[[idxVar]])$unit <- attributes(wrk$data[[idxVar]])$unit
            }
            
            #grab and add both time begin and time end to rpt
            
            
            #for (idxLvLPrdAgr in names(wrk$inpMask$data[[dp01]])){
            #idxLvLPrdAgr <- names(wrk$inpMask$data[[dp01]])[1]
            rpt[[idxAgr]]$timeBgn <- list()
            rpt[[idxAgr]]$timeEnd <- list()
            
            for(idxVar in names(wrk$data)[which(!(names(wrk$data) %in% c("lvlIrga")))]){
              rpt[[idxAgr]]$timeBgn[[idxVar]] <- wrk$idx$timeBgn[idxAgr]
              rpt[[idxAgr]]$timeEnd[[idxVar]] <- wrk$idx$timeEnd[idxAgr]
            }
            
            #check if this period is the period that crdCo2 take over and irga have to move to measure other level
            #and number of sample less than 10% (120-120*0.1)
            if (dp01 == "co2Stor") {numSamp <- rpt[[idxAgr]]$numSamp$rtioMoleDryCo2}
            if (dp01 == "h2oStor") {numSamp <- rpt[[idxAgr]]$numSamp$rtioMoleDryH2o}
            if (data$crdCo2ValvLvl[[lvlCrdCo2Valv]]$lvlCrdCo2[wrk$idx$idxEnd[idxAgr]] == lvlIrga &  numSamp < 108){
              rpt[[idxAgr]] <- NULL
            }
            
            #Remove any empty lists in case valve issues
            if(length(wrk$idx$idxBgn) == idxAgr) rpt <- Filter(Negate(is.null), rpt)
            
            #Check if after removing data for valve kickooff if no data remains
            if(length(wrk$idx$idxBgn) == idxAgr && length(rpt) == 0){
              rpt[[1]] <- list()
              
              for(idxStat in NameStat){
                #idxStat <- NameStat[1]
                rpt[[1]][[idxStat]] <- as.data.frame(matrix(NaN, nrow = 1, ncol = ncol(wrk$data)))
                #assign name to each column
                names(rpt[[1]][[idxStat]]) <- names(wrk$data)
                #not report lvlIrga
                rpt[[1]][[idxStat]] <- rpt[[1]][[idxStat]][which(!(names(rpt[[1]][[idxStat]]) %in% c("lvlIrga")))]
                
              }; rm(idxStat)
              #add both time begin and time end to rpt
              rpt[[1]]$timeBgn <- list()
              rpt[[1]]$timeEnd <- list()
              
              #output time for dp01
              for(idxVar in names(wrk$data)[which(!(names(wrk$data) %in% c("lvlIrga")))]){
                rpt[[1]]$timeBgn[[idxVar]] <- data$time[1]
                rpt[[1]]$timeEnd[[idxVar]] <- data$time[length(data$time)]
                #unit
                attributes(rpt[[1]]$mean[[idxVar]])$unit <- attributes(wrk$data[[idxVar]])$unit
                
              }; rm(idxVar)
              
            }#end failsafe of if no measurement data at all in the whole day once valve issue removed
            
            #}# end of there is at least one data
            
          }; rm(idxAgr)
        } else {
          
          rpt[[1]] <- list()
          
          for(idxStat in NameStat){
            #idxStat <- NameStat[1]
            rpt[[1]][[idxStat]] <- as.data.frame(matrix(NaN, nrow = 1, ncol = ncol(wrk$data)))
            #assign name to each column
            names(rpt[[1]][[idxStat]]) <- names(wrk$data)
            #not report lvlIrga
            rpt[[1]][[idxStat]] <- rpt[[1]][[idxStat]][which(!(names(rpt[[1]][[idxStat]]) %in% c("lvlIrga")))]

            }; rm(idxStat)
          #add both time begin and time end to rpt
          rpt[[1]]$timeBgn <- list()
          rpt[[1]]$timeEnd <- list()
          
          #output time for dp01
          for(idxVar in names(wrk$data)[which(!(names(wrk$data) %in% c("lvlIrga")))]){
            rpt[[1]]$timeBgn[[idxVar]] <- data$time[1]
            rpt[[1]]$timeEnd[[idxVar]] <- data$time[length(data$time)]
            #unit
            attributes(rpt[[1]]$mean[[idxVar]])$unit <- attributes(wrk$data[[idxVar]])$unit
            
          }; rm(idxVar)
          
        }#end of if no measurement data at all in the whole day
      } #end of PrdAgr == 2
      
      if (PrdMeas != PrdAgr) {
        #PrdAgr <- 30
        #if there is at least one measurement
        if(length(which(!is.na(wrk$qfqm$irgaStor$qfRngTemp))) > 0){
          
          #determine the index of each measurement  
          wrk$idx <- eddy4R.base::def.idx.agr(time = data$time, PrdAgr = (PrdMeas*60), FreqLoca = 1, MethIdx = "specBgn", data = wrk$qfqm$irgaStor$qfRngTemp, CritTime = 60)
          #delete row if last timeBgn and timeEnd is NA
          wrk$idx <- wrk$idx[rowSums(is.na(wrk$idx)) != 2,]
          #replace last idxEnd > 86400 by 86400
          wrk$idx$idxEnd <- ifelse(wrk$idx$idxEnd > 86400, 86400, wrk$idx$idxEnd)
          #if last timeEnd is NA, replce that time to the last time value in data$time
          wrk$idx$timeEnd <- as.POSIXct(ifelse(is.na(wrk$idx$timeEnd), data$time[length(data$time)], wrk$idx$timeEnd), origin = "1970-01-01", tz = "UTC")
          
          whrSamp <- wrk$idx$idxBgn[1]:wrk$idx$idxEnd[1]
          if (length (wrk$idx$idxBgn) > 1 ){
            for(ii in 2:length (wrk$idx$idxBgn)){
              whrSamp <- c(whrSamp, wrk$idx$idxBgn[ii]:wrk$idx$idxEnd[ii])
            }
          }
          
          tmpAttr <- list()
          for (idxData in c("frt00", "presEnvHut", "rhEnvHut", "rtioMoleWetH2oEnvHut", "tempEnvHut")){
            #defined attributes 
            tmpAttr[[idxData]] <- attributes(wrk$data[[idxData]])
            #replace idxData data with NaN when irga got kick out to measure the new measurement level
            wrk$data[[idxData]] <- ifelse(wrk$data$lvlIrga == lvlIrga, wrk$data[[idxData]], NaN)
          }
        
          wrk$data[-whrSamp, 1:9] <- NaN
          #added attributes
          for (idxData in c("frt00", "presEnvHut", "rhEnvHut", "rtioMoleWetH2oEnvHut", "tempEnvHut")){
            attributes(wrk$data[[idxData]]) <- tmpAttr[[idxData]]
          }
          
        } else {#if there are no data at all in wrk$data$temp
          wrk$data[,1:9] <- NaN
        } 
        
        
        for(idxAgr in c(1:length(idxTime[[paste0(PrdAgr, "min")]]$Bgn))) {
          #idxAgr <- 48
          
          
          ## grab data at the selected mask data
          # for data
          wrk$inpMask$data <- list()
          idxLvLPrdAgr <- paste0(lvl, "_", sprintf("%02d", PrdAgr), "m")
          
          wrk$inpMask$data <- wrk$data[idxTime[[paste0(PrdAgr, "min")]]$Bgn[idxAgr]:idxTime[[paste0(PrdAgr, "min")]]$End[idxAgr],]
          #get rid of lvlIrga
          wrk$inpMask$data <- wrk$inpMask$data[,-which(names(wrk$inpMask$data) == "lvlIrga")]
          
          rpt[[idxAgr]] <- eddy4R.base::wrap.dp01(
            # assign data: data.frame or list of type numeric or integer
            data = wrk$inpMask$data#,
            # if data is a list, which list entries should be processed into Level 1 data products?
            # defaults to NULL which expects data to be a data.frame
            #idx = 
            #names(wrk$inpMask$data[[dp01]]) #"000_010_02m"
          )
          
          #units:
          for (idxVar in names(rpt[[1]]$mean)){
            #idxVar <- names(rpt[[1]]$mean)[1]
            attributes(rpt[[1]]$mean[[idxVar]])$unit <- attributes(wrk$data[[idxVar]])$unit
          }
          
          #grab and add both time begin and time end to rpt
          rpt[[idxAgr]]$timeBgn <- list()
          rpt[[idxAgr]]$timeEnd <- list()
          
          for(idxVar in names(wrk$data)[which(!(names(wrk$data) %in% c("lvlIrga")))]){
            rpt[[idxAgr]]$timeBgn[[idxVar]] <- data$time[idxTime[[paste0(PrdAgr, "min")]]$Bgn[idxAgr]]
            rpt[[idxAgr]]$timeEnd[[idxVar]] <- data$time[idxTime[[paste0(PrdAgr, "min")]]$End[idxAgr]]
          }
          
        }; #rm(idxAgr)
        
      }#end of PrdAgr == 30
    }#end of TypeMeas %in% "samp"
    
    #during validation period
    if (TypeMeas %in% "vali"){
      if(dp01 == "co2Stor"){
        wrk$data <- data.frame(stringsAsFactors = FALSE,
                               "frt00" = data$mfcSampStor[[lvlMfcSampStor]]$frt00,
                               #wrk$data$mfcSampStor[[paste0(Para$Flow$LevlTowr$mfcSampStor, "_", sprintf("%02d", PrdAgr), "m")]]$frt00,
                               "pres" = data$irgaStor[[lvl]]$pres,
                               "presEnvHut" = data$envHut[[lvlEnvHut]]$pres,
                               "rhEnvHut" = data$envHut[[lvlEnvHut]]$rh,
                               "rtioMoleDryCo2" = data$irgaStor[[lvl]]$rtioMoleDryCo2,
                               "rtioMoleDryCo2Refe" = data$irgaStor[[lvl]]$rtioMoleDryCo2Refe,
                               "rtioMoleWetCo2" = data$irgaStor[[lvl]]$rtioMoleWetCo2,
                               "rtioMoleWetH2oEnvHut" = data$envHut[[lvlEnvHut]]$rtioMoleWetH2o,
                               "temp" = data$irgaStor[[lvl]]$temp,
                               "tempEnvHut" = data$envHut[[lvlEnvHut]]$temp
                               #data$tempAirLvl$`000_010_01m`$temp
                               
        )
      }
      
      if(dp01 == "h2oStor"){
        wrk$data <- data.frame(stringsAsFactors = FALSE,
                               "frt00" = data$mfcSampStor[[lvlMfcSampStor]]$frt00,
                               #wrk$data$mfcSampStor[[paste0(Para$Flow$LevlTowr$mfcSampStor, "_", sprintf("%02d", PrdAgr), "m")]]$frt00,
                               "pres" = data$irgaStor[[lvl]]$pres,
                               "presEnvHut" = data$envHut[[lvlEnvHut]]$pres,
                               "rhEnvHut" = data$envHut[[lvlEnvHut]]$rh,
                               "rtioMoleDryH2o" = data$irgaStor[[lvl]]$rtioMoleDryH2o,
                               "rtioMoleWetH2o" = data$irgaStor[[lvl]]$rtioMoleWetH2o,
                               "rtioMoleWetH2oEnvHut" = data$envHut[[lvlEnvHut]]$rtioMoleWetH2o,
                               "temp" = data$irgaStor[[lvl]]$temp,
                               "tempEnvHut" = data$envHut[[lvlEnvHut]]$temp
                               #data$tempAirLvl$`000_010_01m`$temp
                               
        )
      }
      
      #input the whole day qfqm 
      wrk$qfqm <- list()
      wrk$qfqm$irgaStor <- qfInp$irga[[lvl]]
      
      if (PrdMeas == PrdAgr) {
        #PrdAgr <- 2
        #2 minutely sampling data
        #idxLvLPrdAgr <- paste0(lvl, "_", sprintf("%02d", PrdAgr), "m")
        #rpt[[dp01]][[idxLvLPrdAgr]] <- list()
        
        #if there is at least one measurement
        if(length(which(!is.na(wrk$qfqm$irgaStor$qfRngTemp))) > 0){
          #determine the end time of each measurement
          wrk$idx <- eddy4R.base::def.idx.agr(time = data$time, PrdAgr = (PrdMeas*60), FreqLoca = 1, MethIdx = "specEnd", data = wrk$qfqm$irgaStor$qfRngTemp, CritTime = 20)
          #delete row if last timeBgn and timeEnd is NA
          wrk$idx <- wrk$idx[rowSums(is.na(wrk$idx)) != 2,]
          #if last timeEnd is NA, replce that time to the last time value in data$time
          wrk$idx$timeEnd <- as.POSIXct(ifelse(is.na(wrk$idx$timeEnd), data$time[length(data$time)], wrk$idx$timeEnd), origin = "1970-01-01", tz = "UTC")
          
          #idxAgr2 <- 0
          for (idxAgr in 1:length(wrk$idx$idxBgn)){
            #idxAgr <- 1
            #only use the middle 2 min before the last 20 s
            wrk$inpMask$data <- list()
            wrk$inpMask$data <- wrk$data[wrk$idx$idxBgn[idxAgr]:wrk$idx$idxEnd[idxAgr],] 
            
            #dp01 processing
            rpt[[idxAgr]] <- eddy4R.base::wrap.dp01(
              # assign data: data.frame or list of type numeric or integer
              data = wrk$inpMask$data#,
            )
            
            #units:
            for (idxVar in names(rpt[[1]]$mean)){
              #idxVar <- names(rpt[[1]]$mean)[1]
              attributes(rpt[[1]]$mean[[idxVar]])$unit <- attributes(wrk$data[[idxVar]])$unit
            }
            
            #grab and add both time begin and time end to rpt
            rpt[[idxAgr]]$timeBgn <- list()
            rpt[[idxAgr]]$timeEnd <- list()
            
            #output time for dp01
            for(idxVar in names(wrk$data)){
              rpt[[idxAgr]]$timeBgn[[idxVar]] <- wrk$idx$timeBgn[idxAgr]
              rpt[[idxAgr]]$timeEnd[[idxVar]] <- wrk$idx$timeEnd[idxAgr]
            }; rm(idxVar)
          }#; rm(idxAgr)
          
        } else {
          
          rpt[[1]] <- list()
          
          for(idxStat in NameStat){
            #idxStat <- NameStat[1]
            rpt[[1]][[idxStat]] <- as.data.frame(matrix(NaN, nrow = 1, ncol = ncol(wrk$data)))
            #assign name to each column
            names(rpt[[1]][[idxStat]]) <- names(wrk$data)
            
            }; rm(idxStat)
          #add both time begin and time end to rpt
          rpt[[1]]$timeBgn <- list()
          rpt[[1]]$timeEnd <- list()
          
          #output time for dp01
          for(idxVar in names(wrk$data)){
            rpt[[1]]$timeBgn[[idxVar]] <- data$time[1]
            rpt[[1]]$timeEnd[[idxVar]] <- data$time[length(data$time)]
            #unit
            attributes(rpt[[1]]$mean[[idxVar]])$unit <- attributes(wrk$data[[idxVar]])$unit
            
          }; rm(idxVar) 
          
        }#end of if no measurement data at all in the whole day
        
      } #end of PrdAgr == 2
      
      if (PrdMeas != PrdAgr) {
        #PrdAgr <- 30
        #if there is at least one measurement
        if(length(which(!is.na(wrk$qfqm$irgaStor$qfRngTemp))) > 0){
          #   #determine the end time of each measurement  
          wrk$idx <- eddy4R.base::def.idx.agr(time = data$time, PrdAgr = (PrdMeas*60), FreqLoca = 1, MethIdx = "specEnd", data = wrk$qfqm$irgaStor$qfRngTemp, CritTime = 20)
          #delete row if last timeBgn and timeEnd is NA
          wrk$idx <- wrk$idx[rowSums(is.na(wrk$idx)) != 2,]
          #if last timeEnd is NA, replce that time to the last time value in data$time
          wrk$idx$timeEnd <- as.POSIXct(ifelse(is.na(wrk$idx$timeEnd), data$time[length(data$time)], wrk$idx$timeEnd), origin = "1970-01-01", tz = "UTC")
          
          whrSamp <- wrk$idx$idxBgn[1]:wrk$idx$idxEnd[1]
          if (length (wrk$idx$idxBgn) > 1 ){
            for(ii in 2:length (wrk$idx$idxBgn)){
              whrSamp <- c(whrSamp, wrk$idx$idxBgn[ii]:wrk$idx$idxEnd[ii])
            }
          }
          wrk$data[-whrSamp, ] <- NaN
        } else {#end of if no measurement data at all in the whole day
          tmpAttr <- list()
          for (idxData in c("frt00", "presEnvHut", "rhEnvHut", "rtioMoleWetH2oEnvHut", "tempEnvHut")){
            #defined attributes 
            tmpAttr[[idxData]] <- attributes(wrk$data[[idxData]])
            wrk$data[[idxData]] <- NaN
            attributes(wrk$data[[idxData]]) <- tmpAttr[[idxData]]
          }
        }
        
        for(idxAgr in c(1:length(idxTime[[paste0(PrdAgr, "min")]]$Bgn))) {
          #idxAgr <- 34
          ## grab data at the selected mask data
          # for data
          wrk$inpMask$data <- list()
          idxLvLPrdAgr <- paste0(lvl, "_", sprintf("%02d", PrdAgr), "m")
          wrk$inpMask$data <- wrk$data[idxTime[[paste0(PrdAgr, "min")]]$Bgn[idxAgr]:idxTime[[paste0(PrdAgr, "min")]]$End[idxAgr],]
          
          #dp01 processing
          rpt[[idxAgr]] <- eddy4R.base::wrap.dp01(
            # assign data: data.frame or list of type numeric or integer
            data = wrk$inpMask$data
          )
          
          #units:
          for (idxVar in names(rpt[[1]]$mean)){
            #idxVar <- names(rpt[[1]]$mean)[1]
            attributes(rpt[[1]]$mean[[idxVar]])$unit <- attributes(wrk$data[[idxVar]])$unit
          }
          
          #grab and add both time begin and time end to rpt
          rpt[[idxAgr]]$timeBgn <- list()
          rpt[[idxAgr]]$timeEnd <- list()
          
          #output time for dp01
          for(idxVar in names(wrk$data)){
            rpt[[idxAgr]]$timeBgn[[idxVar]] <- data$time[idxTime[[paste0(PrdAgr, "min")]]$Bgn[idxAgr]]
            rpt[[idxAgr]]$timeEnd[[idxVar]] <- data$time[idxTime[[paste0(PrdAgr, "min")]]$End[idxAgr]]
          }; rm(idxVar)
          
        }; #rm(idxAgr)
      } #end of PrdAgr == 30
      
    }#end of TypeMeas %in% "vali"
    
  }#end of dp01 if statement 
  
  #calculate dp01 for tempAirLvl and tempAirTop ########################################################################################
  if (dp01 %in% c("tempAirLvl", "tempAirTop")){
    
    for(idxAgr in c(1:length(idxTime[[paste0(PrdAgr, "min")]]$Bgn))) {
      #idxAgr <- 1
      ## grab data at the selected mask data
      # for data
      wrk$inpMask$data <- list()
      
      #idxLvLPrdAgr <- paste0(lvl, "_", sprintf("%02d", PrdAgr), "m")
      
      
      if(dp01 == "tempAirLvl") {
        wrk$inpMask$data <- data.frame(stringsAsFactors = FALSE,
                                       "temp" = data$tempAirLvl[[lvl]][["temp"]][idxTime[[paste0(PrdAgr, "min")]]$Bgn[idxAgr]:idxTime[[paste0(PrdAgr, "min")]]$End[idxAgr]]
        )
      } 
      
      if(dp01 == "tempAirTop") {
        wrk$inpMask$data <- data.frame(stringsAsFactors = FALSE,
                                       "temp" = colMeans(rbind(data$tempAirTop[[lvl]][["temp01"]], data$tempAirTop[[lvl]][["temp02"]], data$tempAirTop[[lvl]][["temp03"]]), na.rm=TRUE)[idxTime[[paste0(PrdAgr, "min")]]$Bgn[idxAgr]:idxTime[[paste0(PrdAgr, "min")]]$End[idxAgr]]
        )
      } 
      
      #call wrap.dp01.R to calculate descriptive statistics       
      rpt[[idxAgr]] <- eddy4R.base::wrap.dp01(
        # assign data: data.frame or list of type numeric or integer
        data = wrk$inpMask$data#,
      )
      
      #units:
      for (idxVar in names(rpt[[1]]$mean)){
        #idxVar <- names(rpt[[1]]$mean)[1]
        attributes(rpt[[1]]$mean[[idxVar]])$unit <- attributes(data[[dp01]][[lvl]][[idxVar]])$unit
      }
      
      #grab and add both time begin and time end to rpt
      rpt[[idxAgr]]$timeBgn <- list()
      rpt[[idxAgr]]$timeEnd <- list()
      
      for(idxVar in names(rpt[[idxAgr]]$mean)){
        rpt[[idxAgr]]$timeBgn[[idxVar]] <- data$time[idxTime[[paste0(PrdAgr, "min")]]$Bgn[idxAgr]]
        rpt[[idxAgr]]$timeEnd[[idxVar]] <- data$time[idxTime[[paste0(PrdAgr, "min")]]$End[idxAgr]]
      }
      
    }; #rm(idxAgr)
    
  }#end of dp01 if statement 
  
  #calculate dp01 for "isoCo2" ########################################################################################
  if (dp01 %in% c("isoCo2")){
    #during sampling period 
    if (TypeMeas %in% "samp"){
      #assign lvlCrdCo2 for each measurement level
      if (lvl == "000_010") {lvlCrdCo2 <- "lvl01"}
      if (lvl == "000_020") {lvlCrdCo2 <- "lvl02"}
      if (lvl == "000_030") {lvlCrdCo2 <- "lvl03"}
      if (lvl == "000_040") {lvlCrdCo2 <- "lvl04"}
      if (lvl == "000_050") {lvlCrdCo2 <- "lvl05"}
      if (lvl == "000_060") {lvlCrdCo2 <- "lvl06"}
      if (lvl == "000_070") {lvlCrdCo2 <- "lvl07"}
      if (lvl == "000_080") {lvlCrdCo2 <- "lvl08"}
      
      #input the whole day data
      wrk$data <- data.frame(stringsAsFactors = FALSE,
                             "dlta13CCo2" = data$crdCo2[[lvl]]$dlta13CCo2,
                             "pres" = data$crdCo2[[lvl]]$pres,
                             "presEnvHut" = data$envHut[[lvlEnvHut]]$pres,
                             "rhEnvHut" = data$envHut[[lvlEnvHut]]$rh,
                             "rtioMoleDry12CCo2" = data$crdCo2[[lvl]]$rtioMoleDry12CCo2,
                             "rtioMoleDry13CCo2" = data$crdCo2[[lvl]]$rtioMoleDry13CCo2,
                             "rtioMoleDryCo2" = data$crdCo2[[lvl]]$rtioMoleDryCo2,
                             "rtioMoleDryH2o" = data$crdCo2[[lvl]]$rtioMoleDryH2o,
                             "rtioMoleWet12CCo2" = data$crdCo2[[lvl]]$rtioMoleWet12CCo2,
                             "rtioMoleWet13CCo2" = data$crdCo2[[lvl]]$rtioMoleWet13CCo2,
                             "rtioMoleWetCo2" = data$crdCo2[[lvl]]$rtioMoleWetCo2,
                             "rtioMoleWetH2o" = data$crdCo2[[lvl]]$rtioMoleWetH2o,
                             "rtioMoleWetH2oEnvHut" = data$envHut[[lvlEnvHut]]$rtioMoleWetH2o,
                             "temp" = data$crdCo2[[lvl]]$temp, 
                             "tempEnvHut" = data$envHut[[lvlEnvHut]]$temp,
                             "lvlCrdCo2" = data$crdCo2ValvLvl[[lvlValv]]$lvlCrdCo2
      )
      
      #input the whole day qfqm 
      wrk$qfqm <- list()
      #subset only
      wrk$qfqm$crdCo2 <- qfInp$crdCo2[[lvl]]
      
      if (PrdMeas == PrdAgr) {
        #PrdAgr <- 9
        #9 minutely sampling data
        #idxLvLPrdAgr <- paste0(lvl, "_", sprintf("%02d", PrdAgr), "m")
        #rpt[[dp01]][[idxLvLPrdAgr]] <- list()
        
        #if there is at least one measurement
        if(length(which(!is.na(wrk$qfqm$crdCo2$qfRngTemp))) > 0){
          #determine the index of each measurement  
          wrk$idx <- eddy4R.base::def.idx.agr(time = data$time, PrdAgr = (PrdMeas*60), FreqLoca = 1, MethIdx = "specBgn", data = wrk$qfqm$crdCo2$qfRngTemp, CritTime = 60)
          #delete row if last timeBgn and timeEnd is NA
          wrk$idx <- wrk$idx[rowSums(is.na(wrk$idx)) != 2,]
          #if last timeEnd is NA, replce that time to the last time value in data$time
          wrk$idx$timeEnd <- as.POSIXct(ifelse(is.na(wrk$idx$timeEnd), data$time[length(data$time)], wrk$idx$timeEnd), origin = "1970-01-01", tz = "UTC")
          #idxAgr2 <- 0
          for (idxAgr in 1:length(wrk$idx$idxBgn)){
            #idxAgr <- 25
            #get data for each idxAgr
            wrk$inpMask$data <- list()
            wrk$inpMask$data <- wrk$data[wrk$idx$idxBgn[idxAgr]:wrk$idx$idxEnd[idxAgr],]
            
            #replace data with NaN when valve switch to measure to next level before schedule time (9 min)
            wrk$inpMask$data$presEnvHut <- ifelse(wrk$inpMask$data$lvlCrdCo2 == lvlCrdCo2, wrk$inpMask$data$presEnvHut, NaN)
            wrk$inpMask$data$rhEnvHut <- ifelse(wrk$inpMask$data$lvlCrdCo2 == lvlCrdCo2, wrk$inpMask$data$rhEnvHut, NaN)
            wrk$inpMask$data$rtioMoleWetH2oEnvHut <- ifelse(wrk$inpMask$data$lvlCrdCo2 == lvlCrdCo2, wrk$inpMask$data$rtioMoleWetH2oEnvHut, NaN)
            wrk$inpMask$data$tempEnvHut <- ifelse(wrk$inpMask$data$lvlCrdCo2 == lvlCrdCo2, wrk$inpMask$data$tempEnvHut, NaN)
            
            #get rid of lvlCrdCo2
            wrk$inpMask$data <- wrk$inpMask$data[,-which(names(wrk$inpMask$data) == "lvlCrdCo2")]
            
            #dp01 processing
            rpt[[idxAgr]] <- eddy4R.base::wrap.dp01(
              # assign data: data.frame or list of type numeric or integer
              data = wrk$inpMask$data#,
              # if data is a list, which list entries should be processed into Level 1 data products?
              # defaults to NULL which expects data to be a data.frame
              #idx = 
              #names(wrk$inpMask$data[[dp01]]) #"000_010_02m"
            )
            
            #units:
            for (idxVar in names(rpt[[1]]$mean)){
              #idxVar <- names(rpt[[1]]$mean)[1]
              attributes(rpt[[1]]$mean[[idxVar]])$unit <- attributes(wrk$data[[idxVar]])$unit
            }
            
            #grab and add both time begin and time end to rpt
            rpt[[idxAgr]]$timeBgn <- list()
            rpt[[idxAgr]]$timeEnd <- list()
            
            #output time for dp01
            for(idxVar in names(wrk$data)[which(!(names(wrk$data) %in% c("lvlCrdCo2")))]){
              rpt[[idxAgr]]$timeBgn[[idxVar]] <- wrk$idx$timeBgn[idxAgr]
              rpt[[idxAgr]]$timeEnd[[idxVar]] <- wrk$idx$timeEnd[idxAgr]
            }; rm(idxVar)
            
            #}# end of there is at least one data
            
          }; rm(idxAgr)
        } else {
          
          rpt[[1]] <- list()
          
          for(idxStat in NameStat){
            #idxStat <- NameStat[1]
            rpt[[1]][[idxStat]] <- as.data.frame(matrix(NaN, nrow = 1, ncol = ncol(wrk$data)))
            #assign name to each column
            names(rpt[[1]][[idxStat]]) <- names(wrk$data)
            #not report lvlCrdCo2
            rpt[[1]][[idxStat]] <- rpt[[1]][[idxStat]][which(!(names(rpt[[1]][[idxStat]]) %in% c("lvlCrdCo2")))]
            
            }; rm(idxStat)
          #add both time begin and time end to rpt
          rpt[[1]]$timeBgn <- list()
          rpt[[1]]$timeEnd <- list()
          
          #output time for dp01
          for(idxVar in names(wrk$data)[which(!(names(wrk$data) %in% c("lvlCrdCo2")))]){
            rpt[[1]]$timeBgn[[idxVar]] <- data$time[1]
            rpt[[1]]$timeEnd[[idxVar]] <- data$time[length(data$time)]
            #unit
            attributes(rpt[[1]]$mean[[idxVar]])$unit <- attributes(wrk$data[[idxVar]])$unit
            
          }; rm(idxVar)
          
        }#end of if no measurement data at all in the whole day
      } #end of PrdAgr
      
      if (PrdMeas != PrdAgr) {
        #PrdAgr <- 30
        #if there is at least one measurement
        if(length(which(!is.na(wrk$qfqm$crdCo2$qfRngTemp))) > 0){
          
          #determine the index of each measurement  
          wrk$idx <- eddy4R.base::def.idx.agr(time = data$time, PrdAgr = (PrdMeas*60), FreqLoca = 1, MethIdx = "specBgn", data = wrk$qfqm$crdCo2$qfRngTemp, CritTime = 60)
          #delete row if last timeBgn and timeEnd is NA
          wrk$idx <- wrk$idx[rowSums(is.na(wrk$idx)) != 2,]
          #if last timeEnd is NA, replce that time to the last time value in data$time
          wrk$idx$timeEnd <- as.POSIXct(ifelse(is.na(wrk$idx$timeEnd), data$time[length(data$time)], wrk$idx$timeEnd), origin = "1970-01-01", tz = "UTC")
          
          whrSamp <- wrk$idx$idxBgn[1]:wrk$idx$idxEnd[1]
          if (length (wrk$idx$idxBgn) > 1 ){
            for(ii in 2:length (wrk$idx$idxBgn)){
              whrSamp <- c(whrSamp, wrk$idx$idxBgn[ii]:wrk$idx$idxEnd[ii])
            }
          }
          
          tmpAttr <- list()
          for (idxData in c("presEnvHut", "rhEnvHut", "rtioMoleWetH2oEnvHut", "tempEnvHut")){
            #defined attributes 
            tmpAttr[[idxData]] <- attributes(wrk$data[[idxData]])
            #replace idxData data with NaN when irga got kick out to measure the new measurement level
            wrk$data[[idxData]] <- ifelse(wrk$data$lvlCrdCo2 == lvlCrdCo2, wrk$data[[idxData]], NaN)
          }
          wrk$data[-whrSamp, 1:15] <- NaN
          #added attributes
          for (idxData in c("presEnvHut", "rhEnvHut", "rtioMoleWetH2oEnvHut", "tempEnvHut")){
            attributes(wrk$data[[idxData]]) <- tmpAttr[[idxData]]
          }
        } else {#if there are no data at all in wrk$data$temp
          wrk$data[,1:15] <- NaN
        } 
        
        
        for(idxAgr in c(1:length(idxTime[[paste0(PrdAgr, "min")]]$Bgn))) {
          #idxAgr <- 48
          
          
          ## grab data at the selected mask data
          # for data
          wrk$inpMask$data <- list()
          idxLvLPrdAgr <- paste0(lvl, "_", sprintf("%02d", PrdAgr), "m")
          
          wrk$inpMask$data <- wrk$data[idxTime[[paste0(PrdAgr, "min")]]$Bgn[idxAgr]:idxTime[[paste0(PrdAgr, "min")]]$End[idxAgr],]
          #get rid of lvlCrdCo2
          wrk$inpMask$data <- wrk$inpMask$data[,-which(names(wrk$inpMask$data) == "lvlCrdCo2")]
          
          #call wrap.dp01.R to calculate descriptive statistics 
          rpt[[idxAgr]] <- eddy4R.base::wrap.dp01(
            # assign data: data.frame or list of type numeric or integer
            data = wrk$inpMask$data#,
            # if data is a list, which list entries should be processed into Level 1 data products?
            # defaults to NULL which expects data to be a data.frame
            #idx = 
            #names(wrk$inpMask$data[[dp01]]) #"000_010_02m"
          )
          
          #units:
          for (idxVar in names(rpt[[1]]$mean)){
            #idxVar <- names(rpt[[1]]$mean)[1]
            attributes(rpt[[1]]$mean[[idxVar]])$unit <- attributes(wrk$data[[idxVar]])$unit
          } 
          
          #grab and add both time begin and time end to rpt
          rpt[[idxAgr]]$timeBgn <- list()
          rpt[[idxAgr]]$timeEnd <- list()
          
          for(idxVar in names(wrk$data)[which(!(names(wrk$data) %in% c("lvlCrdCo2")))]){
            rpt[[idxAgr]]$timeBgn[[idxVar]] <- data$time[idxTime[[paste0(PrdAgr, "min")]]$Bgn[idxAgr]]
            rpt[[idxAgr]]$timeEnd[[idxVar]] <- data$time[idxTime[[paste0(PrdAgr, "min")]]$End[idxAgr]]
          }
          
        }; #rm(idxAgr)
        
      }#end of PrdAgr == 30
      
    }#end of TypeMeas %in% "samp" if statement
    
    #during validation period 
    if (TypeMeas %in% "vali"){
      #input the whole day data
      wrk$data <- data.frame(stringsAsFactors = FALSE,
                             "dlta13CCo2" = data$crdCo2[[lvl]]$dlta13CCo2,
                             "dlta13CCo2Refe" = data$crdCo2[[lvl]]$dlta13CCo2Refe,
                             "pres" = data$crdCo2[[lvl]]$pres,
                             "presEnvHut" = data$envHut[[lvlEnvHut]]$pres,
                             "rhEnvHut" = data$envHut[[lvlEnvHut]]$rh,
                             "rtioMoleDry12CCo2" = data$crdCo2[[lvl]]$rtioMoleDry12CCo2,
                             "rtioMoleDry13CCo2" = data$crdCo2[[lvl]]$rtioMoleDry13CCo2,
                             "rtioMoleDryCo2" = data$crdCo2[[lvl]]$rtioMoleDryCo2,
                             "rtioMoleDryCo2Refe" = data$crdCo2[[lvl]]$rtioMoleDryCo2Refe,
                             "rtioMoleDryH2o" = data$crdCo2[[lvl]]$rtioMoleDryH2o,
                             "rtioMoleWet12CCo2" = data$crdCo2[[lvl]]$rtioMoleWet12CCo2,
                             "rtioMoleWet13CCo2" = data$crdCo2[[lvl]]$rtioMoleWet13CCo2,
                             "rtioMoleWetCo2" = data$crdCo2[[lvl]]$rtioMoleWetCo2,
                             "rtioMoleWetH2o" = data$crdCo2[[lvl]]$rtioMoleWetH2o,
                             "rtioMoleWetH2oEnvHut" = data$envHut[[lvlEnvHut]]$rtioMoleWetH2o,
                             "temp" = data$crdCo2[[lvl]]$temp, 
                             "tempEnvHut" = data$envHut[[lvlEnvHut]]$temp
      )
      
      #input the whole day qfqm 
      wrk$qfqm <- list()
      wrk$qfqm$crdCo2 <- qfInp$crdCo2[[lvl]]
      
      if (PrdMeas == PrdAgr) {
        #PrdAgr <- 9
        #9 minutely sampling data
        #idxLvLPrdAgr <- paste0(lvl, "_", sprintf("%02d", PrdAgr), "m")
        #rpt[[dp01]][[idxLvLPrdAgr]] <- list()
        
        #if there is at least one measurement
        if(length(which(!is.na(wrk$qfqm$crdCo2$qfRngTemp))) > 0){
          #determine the end time of each measurement
          wrk$idx <- eddy4R.base::def.idx.agr(time = data$time, PrdAgr = (PrdMeas*60), FreqLoca = 1, MethIdx = "specBgn", data = wrk$qfqm$crdCo2$qfRngTemp, CritTime = 60)
          #delete row if last timeBgn and timeEnd is NA
          wrk$idx <- wrk$idx[rowSums(is.na(wrk$idx)) != 2,]
          #if last timeEnd is NA, replce that time to the last time value in data$time
          wrk$idx$timeEnd <- as.POSIXct(ifelse(is.na(wrk$idx$timeEnd), data$time[length(data$time)], wrk$idx$timeEnd), origin = "1970-01-01", tz = "UTC")
          
          #idxAgr2 <- 0
          for (idxAgr in 1:length(wrk$idx$idxBgn)){
            #idxAgr <- 1
            #determine input data for each idxAgr
            wrk$inpMask$data <- list()
            wrk$inpMask$data <- wrk$data[wrk$idx$idxBgn[idxAgr]:wrk$idx$idxEnd[idxAgr],] 
            
            #calculate dp01
            rpt[[idxAgr]] <- eddy4R.base::wrap.dp01(
              # assign data: data.frame or list of type numeric or integer
              data = wrk$inpMask$data#,
              # if data is a list, which list entries should be processed into Level 1 data products?
              # defaults to NULL which expects data to be a data.frame
              #idx = 
              #names(wrk$inpMask$data[[dp01]]) #"000_010_02m"
            )
            
            #units:
            for (idxVar in names(rpt[[1]]$mean)){
              #idxVar <- names(rpt[[1]]$mean)[1]
              attributes(rpt[[1]]$mean[[idxVar]])$unit <- attributes(wrk$data[[idxVar]])$unit
            }
            
            #grab and add both time begin and time end to rpt
            rpt[[idxAgr]]$timeBgn <- list()
            rpt[[idxAgr]]$timeEnd <- list()
            
            #output time for dp01
            for(idxVar in names(wrk$data)){
              # rpt[[idxAgr2]]$timeBgn[[idxVar]] <- data$time[(whrEnd[idxAgr] - 20 - 2*60+1)]
              # rpt[[idxAgr2]]$timeEnd[[idxVar]] <- data$time[(whrEnd[idxAgr] - 20)]
              rpt[[idxAgr]]$timeBgn[[idxVar]] <- wrk$idx$timeBgn[idxAgr]
              rpt[[idxAgr]]$timeEnd[[idxVar]] <- wrk$idx$timeEnd[idxAgr]
            }; rm(idxVar)
            
          }#; rm(idxAgr)
          
        } else {
          
          rpt[[1]] <- list()
          
          for(idxStat in NameStat){
            #idxStat <- NameStat[1]
            rpt[[1]][[idxStat]] <- as.data.frame(matrix(NaN, nrow = 1, ncol = ncol(wrk$data)))
            #assign name to each column
            names(rpt[[1]][[idxStat]]) <- names(wrk$data)
            
            }; rm(idxStat)
          #add both time begin and time end to rpt
          rpt[[1]]$timeBgn <- list()
          rpt[[1]]$timeEnd <- list()
          
          #output time for dp01
          for(idxVar in names(wrk$data)){
            rpt[[1]]$timeBgn[[idxVar]] <- data$time[1]
            rpt[[1]]$timeEnd[[idxVar]] <- data$time[length(data$time)]
            #unit
            attributes(rpt[[1]]$mean[[idxVar]])$unit <- attributes(wrk$data[[idxVar]])$unit
            
          }; rm(idxVar)
          
        }#end of if no measurement data at all in the whole day
        
      } #end of PrdAgr == 9
      
      
      
      if (PrdMeas != PrdAgr) {
        #PrdAgr <- 30
        #if there is at least one measurement
        if(length(which(!is.na(wrk$qfqm$crdCo2$qfRngTemp))) > 0){
          #   #determine the end time of each measurement  
          wrk$idx <- eddy4R.base::def.idx.agr(time = data$time, PrdAgr = (PrdMeas*60), FreqLoca = 1, MethIdx = "specBgn", data = wrk$qfqm$crdCo2$qfRngTemp, CritTime = 60)
          #delete row if last timeBgn and timeEnd is NA
          wrk$idx <- wrk$idx[rowSums(is.na(wrk$idx)) != 2,]
          #if last timeEnd is NA, replce that time to the last time value in data$time
          wrk$idx$timeEnd <- as.POSIXct(ifelse(is.na(wrk$idx$timeEnd), data$time[length(data$time)], wrk$idx$timeEnd), origin = "1970-01-01", tz = "UTC")
          
          whrSamp <- wrk$idx$idxBgn[1]:wrk$idx$idxEnd[1]
          if (length (wrk$idx$idxBgn) > 1 ){
            for(ii in 2:length (wrk$idx$idxBgn)){
              whrSamp <- c(whrSamp, wrk$idx$idxBgn[ii]:wrk$idx$idxEnd[ii])
            }
          }
          wrk$data[-whrSamp, ] <- NaN
        } else {#end of if no measurement data at all in the whole day
          tmpAttr <- list()
          for (idxData in c("presEnvHut", "rhEnvHut", "rtioMoleWetH2oEnvHut", "tempEnvHut")){
            #defined attributes 
            tmpAttr[[idxData]] <- attributes(wrk$data[[idxData]])
            wrk$data[[idxData]] <- NaN
            attributes(wrk$data[[idxData]]) <- tmpAttr[[idxData]]
          }
        }
        
        for(idxAgr in c(1:length(idxTime[[paste0(PrdAgr, "min")]]$Bgn))) {
          #idxAgr <- 1
          
          ## grab data at the selected mask data
          # for data
          wrk$inpMask$data <- list()
          idxLvLPrdAgr <- paste0(lvl, "_", sprintf("%02d", PrdAgr), "m")      
          wrk$inpMask$data <- wrk$data[idxTime[[paste0(PrdAgr, "min")]]$Bgn[idxAgr]:idxTime[[paste0(PrdAgr, "min")]]$End[idxAgr],]
          
          #call wrap.dp01.R to calculate descriptive statistics 
          rpt[[idxAgr]] <- eddy4R.base::wrap.dp01(
            # assign data: data.frame or list of type numeric or integer
            data = wrk$inpMask$data#,
          )
          
          #units:
          for (idxVar in names(rpt[[1]]$mean)){
            #idxVar <- names(rpt[[1]]$mean)[1]
            attributes(rpt[[1]]$mean[[idxVar]])$unit <- attributes(wrk$data[[idxVar]])$unit
          }
          
          #grab and add both time begin and time end to rpt
          rpt[[idxAgr]]$timeBgn <- list()
          rpt[[idxAgr]]$timeEnd <- list()
          
          #output time for dp01
          for(idxVar in names(wrk$data)){
            # rpt[[idxAgr2]]$timeBgn[[idxVar]] <- data$time[(whrEnd[idxAgr] - 20 - 2*60+1)]
            # rpt[[idxAgr2]]$timeEnd[[idxVar]] <- data$time[(whrEnd[idxAgr] - 20)]
            rpt[[idxAgr]]$timeBgn[[idxVar]] <- data$time[idxTime[[paste0(PrdAgr, "min")]]$Bgn[idxAgr]]
            rpt[[idxAgr]]$timeEnd[[idxVar]] <- data$time[idxTime[[paste0(PrdAgr, "min")]]$End[idxAgr]]
          }; rm(idxVar)
          
        }; #rm(idxAgr)
      } #end of PrdAgr == 30
    }#end of TypeMeas %in% "vali" if statement
  }##end of dp01 if statement 
  
  #calculate dp01 for "isoH2o" ########################################################################################
  if (dp01 %in% c("isoH2o")){
    #during sampling period 
    if (TypeMeas %in% "samp"){ 
      #assign lvlCrdH2o for each measurement level
      if (lvl == "000_010") {lvlCrdH2o <- "lvl01"}
      if (lvl == "000_020") {lvlCrdH2o <- "lvl02"}
      if (lvl == "000_030") {lvlCrdH2o <- "lvl03"}
      if (lvl == "000_040") {lvlCrdH2o <- "lvl04"}
      if (lvl == "000_050") {lvlCrdH2o <- "lvl05"}
      if (lvl == "000_060") {lvlCrdH2o <- "lvl06"}
      if (lvl == "000_070") {lvlCrdH2o <- "lvl07"}
      if (lvl == "000_080") {lvlCrdH2o <- "lvl08"}
      
      wrk$data <- data.frame(stringsAsFactors = FALSE,
                             "dlta18OH2o" = data$crdH2o[[lvl]]$dlta18OH2o,
                             "dlta2HH2o" = data$crdH2o[[lvl]]$dlta2HH2o,
                             "pres" = data$crdH2o[[lvl]]$pres,
                             "presEnvHut" = data$envHut[[lvlEnvHut]]$pres,
                             "rhEnvHut" = data$envHut[[lvlEnvHut]]$rh,
                             "rtioMoleDryH2o" = data$crdH2o[[lvl]]$rtioMoleDryH2o,
                             "rtioMoleWetH2o" = data$crdH2o[[lvl]]$rtioMoleWetH2o,
                             "rtioMoleWetH2oEnvHut" = data$envHut[[lvlEnvHut]]$rtioMoleWetH2o,
                             "temp" = data$crdH2o[[lvl]]$temp,
                             "tempEnvHut" = data$envHut[[lvlEnvHut]]$temp,
                             "lvlCrdH2o" = data$crdH2oValvLvl[[lvlValv]]$lvlCrdH2o
      )
      
      #input the whole day qfqm
      wrk$qfqm <- list()
      wrk$qfqm$crdH2o <- qfInp$crdH2o[[lvl]]
      
      if (PrdMeas == PrdAgr) {
        #PrdAgr <- 9
        #9 minutely sampling data
        #idxLvLPrdAgr <- paste0(lvl, "_", sprintf("%02d", PrdAgr), "m")
        #rpt[[dp01]][[idxLvLPrdAgr]] <- list()
        
        #if there is at least one measurement
        if(length(which(!is.na(wrk$qfqm$crdH2o$qfRngTemp))) > 0){
          #determine the index of each measurement  
          wrk$idx <- eddy4R.base::def.idx.agr(time = data$time, PrdAgr = (PrdMeas*60), FreqLoca = 1, MethIdx = "specBgn", data = wrk$qfqm$crdH2o$qfRngTemp, CritTime = 60)
          #delete row if last timeBgn and timeEnd is NA
          wrk$idx <- wrk$idx[rowSums(is.na(wrk$idx)) != 2,]
          #if last timeEnd is NA, replce that time to the last time value in data$time
          wrk$idx$timeEnd <- as.POSIXct(ifelse(is.na(wrk$idx$timeEnd), data$time[length(data$time)], wrk$idx$timeEnd), origin = "1970-01-01", tz = "UTC")
          #idxAgr2 <- 0
          for (idxAgr in 1:length(wrk$idx$idxBgn)){
            #idxAgr <- 124
            #get data for each idxAgr
            wrk$inpMask$data <- list()
            wrk$inpMask$data <- wrk$data[wrk$idx$idxBgn[idxAgr]:wrk$idx$idxEnd[idxAgr],]  
            #replace data with NaN when valve switch to measure to next level before schedule time (9 min)
            wrk$inpMask$data$presEnvHut <- ifelse(wrk$inpMask$data$lvlCrdH2o == lvlCrdH2o, wrk$inpMask$data$presEnvHut, NaN)
            wrk$inpMask$data$rhEnvHut <- ifelse(wrk$inpMask$data$lvlCrdH2o == lvlCrdH2o, wrk$inpMask$data$rhEnvHut, NaN)
            wrk$inpMask$data$rtioMoleWetH2oEnvHut <- ifelse(wrk$inpMask$data$lvlCrdH2o == lvlCrdH2o, wrk$inpMask$data$rtioMoleWetH2oEnvHut, NaN)
            wrk$inpMask$data$tempEnvHut <- ifelse(wrk$inpMask$data$lvlCrdH2o == lvlCrdH2o, wrk$inpMask$data$tempEnvHut, NaN)
            #get rid of lvlCrdH2o
            wrk$inpMask$data <- wrk$inpMask$data[,-which(names(wrk$inpMask$data) == "lvlCrdH2o")]
            
            #dp01 processing
            rpt[[idxAgr]] <- eddy4R.base::wrap.dp01(
              # assign data: data.frame or list of type numeric or integer
              data = wrk$inpMask$data#,
            )
            
            #units:
            for (idxVar in names(rpt[[1]]$mean)){
              #idxVar <- names(rpt[[1]]$mean)[1]
              attributes(rpt[[1]]$mean[[idxVar]])$unit <- attributes(wrk$data[[idxVar]])$unit
            }
            
            #grab and add both time begin and time end to rpt
            rpt[[idxAgr]]$timeBgn <- list()
            rpt[[idxAgr]]$timeEnd <- list()
            
            for(idxVar in names(wrk$data)[which(!(names(wrk$data) %in% c("lvlCrdH2o")))]){
              rpt[[idxAgr]]$timeBgn[[idxVar]] <- wrk$idx$timeBgn[idxAgr]
              rpt[[idxAgr]]$timeEnd[[idxVar]] <- wrk$idx$timeEnd[idxAgr]
            }
            
            #}# end of there is at least one data
            
            
          }; rm(idxAgr)
        } else {
          
          rpt[[1]] <- list()
          for(idxStat in NameStat){
            #idxStat <- NameStat[1]
            rpt[[1]][[idxStat]] <- as.data.frame(matrix(NaN, nrow = 1, ncol = ncol(wrk$data)))
            #assign name to each column
            names(rpt[[1]][[idxStat]]) <- names(wrk$data)
            #not report lvlCrdH2o
            rpt[[1]][[idxStat]] <- rpt[[1]][[idxStat]][which(!(names(rpt[[1]][[idxStat]]) %in% c("lvlCrdH2o")))]
            
            }; rm(idxStat)
          #add both time begin and time end to rpt
          rpt[[1]]$timeBgn <- list()
          rpt[[1]]$timeEnd <- list()
          
          #output time for dp01
          for(idxVar in names(wrk$data)[which(!(names(wrk$data) %in% c("lvlCrdH2o")))]){
            rpt[[1]]$timeBgn[[idxVar]] <- data$time[1]
            rpt[[1]]$timeEnd[[idxVar]] <- data$time[length(data$time)]
            #unit
            attributes(rpt[[1]]$mean[[idxVar]])$unit <- attributes(wrk$data[[idxVar]])$unit
            
          }; rm(idxVar)
          
        }#end of if no measurement data at all in the whole day
      } #end of PrdAgr == 9
      
      if (PrdMeas != PrdAgr) {
        #PrdAgr <- 30
        #if there is at least one measurement
        if(length(which(!is.na(wrk$qfqm$crdH2o$qfRngTemp))) > 0){
          
          #determine the index of each measurement  
          wrk$idx <- eddy4R.base::def.idx.agr(time = data$time, PrdAgr = (PrdMeas*60), FreqLoca = 1, MethIdx = "specBgn", data = wrk$qfqm$crdH2o$qfRngTemp, CritTime = 60)
          #delete row if last timeBgn and timeEnd is NA
          wrk$idx <- wrk$idx[rowSums(is.na(wrk$idx)) != 2,]
          #if last timeEnd is NA, replce that time to the last time value in data$time
          wrk$idx$timeEnd <- as.POSIXct(ifelse(is.na(wrk$idx$timeEnd), data$time[length(data$time)], wrk$idx$timeEnd), origin = "1970-01-01", tz = "UTC")
          
          whrSamp <- wrk$idx$idxBgn[1]:wrk$idx$idxEnd[1]
          if (length (wrk$idx$idxBgn) > 1 ){
            for(ii in 2:length (wrk$idx$idxBgn)){
              whrSamp <- c(whrSamp, wrk$idx$idxBgn[ii]:wrk$idx$idxEnd[ii])
            }
          }
          
          tmpAttr <- list()
          for (idxData in c("presEnvHut", "rhEnvHut", "rtioMoleWetH2oEnvHut", "tempEnvHut")){
            #defined attributes 
            tmpAttr[[idxData]] <- attributes(wrk$data[[idxData]])
            #replace idxData data with NaN when irga got kick out to measure the new measurement level
            wrk$data[[idxData]] <- ifelse(wrk$data$lvlCrdH2o == lvlCrdH2o, wrk$data[[idxData]], NaN)
          }
          
          wrk$data[-whrSamp, 1:10] <- NaN
          #added attributes
          for (idxData in c("presEnvHut", "rhEnvHut", "rtioMoleWetH2oEnvHut", "tempEnvHut")){
            attributes(wrk$data[[idxData]]) <- tmpAttr[[idxData]]
          }
        } else {#if there are no data at all in wrk$data$temp
          wrk$data[,1:10] <- NaN
        } 
        
        
        for(idxAgr in c(1:length(idxTime[[paste0(PrdAgr, "min")]]$Bgn))) {
          #idxAgr <- 48
          
          
          ## grab data at the selected mask data
          # for data
          wrk$inpMask$data <- list()
          idxLvLPrdAgr <- paste0(lvl, "_", sprintf("%02d", PrdAgr), "m")
          
          wrk$inpMask$data <- wrk$data[idxTime[[paste0(PrdAgr, "min")]]$Bgn[idxAgr]:idxTime[[paste0(PrdAgr, "min")]]$End[idxAgr],]
          #get rid of lvlCrdH2o
          wrk$inpMask$data <- wrk$inpMask$data[,-which(names(wrk$inpMask$data) == "lvlCrdH2o")]
          # for qfqm
          #call wrap.dp01.R to calculate descriptive statistics
          
          rpt[[idxAgr]] <- eddy4R.base::wrap.dp01(
            # assign data: data.frame or list of type numeric or integer
            data = wrk$inpMask$data#,
            
          )
          
          #units:
          for (idxVar in names(rpt[[1]]$mean)){
            #idxVar <- names(rpt[[1]]$mean)[1]
            attributes(rpt[[1]]$mean[[idxVar]])$unit <- attributes(wrk$data[[idxVar]])$unit
          }
          
          #grab and add both time begin and time end to rpt
          rpt[[idxAgr]]$timeBgn <- list()
          rpt[[idxAgr]]$timeEnd <- list()
          
          for(idxVar in names(wrk$data)){
            rpt[[idxAgr]]$timeBgn[[idxVar]] <- data$time[idxTime[[paste0(PrdAgr, "min")]]$Bgn[idxAgr]]
            rpt[[idxAgr]]$timeEnd[[idxVar]] <- data$time[idxTime[[paste0(PrdAgr, "min")]]$End[idxAgr]]
          }
          
        }; #rm(idxAgr)
        
      }#end of PrdAgr == 30
      
    }#end of TypeMeas %in% "samp" if statement
    
    #during validation period 
    if (TypeMeas %in% "vali"){
      #assign name for dlta18OH2oRefe and dlta2HH2oRefe
      if (lvl == "h2oHigh") {
        tmpDlta18OH2oRefe <- "dlta18OH2oRefeHigh"
        tmpDlta2HH2oRefe <- "dlta2HH2oRefeHigh"}
      if (lvl == "h2oLow") {
        tmpDlta18OH2oRefe <- "dlta18OH2oRefeLow"
        tmpDlta2HH2oRefe <- "dlta2HH2oRefeLow"}
      if (lvl == "h2oMed") {
        tmpDlta18OH2oRefe <- "dlta18OH2oRefeMed"
        tmpDlta2HH2oRefe <- "dlta2HH2oRefeMed"}
      
      wrk$data <- data.frame(stringsAsFactors = FALSE,
                             "dlta18OH2o" = data$crdH2o[[lvl]]$dlta18OH2o,
                             "dlta18OH2oRefe" = data$crdH2o[[lvl]][[tmpDlta18OH2oRefe]],
                             "dlta2HH2o" = data$crdH2o[[lvl]]$dlta2HH2o,
                             "dlta2HH2oRefe" = data$crdH2o[[lvl]][[tmpDlta2HH2oRefe]],
                             "pres" = data$crdH2o[[lvl]]$pres,
                             "presEnvHut" = data$envHut[[lvlEnvHut]]$pres,
                             "rhEnvHut" = data$envHut[[lvlEnvHut]]$rh,
                             "rtioMoleDryH2o" = data$crdH2o[[lvl]]$rtioMoleDryH2o,
                             "rtioMoleWetH2o" = data$crdH2o[[lvl]]$rtioMoleWetH2o,
                             "rtioMoleWetH2oEnvHut" = data$envHut[[lvlEnvHut]]$rtioMoleWetH2o,
                             "temp" = data$crdH2o[[lvl]]$temp,
                             "tempEnvHut" = data$envHut[[lvlEnvHut]]$temp,
                             "injNum" = data$crdH2oValvVali[[lvlCrdH2oValvVali]]$injNum
                             
      )
      
      #input the whole day qfqm
      wrk$qfqm <- list()
      wrk$qfqm$crdH2o <- qfInp$crdH2o[[lvl]]
      #replace injNum to NaN when they are not measured at that period
      wrk$data$injNum <- ifelse(is.na(wrk$qfqm$crdH2o$qfRngTemp), NaN, wrk$data$injNum)
      
      #TODO: ND added this previously, but we need to change back till a merged solution can be created
      # if (lvl == "h2oHigh") {
      #   wrk$data$injNum <- ifelse(wrk$data$injNum %in% c(1:6), wrk$data$injNum, NaN)
      # }
      # if (lvl == "h2oMed") {
      #   wrk$data$injNum <- ifelse(wrk$data$injNum %in% c(7:12), wrk$data$injNum, NaN)
      # }
      # if (lvl == "h2oLow") {
      #   wrk$data$injNum <- ifelse(wrk$data$injNum %in% c(13:18), wrk$data$injNum, NaN)
      # }
      
      if (PrdMeas == PrdAgr) {        
        #idxLvLPrdAgr <- paste0(lvl, "_", sprintf("%02d", PrdAgr), "m")
        #rpt[[dp01]][[idxLvLPrdAgr]] <- list()
        
        #if there is at least one measurement
        if(length(which(!is.na(wrk$data$injNum))) > 0){
          #determine the end time of each measurement
          wrk$idx <- eddy4R.base::def.idx.agr(time = data$time, PrdAgr = (PrdMeas*60), FreqLoca = 1, MethIdx = "specEnd", crdH2oVali = TRUE, data = wrk$data$injNum, CritTime = 15)
          #delete row if last timeBgn and timeEnd is NA
          wrk$idx <- wrk$idx[rowSums(is.na(wrk$idx)) != 2,]
          #if last timeEnd is NA, replce that time to the last time value in data$time
          wrk$idx$timeEnd <- as.POSIXct(ifelse(is.na(wrk$idx$timeEnd), data$time[length(data$time)], wrk$idx$timeEnd), origin = "1970-01-01", tz = "UTC")
          
          #idxAgr2 <- 0
          for (idxAgr in 1:length(wrk$idx$idxBgn)){
            #idxAgr <- 1
            #determine input data for each idxAgr
            wrk$inpMask$data <- list()
            wrk$inpMask$data <- wrk$data[wrk$idx$idxBgn[idxAgr]:wrk$idx$idxEnd[idxAgr],] 
            #get rid of injNum
            wrk$inpMask$data <- wrk$inpMask$data[,-which(names(wrk$inpMask$data) == "injNum")]
            #calculate dp01
            rpt[[idxAgr]] <- eddy4R.base::wrap.dp01(
              # assign data: data.frame or list of type numeric or integer
              data = wrk$inpMask$data#,
              # if data is a list, which list entries should be processed into Level 1 data products?
              # defaults to NULL which expects data to be a data.frame
              #idx = 
              #names(wrk$inpMask$data[[dp01]]) #"000_010_02m"
            )
            
            #units:
            for (idxVar in names(rpt[[1]]$mean)){
              #idxVar <- names(rpt[[1]]$mean)[1]
              attributes(rpt[[1]]$mean[[idxVar]])$unit <- attributes(wrk$data[[idxVar]])$unit
            }
            
            #grab and add both time begin and time end to rpt
            
            #for (idxLvLPrdAgr in names(wrk$inpMask$data[[dp01]])){
            #idxLvLPrdAgr <- names(wrk$inpMask$data[[dp01]])[1]
            rpt[[idxAgr]]$timeBgn <- list()
            rpt[[idxAgr]]$timeEnd <- list()
            
            for(idxVar in names(rpt[[idxAgr]]$mean)){
              # rpt[[idxAgr2]]$timeBgn[[idxVar]] <- data$time[(whrEnd[idxAgr] - 20 - 2*60+1)]
              # rpt[[idxAgr2]]$timeEnd[[idxVar]] <- data$time[(whrEnd[idxAgr] - 20)]
              rpt[[idxAgr]]$timeBgn[[idxVar]] <- wrk$idx$timeBgn[idxAgr]
              rpt[[idxAgr]]$timeEnd[[idxVar]] <- wrk$idx$timeEnd[idxAgr]
            }
            
          }#; rm(idxAgr)
          
        } else {
          
          rpt[[1]] <- list()
          
          for(idxStat in NameStat){
            #idxStat <- NameStat[1]
            rpt[[1]][[idxStat]] <- as.data.frame(matrix(NaN, nrow = 1, ncol = ncol(wrk$data)))
            #assign name to each column
            names(rpt[[1]][[idxStat]]) <- names(wrk$data)
            #not report injNum
            rpt[[1]][[idxStat]] <- rpt[[1]][[idxStat]][which(!(names(rpt[[1]][[idxStat]]) %in% c("injNum")))]
            
            }; rm(idxStat)
          #add both time begin and time end to rpt
          rpt[[1]]$timeBgn <- list()
          rpt[[1]]$timeEnd <- list()
          
          #output time for dp01
          for(idxVar in names(wrk$data)[which(!(names(wrk$data) %in% c("injNum")))]){
            rpt[[1]]$timeBgn[[idxVar]] <- data$time[1]
            rpt[[1]]$timeEnd[[idxVar]] <- data$time[length(data$time)]
            #unit
            attributes(rpt[[1]]$mean[[idxVar]])$unit <- attributes(wrk$data[[idxVar]])$unit
            
          };  rm(idxVar)
        }#end of if no measurement data at all in the whole day
      }#end of PrdAgr == 3
      
      if (PrdMeas != PrdAgr) {
        #if there is at least one measurement
        if(length(which(!is.na(wrk$qfqm$crdH2o$qfRngTemp))) > 0){
          #   #determine the end time of each measurement  
          wrk$idx <- eddy4R.base::def.idx.agr(time = data$time, PrdAgr = (PrdMeas*60), FreqLoca = 1, MethIdx = "specEnd", crdH2oVali = TRUE, data = wrk$data$injNum, CritTime = 15)
          #delete row if last timeBgn and timeEnd is NA
          wrk$idx <- wrk$idx[rowSums(is.na(wrk$idx)) != 2,]
          #if last timeEnd is NA, replce that time to the last time value in data$time
          wrk$idx$timeEnd <- as.POSIXct(ifelse(is.na(wrk$idx$timeEnd), data$time[length(data$time)], wrk$idx$timeEnd), origin = "1970-01-01", tz = "UTC")
          
          whrSamp <- wrk$idx$idxBgn[1]:wrk$idx$idxEnd[1]
          if (length (wrk$idx$idxBgn) > 1 ){
            for(ii in 2:length (wrk$idx$idxBgn)){
              whrSamp <- c(whrSamp, wrk$idx$idxBgn[ii]:wrk$idx$idxEnd[ii])
            }
          }
          wrk$data[-whrSamp, ] <- NaN
        } else {#end of if no measurement data at all in the whole day
          tmpAttr <- list()
          for (idxData in c("presEnvHut", "rhEnvHut", "rtioMoleWetH2oEnvHut", "tempEnvHut")){
            #defined attributes 
            tmpAttr[[idxData]] <- attributes(wrk$data[[idxData]])
            wrk$data[[idxData]] <- NaN
            attributes(wrk$data[[idxData]]) <- tmpAttr[[idxData]]
          }
        }
        
        for(idxAgr in c(1:length(idxTime[[paste0(PrdAgr, "min")]]$Bgn))) {
          #idxAgr <- 1
          
          
          ## grab data at the selected mask data
          # for data
          wrk$inpMask$data <- list()
          idxLvLPrdAgr <- paste0(lvl, "_", sprintf("%02d", PrdAgr), "m")
          
          
          wrk$inpMask$data <- wrk$data[idxTime[[paste0(PrdAgr, "min")]]$Bgn[idxAgr]:idxTime[[paste0(PrdAgr, "min")]]$End[idxAgr],]
          #get rid of injNum
          wrk$inpMask$data <- wrk$inpMask$data[,-which(names(wrk$inpMask$data) == "injNum")]
          
          # http://stackoverflow.com/questions/26843861/replace-rbind-in-for-loop-with-lapply-2nd-circle-of-hell
          #call wrap.dp01.R to calculate descriptive statistics
          
          rpt[[idxAgr]] <- eddy4R.base::wrap.dp01(
            # assign data: data.frame or list of type numeric or integer
            data = wrk$inpMask$data#,
            # if data is a list, which list entries should be processed into Level 1 data products?
            # defaults to NULL which expects data to be a data.frame
            #idx = 
            #names(wrk$inpMask$data[[dp01]]) #"000_010_02m"
          )
          
          #units:
          for (idxVar in names(rpt[[1]]$mean)){
            #idxVar <- names(rpt[[1]]$mean)[1]
            attributes(rpt[[1]]$mean[[idxVar]])$unit <- attributes(wrk$data[[idxVar]])$unit
          }
          
          #grab and add both time begin and time end to rpt
          rpt[[idxAgr]]$timeBgn <- list()
          rpt[[idxAgr]]$timeEnd <- list()
          
          for(idxVar in names(rpt[[idxAgr]]$mean)){
            rpt[[idxAgr]]$timeBgn[[idxVar]] <- data$time[idxTime[[paste0(PrdAgr, "min")]]$Bgn[idxAgr]]
            rpt[[idxAgr]]$timeEnd[[idxVar]] <- data$time[idxTime[[paste0(PrdAgr, "min")]]$End[idxAgr]]
          }
          
        }; #rm(idxAgr)
      } #end of PrdAgr == 30
      
    }#end of TypeMeas %in% "vali" if statement
    
    
  }#end of dp01 if statement
  
  #remove NULL list from rpt
  rpt <- rpt[!sapply(rpt, is.null)]
  #return results
  return(rpt)
  
}#end function wrap.dp01.ecse()
