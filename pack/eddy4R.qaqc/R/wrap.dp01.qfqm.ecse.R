##############################################################################################
#' @title Wrapper function:  Preprocessing and calculating quality metrics, alpha and beta quality metrics, and final quality flag for the NEON eddy-covariance stroage exchange L1 data products

#' @author
#' Natchaya Pingintha-Durden \email{eddy4R.info@gmail.com}

#' @description 
#' Wrapper function. Preprocessing and calculating quality metrics, alpha and beta quality metrics, and final quality flag for the NEON eddy-covariance stroage exchange (ECSE) Level 1 data products (dp01).

#' @param dp01 A vector of class "character" containing the name of NEON ECSE dp01 which descriptive statistics are being calculated, \cr
#' c("co2Stor", "h2oStor", "tempAirLvl", "tempAirTop", "isoCo2", "isoH2o"). Defaults to "co2Stor". [-] 
#' @param RptExpd A logical parameter that determines if the full quality metric \code{qm} is output in the returned list (defaults to FALSE).
#' @param lvl  Measurement level of dp01 which descriptive statistics are being calculated. Of type character. [-]
#' @param lvlMfcSampStor Measurement level of mfcSampStor which apply to only  dp01 equal to "co2Stor" or "h2oStor". Defaults to NULL. Of type character. [-]
#' @param lvlMfcValiStor Measurement level of mfcValiStor which apply to only  dp01 equal to "co2Stor", "h2oStor", or "isoCo2. Defaults to NULL. Of type character. [-]
#' @param lvlEnvHut Measurement level of envHut. Defaults to NULL. Of type character. [-]
#' @param lvlValv Measurement level of irgaValvLvl, crdCo2ValvLvl, or crdH2oValvLvl. Defaults to NULL. Of type character. [-]
#' @param lvlValvAux Location of valvAux which apply to only  dp01 equal to "co2Stor" or "h2oStor". Defaults to NULL. Of type character. [-]
#' @param lvlCrdH2oValvVali Measurement level of crdH2oValvVali which apply to only  dp01 equal to "isoH2o". Defaults to NULL. Of type character. [-]
#' @param data A list of data frame containing the input dp0p data that related to dp01 which qfqm are being calculated. Of class integer". [User defined] 
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

#' @keywords dp01, ECSE, NEON QAQC, quality flags, quality metrics, 

#' @examples Currently none.

#' @seealso Currently none.

#' @export

# changelog and author contributions / copyrights 
#   Natchaya Pingintha-Durden (2017-07-28)
#     original creation
#   Natchaya Pingintha-Durden (2017-09-19)
#     added envHut data
#   Natchaya Pingintha-Durden (2017-12-04)
#     modified the logic to not output the empty list when there is no data for a whole day
#   Natchaya Pingintha-Durden (2018-01-23)
#     use quality flag to determine indices
#   Natchaya P-Durden (2018-04-03)
#     update @param format
#   Ke Xu (2018-04-19)
#     applied term name convention; replaced qfInput by qfInp
#   Natchaya P-Durden (2018-05-23)
#     rename function from wrap.neon.dp01.qfqm.ecse() to wrap.dp01.qfqm.ecse()
#     rename function from wrap.neon.dp01.qfqm() to wrap.dp01.qfqm.eddy()
#   Natchaya P-Durden (2019-01-30)
#     bugs fixed to report qfValiH2o when no h2oRefe data
#   Natchaya P-Durden (2019-01-31)
#     using injNum instate of qfRngTmp to determine missing data
#   Natchaya P-Durden (2019-04-09)
#     adding lvlMfcValiStor into input parameter
#   Natchaya P-Durden (2019-04-11)
#     adding RptExpd into input parameter
#   Natchaya P-Durden (2019-05-06)
#     assign lvlMfm in data flow
#   Natchaya P-Durden (2019-05-21)
#     pull in the qf from presInlt
##############################################################################################
wrap.dp01.qfqm.ecse <- function(
  dp01 = c("co2Stor", "h2oStor", "tempAirLvl", "tempAirTop", "isoCo2", "isoH2o")[1],
  RptExpd = FALSE,
  lvl,
  lvlMfcSampStor = NULL,
  lvlMfcValiStor = NULL,
  lvlEnvHut = NULL,
  lvlValv = NULL,
  lvlValvAux = NULL,
  lvlCrdH2oValvVali = NULL,
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
  #qfqm names
  NameQf <-c("qmAlph", "qmBeta", "qfFinl", "qfSciRevw")
#calculate qfqm for co2Stor and h2oStor ########################################################################################
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
      #assign lvlMfm
      lvlMfm <- paste0("700_", strsplit(lvl, "_")[[1]][2])
      
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
      wrk$qfqm$mfcSampStor <- qfInp$mfcSampStor[[lvlMfcSampStor]]
      wrk$qfqm$envHut <- qfInp$envHut[[lvlEnvHut]]
      wrk$qfqm$valvAux <- qfInp$valvAux[[lvlValvAux]]
      wrk$qfqm$mfm <- qfInp$mfm[[lvlMfm]]
      if ("presInlt" %in% names(qfInp)) wrk$qfqm$presInlt <- qfInp$presInlt[[lvl]]  
        
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
          #if last timeEnd is NA, replce that time to the last time value in data$time
          wrk$idx$timeEnd <- as.POSIXct(ifelse(is.na(wrk$idx$timeEnd), data$time[length(data$time)], wrk$idx$timeEnd), origin = "1970-01-01", tz = "UTC")
          #idxAgr2 <- 0
          for (idxAgr in 1:length(wrk$idx$idxBgn)){
            #idxAgr <- 4
            
            #wrk$inpMask for qfqm
            wrk$inpMask$qfqm <- list()
            #wrk$inpMask for data (will use to determine when irga got kick out)
            wrk$inpMask$data <- list()
            wrk$inpMask$data <- wrk$data[wrk$idx$idxBgn[idxAgr]:wrk$idx$idxEnd[idxAgr],] 
            #assign name to wrk$inpMask$qfqm
            lapply(names(wrk$qfqm), function (x) wrk$inpMask$qfqm[[x]] <<- wrk$qfqm[[x]][wrk$idx$idxBgn[idxAgr]:wrk$idx$idxEnd[idxAgr], ,drop=FALSE] )
            #replace qfqm$irgaStor with -1 when irga got kick out to measure the new measurement level
            for (idxSens in names(wrk$inpMask$qfqm)){
            for (tmp in 1:length(wrk$inpMask$qfqm[[idxSens]])){
              wrk$inpMask$qfqm[[idxSens]][[tmp]][wrk$inpMask$data$lvlIrga != lvlIrga] <- -1
            }
            
            #qfqm processing
            rpt[[idxAgr]] <- eddy4R.qaqc::wrap.dp01.qfqm.eddy(
              qfInp = wrk$inpMask$qfqm, 
              MethMeas = "ecse",
              TypeMeas = "samp",
              RptExpd = FALSE,
              dp01 = dp01
            )
            
            #grab and add both time begin and time end to rpt
            #for (idxLvLPrdAgr in names(wrk$inpMask$data[[dp01]])){
            #idxLvLPrdAgr <- names(wrk$inpMask$data[[dp01]])[1]
            rpt[[idxAgr]]$timeBgn <- list()
            rpt[[idxAgr]]$timeEnd <- list()
            
            for(idxVar in names(wrk$data)[which(!(names(wrk$data) %in% c("lvlIrga")))]){
              rpt[[idxAgr]]$timeBgn[[idxVar]] <- wrk$idx$timeBgn[idxAgr]
              rpt[[idxAgr]]$timeEnd[[idxVar]] <- wrk$idx$timeEnd[idxAgr]
            }
            
            #}# end of there is at least one data
            
          }; rm(idxAgr)
        } else {
          
          rpt[[1]] <- list()
          #idxStat <- NameQf[1]
          rpt[[1]]$qmAlph <- as.data.frame(matrix(0, nrow = 1, ncol = ncol(wrk$data)))
          rpt[[1]]$qmBeta <- as.data.frame(matrix(1, nrow = 1, ncol = ncol(wrk$data)))
          rpt[[1]]$qfFinl <- as.data.frame(matrix(1, nrow = 1, ncol = ncol(wrk$data)))
          rpt[[1]]$qfSciRevw <- as.data.frame(matrix(0, nrow = 1, ncol = ncol(wrk$data)))
          #change data type
          rpt[[1]]$qfFinl[,1:ncol(wrk$data)] <- sapply(rpt[[1]]$qfFinl[,1:ncol(wrk$data)], as.integer)
          rpt[[1]]$qfSciRevw[,1:ncol(wrk$data)] <- sapply(rpt[[1]]$qfSciRevw[,1:ncol(wrk$data)], as.integer)
          
          for(idxQf in NameQf){
            #assign name to each column
            names(rpt[[1]][[idxQf]]) <- names(wrk$data)
            #not report lvlIrga
            rpt[[1]][[idxQf]] <- rpt[[1]][[idxQf]][which(!(names(rpt[[1]][[idxQf]]) %in% c("lvlIrga")))]
            }; rm(idxQf)
          
          #add both time begin and time end to rpt
          rpt[[1]]$timeBgn <- list()
          rpt[[1]]$timeEnd <- list()
          
          #output time for dp01
          for(idxVar in names(wrk$data)[which(!(names(wrk$data) %in% c("lvlIrga")))]){
            rpt[[1]]$timeBgn[[idxVar]] <- data$time[1]
            rpt[[1]]$timeEnd[[idxVar]] <- data$time[length(data$time)]
            #unit
            attributes(rpt[[1]]$qmAlph[[idxVar]])$unit <- "-"
            attributes(rpt[[1]]$qmBeta[[idxVar]])$unit <- "-"
            attributes(rpt[[1]]$qfFinl[[idxVar]])$unit <- "NA"
            attributes(rpt[[1]]$qfSciRevw[[idxVar]])$unit <- "NA"
            
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
          #if last timeEnd is NA, replce that time to the last time value in data$time
          wrk$idx$timeEnd <- as.POSIXct(ifelse(is.na(wrk$idx$timeEnd), data$time[length(data$time)], wrk$idx$timeEnd), origin = "1970-01-01", tz = "UTC")
          
          whrSamp <- wrk$idx$idxBgn[1]:wrk$idx$idxEnd[1]
          if (length (wrk$idx$idxBgn) > 1 ){
            for(ii in 2:length (wrk$idx$idxBgn)){
              whrSamp <- c(whrSamp, wrk$idx$idxBgn[ii]:wrk$idx$idxEnd[ii])
            }
          }
          wrk$data[-whrSamp, 1:9] <- NaN
          #replace qfqm$irgaStor with -1 when irga got kick out to measure the new measurement level
          for (tmp in 1:length(wrk$qfqm$irgaStor)){
            wrk$qfqm$irgaStor[[tmp]][wrk$data$lvlIrga != lvlIrga] <- -1
          }
          for (tmp in 1:length(wrk$qfqm$mfcSampStor)){
            wrk$qfqm$mfcSampStor[[tmp]][wrk$data$lvlIrga != lvlIrga] <- -1
          }
          for (tmp in 1:length(wrk$qfqm$envHut)){
            wrk$qfqm$envHut[[tmp]][wrk$data$lvlIrga != lvlIrga] <- -1
          }
          for (tmp in 1:length(wrk$qfqm$valvAux)){
            wrk$qfqm$valvAux[[tmp]][wrk$data$lvlIrga != lvlIrga] <- -1
          }
          for (tmp in 1:length(wrk$qfqm$mfm)){
            wrk$qfqm$mfm[[tmp]][wrk$data$lvlIrga != lvlIrga] <- -1
          }
          #replace all qf that not belong to that measurement level by NaN
          wrk$qfqm$irgaStor[-whrSamp, 1:length(wrk$qfqm$irgaStor)] <- NaN
          wrk$qfqm$mfcSampStor[-whrSamp, 1:length(wrk$qfqm$mfcSampStor)] <- NaN
          wrk$qfqm$envHut[-whrSamp, 1:length(wrk$qfqm$envHut)] <- NaN
          wrk$qfqm$valvAux[-whrSamp, 1:length(wrk$qfqm$valvAux)] <- NaN
          wrk$qfqm$mfm[-whrSamp, 1:length(wrk$qfqm$mfm)] <- NaN
          
          #replace qf from mfcSampStor data with -1 when irga got kick out to measure the new measurement level
          #wrk$qfqm$mfcSampStor <- as.data.frame(sapply(wrk$qfqm$mfcSampStor, function(x) ifelse(wrk$data$lvlIrga == lvlIrga, x, -1)))
        } 
        
        
        for(idxAgr in c(1:length(idxTime[[paste0(PrdAgr, "min")]]$Bgn))) {
          #idxAgr <- 48
          
          
          ## grab data at the selected mask data
          # for qfqm
          #wrk$inpMask$data <- list()
          #wrk$inpMask$data <- wrk$data[idxTime[[paste0(PrdAgr, "min")]]$Bgn[idxAgr]:idxTime[[paste0(PrdAgr, "min")]]$End[idxAgr],]
          idxLvLPrdAgr <- paste0(lvl, "_", sprintf("%02d", PrdAgr), "m")
          
          wrk$inpMask$qfqm <- list()
          lapply(names(wrk$qfqm), function (x) wrk$inpMask$qfqm[[x]] <<- wrk$qfqm[[x]][idxTime[[paste0(PrdAgr, "min")]]$Bgn[idxAgr]:idxTime[[paste0(PrdAgr, "min")]]$End[idxAgr], , drop=FALSE])
          
          #qfqm processing
          rpt[[idxAgr]] <- eddy4R.qaqc::wrap.dp01.qfqm.eddy(
            qfInp = wrk$inpMask$qfqm, 
            MethMeas = "ecse",
            TypeMeas = "samp",
            RptExpd = RptExpd,
            dp01 = dp01
          )
          
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
      wrk$qfqm$mfcSampStor <- qfInp$mfcSampStor[[lvlMfcSampStor]]
      wrk$qfqm$envHut <- qfInp$envHut[[lvlEnvHut]]
      wrk$qfqm$valvAux <- qfInp$valvAux[[lvlValvAux]]
      wrk$qfqm$mfcValiStor <- qfInp$mfcValiStor[[lvlMfcValiStor]]
      
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
            #wrk$inpMask for qfqm
            wrk$inpMask$qfqm <- list()
            lapply(names(wrk$qfqm), function (x) wrk$inpMask$qfqm[[x]] <<- wrk$qfqm[[x]][wrk$idx$idxBgn[idxAgr]:wrk$idx$idxEnd[idxAgr], ,drop = FALSE] )
            
            #qfqm processing
            rpt[[idxAgr]] <- eddy4R.qaqc::wrap.dp01.qfqm.eddy(
              qfInp = wrk$inpMask$qfqm, 
              MethMeas = "ecse",
              TypeMeas = "vali",
              RptExpd = FALSE,
              dp01 = dp01
            )
            
            #grab and add both time begin and time end to rpt
            rpt[[idxAgr]]$timeBgn <- list()
            rpt[[idxAgr]]$timeEnd <- list()
            
            #output time for qf dp01; do not output reference gas
            for(idxVar in names(wrk$data)[which(!(names(wrk$data) %in% c("rtioMoleDryCo2Refe")))]){
              rpt[[idxAgr]]$timeBgn[[idxVar]] <- wrk$idx$timeBgn[idxAgr]
              rpt[[idxAgr]]$timeEnd[[idxVar]] <- wrk$idx$timeEnd[idxAgr]
            }; rm(idxVar)
          }#; rm(idxAgr)
          
        } else {
          
          rpt[[1]] <- list()
          #idxStat <- NameQf[1]
          if(lvl %in% c("co2Arch")){
          rpt[[1]]$qmAlph <- as.data.frame(matrix(NaN, nrow = 1, ncol = ncol(wrk$data)))
          rpt[[1]]$qmBeta <- as.data.frame(matrix(NaN, nrow = 1, ncol = ncol(wrk$data)))
          rpt[[1]]$qfFinl <- as.data.frame(matrix(NaN, nrow = 1, ncol = ncol(wrk$data)))
          rpt[[1]]$qfSciRevw <- as.data.frame(matrix(0, nrow = 1, ncol = ncol(wrk$data)))
          #change data type
          rpt[[1]]$qfSciRevw[,1:ncol(wrk$data)] <- sapply(rpt[[1]]$qfSciRevw[,1:ncol(wrk$data)], as.integer)
          }else{
            rpt[[1]]$qmAlph <- as.data.frame(matrix(0, nrow = 1, ncol = ncol(wrk$data)))
            rpt[[1]]$qmBeta <- as.data.frame(matrix(1, nrow = 1, ncol = ncol(wrk$data)))
            rpt[[1]]$qfFinl <- as.data.frame(matrix(1, nrow = 1, ncol = ncol(wrk$data)))
            rpt[[1]]$qfSciRevw <- as.data.frame(matrix(0, nrow = 1, ncol = ncol(wrk$data)))
            #change data type
            rpt[[1]]$qfFinl[,1:ncol(wrk$data)] <- sapply(rpt[[1]]$qfFinl[,1:ncol(wrk$data)], as.integer)
            rpt[[1]]$qfSciRevw[,1:ncol(wrk$data)] <- sapply(rpt[[1]]$qfSciRevw[,1:ncol(wrk$data)], as.integer)
          }
          
          for(idxQf in NameQf){
            #assign name to each column
            names(rpt[[1]][[idxQf]]) <- names(wrk$data)
            #not report lvlIrga
            rpt[[1]][[idxQf]] <- rpt[[1]][[idxQf]][which(!(names(rpt[[1]][[idxQf]]) %in% c("rtioMoleDryCo2Refe")))]
          }; rm(idxQf)
          
          #add both time begin and time end to rpt
          rpt[[1]]$timeBgn <- list()
          rpt[[1]]$timeEnd <- list()
          
          #output time for dp01
          for(idxVar in names(wrk$data)[which(!(names(wrk$data) %in% c("rtioMoleDryCo2Refe")))]){
            rpt[[1]]$timeBgn[[idxVar]] <- data$time[1]
            rpt[[1]]$timeEnd[[idxVar]] <- data$time[length(data$time)]
            #unit
            attributes(rpt[[1]]$qmAlph[[idxVar]])$unit <- "-"
            attributes(rpt[[1]]$qmBeta[[idxVar]])$unit <- "-"
            attributes(rpt[[1]]$qfFinl[[idxVar]])$unit <- "NA"
            attributes(rpt[[1]]$qfSciRevw[[idxVar]])$unit <- "NA"
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
          #wrk$data[-whrSamp, ] <- NaN
          wrk$qfqm$irgaStor[-whrSamp, 1:length(wrk$qfqm$irgaStor)] <- NaN
          wrk$qfqm$mfcSampStor[-whrSamp, 1:length(wrk$qfqm$mfcSampStor)] <- NaN
          wrk$qfqm$envHut[-whrSamp, 1:length(wrk$qfqm$envHut)] <- NaN
          wrk$qfqm$valvAux[-whrSamp, 1:length(wrk$qfqm$valvAux)] <- NaN
          wrk$qfqm$mfcValiStor[-whrSamp, 1:length(wrk$qfqm$mfcValiStor)] <- NaN
        } #else {#end of if no measurement data at all in the whole day
        #   wrk$data$frt00 <- NaN #assign NaN to frt00 data
        # }
        
        for(idxAgr in c(1:length(idxTime[[paste0(PrdAgr, "min")]]$Bgn))) {
          #idxAgr <- 34
          ## grab data at the selected mask data
          #for qfqm
          wrk$inpMask$qfqm <- list()
          idxLvLPrdAgr <- paste0(lvl, "_", sprintf("%02d", PrdAgr), "m")
          lapply(names(wrk$qfqm), function (x) wrk$inpMask$qfqm[[x]] <<- wrk$qfqm[[x]][idxTime[[paste0(PrdAgr, "min")]]$Bgn[idxAgr]:idxTime[[paste0(PrdAgr, "min")]]$End[idxAgr], , drop = FALSE])
          
          #qfqm processing
          rpt[[idxAgr]] <- eddy4R.qaqc::wrap.dp01.qfqm.eddy(
            qfInp = wrk$inpMask$qfqm, 
            MethMeas = "ecse",
            TypeMeas = "vali",
            RptExpd = RptExpd,
            dp01 = dp01
          )
          #grab and add both time begin and time end to rpt
          rpt[[idxAgr]]$timeBgn <- list()
          rpt[[idxAgr]]$timeEnd <- list()
          
          #output time for qf dp01; do not output reference gas
          for(idxVar in names(wrk$data)[which(!(names(wrk$data) %in% c("rtioMoleDryCo2Refe")))]){
            rpt[[idxAgr]]$timeBgn[[idxVar]] <- data$time[idxTime[[paste0(PrdAgr, "min")]]$Bgn[idxAgr]]
            rpt[[idxAgr]]$timeEnd[[idxVar]] <- data$time[idxTime[[paste0(PrdAgr, "min")]]$Bgn[idxAgr]]
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
      #wrk$qfqm <- list()
      wrk$inpMask$qfqm <- list()
      
      idxLvLPrdAgr <- paste0(lvl, "_", sprintf("%02d", PrdAgr), "m")
      
      
      if(dp01 == "tempAirLvl") {
        wrk$inpMask$data <- data.frame(stringsAsFactors = FALSE,
                                       "temp" = data$tempAirLvl[[lvl]][["temp"]][idxTime[[paste0(PrdAgr, "min")]]$Bgn[idxAgr]:idxTime[[paste0(PrdAgr, "min")]]$End[idxAgr]]
        )
        
        wrk$inpMask$qfqm[[dp01]] <- data.frame(stringsAsFactors = FALSE,
                                               qfInp[[dp01]][[lvl]][idxTime[[paste0(PrdAgr, "min")]]$Bgn[idxAgr]:idxTime[[paste0(PrdAgr, "min")]]$End[idxAgr],]
        )
      } 
      
      if(dp01 == "tempAirTop") {
        wrk$inpMask$data <- data.frame(stringsAsFactors = FALSE,
                                       "temp" = colMeans(rbind(data$tempAirTop[[lvl]][["temp01"]], data$tempAirTop[[lvl]][["temp02"]], data$tempAirTop[[lvl]][["temp03"]]), na.rm=TRUE)[idxTime[[paste0(PrdAgr, "min")]]$Bgn[idxAgr]:idxTime[[paste0(PrdAgr, "min")]]$End[idxAgr]]
        )
        
        wrk$inpMask$qfqm[[dp01]] <- data.frame(stringsAsFactors = FALSE,
                                               qfInp[[dp01]][[lvl]][idxTime[[paste0(PrdAgr, "min")]]$Bgn[idxAgr]:idxTime[[paste0(PrdAgr, "min")]]$End[idxAgr],]
        )
      } 
      
      #qfqm processing
      rpt[[idxAgr]] <- eddy4R.qaqc::wrap.dp01.qfqm.eddy(
        qfInp = wrk$inpMask$qfqm, 
        MethMeas = "ecse",
        TypeMeas = "samp",
        RptExpd = RptExpd,
        dp01 = dp01
      )
      
      #grab and add both time begin and time end to rpt
      rpt[[idxAgr]]$timeBgn <- list()
      rpt[[idxAgr]]$timeEnd <- list()
      
      for(idxVar in names(wrk$inpMask$data)){
        rpt[[idxAgr]]$timeBgn[[idxVar]] <- data$time[idxTime[[paste0(PrdAgr, "min")]]$Bgn[idxAgr]]
        rpt[[idxAgr]]$timeEnd[[idxVar]] <- data$time[idxTime[[paste0(PrdAgr, "min")]]$End[idxAgr]]
      }
      
    }; #rm(idxAgr)
    
  }#end of dp01 if statement 
  
#calculate dp01 for "isoCo2" ########################################################################################
  if (dp01 %in% c("isoCo2")){
    #during sampling period 
    if (TypeMeas %in% "samp"){
      #assign lvlIrga for each measurement level
      if (lvl == "000_010") {lvlCrdCo2 <- "lvl01"}
      if (lvl == "000_020") {lvlCrdCo2 <- "lvl02"}
      if (lvl == "000_030") {lvlCrdCo2 <- "lvl03"}
      if (lvl == "000_040") {lvlCrdCo2 <- "lvl04"}
      if (lvl == "000_050") {lvlCrdCo2 <- "lvl05"}
      if (lvl == "000_060") {lvlCrdCo2 <- "lvl06"}
      if (lvl == "000_070") {lvlCrdCo2 <- "lvl07"}
      if (lvl == "000_080") {lvlCrdCo2 <- "lvl08"}
      #assign lvlMfm
      lvlMfm <- paste0("700_", strsplit(lvl, "_")[[1]][2])
      
      #input the whole day data
      wrk$data <- data.frame(stringsAsFactors = FALSE,
                             "dlta13CCo2" = data$crdCo2[[lvl]]$dlta13CCo2,
                             "idGas" = data$crdCo2[[lvl]]$idGas,
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
      wrk$qfqm$envHut <- qfInp$envHut[[lvlEnvHut]]
      wrk$qfqm$mfm <- qfInp$mfm[[lvlMfm]]
      
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
            #wrk$inpMask for qfqm
            wrk$inpMask$qfqm <- list()
            lapply(names(wrk$qfqm), function (x) wrk$inpMask$qfqm[[x]] <<- wrk$qfqm[[x]][wrk$idx$idxBgn[idxAgr]:wrk$idx$idxEnd[idxAgr],] )
            #replace qfqm$crdCo2 with -1 when valve switch to measure to next level before schedule time (9 min)
            for (tmp in 1:length(wrk$inpMask$qfqm$crdCo2)){
              wrk$inpMask$qfqm$crdCo2[[tmp]][wrk$inpMask$data$lvlCrdCo2 != lvlCrdCo2] <- -1
            }
            for (tmp in 1:length(wrk$inpMask$qfqm$envHut)){
              wrk$inpMask$qfqm$envHut[[tmp]][wrk$inpMask$data$lvlCrdCo2 != lvlCrdCo2] <- -1
            }
            for (tmp in 1:length(wrk$inpMask$qfqm$mfm)){
              wrk$inpMask$qfqm$mfm[[tmp]][wrk$inpMask$data$lvlCrdCo2 != lvlCrdCo2] <- -1
            }
            
            #qfqm processing
            rpt[[idxAgr]] <- eddy4R.qaqc::wrap.dp01.qfqm.eddy(
              qfInp = wrk$inpMask$qfqm, 
              MethMeas = "ecse",
              TypeMeas = "samp",
              RptExpd = FALSE,
              dp01 = dp01,
              idGas = wrk$inpMask$data$idGas
            )
            #grab and add both time begin and time end to rpt
            rpt[[idxAgr]]$timeBgn <- list()
            rpt[[idxAgr]]$timeEnd <- list()
            
            #output time for dp01
            for(idxVar in names(wrk$data)[which(!(names(wrk$data) %in% c("idGas", "lvlCrdCo2")))]){
              rpt[[idxAgr]]$timeBgn[[idxVar]] <- wrk$idx$timeBgn[idxAgr]
              rpt[[idxAgr]]$timeEnd[[idxVar]] <- wrk$idx$timeEnd[idxAgr]
            }; rm(idxVar)
            
            #}# end of there is at least one data
            
          }; rm(idxAgr)
        } else {
          
          rpt[[1]] <- list()
          #idxStat <- NameQf[1]
          rpt[[1]]$qmAlph <- as.data.frame(matrix(0, nrow = 1, ncol = ncol(wrk$data)))
          rpt[[1]]$qmBeta <- as.data.frame(matrix(1, nrow = 1, ncol = ncol(wrk$data)))
          rpt[[1]]$qfFinl <- as.data.frame(matrix(1, nrow = 1, ncol = ncol(wrk$data)))
          rpt[[1]]$qfSciRevw <- as.data.frame(matrix(0, nrow = 1, ncol = ncol(wrk$data)))
          #change data type
          rpt[[1]]$qfFinl[,1:ncol(wrk$data)] <- sapply(rpt[[1]]$qfFinl[,1:ncol(wrk$data)], as.integer)
          rpt[[1]]$qfSciRevw[,1:ncol(wrk$data)] <- sapply(rpt[[1]]$qfSciRevw[,1:ncol(wrk$data)], as.integer)
          
          for(idxQf in NameQf){
            #assign name to each column
            names(rpt[[1]][[idxQf]]) <- names(wrk$data)
            #not report lvlIrga
            rpt[[1]][[idxQf]] <- rpt[[1]][[idxQf]][which(!(names(rpt[[1]][[idxQf]]) %in% c("idGas", "lvlCrdCo2")))]
          }; rm(idxQf)
          
          #add both time begin and time end to rpt
          rpt[[1]]$timeBgn <- list()
          rpt[[1]]$timeEnd <- list()
          
          #output time for dp01
          for(idxVar in names(wrk$data)[which(!(names(wrk$data) %in% c("idGas", "lvlCrdCo2")))]){
            rpt[[1]]$timeBgn[[idxVar]] <- data$time[1]
            rpt[[1]]$timeEnd[[idxVar]] <- data$time[length(data$time)]
            #unit
            attributes(rpt[[1]]$qmAlph[[idxVar]])$unit <- "-"
            attributes(rpt[[1]]$qmBeta[[idxVar]])$unit <- "-"
            attributes(rpt[[1]]$qfFinl[[idxVar]])$unit <- "NA"
            attributes(rpt[[1]]$qfSciRevw[[idxVar]])$unit <- "NA"
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
          wrk$data[-whrSamp, 1:16] <- NaN
          #replace qfqm$crdCo2 with -1 when valve switch to measure to next level before schedule time (9 min)
          for (tmp in 1:length(wrk$qfqm$crdCo2)){
            wrk$qfqm$crdCo2[[tmp]][wrk$data$lvlCrdCo2 != lvlCrdCo2] <- -1
          }
          for (tmp in 1:length(wrk$qfqm$envHut)){
            wrk$qfqm$envHut[[tmp]][wrk$data$lvlCrdCo2 != lvlCrdCo2] <- -1
          }
          for (tmp in 1:length(wrk$qfqm$mfm)){
            wrk$qfqm$mfm[[tmp]][wrk$data$lvlCrdCo2 != lvlCrdCo2] <- -1
          }
          #replace all qf that not belong to that measurement level by NaN
          wrk$qfqm$crdCo2[-whrSamp, 1:length(wrk$qfqm$crdCo2)] <- NaN
          wrk$qfqm$envHut[-whrSamp, 1:length(wrk$qfqm$envHut)] <- NaN
          wrk$qfqm$mfm[-whrSamp, 1:length(wrk$qfqm$mfm)] <- NaN
        } 
        
        
        for(idxAgr in c(1:length(idxTime[[paste0(PrdAgr, "min")]]$Bgn))) {
          #idxAgr <- 48
          #get data for each idxAgr
          wrk$inpMask$data <- list()
          idxLvLPrdAgr <- paste0(lvl, "_", sprintf("%02d", PrdAgr), "m")
          
          wrk$inpMask$data <- wrk$data[idxTime[[paste0(PrdAgr, "min")]]$Bgn[idxAgr]:idxTime[[paste0(PrdAgr, "min")]]$End[idxAgr],]
          # for qfqm
          wrk$inpMask$qfqm <- list()
          lapply(names(wrk$qfqm), function (x) wrk$inpMask$qfqm[[x]] <<- wrk$qfqm[[x]][idxTime[[paste0(PrdAgr, "min")]]$Bgn[idxAgr]:idxTime[[paste0(PrdAgr, "min")]]$End[idxAgr],])
          
          #qfqm processing
          rpt[[idxAgr]] <- eddy4R.qaqc::wrap.dp01.qfqm.eddy(
            qfInp = wrk$inpMask$qfqm, 
            MethMeas = "ecse",
            TypeMeas = "samp",
            RptExpd = RptExpd,
            dp01 = dp01,
            idGas = wrk$inpMask$data$idGas
          )
          
          #grab and add both time begin and time end to rpt
          rpt[[idxAgr]]$timeBgn <- list()
          rpt[[idxAgr]]$timeEnd <- list()
          
          for(idxVar in names(wrk$data)[which(!(names(wrk$data) %in% c("idGas", "lvlCrdCo2")))]){
            rpt[[idxAgr]]$timeBgn[[idxVar]] <- data$time[idxTime[[paste0(PrdAgr, "min")]]$Bgn[idxAgr]]
            rpt[[idxAgr]]$timeEnd[[idxVar]] <- data$time[idxTime[[paste0(PrdAgr, "min")]]$End[idxAgr]]
          }
          
        }; #rm(idxAgr)
        
      }#end of PrdAgr == 30
    }#end of TypeMeas %in% "samp"
    
    #during validation period 
    if (TypeMeas %in% "vali"){
      #input the whole day data
      wrk$data <- data.frame(stringsAsFactors = FALSE,
                             "dlta13CCo2" = data$crdCo2[[lvl]]$dlta13CCo2,
                             "dlta13CCo2Refe" = data$crdCo2[[lvl]]$dlta13CCo2Refe,
                             "idGas" = data$crdCo2[[lvl]]$idGas,
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
      wrk$qfqm$envHut <- qfInp$envHut[[lvlEnvHut]]
      wrk$qfqm$mfcValiStor <- qfInp$mfcValiStor[[lvlMfcValiStor]]
      
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
            #wrk$inpMask for qfqm
            wrk$inpMask$qfqm <- list()
            lapply(names(wrk$qfqm), function (x) wrk$inpMask$qfqm[[x]] <<- wrk$qfqm[[x]][wrk$idx$idxBgn[idxAgr]:wrk$idx$idxEnd[idxAgr],] )
            
            #qfqm processing
            rpt[[idxAgr]] <- eddy4R.qaqc::wrap.dp01.qfqm.eddy(
              qfInp = wrk$inpMask$qfqm, 
              MethMeas = "ecse",
              TypeMeas = "vali",
              RptExpd = FALSE,
              dp01 = dp01,
              idGas = wrk$inpMask$data$idGas
            )
            
            #grab and add both time begin and time end to rpt
            rpt[[idxAgr]]$timeBgn <- list()
            rpt[[idxAgr]]$timeEnd <- list()
            
            #output time for qf dp01; do not output reference gas
            for(idxVar in names(wrk$data)[which(!(names(wrk$data) %in% c("rtioMoleDryCo2Refe", "dlta13CCo2Refe", "idGas")))]){
              rpt[[idxAgr]]$timeBgn[[idxVar]] <- wrk$idx$timeBgn[idxAgr]
              rpt[[idxAgr]]$timeEnd[[idxVar]] <- wrk$idx$timeEnd[idxAgr]
            }; rm(idxVar)
            
          }#; rm(idxAgr)
          
        } else {
          
          rpt[[1]] <- list()
          
          if(lvl %in% c("co2Arch")){
            rpt[[1]]$qmAlph <- as.data.frame(matrix(NaN, nrow = 1, ncol = ncol(wrk$data)))
            rpt[[1]]$qmBeta <- as.data.frame(matrix(NaN, nrow = 1, ncol = ncol(wrk$data)))
            rpt[[1]]$qfFinl <- as.data.frame(matrix(NaN, nrow = 1, ncol = ncol(wrk$data)))
            rpt[[1]]$qfSciRevw <- as.data.frame(matrix(0, nrow = 1, ncol = ncol(wrk$data)))
            #change data type
            rpt[[1]]$qfSciRevw[,1:ncol(wrk$data)] <- sapply(rpt[[1]]$qfSciRevw[,1:ncol(wrk$data)], as.integer)
          }else{
            rpt[[1]]$qmAlph <- as.data.frame(matrix(0, nrow = 1, ncol = ncol(wrk$data)))
            rpt[[1]]$qmBeta <- as.data.frame(matrix(1, nrow = 1, ncol = ncol(wrk$data)))
            rpt[[1]]$qfFinl <- as.data.frame(matrix(1, nrow = 1, ncol = ncol(wrk$data)))
            rpt[[1]]$qfSciRevw <- as.data.frame(matrix(0, nrow = 1, ncol = ncol(wrk$data)))
            #change data type
            rpt[[1]]$qfFinl[,1:ncol(wrk$data)] <- sapply(rpt[[1]]$qfFinl[,1:ncol(wrk$data)], as.integer)
            rpt[[1]]$qfSciRevw[,1:ncol(wrk$data)] <- sapply(rpt[[1]]$qfSciRevw[,1:ncol(wrk$data)], as.integer)
          }
          
          for(idxQf in NameQf){
            #assign name to each column
            names(rpt[[1]][[idxQf]]) <- names(wrk$data)
            #not report lvlIrga
            rpt[[1]][[idxQf]] <- rpt[[1]][[idxQf]][which(!(names(rpt[[1]][[idxQf]]) %in% c("rtioMoleDryCo2Refe", "dlta13CCo2Refe", "idGas")))]
          }; rm(idxQf)
          
          #add both time begin and time end to rpt
          rpt[[1]]$timeBgn <- list()
          rpt[[1]]$timeEnd <- list()
          
          #output time for dp01
          for(idxVar in names(wrk$data)[which(!(names(wrk$data) %in% c("rtioMoleDryCo2Refe", "dlta13CCo2Refe", "idGas")))]){
            rpt[[1]]$timeBgn[[idxVar]] <- data$time[1]
            rpt[[1]]$timeEnd[[idxVar]] <- data$time[length(data$time)]
            #unit
            attributes(rpt[[1]]$qmAlph[[idxVar]])$unit <- "-"
            attributes(rpt[[1]]$qmBeta[[idxVar]])$unit <- "-"
            attributes(rpt[[1]]$qfFinl[[idxVar]])$unit <- "NA"
            attributes(rpt[[1]]$qfSciRevw[[idxVar]])$unit <- "NA"
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
          wrk$qfqm$crdCo2[-whrSamp, 1:length(wrk$qfqm$crdCo2)] <- NaN
          wrk$qfqm$envHut[-whrSamp, 1:length(wrk$qfqm$envHut)] <- NaN
          wrk$qfqm$mfcValiStor[-whrSamp, 1:length(wrk$qfqm$mfcValiStor)] <- NaN
        } 
        
        for(idxAgr in c(1:length(idxTime[[paste0(PrdAgr, "min")]]$Bgn))) {
          #idxAgr <- 1
          
          ## grab data at the selected mask data
          # for data
          wrk$inpMask$data <- list()
          idxLvLPrdAgr <- paste0(lvl, "_", sprintf("%02d", PrdAgr), "m")      
          wrk$inpMask$data <- wrk$data[idxTime[[paste0(PrdAgr, "min")]]$Bgn[idxAgr]:idxTime[[paste0(PrdAgr, "min")]]$End[idxAgr],]
          
          # for qfqm
          wrk$inpMask$qfqm <- list()
          lapply(names(wrk$qfqm), function (x) wrk$inpMask$qfqm[[x]] <<- wrk$qfqm[[x]][idxTime[[paste0(PrdAgr, "min")]]$Bgn[idxAgr]:idxTime[[paste0(PrdAgr, "min")]]$End[idxAgr],])
          
          #qfqm processing
          rpt[[idxAgr]] <- eddy4R.qaqc::wrap.dp01.qfqm.eddy(
            qfInp = wrk$inpMask$qfqm, 
            MethMeas = "ecse",
            TypeMeas = "vali",
            RptExpd = RptExpd,
            dp01 = dp01,
            idGas = wrk$inpMask$data$idGas
          )
          
          #grab and add both time begin and time end to rpt
          rpt[[idxAgr]]$timeBgn <- list()
          rpt[[idxAgr]]$timeEnd <- list()
          
          #output time for qf dp01; do not output reference gas
          for(idxVar in names(wrk$data)[which(!(names(wrk$data) %in% c("rtioMoleDryCo2Refe", "dlta13CCo2Refe", "idGas")))]){
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
      #assign lvlMfm
      lvlMfm <- paste0("700_", strsplit(lvl, "_")[[1]][2])
      
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
      wrk$qfqm$envHut <- qfInp$envHut[[lvlEnvHut]]
      wrk$qfqm$mfm <- qfInp$mfm[[lvlMfm]]
      
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
            #wrk$inpMask for qfqm
            wrk$inpMask$qfqm <- list()
            lapply(names(wrk$qfqm), function (x) wrk$inpMask$qfqm[[x]] <<- wrk$qfqm[[x]][wrk$idx$idxBgn[idxAgr]:wrk$idx$idxEnd[idxAgr],] )
            #replace qfqm$crdH2o with -1 when valve switch to measure to next level before schedule time (9 min)
            for (tmp in 1:length(wrk$inpMask$qfqm$crdH2o)){
              wrk$inpMask$qfqm$crdH2o[[tmp]][wrk$inpMask$data$lvlCrdH2o != lvlCrdH2o] <- -1
            }
            for (tmp in 1:length(wrk$inpMask$qfqm$envHut)){
              wrk$inpMask$qfqm$envHut[[tmp]][wrk$inpMask$data$lvlCrdH2o != lvlCrdH2o] <- -1
            }
            for (tmp in 1:length(wrk$inpMask$qfqm$mfm)){
              wrk$inpMask$qfqm$mfm[[tmp]][wrk$inpMask$data$lvlCrdH2o != lvlCrdH2o] <- -1
            }
            
            #qfqm processing
            rpt[[idxAgr]] <- eddy4R.qaqc::wrap.dp01.qfqm.eddy(
              qfInp = wrk$inpMask$qfqm, 
              MethMeas = "ecse",
              TypeMeas = "samp",
              RptExpd = FALSE,
              dp01 = dp01
            )
            
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
          #idxStat <- NameQf[1]
          rpt[[1]]$qmAlph <- as.data.frame(matrix(0, nrow = 1, ncol = ncol(wrk$data)))
          rpt[[1]]$qmBeta <- as.data.frame(matrix(1, nrow = 1, ncol = ncol(wrk$data)))
          rpt[[1]]$qfFinl <- as.data.frame(matrix(1, nrow = 1, ncol = ncol(wrk$data)))
          rpt[[1]]$qfSciRevw <- as.data.frame(matrix(0, nrow = 1, ncol = ncol(wrk$data)))
          #change data type
          rpt[[1]]$qfFinl[,1:ncol(wrk$data)] <- sapply(rpt[[1]]$qfFinl[,1:ncol(wrk$data)], as.integer)
          rpt[[1]]$qfSciRevw[,1:ncol(wrk$data)] <- sapply(rpt[[1]]$qfSciRevw[,1:ncol(wrk$data)], as.integer)
          
          for(idxQf in NameQf){
            #assign name to each column
            names(rpt[[1]][[idxQf]]) <- names(wrk$data)
            #not report lvlIrga
            rpt[[1]][[idxQf]] <- rpt[[1]][[idxQf]][which(!(names(rpt[[1]][[idxQf]]) %in% c("lvlCrdH2o")))]
          }; rm(idxQf)
          
          #add both time begin and time end to rpt
          rpt[[1]]$timeBgn <- list()
          rpt[[1]]$timeEnd <- list()
          
          #output time for dp01
          for(idxVar in names(wrk$data)[which(!(names(wrk$data) %in% c("lvlCrdH2o")))]){
            rpt[[1]]$timeBgn[[idxVar]] <- data$time[1]
            rpt[[1]]$timeEnd[[idxVar]] <- data$time[length(data$time)]
            #unit
            attributes(rpt[[1]]$qmAlph[[idxVar]])$unit <- "-"
            attributes(rpt[[1]]$qmBeta[[idxVar]])$unit <- "-"
            attributes(rpt[[1]]$qfFinl[[idxVar]])$unit <- "NA"
            attributes(rpt[[1]]$qfSciRevw[[idxVar]])$unit <- "NA"
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
          #replace qfqm$crdH2o with -1 when valve switch to measure to next level before schedule time (9 min)
          for (tmp in 1:length(wrk$qfqm$crdH2o)){
            wrk$qfqm$crdH2o[[tmp]][wrk$data$lvlCrdH2o != lvlCrdH2o] <- -1
          }
          for (tmp in 1:length(wrk$qfqm$envHut)){
            wrk$qfqm$envHut[[tmp]][wrk$data$lvlCrdH2o != lvlCrdH2o] <- -1
          }
          for (tmp in 1:length(wrk$qfqm$mfm)){
            wrk$qfqm$mfm[[tmp]][wrk$data$lvlCrdH2o != lvlCrdH2o] <- -1
          }
          wrk$qfqm$crdH2o[-whrSamp, 1:length(wrk$qfqm$crdH2o)] <- NaN
          wrk$qfqm$envHut[-whrSamp, 1:length(wrk$qfqm$envHut)] <- NaN
          wrk$qfqm$mfm[-whrSamp, 1:length(wrk$qfqm$mfm)] <- NaN
        } 
        
        
        for(idxAgr in c(1:length(idxTime[[paste0(PrdAgr, "min")]]$Bgn))) {
          #idxAgr <- 48
          
          
          ## grab data at the selected mask data
          
          idxLvLPrdAgr <- paste0(lvl, "_", sprintf("%02d", PrdAgr), "m")
          
          # for qfqm
          wrk$inpMask$qfqm <- list()
          lapply(names(wrk$qfqm), function (x) wrk$inpMask$qfqm[[x]] <<- wrk$qfqm[[x]][idxTime[[paste0(PrdAgr, "min")]]$Bgn[idxAgr]:idxTime[[paste0(PrdAgr, "min")]]$End[idxAgr],])
          
          #qfqm processing
          rpt[[idxAgr]] <- eddy4R.qaqc::wrap.dp01.qfqm.eddy(
            qfInp = wrk$inpMask$qfqm, 
            MethMeas = "ecse",
            TypeMeas = "samp",
            RptExpd = RptExpd,
            dp01 = dp01
          )
          
          #grab and add both time begin and time end to rpt
          rpt[[idxAgr]]$timeBgn <- list()
          rpt[[idxAgr]]$timeEnd <- list()
          
          for(idxVar in names(wrk$data)[which(!(names(wrk$data) %in% c("lvlCrdH2o")))]){
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
      wrk$qfqm$envHut <- qfInp$envHut[[lvlEnvHut]]
      
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
      
      #calculated the qfValiH2o: injNum 1, 2, 3, 7, 8, 9, 13, 14, and 15 set to 1
      #threshold to determine qfValiH2o (default to reference water +/- 30% of reference water)
      Thsh <- 0.3
      wrk$qfqm$crdH2o$qfValiH2o <- ifelse(is.na(wrk$data$injNum) | is.na(wrk$data$dlta18OH2o) | is.na(wrk$data$dlta2HH2o) | is.na(wrk$data$dlta18OH2oRefe) | is.na(wrk$data$dlta2HH2oRefe), -1,
                                          ifelse((wrk$data$injNum %in% c(1, 2, 3, 7, 8, 9, 13, 14, 15)) | 
                                                   (wrk$data$injNum %in% c(4, 5, 6, 10, 11, 12, 16, 17, 18) & (wrk$data$dlta18OH2o < (wrk$data$dlta18OH2oRefe + Thsh*wrk$data$dlta18OH2oRefe) | wrk$data$dlta18OH2o > (wrk$data$dlta18OH2oRefe - Thsh*wrk$data$dlta18OH2oRefe))) |
                                                   (wrk$data$injNum %in% c(4, 5, 6, 10, 11, 12, 16, 17, 18) & (wrk$data$dlta2HH2o < (wrk$data$dlta2HH2oRefe + Thsh*wrk$data$dlta2HH2oRefe) | wrk$data$dlta2HH2o > (wrk$data$dlta2HH2oRefe - Thsh*wrk$data$dlta2HH2oRefe))), 1, 0))
      
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
            #wrk$inpMask for qfqm
            wrk$inpMask$qfqm <- list()
            lapply(names(wrk$qfqm), function (x) wrk$inpMask$qfqm[[x]] <<- wrk$qfqm[[x]][wrk$idx$idxBgn[idxAgr]:wrk$idx$idxEnd[idxAgr],] )
            
            #qfqm processing
            rpt[[idxAgr]] <- eddy4R.qaqc::wrap.dp01.qfqm.eddy(
              qfInp = wrk$inpMask$qfqm, 
              MethMeas = "ecse",
              TypeMeas = "vali",
              RptExpd = FALSE,
              dp01 = dp01
            )
            
            #grab and add both time begin and time end to rpt
            
            #for (idxLvLPrdAgr in names(wrk$inpMask$data[[dp01]])){
            #idxLvLPrdAgr <- names(wrk$inpMask$data[[dp01]])[1]
            rpt[[idxAgr]]$timeBgn <- list()
            rpt[[idxAgr]]$timeEnd <- list()
            
            for(idxVar in names(wrk$data)[which(!(names(wrk$data) %in% c("dlta18OH2oRefe", "dlta2HH2oRefe", "injNum")))]){
              # rpt[[idxAgr2]]$timeBgn[[idxVar]] <- data$time[(whrEnd[idxAgr] - 20 - 2*60+1)]
              # rpt[[idxAgr2]]$timeEnd[[idxVar]] <- data$time[(whrEnd[idxAgr] - 20)]
              rpt[[idxAgr]]$timeBgn[[idxVar]] <- wrk$idx$timeBgn[idxAgr]
              rpt[[idxAgr]]$timeEnd[[idxVar]] <- wrk$idx$timeEnd[idxAgr]
            }
            
          }#; rm(idxAgr)
          
        } else {
          
          rpt[[1]] <- list()
          #idxStat <- NameQf[1]
          rpt[[1]]$qmAlph <- as.data.frame(matrix(0, nrow = 1, ncol = ncol(wrk$data)))
          rpt[[1]]$qmBeta <- as.data.frame(matrix(1, nrow = 1, ncol = ncol(wrk$data)))
          rpt[[1]]$qfFinl <- as.data.frame(matrix(1, nrow = 1, ncol = ncol(wrk$data)))
          rpt[[1]]$qfSciRevw <- as.data.frame(matrix(0, nrow = 1, ncol = ncol(wrk$data)))
          #change data type
          rpt[[1]]$qfFinl[,1:ncol(wrk$data)] <- sapply(rpt[[1]]$qfFinl[,1:ncol(wrk$data)], as.integer)
          rpt[[1]]$qfSciRevw[,1:ncol(wrk$data)] <- sapply(rpt[[1]]$qfSciRevw[,1:ncol(wrk$data)], as.integer)
          
          for(idxQf in NameQf){
            #assign name to each column
            names(rpt[[1]][[idxQf]]) <- names(wrk$data)
            #not report lvlIrga
            rpt[[1]][[idxQf]] <- rpt[[1]][[idxQf]][which(!(names(rpt[[1]][[idxQf]]) %in% c("dlta18OH2oRefe", "dlta2HH2oRefe", "injNum")))]
          }; rm(idxQf)
          
          #add both time begin and time end to rpt
          rpt[[1]]$timeBgn <- list()
          rpt[[1]]$timeEnd <- list()
          
          #output time for dp01
          for(idxVar in names(wrk$data)[which(!(names(wrk$data) %in% c("dlta18OH2oRefe", "dlta2HH2oRefe", "injNum")))]){
            rpt[[1]]$timeBgn[[idxVar]] <- data$time[1]
            rpt[[1]]$timeEnd[[idxVar]] <- data$time[length(data$time)]
            #unit
            attributes(rpt[[1]]$qmAlph[[idxVar]])$unit <- "-"
            attributes(rpt[[1]]$qmBeta[[idxVar]])$unit <- "-"
            attributes(rpt[[1]]$qfFinl[[idxVar]])$unit <- "NA"
            attributes(rpt[[1]]$qfSciRevw[[idxVar]])$unit <- "NA"
          }; rm(idxVar)
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
          wrk$qfqm$crdH2o[-whrSamp, 1:length(wrk$qfqm$crdH2o)] <- NaN
          wrk$qfqm$envHut[-whrSamp, 1:length(wrk$qfqm$envHut)] <- NaN
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
          
          # for qfqm
          wrk$inpMask$qfqm <- list()
          lapply(names(wrk$qfqm), function (x) wrk$inpMask$qfqm[[x]] <<- wrk$qfqm[[x]][idxTime[[paste0(PrdAgr, "min")]]$Bgn[idxAgr]:idxTime[[paste0(PrdAgr, "min")]]$End[idxAgr],])
          
          
          #qfqm processing
          rpt[[idxAgr]] <- eddy4R.qaqc::wrap.dp01.qfqm.eddy(
            qfInp = wrk$inpMask$qfqm, 
            MethMeas = "ecse",
            TypeMeas = "vali",
            RptExpd = RptExpd,
            dp01 = dp01
          )
          
          
          #grab and add both time begin and time end to rpt
          rpt[[idxAgr]]$timeBgn <- list()
          rpt[[idxAgr]]$timeEnd <- list()
          
          #output time for qf dp01; do not output reference gas
          for(idxVar in names(wrk$data)[which(!(names(wrk$data) %in% c("dlta18OH2oRefe", "dlta2HH2oRefe", "injNum")))]){
            rpt[[idxAgr]]$timeBgn[[idxVar]] <- data$time[idxTime[[paste0(PrdAgr, "min")]]$Bgn[idxAgr]]
            rpt[[idxAgr]]$timeEnd[[idxVar]] <- data$time[idxTime[[paste0(PrdAgr, "min")]]$End[idxAgr]]
          }; rm(idxVar)
          
        }; #rm(idxAgr)
      } #end of PrdAgr == 30
      
    }#end of TypeMeas %in% "vali" if statement
    
    
  }#end of dp01 if statement
  
  #return results
  return(rpt)
  
}#end function wrap.dp01.qfqm.ecse()
