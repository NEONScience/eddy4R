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
#' @param lvlCrdCo2Valv Horizontal and vertical location for crdCo2 valve. Defaults to NULL. Of type character. [-]
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
#   Natchaya P-Durden (2019-05-23)
#     pull in the qf from pumpStor, presValiRegInStor and presValiRegOutStor
#   Natchaya P-Durden (2020-03-12)
#     In irgaCo2 an irgaH2o, not include the period when crdCo2 take over to measure at that level 
#     and irga have to move to measure next level
#   Natchaya P-Durden (2020-03-25)
#     added lvlCrdCo2Valv to the function's parameter
#   David Durden (2020-05-15)
#     failsafe for crd kickoff removal causing no data for entire day
#   David Durden (2020-05-21)
#     bug fix for qmBeta causing differing number of values between data and qfqm
#   David Durden (2020-07-23)
#     bug fix for valve issues where looks like consistently Stor data thrown off by Crd
#   Chris Florian (2021-02-26)
#     adding ch4Conc to dp01 list
#   Chris Florian (2021-06-03)
#     adjusting data input for crdCh4 validation time periods
#   Chris Florian (2021-06-07)
#     updating list to remove the CH4 reference values from qfqm
#   Natchaya Pingintha-Durden (2024-11-26)
#     bug fix for report NaN values when the validation occurred between 2 days and the start and end time cannot be determined
##############################################################################################
wrap.dp01.qfqm.ecse <- function(
  dp01 = c("co2Stor", "h2oStor", "tempAirLvl", "tempAirTop", "isoCo2", "isoH2o", "ch4Conc")[1],
  RptExpd = FALSE,
  lvl,
  lvlMfcSampStor = NULL,
  lvlMfcValiStor = NULL,
  lvlEnvHut = NULL,
  lvlValv = NULL,
  lvlValvAux = NULL,
  lvlCrdH2oValvVali = NULL,
  lvlCrdCo2Valv = NULL,
  data = list(),
  qfInp = list(),
  TypeMeas = c("samp", "vali")[1],
  PrdMeas,
  PrdAgr,
  idxTime = list() 
) {
  library(eddy4R.base)
  rlog = Logger.Singleton$new() #class defined in eddy4R.base
  rlog$debug("in function wrap.dp01.qfqm.ecse(...)")
  
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
      #measurement level pump
      if ("pumpStor" %in% names(qfInp)) wrk$qfqm$pumpStor <- qfInp$pumpStor[[lvlMfm]] 
      #irga pump
      if ("pumpStor" %in% names(qfInp)) wrk$qfqm$pumpIrgaStor <- qfInp$pumpStor[["700_000"]] 
      
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
            #idxAgr <- 4
            
            #wrk$inpMask for qfqm
            wrk$inpMask$qfqm <- list()
            #wrk$inpMask for data (will use to determine when irga got kick out)
            wrk$inpMask$data <- list()
            wrk$inpMask$data <- wrk$data[wrk$idx$idxBgn[idxAgr]:wrk$idx$idxEnd[idxAgr],] 
            #assign name to wrk$inpMask$qfqm
            lapply(names(wrk$qfqm), function (x) wrk$inpMask$qfqm[[x]] <<- wrk$qfqm[[x]][wrk$idx$idxBgn[idxAgr]:wrk$idx$idxEnd[idxAgr], ,drop=FALSE] )
            for (idxSens in names(wrk$inpMask$qfqm)){
              #replace qfqm with -1 when irga got kick out to measure the new measurement level
              wrk$inpMask$qfqm[[idxSens]][wrk$inpMask$data$lvlIrga != lvlIrga, 1:length(wrk$inpMask$qfqm[[idxSens]])] <- -1
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
            
            #get rid of period  that crdCo2 take over and irga have to move to measure other level
            #and number of sample less than 10% (120-120*0.1)
            if (dp01 == "co2Stor") {numSamp <- sum(!is.na(wrk$data$rtioMoleDryCo2[wrk$idx$idxBgn[idxAgr]:wrk$idx$idxEnd[idxAgr]]))}
            if (dp01 == "h2oStor") {numSamp <- sum(!is.na(wrk$data$rtioMoleDryH2o[wrk$idx$idxBgn[idxAgr]:wrk$idx$idxEnd[idxAgr]]))}
            if (data$crdCo2ValvLvl[[lvlCrdCo2Valv]]$lvlCrdCo2[wrk$idx$idxEnd[idxAgr]] == lvlIrga & numSamp < 108){
              rpt[[idxAgr]] <- NULL
            }
            
            #Remove any empty lists in case valve issues
            if(length(wrk$idx$idxBgn) == idxAgr) rpt <- Filter(Negate(is.null), rpt)
            
            #Check if after removing data for valve kickooff if no data remains
            if(length(wrk$idx$idxBgn) == idxAgr && length(rpt) == 0){
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
            
          }#end of failsafe to check if no measurement data at all in the whole day after valve kickoff removal
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
          #replace last idxEnd > 86400 by 86400
          wrk$idx$idxEnd <- ifelse(wrk$idx$idxEnd > 86400, 86400, wrk$idx$idxEnd)
          #if last timeEnd is NA, replce that time to the last time value in data$time
          wrk$idx$timeEnd <- as.POSIXct(ifelse(is.na(wrk$idx$timeEnd), data$time[length(data$time)], wrk$idx$timeEnd), origin = "1970-01-01", tz = "UTC")
          
          #get rid of period  that crdCo2 take over and irga have to move to measure other level
          #and number of sample less than 10% (120-120*0.1)
          tmpWrkIdx <- list()
          for (idxAgr in 1:length(wrk$idx$idxEnd)){
            if (dp01 == "co2Stor") {numSamp <- sum(!is.na(wrk$data$rtioMoleDryCo2[wrk$idx$idxBgn[idxAgr]:wrk$idx$idxEnd[idxAgr]]))}
            if (dp01 == "h2oStor") {numSamp <- sum(!is.na(wrk$data$rtioMoleDryH2o[wrk$idx$idxBgn[idxAgr]:wrk$idx$idxEnd[idxAgr]]))}
            if (data$crdCo2ValvLvl[[lvlCrdCo2Valv]]$lvlCrdCo2[wrk$idx$idxEnd[idxAgr]] == lvlIrga &  numSamp < 108){
              tmpWrkIdx[[idxAgr]] <- idxAgr
              
            }
          }
          
          #combine idxAgr which will be removed
          tmpRmv <- do.call(cbind,tmpWrkIdx)
          #remove those idxAgr from wrk$idx
          if(!is.null(tmpRmv)) wrk$idx <- wrk$idx[-c(tmpRmv),]
          
          #If statement to check if all values were removed
          if(nrow(wrk$idx) > 0){
            
          whrSamp <- wrk$idx$idxBgn[1]:wrk$idx$idxEnd[1]
          if (length (wrk$idx$idxBgn) > 1 ){
            for(ii in 2:length (wrk$idx$idxBgn)){
              whrSamp <- c(whrSamp, wrk$idx$idxBgn[ii]:wrk$idx$idxEnd[ii])
            }
          }
          wrk$data[-whrSamp, 1:9] <- NaN
          
          for (idxSens in names(wrk$qfqm)){
            #replace qfqm with -1 when irga got kick out to measure the new measurement level
            wrk$qfqm[[idxSens]][wrk$data$lvlIrga != lvlIrga, 1:length(wrk$qfqm[[idxSens]])] <- -1
            #replace all qf that not belong to that measurement level by NaN
            wrk$qfqm[[idxSens]][-whrSamp, 1:length(wrk$qfqm[[idxSens]])] <- NaN
          }
          }#End if statement after qfRmv
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
      #assign lvlPresValiRegInStor for each gas
      if (lvl == "co2Zero") lvlPresValiRegInStor <- "709_000"
      if (lvl == "co2Arch") lvlPresValiRegInStor <- "710_000"
      if (lvl == "co2Low") lvlPresValiRegInStor <- "712_000"
      if (lvl == "co2Med") lvlPresValiRegInStor <- "713_000"
      if (lvl == "co2High") lvlPresValiRegInStor <- "714_000"
      
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
      if ("presValiRegInStor" %in% names(qfInp)) wrk$qfqm$presValiRegInStor <- qfInp$presValiRegInStor[[lvlPresValiRegInStor]]
      
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
          #report NaN values when the validation occurred between 2 days and the start and end time cannot be determined
          if (length(wrk$idx$idxBgn) == 0) {
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
            
          } else{
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
          }}#; rm(idxAgr)
          
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
          
          #replace data with NaN values when the validation occurred between 2 days and the start and end time cannot be determined
          if (length(wrk$idx$idxBgn) == 0) {
            wrk$data[] <- NaN
            for (idxSens in names(wrk$qfqm)){
              #replace all qf that not belong to that measurement level by NaN
              wrk$qfqm[[idxSens]][] <- NaN
            }
            
          } else {
          whrSamp <- wrk$idx$idxBgn[1]:wrk$idx$idxEnd[1]
          if (length (wrk$idx$idxBgn) > 1 ){
            for(ii in 2:length (wrk$idx$idxBgn)){
              whrSamp <- c(whrSamp, wrk$idx$idxBgn[ii]:wrk$idx$idxEnd[ii])
            }
          }
          wrk$data[-whrSamp, ] <- NaN
          for (idxSens in names(wrk$qfqm)){
            #replace all qf that not belong to that measurement level by NaN
            wrk$qfqm[[idxSens]][-whrSamp, 1:length(wrk$qfqm[[idxSens]])] <- NaN
          }
          }#close else if length(wrk$idx$idxBgn) > 0
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
      if ("presInlt" %in% names(qfInp)) wrk$qfqm$presInlt <- qfInp$presInlt[[lvl]] 
      if ("pumpStor" %in% names(qfInp)) wrk$qfqm$pumpStor <- qfInp$pumpStor[[lvlMfm]] 
      
      if (PrdMeas == PrdAgr) {
        #PrdAgr <- 9
        #9 minutely sampling data
        #idxLvLPrdAgr <- paste0(lvl, "_", sprintf("%02d", PrdAgr), "m")
        #rpt[[dp01]][[idxLvLPrdAgr]] <- list()
        
        #if there is at least one measurement
        if(length(which(!is.na(wrk$qfqm$crdCo2$qfRngTemp))) > 0){
          #determine the index of each measurement  
          wrk$idx <- eddy4R.base::def.idx.agr(time = data$time, PrdAgr = (PrdMeas*60), FreqLoca = 1, MethIdx = "specBgn", data = wrk$qfqm$crdCo2$qfRngTemp, CritTime = 240)
          #delete row if last timeBgn and timeEnd is NA
          wrk$idx <- wrk$idx[rowSums(is.na(wrk$idx)) != 2,]
          #replace last idxEnd > 86400 by 86400
          wrk$idx$idxEnd <- ifelse(wrk$idx$idxEnd > 86400, 86400, wrk$idx$idxEnd)
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
            
            for (idxSens in names(wrk$inpMask$qfqm)){
              #replace qfqm with -1 when valve switch to measure to next level before schedule time (9 min)
              wrk$inpMask$qfqm[[idxSens]][wrk$inpMask$data$lvlCrdCo2 != lvlCrdCo2, 1:length(wrk$inpMask$qfqm[[idxSens]])] <- -1
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
          wrk$idx <- eddy4R.base::def.idx.agr(time = data$time, PrdAgr = (PrdMeas*60), FreqLoca = 1, MethIdx = "specBgn", data = wrk$qfqm$crdCo2$qfRngTemp, CritTime = 240)
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
         
          for (idxSens in names(wrk$qfqm)){
            #replace qfqm with -1 when valve switch to measure to next level before schedule time (9 min)
            wrk$qfqm[[idxSens]][wrk$data$lvlCrdCo2 != lvlCrdCo2, 1:length(wrk$qfqm[[idxSens]])] <- -1
            #replace all qf that not belong to that measurement level by NaN
            wrk$qfqm[[idxSens]][-whrSamp, 1:length(wrk$qfqm[[idxSens]])] <- NaN
          }
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
      #assign lvlPresValiRegInStor for each gas
      if (lvl == "co2Arch") lvlPresValiRegInStor <- "710_000"
      if (lvl == "co2Low") lvlPresValiRegInStor <- "712_000"
      if (lvl == "co2Med") lvlPresValiRegInStor <- "713_000"
      if (lvl == "co2High") lvlPresValiRegInStor <- "714_000"
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
      if ("presValiRegInStor" %in% names(qfInp)) wrk$qfqm$presValiRegInStor <- qfInp$presValiRegInStor[[lvlPresValiRegInStor]]
      
      if (PrdMeas == PrdAgr) {
        #PrdAgr <- 9
        #9 minutely sampling data
        #idxLvLPrdAgr <- paste0(lvl, "_", sprintf("%02d", PrdAgr), "m")
        #rpt[[dp01]][[idxLvLPrdAgr]] <- list()
        
        #if there is at least one measurement
        if(length(which(!is.na(wrk$qfqm$crdCo2$qfRngTemp))) > 0){
          #determine the end time of each measurement
          wrk$idx <- eddy4R.base::def.idx.agr(time = data$time, PrdAgr = (PrdMeas*60), FreqLoca = 1, MethIdx = "specBgn", data = wrk$qfqm$crdCo2$qfRngTemp, CritTime = 240)
          #delete row if last timeBgn and timeEnd is NA
          wrk$idx <- wrk$idx[rowSums(is.na(wrk$idx)) != 2,]
          #if last timeEnd is NA, replce that time to the last time value in data$time
          wrk$idx$timeEnd <- as.POSIXct(ifelse(is.na(wrk$idx$timeEnd), data$time[length(data$time)], wrk$idx$timeEnd), origin = "1970-01-01", tz = "UTC")
          
          #idxAgr2 <- 0
          #report NaN values when the validation occurred between 2 days and the start and end time cannot be determined
          if (length(wrk$idx$idxBgn) == 0) {
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
          } else {
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
            
          }}#; rm(idxAgr)
          
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
          wrk$idx <- eddy4R.base::def.idx.agr(time = data$time, PrdAgr = (PrdMeas*60), FreqLoca = 1, MethIdx = "specBgn", data = wrk$qfqm$crdCo2$qfRngTemp, CritTime = 240)
          #delete row if last timeBgn and timeEnd is NA
          wrk$idx <- wrk$idx[rowSums(is.na(wrk$idx)) != 2,]
          #if last timeEnd is NA, replce that time to the last time value in data$time
          wrk$idx$timeEnd <- as.POSIXct(ifelse(is.na(wrk$idx$timeEnd), data$time[length(data$time)], wrk$idx$timeEnd), origin = "1970-01-01", tz = "UTC")
          
          #replace data with NaN values when the validation occurred between 2 days and the start and end time cannot be determined
          if (length(wrk$idx$idxBgn) == 0) {
            wrk$data[] <- NaN
            for (idxSens in names(wrk$qfqm)){
              #replace all qf that not belong to that measurement level by NaN
              wrk$qfqm[[idxSens]][] <- NaN
            }
            
          } else {
          whrSamp <- wrk$idx$idxBgn[1]:wrk$idx$idxEnd[1]
          if (length (wrk$idx$idxBgn) > 1 ){
            for(ii in 2:length (wrk$idx$idxBgn)){
              whrSamp <- c(whrSamp, wrk$idx$idxBgn[ii]:wrk$idx$idxEnd[ii])
            }
          }
          wrk$data[-whrSamp, ] <- NaN
          for (idxSens in names(wrk$qfqm)){
            #replace all qf that not belong to that measurement level by NaN
            wrk$qfqm[[idxSens]][-whrSamp, 1:length(wrk$qfqm[[idxSens]])] <- NaN
          }
          }#close else if length(wrk$idx$idxBgn) > 0
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
  

  #calculate dp01 for "ch4Conc" ########################################################################################
  if (dp01 %in% c("ch4Conc")){
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
                             "pres" = data$crdCo2[[lvl]]$pres,
                             "presEnvHut" = data$envHut[[lvlEnvHut]]$pres,
                             "rhEnvHut" = data$envHut[[lvlEnvHut]]$rh,
                             "rtioMoleDryCh4" = data$crdCo2[[lvl]]$rtioMoleDryCh4,
                             "rtioMoleWetCh4" = data$crdCo2[[lvl]]$rtioMoleWetCh4,
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
      if ("presInlt" %in% names(qfInp)) wrk$qfqm$presInlt <- qfInp$presInlt[[lvl]] 
      if ("pumpStor" %in% names(qfInp)) wrk$qfqm$pumpStor <- qfInp$pumpStor[[lvlMfm]] 
      
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
          #replace last idxEnd > 86400 by 86400
          wrk$idx$idxEnd <- ifelse(wrk$idx$idxEnd > 86400, 86400, wrk$idx$idxEnd)
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
            
            for (idxSens in names(wrk$inpMask$qfqm)){
              #replace qfqm with -1 when valve switch to measure to next level before schedule time (9 min)
              wrk$inpMask$qfqm[[idxSens]][wrk$inpMask$data$lvlCrdCo2 != lvlCrdCo2, 1:length(wrk$inpMask$qfqm[[idxSens]])] <- -1
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
          
          for (idxSens in names(wrk$qfqm)){
            #replace qfqm with -1 when valve switch to measure to next level before schedule time (9 min)
            wrk$qfqm[[idxSens]][wrk$data$lvlCrdCo2 != lvlCrdCo2, 1:length(wrk$qfqm[[idxSens]])] <- -1
            #replace all qf that not belong to that measurement level by NaN
            wrk$qfqm[[idxSens]][-whrSamp, 1:length(wrk$qfqm[[idxSens]])] <- NaN
          }
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
      #assign lvlPresValiRegInStor for each gas
      if (lvl == "co2Arch") lvlPresValiRegInStor <- "710_000"
      if (lvl == "co2Low") lvlPresValiRegInStor <- "712_000"
      if (lvl == "co2Med") lvlPresValiRegInStor <- "713_000"
      if (lvl == "co2High") lvlPresValiRegInStor <- "714_000"
      #input the whole day data
      wrk$data <- data.frame(stringsAsFactors = FALSE,
                             "idGas" = data$crdCo2[[lvl]]$idGas,
                             "pres" = data$crdCo2[[lvl]]$pres,
                             "presEnvHut" = data$envHut[[lvlEnvHut]]$pres,
                             "rhEnvHut" = data$envHut[[lvlEnvHut]]$rh,
                             "rtioMoleDryCh4" = data$crdCo2[[lvl]]$rtioMoleDryCo2,
                             "rtioMoleDryCh4Refe" = data$crdCo2[[lvl]]$rtioMoleDryCo2Refe,
                             "rtioMoleWetCh4" = data$crdCo2[[lvl]]$rtioMoleWetCo2,
                             "rtioMoleWetH2oEnvHut" = data$envHut[[lvlEnvHut]]$rtioMoleWetH2o,
                             "temp" = data$crdCo2[[lvl]]$temp, 
                             "tempEnvHut" = data$envHut[[lvlEnvHut]]$temp
                             
      )
      #input the whole day qfqm 
      wrk$qfqm <- list()
      wrk$qfqm$crdCo2 <- qfInp$crdCo2[[lvl]]
      wrk$qfqm$envHut <- qfInp$envHut[[lvlEnvHut]]
      wrk$qfqm$mfcValiStor <- qfInp$mfcValiStor[[lvlMfcValiStor]]
      if ("presValiRegInStor" %in% names(qfInp)) wrk$qfqm$presValiRegInStor <- qfInp$presValiRegInStor[[lvlPresValiRegInStor]]
      
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
          #report NaN values when the validation occurred between 2 days and the start and end time cannot be determined
          if (length(wrk$idx$idxBgn) == 0) {
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
              rpt[[1]][[idxQf]] <- rpt[[1]][[idxQf]][which(!(names(rpt[[1]][[idxQf]]) %in% c("rtioMoleDryCh4Refe", "idGas")))]
            }; rm(idxQf)
            
            #add both time begin and time end to rpt
            rpt[[1]]$timeBgn <- list()
            rpt[[1]]$timeEnd <- list()
            
            #output time for dp01
            for(idxVar in names(wrk$data)[which(!(names(wrk$data) %in% c("rtioMoleDryCh4Refe", "idGas")))]){
              rpt[[1]]$timeBgn[[idxVar]] <- data$time[1]
              rpt[[1]]$timeEnd[[idxVar]] <- data$time[length(data$time)]
              #unit
              attributes(rpt[[1]]$qmAlph[[idxVar]])$unit <- "-"
              attributes(rpt[[1]]$qmBeta[[idxVar]])$unit <- "-"
              attributes(rpt[[1]]$qfFinl[[idxVar]])$unit <- "NA"
              attributes(rpt[[1]]$qfSciRevw[[idxVar]])$unit <- "NA"
            }; rm(idxVar)
          } else {
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
            for(idxVar in names(wrk$data)[which(!(names(wrk$data) %in% c("rtioMoleDryCh4Refe", "idGas")))]){
              rpt[[idxAgr]]$timeBgn[[idxVar]] <- wrk$idx$timeBgn[idxAgr]
              rpt[[idxAgr]]$timeEnd[[idxVar]] <- wrk$idx$timeEnd[idxAgr]
            }; rm(idxVar)
            
          }}#; rm(idxAgr)
          
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
            rpt[[1]][[idxQf]] <- rpt[[1]][[idxQf]][which(!(names(rpt[[1]][[idxQf]]) %in% c("rtioMoleDryCh4Refe", "idGas")))]
          }; rm(idxQf)
          
          #add both time begin and time end to rpt
          rpt[[1]]$timeBgn <- list()
          rpt[[1]]$timeEnd <- list()
          
          #output time for dp01
          for(idxVar in names(wrk$data)[which(!(names(wrk$data) %in% c("rtioMoleDryCh4Refe", "idGas")))]){
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
          
          #replace data with NaN values when the validation occurred between 2 days and the start and end time cannot be determined
          if (length(wrk$idx$idxBgn) == 0) {
            wrk$data[] <- NaN
            for (idxSens in names(wrk$qfqm)){
              #replace all qf that not belong to that measurement level by NaN
              wrk$qfqm[[idxSens]][] <- NaN
            }
            
          } else {
          whrSamp <- wrk$idx$idxBgn[1]:wrk$idx$idxEnd[1]
          if (length (wrk$idx$idxBgn) > 1 ){
            for(ii in 2:length (wrk$idx$idxBgn)){
              whrSamp <- c(whrSamp, wrk$idx$idxBgn[ii]:wrk$idx$idxEnd[ii])
            }
          }
          wrk$data[-whrSamp, ] <- NaN
          for (idxSens in names(wrk$qfqm)){
            #replace all qf that not belong to that measurement level by NaN
            wrk$qfqm[[idxSens]][-whrSamp, 1:length(wrk$qfqm[[idxSens]])] <- NaN
          }
          } #close else if length(wrk$idx$idxBgn) > 0
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
          for(idxVar in names(wrk$data)[which(!(names(wrk$data) %in% c("rtioMoleDryCh4Refe", "idGas")))]){
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
      if ("presInlt" %in% names(qfInp)) wrk$qfqm$presInlt <- qfInp$presInlt[[lvl]]
      if ("pumpStor" %in% names(qfInp)) wrk$qfqm$pumpStor <- qfInp$pumpStor[[lvlMfm]] 
      
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
          #replace last idxEnd > 86400 by 86400
          wrk$idx$idxEnd <- ifelse(wrk$idx$idxEnd > 86400, 86400, wrk$idx$idxEnd)
          #if last timeEnd is NA, replce that time to the last time value in data$time
          wrk$idx$timeEnd <- as.POSIXct(ifelse(is.na(wrk$idx$timeEnd), data$time[length(data$time)], wrk$idx$timeEnd), origin = "1970-01-01", tz = "UTC")
          #idxAgr2 <- 0
          for (idxAgr in 1:length(wrk$idx$idxBgn)){
            #idxAgr <- 124
            #wrk$inpMask for qfqm
            wrk$inpMask$qfqm <- list()
            lapply(names(wrk$qfqm), function (x) wrk$inpMask$qfqm[[x]] <<- wrk$qfqm[[x]][wrk$idx$idxBgn[idxAgr]:wrk$idx$idxEnd[idxAgr],] )
            #replace qfqm$crdH2o with -1 when valve switch to measure to next level before schedule time (9 min)
            for (idxSens in names(wrk$inpMask$qfqm)){
              #replace qfqm with -1 when valve switch to measure to next level before schedule time (9 min)
              wrk$inpMask$qfqm[[idxSens]][wrk$inpMask$data$lvlCrdH2o != lvlCrdH2o, 1:length(wrk$inpMask$qfqm[[idxSens]])] <- -1
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
          
          for (idxSens in names(wrk$qfqm)){
            #replace qfqm with -1 when when valve switch to measure to next level before schedule time (9 min)
            wrk$qfqm[[idxSens]][wrk$data$lvlCrdH2o != lvlCrdH2o, 1:length(wrk$qfqm[[idxSens]])] <- -1
            #replace all qf that not belong to that measurement level by NaN
            wrk$qfqm[[idxSens]][-whrSamp, 1:length(wrk$qfqm[[idxSens]])] <- NaN
          }
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
      #assign lvl for both presValiRegInStor and presValiRegOutStor
      lvlPresValiReg <- "711_000"
      
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
      if ("presValiRegInStor" %in% names(qfInp)) wrk$qfqm$presValiRegInStor <- qfInp$presValiRegInStor[[lvlPresValiReg]] 
      if ("presValiRegOutStor" %in% names(qfInp)) wrk$qfqm$presValiRegOutStor <- qfInp$presValiRegOutStor[[lvlPresValiReg]] 
      
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
          for (idxSens in names(wrk$qfqm)){
            #replace all qf that not belong to that measurement level by NaN
            wrk$qfqm[[idxSens]][-whrSamp, 1:length(wrk$qfqm[[idxSens]])] <- NaN
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
  
  #remove NULL list from rpt
  rpt <- rpt[!sapply(rpt, is.null)]
  #return results
  return(rpt)
  
}#end function wrap.dp01.qfqm.ecse()
