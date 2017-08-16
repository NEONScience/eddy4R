##############################################################################################
#' @title Wrapper function:  Preprocessing and calculating quality metrics, alpha and beta quality metrics, and final quality flag for the NEON eddy-covariance stroage exchange L1 data products

#' @author
#' Natchaya Pingintha-Durden \email{eddy4R.info@gmail.com}

#' @description 
#' Wrapper function. Preprocessing and calculating quality metrics, alpha and beta quality metrics, and final quality flag for the NEON eddy-covariance stroage exchange (ECSE) Level 1 data products (dp01).

#' @param \code{dp01} A vector of class "character" containing the name of NEON ECSE dp01 which descriptive statistics are being calculated, \cr
#' c("irgaCo2", "irgaH2o", "tempAirLvl", "tempAirTop", "isoCo2", "isoH2o"). Defaults to "irgaCo2". [-] 
#' @param \code{lvl}  Measurement level of dp01 which descriptive statistics are being calculated. Of type character. [-]
#' @param \code{lvlIrgaMfcSamp} Measurement level of irgaMfcSamp which apply to only  dp01 equal to "irgaCo2" or "irgaH2o". Defaults to NULL. Of type character. [-]
#' @param \code{lvlIrgaValvLvl} Measurement level of irgaValvLvl which apply to only  dp01 equal to "irgaCo2" or "irgaH2o". Defaults to NULL. Of type character. [-]
#' @param \code{lvlCrdH2oValvVali} Measurement level of crdH2oValvVali which apply to only  dp01 equal to "isoH2o". Defaults to NULL. Of type character. [-]
#' @param \code{data} A list of data frame containing the input dp0p data that related to dp01 which qfqm are being calculated. Of class integer". [User defined] 
#' @param \code{qfInput} A list of data frame containing the input quality flag data that related to dp01 are being grouped. Of class integer". [NA] 
#' @param \code{TypeMeas} A vector of class "character" containing the name of measurement type (sampling or validation), TypeMeas = c("samp", "vali"). Defaults to "samp". [-]
#' @param \code{PrdMeas} The measurement time period in minute.  [min]
#' @param \code{PrdAgr} The time period to aggregate to averaging in minute. [min]
#' @param \code{idxTime} A list of data frame containing the indices and corresponding times for aggregation periods. [-]

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
##############################################################################################
wrap.neon.dp01.qfqm.ecse <- function(
  dp01 = c("irgaCo2", "irgaH2o", "tempAirLvl", "tempAirTop", "isoCo2", "isoH2o")[1],
  lvl,
  lvlIrgaMfcSamp = NULL,
  lvlIrgaValvLvl = NULL,
  lvlCrdH2oValvVali = NULL,
  data = list(),
  qfInput = list(),
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
#calculate qfqm for irgaCo2 and irgaH2o ########################################################################################
  if (dp01 %in% c("irgaCo2", "irgaH2o")){
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
      if(dp01 == "irgaCo2"){
        wrk$data <- data.frame(stringsAsFactors = FALSE,
                               "frt00" = data$irgaMfcSamp[[lvlIrgaMfcSamp]][["frt00"]],
                               #wrk$data$irgaMfcSamp[[paste0(Para$Flow$LevlTowr$irgaMfcSamp, "_", sprintf("%02d", idxPrdAgr), "m")]]$frt00,
                               "pres" = data$irga[[lvl]]$pres,
                               "rtioMoleDryCo2" = data$irga[[lvl]]$rtioMoleDryCo2,
                               "rtioMoleWetCo2" = data$irga[[lvl]]$rtioMoleWetCo2,
                               "temp" = data$irga[[lvl]]$temp,
                               "lvlIrga" = data$irgaValvLvl[[lvlIrgaValvLvl]][["lvlIrga"]]
                               
        )
      }
      
      if(dp01 == "irgaH2o"){
        wrk$data <- data.frame(stringsAsFactors = FALSE,
                               "frt00" = data$irgaMfcSamp[[lvlIrgaMfcSamp]][["frt00"]],
                               #wrk$data$irgaMfcSamp[[paste0(Para$Flow$LevlTowr$irgaMfcSamp, "_", sprintf("%02d", idxPrdAgr), "m")]]$frt00,
                               "pres" = data$irga[[lvl]]$pres,
                               "rtioMoleDryH2o" = data$irga[[lvl]]$rtioMoleDryH2o,
                               "rtioMoleWetH2o" = data$irga[[lvl]]$rtioMoleWetH2o,
                               "temp" = data$irga[[lvl]]$temp,
                               "lvlIrga" = data$irgaValvLvl[[lvlIrgaValvLvl]][["lvlIrga"]]
                               
        )
      }
      #input the whole day qfqm 
      wrk$qfqm <- list()
      wrk$qfqm$irga <- qfInput$irga[[lvl]]
      wrk$qfqm$irgaMfcSamp <- qfInput$irgaMfcSamp[[lvlIrgaMfcSamp]]
      
      if (PrdMeas == PrdAgr) {
        #PrdAgr <- 2
        #2 minutely sampling data
        #idxLvLPrdAgr <- paste0(lvl, "_", sprintf("%02d", PrdAgr), "m")
        #rpt[[dp01]][[idxLvLPrdAgr]] <- list()
        
        #if there is at least one measurement
        if(length(which(!is.na(wrk$data$temp))) > 0){
          #determine the index of each measurement  
          wrk$idx <- eddy4R.base::def.idx.agr(time = data$time, PrdAgr = (PrdMeas*60), FreqLoca = 1, MethIdx = "specBgn", data = wrk$data$temp, CritTime = 60)
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
            lapply(names(wrk$qfqm), function (x) wrk$inpMask$qfqm[[x]] <<- wrk$qfqm[[x]][wrk$idx$idxBgn[idxAgr]:wrk$idx$idxEnd[idxAgr],] )
            #replace qfqm$irga with -1 when irga got kick out to measure the new measurement level
            for (tmp in 1:length(wrk$inpMask$qfqm$irga)){
              wrk$inpMask$qfqm$irga[[tmp]][wrk$inpMask$data$lvlIrga != lvlIrga] <- -1
            }
            for (tmp in 1:length(wrk$inpMask$qfqm$irgaMfcSamp)){
              wrk$inpMask$qfqm$irgaMfcSamp[[tmp]][wrk$inpMask$data$lvlIrga != lvlIrga] <- -1
            }
            
            #qfqm processing
            rpt[[idxAgr]] <- eddy4R.qaqc::wrap.neon.dp01.qfqm(
              qfInput = wrk$inpMask$qfqm, 
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
          for(idxQf in NameQf){
            rpt[[1]][[idxStat]] <- list()
            
            
            for (idxVar in names(wrk$data)[which(!(names(wrk$data) %in% c("lvlIrga")))]){
              rpt[[1]][[idxQf]][[idxVar]] <- list()  
            }; rm(idxVar)
            
          }; rm(idxQf)
          
        }#end of if no measurement data at all in the whole day
      } #end of PrdAgr == 2
      
      if (PrdMeas != PrdAgr) {
        #PrdAgr <- 30
        #if there is at least one measurement
        if(length(which(!is.na(wrk$data$temp))) > 0){
          
          #determine the index of each measurement  
          wrk$idx <- eddy4R.base::def.idx.agr(time = data$time, PrdAgr = (PrdMeas*60), FreqLoca = 1, MethIdx = "specBgn", data = wrk$data$temp, CritTime = 60)
          whrSamp <- wrk$idx$idxBgn[1]:wrk$idx$idxEnd[1]
          if (length (wrk$idx$idxBgn) > 1 ){
            for(ii in 2:length (wrk$idx$idxBgn)){
              whrSamp <- c(whrSamp, wrk$idx$idxBgn[ii]:wrk$idx$idxEnd[ii])
            }
          }
          wrk$data[-whrSamp, 1:5] <- NaN
          #replace qfqm$irga with -1 when irga got kick out to measure the new measurement level
          for (tmp in 1:length(wrk$qfqm$irga)){
            wrk$qfqm$irga[[tmp]][wrk$data$lvlIrga != lvlIrga] <- -1
          }
          for (tmp in 1:length(wrk$qfqm$irgaMfcSamp)){
            wrk$qfqm$irgaMfcSamp[[tmp]][wrk$data$lvlIrga != lvlIrga] <- -1
          }
          #replace all qf that not belong to that measurement level by NaN
          wrk$qfqm$irga[-whrSamp, 1:length(wrk$qfqm$irga)] <- NaN
          wrk$qfqm$irgaMfcSamp[-whrSamp, 1:length(wrk$qfqm$irgaMfcSamp)] <- NaN
          
          #replace qf from irgaMfcSamp data with -1 when irga got kick out to measure the new measurement level
          #wrk$qfqm$irgaMfcSamp <- as.data.frame(sapply(wrk$qfqm$irgaMfcSamp, function(x) ifelse(wrk$data$lvlIrga == lvlIrga, x, -1)))
        } 
        
        
        for(idxAgr in c(1:length(idxTime[[paste0(PrdAgr, "min")]]$Bgn))) {
          #idxAgr <- 48
          
          
          ## grab data at the selected mask data
          # for qfqm
          #wrk$inpMask$data <- list()
          #wrk$inpMask$data <- wrk$data[idxTime[[paste0(PrdAgr, "min")]]$Bgn[idxAgr]:idxTime[[paste0(PrdAgr, "min")]]$End[idxAgr],]
          idxLvLPrdAgr <- paste0(lvl, "_", sprintf("%02d", PrdAgr), "m")
          
          wrk$inpMask$qfqm <- list()
          lapply(names(wrk$qfqm), function (x) wrk$inpMask$qfqm[[x]] <<- wrk$qfqm[[x]][idxTime[[paste0(PrdAgr, "min")]]$Bgn[idxAgr]:idxTime[[paste0(PrdAgr, "min")]]$End[idxAgr],])
          
          #qfqm processing
          rpt[[idxAgr]] <- eddy4R.qaqc::wrap.neon.dp01.qfqm(
            qfInput = wrk$inpMask$qfqm, 
            MethMeas = "ecse",
            TypeMeas = "samp",
            RptExpd = FALSE,
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
      if(dp01 == "irgaCo2"){
        wrk$data <- data.frame(stringsAsFactors = FALSE,
                               "frt00" = data$irgaMfcSamp[[lvlIrgaMfcSamp]][["frt00"]],
                               #wrk$data$irgaMfcSamp[[paste0(Para$Flow$LevlTowr$irgaMfcSamp, "_", sprintf("%02d", PrdAgr), "m")]]$frt00,
                               "pres" = data$irga[[lvl]]$pres,
                               "rtioMoleDryCo2" = data$irga[[lvl]]$rtioMoleDryCo2,
                               "rtioMoleDryCo2Refe" = data$irga[[lvl]]$rtioMoleDryCo2Refe,
                               "rtioMoleWetCo2" = data$irga[[lvl]]$rtioMoleWetCo2,
                               "temp" = data$irga[[lvl]]$temp
                               #data$tempAirLvl$`000_010_01m`$temp
                               
        )
      }
      
      if(dp01 == "irgaH2o"){
        wrk$data <- data.frame(stringsAsFactors = FALSE,
                               "frt00" = data$irgaMfcSamp[[lvlIrgaMfcSamp]][["frt00"]],
                               #wrk$data$irgaMfcSamp[[paste0(Para$Flow$LevlTowr$irgaMfcSamp, "_", sprintf("%02d", PrdAgr), "m")]]$frt00,
                               "pres" = data$irga[[lvl]]$pres,
                               "rtioMoleDryH2o" = data$irga[[lvl]]$rtioMoleDryH2o,
                               "rtioMoleWetH2o" = data$irga[[lvl]]$rtioMoleWetH2o,
                               "temp" = data$irga[[lvl]]$temp
                               #data$tempAirLvl$`000_010_01m`$temp
                               
        )
      }
      
      #input the whole day qfqm 
      wrk$qfqm <- list()
      wrk$qfqm$irga <- qfInput$irga[[lvl]]
      wrk$qfqm$irgaMfcSamp <- qfInput$irgaMfcSamp[[lvlIrgaMfcSamp]]
      
      if (PrdMeas == PrdAgr) {
        #PrdAgr <- 2
        #2 minutely sampling data
        #idxLvLPrdAgr <- paste0(lvl, "_", sprintf("%02d", PrdAgr), "m")
        #rpt[[dp01]][[idxLvLPrdAgr]] <- list()
        
        #if there is at least one measurement
        if(length(which(!is.na(wrk$data$temp))) > 0){
          #determine the end time of each measurement
          wrk$idx <- eddy4R.base::def.idx.agr(time = data$time, PrdAgr = (PrdMeas*60), FreqLoca = 1, MethIdx = "specEnd", data = wrk$data$temp, CritTime = 20)
          
          #idxAgr2 <- 0
          for (idxAgr in 1:length(wrk$idx$idxBgn)){
            #idxAgr <- 1
            #wrk$inpMask for qfqm
            wrk$inpMask$qfqm <- list()
            lapply(names(wrk$qfqm), function (x) wrk$inpMask$qfqm[[x]] <<- wrk$qfqm[[x]][wrk$idx$idxBgn[idxAgr]:wrk$idx$idxEnd[idxAgr],] )
            
            #qfqm processing
            rpt[[idxAgr]] <- eddy4R.qaqc::wrap.neon.dp01.qfqm(
              qfInput = wrk$inpMask$qfqm, 
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
          
          for(idxQf in NameQf){
            #idxQf in names(rpt[[1]])
            rpt[[1]][[idxQf]] <- list()
            
            
            for (idxVar in names(wrk$data)[which(!(names(wrk$data) %in% c("rtioMoleDryCo2Refe")))]){
              rpt[[1]][[idxQf]][[idxVar]] <- list()  
            }; rm(idxVar)
            
            
          }; rm(idxQf)
          
        }#end of if no measurement data at all in the whole day
        
      } #end of PrdAgr == 2
      
      if (PrdMeas != PrdAgr) {
        #PrdAgr <- 30
        #if there is at least one measurement
        if(length(which(!is.na(wrk$data$temp))) > 0){
          #   #determine the end time of each measurement  
          wrk$idx <- eddy4R.base::def.idx.agr(time = data$time, PrdAgr = (PrdMeas*60), FreqLoca = 1, MethIdx = "specEnd", data = wrk$data$temp, CritTime = 20)
          whrSamp <- wrk$idx$idxBgn[1]:wrk$idx$idxEnd[1]
          if (length (wrk$idx$idxBgn) > 1 ){
            for(ii in 2:length (wrk$idx$idxBgn)){
              whrSamp <- c(whrSamp, wrk$idx$idxBgn[ii]:wrk$idx$idxEnd[ii])
            }
          }
          #wrk$data[-whrSamp, ] <- NaN
          wrk$qfqm$irga[-whrSamp, 1:length(wrk$qfqm$irga)] <- NaN
          wrk$qfqm$irgaMfcSamp[-whrSamp, 1:length(wrk$qfqm$irgaMfcSamp)] <- NaN
        } #else {#end of if no measurement data at all in the whole day
        #   wrk$data$frt00 <- NaN #assign NaN to frt00 data
        # }
        
        for(idxAgr in c(1:length(idxTime[[paste0(PrdAgr, "min")]]$Bgn))) {
          #idxAgr <- 34
          ## grab data at the selected mask data
          #for qfqm
          wrk$inpMask$qfqm <- list()
          idxLvLPrdAgr <- paste0(lvl, "_", sprintf("%02d", PrdAgr), "m")
          lapply(names(wrk$qfqm), function (x) wrk$inpMask$qfqm[[x]] <<- wrk$qfqm[[x]][idxTime[[paste0(PrdAgr, "min")]]$Bgn[idxAgr]:idxTime[[paste0(PrdAgr, "min")]]$End[idxAgr],])
          
          #qfqm processing
          rpt[[idxAgr]] <- eddy4R.qaqc::wrap.neon.dp01.qfqm(
            qfInput = wrk$inpMask$qfqm, 
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
                                               qfInput[[dp01]][[lvl]][idxTime[[paste0(PrdAgr, "min")]]$Bgn[idxAgr]:idxTime[[paste0(PrdAgr, "min")]]$End[idxAgr],]
        )
      } 
      
      if(dp01 == "tempAirTop") {
        wrk$inpMask$data <- data.frame(stringsAsFactors = FALSE,
                                       "temp" = colMeans(rbind(data$tempAirTop[[lvl]][["temp01"]], data$tempAirTop[[lvl]][["temp02"]], data$tempAirTop[[lvl]][["temp03"]]), na.rm=TRUE)[idxTime[[paste0(PrdAgr, "min")]]$Bgn[idxAgr]:idxTime[[paste0(PrdAgr, "min")]]$End[idxAgr]]
        )
        
        wrk$inpMask$qfqm[[dp01]] <- data.frame(stringsAsFactors = FALSE,
                                               qfInput[[dp01]][[lvl]][idxTime[[paste0(PrdAgr, "min")]]$Bgn[idxAgr]:idxTime[[paste0(PrdAgr, "min")]]$End[idxAgr],]
        )
      } 
      
      #qfqm processing
      rpt[[idxAgr]] <- eddy4R.qaqc::wrap.neon.dp01.qfqm(
        qfInput = wrk$inpMask$qfqm, 
        MethMeas = "ecse",
        TypeMeas = "samp",
        RptExpd = FALSE,
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
      #input the whole day data
      wrk$data <- data.frame(stringsAsFactors = FALSE,
                             "rtioMoleWetCo2" = data$crdCo2[[lvl]]$rtioMoleWetCo2,
                             "rtioMoleDryCo2" = data$crdCo2[[lvl]]$rtioMoleDryCo2,
                             "rtioMoleWet12CCo2" = data$crdCo2[[lvl]]$rtioMoleWet12CCo2,
                             "rtioMoleDry12CCo2" = data$crdCo2[[lvl]]$rtioMoleDry12CCo2,
                             "rtioMoleWet13CCo2" = data$crdCo2[[lvl]]$rtioMoleWet13CCo2,
                             "rtioMoleDry13CCo2" = data$crdCo2[[lvl]]$rtioMoleDry13CCo2,
                             "dlta13CCo2" = data$crdCo2[[lvl]]$dlta13CCo2,
                             "rtioMoleWetH2o" = data$crdCo2[[lvl]]$rtioMoleWetH2o,
                             "rtioMoleDryH2o" = data$crdCo2[[lvl]]$rtioMoleDryH2o,
                             "temp" = data$crdCo2[[lvl]]$temp,
                             "pres" = data$crdCo2[[lvl]]$pres,
                             "idGas" = data$crdCo2[[lvl]]$idGas
                             
      )
      
      #input the whole day qfqm 
      wrk$qfqm <- list()
      #subset only
      wrk$qfqm$crdCo2 <- qfInput$crdCo2[[lvl]]
      
      if (PrdMeas == PrdAgr) {
        #PrdAgr <- 9
        #9 minutely sampling data
        #idxLvLPrdAgr <- paste0(lvl, "_", sprintf("%02d", PrdAgr), "m")
        #rpt[[dp01]][[idxLvLPrdAgr]] <- list()
        
        #if there is at least one measurement
        if(length(which(!is.na(wrk$data$temp))) > 0){
          #determine the index of each measurement  
          wrk$idx <- eddy4R.base::def.idx.agr(time = data$time, PrdAgr = (PrdMeas*60), FreqLoca = 1, MethIdx = "specBgn", data = wrk$data$temp, CritTime = 60)
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
            
            #qfqm processing
            rpt[[idxAgr]] <- eddy4R.qaqc::wrap.neon.dp01.qfqm(
              qfInput = wrk$inpMask$qfqm, 
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
            for(idxVar in names(wrk$data)[which(!(names(wrk$data) %in% c("idGas")))]){
              rpt[[idxAgr]]$timeBgn[[idxVar]] <- wrk$idx$timeBgn[idxAgr]
              rpt[[idxAgr]]$timeEnd[[idxVar]] <- wrk$idx$timeEnd[idxAgr]
            }; rm(idxVar)
            
            #}# end of there is at least one data
            
          }; rm(idxAgr)
        } else {
          
          rpt[[1]] <- list()
          
          for(idxQf in NameQf){
            rpt[[1]][[idxQf]] <- list()
            
            
            for (idxVar in names(wrk$data)[which(!(names(wrk$data) %in% c("idGas")))]){
              rpt[[1]][[idxQf]][[idxVar]] <- list()  
            }; rm(idxVar)
            
          }; rm(idxQf) 
          
        }#end of if no measurement data at all in the whole day
      } #end of PrdAgr
      
      if (PrdMeas != PrdAgr) {
        #PrdAgr <- 30
        #if there is at least one measurement
        if(length(which(!is.na(wrk$data$temp))) > 0){
          
          #determine the index of each measurement  
          wrk$idx <- eddy4R.base::def.idx.agr(time = data$time, PrdAgr = (PrdMeas*60), FreqLoca = 1, MethIdx = "specBgn", data = wrk$data$temp, CritTime = 60)
          whrSamp <- wrk$idx$idxBgn[1]:wrk$idx$idxEnd[1]
          if (length (wrk$idx$idxBgn) > 1 ){
            for(ii in 2:length (wrk$idx$idxBgn)){
              whrSamp <- c(whrSamp, wrk$idx$idxBgn[ii]:wrk$idx$idxEnd[ii])
            }
          }
          wrk$data[-whrSamp, ] <- NaN
          wrk$qfqm$crdCo2[-whrSamp, 1:length(wrk$qfqm$crdCo2)] <- NaN
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
          rpt[[idxAgr]] <- eddy4R.qaqc::wrap.neon.dp01.qfqm(
            qfInput = wrk$inpMask$qfqm, 
            MethMeas = "ecse",
            TypeMeas = "samp",
            RptExpd = FALSE,
            dp01 = dp01,
            idGas = wrk$inpMask$data$idGas
          )
          
          #grab and add both time begin and time end to rpt
          rpt[[idxAgr]]$timeBgn <- list()
          rpt[[idxAgr]]$timeEnd <- list()
          
          for(idxVar in names(wrk$data)[which(!(names(wrk$data) %in% c("idGas")))]){
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
                             "rtioMoleWetCo2" = data$crdCo2[[lvl]]$rtioMoleWetCo2,
                             "rtioMoleDryCo2" = data$crdCo2[[lvl]]$rtioMoleDryCo2,
                             "rtioMoleWet12CCo2" = data$crdCo2[[lvl]]$rtioMoleWet12CCo2,
                             "rtioMoleDry12CCo2" = data$crdCo2[[lvl]]$rtioMoleDry12CCo2,
                             "rtioMoleWet13CCo2" = data$crdCo2[[lvl]]$rtioMoleWet13CCo2,
                             "rtioMoleDry13CCo2" = data$crdCo2[[lvl]]$rtioMoleDry13CCo2,
                             "dlta13CCo2" = data$crdCo2[[lvl]]$dlta13CCo2,
                             "rtioMoleWetH2o" = data$crdCo2[[lvl]]$rtioMoleWetH2o,
                             "rtioMoleDryH2o" = data$crdCo2[[lvl]]$rtioMoleDryH2o,
                             "temp" = data$crdCo2[[lvl]]$temp,
                             "pres" = data$crdCo2[[lvl]]$pres,
                             "rtioMoleDryCo2Refe" = data$crdCo2[[lvl]]$rtioMoleDryCo2Refe,
                             "dlta13CCo2Refe" = data$crdCo2[[lvl]]$dlta13CCo2Refe,
                             "idGas" = data$crdCo2[[lvl]]$idGas
                             
      )
      #input the whole day qfqm 
      wrk$qfqm <- list()
      wrk$qfqm$crdCo2 <- qfInput$crdCo2[[lvl]]
      
      if (PrdMeas == PrdAgr) {
        #PrdAgr <- 9
        #9 minutely sampling data
        #idxLvLPrdAgr <- paste0(lvl, "_", sprintf("%02d", PrdAgr), "m")
        #rpt[[dp01]][[idxLvLPrdAgr]] <- list()
        
        #if there is at least one measurement
        if(length(which(!is.na(wrk$data$temp))) > 0){
          #determine the end time of each measurement
          wrk$idx <- eddy4R.base::def.idx.agr(time = data$time, PrdAgr = (PrdMeas*60), FreqLoca = 1, MethIdx = "specBgn", data = wrk$data$temp, CritTime = 60)
          
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
            rpt[[idxAgr]] <- eddy4R.qaqc::wrap.neon.dp01.qfqm(
              qfInput = wrk$inpMask$qfqm, 
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
          
          for(idxQf in NameQf){
            #idxQf in names(rpt[[1]])
            rpt[[1]][[idxQf]] <- list()
            
            
            for (idxVar in names(wrk$data)[which(!(names(wrk$data) %in% c("rtioMoleDryCo2Refe", "dlta13CCo2Refe", "idGas")))]){
              rpt[[1]][[idxQf]][[idxVar]] <- list()  
            }; rm(idxVar)
            
            
          }; rm(idxQf)
          
        }#end of if no measurement data at all in the whole day
        
      } #end of PrdAgr == 9
      
      if (PrdMeas != PrdAgr) {
        #PrdAgr <- 30
        #if there is at least one measurement
        if(length(which(!is.na(wrk$data$temp))) > 0){
          #   #determine the end time of each measurement  
          wrk$idx <- eddy4R.base::def.idx.agr(time = data$time, PrdAgr = (PrdMeas*60), FreqLoca = 1, MethIdx = "specBgn", data = wrk$data$temp, CritTime = 60)
          whrSamp <- wrk$idx$idxBgn[1]:wrk$idx$idxEnd[1]
          if (length (wrk$idx$idxBgn) > 1 ){
            for(ii in 2:length (wrk$idx$idxBgn)){
              whrSamp <- c(whrSamp, wrk$idx$idxBgn[ii]:wrk$idx$idxEnd[ii])
            }
          }
          wrk$data[-whrSamp, ] <- NaN
          wrk$qfqm$crdCo2[-whrSamp, 1:length(wrk$qfqm$crdCo2)] <- NaN
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
          rpt[[idxAgr]] <- eddy4R.qaqc::wrap.neon.dp01.qfqm(
            qfInput = wrk$inpMask$qfqm, 
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
      wrk$data <- data.frame(stringsAsFactors = FALSE,
                             "rtioMoleWetH2o" = data$crdH2o[[lvl]]$rtioMoleWetH2o,
                             "rtioMoleDryH2o" = data$crdH2o[[lvl]]$rtioMoleDryH2o,
                             "dlta18OH2o" = data$crdH2o[[lvl]]$dlta18OH2o,
                             "dlta2HH2o" = data$crdH2o[[lvl]]$dlta2HH2o,
                             "temp" = data$crdH2o[[lvl]]$temp,
                             "pres" = data$crdH2o[[lvl]]$pres
                             
      )
      
      #input the whole day qfqm
      wrk$qfqm <- list()
      wrk$qfqm$crdH2o <- qfInput$crdH2o[[lvl]]
      
      if (PrdMeas == PrdAgr) {
        #PrdAgr <- 9
        #9 minutely sampling data
        #idxLvLPrdAgr <- paste0(lvl, "_", sprintf("%02d", PrdAgr), "m")
        #rpt[[dp01]][[idxLvLPrdAgr]] <- list()
        
        #if there is at least one measurement
        if(length(which(!is.na(wrk$data$temp))) > 0){
          #determine the index of each measurement  
          wrk$idx <- eddy4R.base::def.idx.agr(time = data$time, PrdAgr = (PrdMeas*60), FreqLoca = 1, MethIdx = "specBgn", data = wrk$data$temp, CritTime = 60)
          #if last timeEnd is NA, replce that time to the last time value in data$time
          wrk$idx$timeEnd <- as.POSIXct(ifelse(is.na(wrk$idx$timeEnd), data$time[length(data$time)], wrk$idx$timeEnd), origin = "1970-01-01", tz = "UTC")
          #idxAgr2 <- 0
          for (idxAgr in 1:length(wrk$idx$idxBgn)){
            #idxAgr <- 124
            #wrk$inpMask for qfqm
            wrk$inpMask$qfqm <- list()
            lapply(names(wrk$qfqm), function (x) wrk$inpMask$qfqm[[x]] <<- wrk$qfqm[[x]][wrk$idx$idxBgn[idxAgr]:wrk$idx$idxEnd[idxAgr],] )
            
            #qfqm processing
            rpt[[idxAgr]] <- eddy4R.qaqc::wrap.neon.dp01.qfqm(
              qfInput = wrk$inpMask$qfqm, 
              MethMeas = "ecse",
              TypeMeas = "samp",
              RptExpd = FALSE,
              dp01 = dp01
            )
            
            #grab and add both time begin and time end to rpt
            rpt[[idxAgr]]$timeBgn <- list()
            rpt[[idxAgr]]$timeEnd <- list()
            
            for(idxVar in names(wrk$data)){
              rpt[[idxAgr]]$timeBgn[[idxVar]] <- wrk$idx$timeBgn[idxAgr]
              rpt[[idxAgr]]$timeEnd[[idxVar]] <- wrk$idx$timeEnd[idxAgr]
            }
            
            #}# end of there is at least one data
            
            
          }; rm(idxAgr)
        } else {
          
          rpt[[1]] <- list()
          
          for(idxQf in NameQf){
            rpt[[1]][[idxQf]] <- list()
            
            
            for (idxVar in names(wrk$data)){
              rpt[[1]][[idxQf]][[idxVar]] <- list()  
            }; rm(idxVar)
            
          }; rm(idxQf) 
          
        }#end of if no measurement data at all in the whole day
      } #end of PrdAgr == 9
      
      if (PrdMeas != PrdAgr) {
        #PrdAgr <- 30
        #if there is at least one measurement
        if(length(which(!is.na(wrk$data$temp))) > 0){
          
          #determine the index of each measurement  
          wrk$idx <- eddy4R.base::def.idx.agr(time = data$time, PrdAgr = (PrdMeas*60), FreqLoca = 1, MethIdx = "specBgn", data = wrk$data$temp, CritTime = 60)
          whrSamp <- wrk$idx$idxBgn[1]:wrk$idx$idxEnd[1]
          if (length (wrk$idx$idxBgn) > 1 ){
            for(ii in 2:length (wrk$idx$idxBgn)){
              whrSamp <- c(whrSamp, wrk$idx$idxBgn[ii]:wrk$idx$idxEnd[ii])
            }
          }
          wrk$qfqm$crdH2o[-whrSamp, 1:length(wrk$qfqm$crdH2o)] <- NaN
        } 
        
        
        for(idxAgr in c(1:length(idxTime[[paste0(PrdAgr, "min")]]$Bgn))) {
          #idxAgr <- 48
          
          
          ## grab data at the selected mask data
          
          idxLvLPrdAgr <- paste0(lvl, "_", sprintf("%02d", PrdAgr), "m")
          
          # for qfqm
          wrk$inpMask$qfqm <- list()
          lapply(names(wrk$qfqm), function (x) wrk$inpMask$qfqm[[x]] <<- wrk$qfqm[[x]][idxTime[[paste0(PrdAgr, "min")]]$Bgn[idxAgr]:idxTime[[paste0(PrdAgr, "min")]]$End[idxAgr],])
          
          #qfqm processing
          rpt[[idxAgr]] <- eddy4R.qaqc::wrap.neon.dp01.qfqm(
            qfInput = wrk$inpMask$qfqm, 
            MethMeas = "ecse",
            TypeMeas = "samp",
            RptExpd = FALSE,
            dp01 = dp01
          )
          
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
      wrk$data <- data.frame(stringsAsFactors = FALSE,
                             "rtioMoleWetH2o" = data$crdH2o[[lvl]]$rtioMoleWetH2o,
                             "rtioMoleDryH2o" = data$crdH2o[[lvl]]$rtioMoleDryH2o,
                             "dlta18OH2o" = data$crdH2o[[lvl]]$dlta18OH2o,
                             "dlta2HH2o" = data$crdH2o[[lvl]]$dlta2HH2o,
                             "temp" = data$crdH2o[[lvl]]$temp,
                             "pres" = data$crdH2o[[lvl]]$pres,
                             "dlta18OH2oRefe" = data$crdH2o[[lvl]]$dlta18OH2oRefe,
                             "dlta2HH2oRefe" = data$crdH2o[[lvl]]$dlta2HH2oRefe,
                             "injNum" = data$crdH2oValvVali[[lvlCrdH2oValvVali]][["injNum"]]
                             
      )
      #replace injNum to NaN when they are not measured at that period
      wrk$data$injNum <- ifelse(is.na(wrk$data$temp), NaN, wrk$data$injNum)
      if (PrdMeas == PrdAgr) {        
        #idxLvLPrdAgr <- paste0(lvl, "_", sprintf("%02d", PrdAgr), "m")
        #rpt[[dp01]][[idxLvLPrdAgr]] <- list()
        
        #if there is at least one measurement
        if(length(which(!is.na(wrk$data$temp))) > 0){
          #determine the end time of each measurement
          wrk$idx <- eddy4R.base::def.idx.agr(time = data$time, PrdAgr = (PrdMeas*60), FreqLoca = 1, MethIdx = "specEnd", crdH2oVali = TRUE, data = wrk$data$injNum, CritTime = 15)
          
          #idxAgr2 <- 0
          for (idxAgr in 1:length(wrk$idx$idxBgn)){
            #idxAgr <- 1
            #determine input data for each idxAgr
            wrk$inpMask$data <- list()
            wrk$inpMask$data <- wrk$data[wrk$idx$idxBgn[idxAgr]:wrk$idx$idxEnd[idxAgr],] 
            #get rid of injNum
            wrk$inpMask$data <- wrk$inpMask$data[,-which(names(wrk$inpMask$data) == "injNum")]
            #calculate dp01
            rpt[[idxAgr]] <- eddy4R.base::wrap.neon.dp01(
              # assign data: data.frame or list of type numeric or integer
              data = wrk$inpMask$data#,
              # if data is a list, which list entries should be processed into Level 1 data products?
              # defaults to NULL which expects data to be a data.frame
              #idx = 
              #names(wrk$inpMask$data[[dp01]]) #"000_010_02m"
            )
            
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
          
          for(idxStat in names(rpt[[dp01]][[1]][[1]])){
            rpt[[1]][[idxStat]] <- list()
            
            for (idxVar in names(rpt[[dp01]][[1]][[1]]$mean)){
              rpt[[1]][[idxStat]][[idxVar]] <- list() 
            }; rm(idxVar)
            
            
          }; rm(idxStat)  
        }#end of if no measurement data at all in the whole day
      }#end of PrdAgr == 3
      
      if (PrdMeas != PrdAgr) {
        #if there is at least one measurement
        if(length(which(!is.na(wrk$data$temp))) > 0){
          #   #determine the end time of each measurement  
          wrk$idx <- eddy4R.base::def.idx.agr(time = data$time, PrdAgr = (PrdMeas*60), FreqLoca = 1, MethIdx = "specEnd", crdH2oVali = TRUE, data = wrk$data$injNum, CritTime = 15)
          whrSamp <- wrk$idx$idxBgn[1]:wrk$idx$idxEnd[1]
          if (length (wrk$idx$idxBgn) > 1 ){
            for(ii in 2:length (wrk$idx$idxBgn)){
              whrSamp <- c(whrSamp, wrk$idx$idxBgn[ii]:wrk$idx$idxEnd[ii])
            }
          }
          wrk$data[-whrSamp, ] <- NaN
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
          #call wrap.neon.dp01.R to calculate descriptive statistics
          
          rpt[[idxAgr]] <- eddy4R.base::wrap.neon.dp01(
            # assign data: data.frame or list of type numeric or integer
            data = wrk$inpMask$data#,
            # if data is a list, which list entries should be processed into Level 1 data products?
            # defaults to NULL which expects data to be a data.frame
            #idx = 
            #names(wrk$inpMask$data[[dp01]]) #"000_010_02m"
          )
          
          
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
  
  #return results
  return(rpt)
  
}#end function wrap.neon.dp01.qfqm.ecse()
