##############################################################################################
#' @title Definition function: Grouping the quality flags for each of NEON ECTE and ECSE L1 data product

#' @author
#' Natchaya Pingintha-Durden \email{ndurden@battelleecology.org}

#' @description Function definition. Grouping the quality flags of each NEON ECTE and ECSE L1 data product into a single dataframe for further use in the calculation of Alpha, Beta, and Final flag.

#' @param \code{qfInput} A list of data frame containing the input quality flag data that related to L1 data products are being grouped. Of class integer". [-] 
#' @param \code{MeasMeth} A vector of class "character" containing the name of measurement method (eddy-covariance turbulent exchange or storage exchange), MeasMeth = c("ecte", "ecse"). Defaults to "ecse". [-] 
#' @param \code{MeasType} A vector of class "character" containing the name of measurement type (sampling or validation), MeasType = c("samp", "ecse"). Defaults to "samp". [-]
#' @param \code{dp01} A vector of class "character" containing the name of NEON ECTE and ECSE L1 data products which the flags are being grouped, \cr
#' c("envHut", "irgaCo2", "irgaH2o", "isoCo2", "isoH2o", "soni", "soniAmrs", "tempAirLvl", "tempAirTop"). Defaults to "irgaCo2". [-] 

#' @return A list of data frame of the quality flags related to that sub-data product. \cr

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

#' @keywords NEON QAQC, quality flags, quality metrics

#' @examples 
#' #generate the fake quality flags for each sensor
#' TimeBgn <- "2016-04-24 02:00:00.000"
#' TimeEnd <- "2016-04-24 02:29:59.950"
#' qf <- list()
#' qf$irga <- eddy4R.qaqc::def.qf.ecte(TimeBgn = TimeBgn, TimeEnd = TimeEnd, Freq = 20, Sens = "irga", PcntQf = 0.05)
#' qf$irgaMfcSamp <- eddy4R.qaqc::def.qf.ecte(TimeBgn = TimeBgn, TimeEnd = TimeEnd, Freq = 20, Sens = "irgaMfcSamp", PcntQf = 0.05)
#' qf$soni <- eddy4R.qaqc::def.qf.ecte(TimeBgn = TimeBgn, TimeEnd = TimeEnd, Freq = 20, Sens = "soni", PcntQf = 0.05)
#' qf$soniAmrs <- eddy4R.qaqc::def.qf.ecte(TimeBgn = TimeBgn, TimeEnd = TimeEnd, Freq = 40, Sens = "soniAmrs", PcntQf = 0.05)
#' 
#' #grouping the set of the flags
#' qfGrpIrgaCo2 <- def.neon.dp01.qf.grp(qfInput = qf, MeasMeth = "ecte", MeasType = "vali", dp01="irgaCo2")
#' qfGrpSoni <- def.neon.dp01.qf.grp(qfInput = qf, MeasMeth = "ecte", MeasType = "samp", dp01="soni")

#' @seealso Currently none

#' @export

# changelog and author contributions / copyrights
#   Natchaya P-Durden (2016-12-01)
#     original creation
#   Natchaya P-Durden (2016-12-09)
#     added flexibility if some sensor is missing
#   Natchaya P-Durden (2017-03-21)
#     revised original function to def.neon.dp01.qf.grp ()
#     added ECSE quality flags
##############################################################################################

def.neon.dp01.qf.grp <- function(
  qfInput = list(),
  MeasMeth = c("ecte", "ecse")[1],
  MeasType = c("samp", "vali")[1], 
  dp01 = c("envHut", "irgaCo2", "irgaH2o", "isoCo2", "isoH2o", "soni", "soniAmrs", "tempAirLvl", "tempAirTop")[1]
){
  
  rpt <- list()
  setQf <- list()
# ecte #######################################################################################
if (MeasMeth == "ecte") {
#irgaCo2 and irgaH2o####################################################################################
  if (dp01 %in% c("irgaCo2", "irgaH2o")) {
    #organized all quality flags from irga into the set of flags (for frequency use)
    #irga sensor flags
    setQf$sens <- data.frame("qfIrgaHead" = qfInput$irga$qfIrgaHead, 
                             "qfIrgaTemp" = qfInput$irga$qfIrgaTemp, 
                             "qfIrgaTempIn" = qfInput$irga$qfIrgaTempIn,
                             "qfIrgaAux" = qfInput$irga$qfIrgaAux, 
                             "qfIrgaPres" = qfInput$irga$qfIrgaPres, 
                             "qfIrgaChop" = qfInput$irga$qfIrgaChop, 
                             "qfIrgaDetc" = qfInput$irga$qfIrgaDetc, 
                             "qfIrgaPll" = qfInput$irga$qfIrgaPll, 
                             "qfIrgaSync" = qfInput$irga$qfIrgaSync, 
                             "qfIrgaAgc" = qfInput$irga$qfIrgaAgc)
    
    setQf$asrpCo2 <- data.frame("qfRngMinAsrpCo2" = qfInput$irga$qfRngMinAsrpCo2, 
                                "qfStepAsrpCo2" = qfInput$irga$qfStepAsrpCo2, 
                                "qfPersAsrpCo2" = qfInput$irga$qfPersAsrpCo2, 
                                "qfCalAsrpCo2" = qfInput$irga$qfCalAsrpCo2)
    
    setQf$asrpH2o <- data.frame("qfRngMinAsrpH2o" = qfInput$irga$qfRngMinAsrpH2o, 
                                "qfStepAsrpH2o" = qfInput$irga$qfStepAsrpH2o, 
                                "qfPersAsrpH2o" = qfInput$irga$qfPersAsrpH2o, 
                                "qfCalAsrpH2o" = qfInput$irga$qfCalAsrpH2o)
    
    setQf$ssiCo2 <- data.frame("qfRngMinSsiCo2" = qfInput$irga$qfRngMinSsiCo2, 
                               "qfStepSsiCo2" = qfInput$irga$qfStepSsiCo2,
                               "qfPersSsiCo2" = qfInput$irga$qfPersSsiCo2, 
                               "qfCalSsiCo2" = qfInput$irga$qfCalSsiCo2)
    setQf$ssiH2o <- data.frame("qfRngMinSsiH2o" = qfInput$irga$qfRngMinSsiH2o, 
                               "qfStepSsiH2o" = qfInput$irga$qfStepSsiH2o,
                               "qfPersSsiH2o" = qfInput$irga$qfPersSsiH2o, 
                               "qfCalSsiH2o" = qfInput$irga$qfCalSsiH2o)
    
    setQf$rtioMoleDryCo2 <- data.frame("qfRngMinRtioMoleDryCo2" = qfInput$irga$qfRngMinRtioMoleDryCo2, 
                                       "qfStepRtioMoleDryCo2" = qfInput$irga$qfStepRtioMoleDryCo2,
                                       "qfPersRtioMoleDryCo2" = qfInput$irga$qfPersRtioMoleDryCo2, 
                                       "qfCalRtioMoleDryCo2" = qfInput$irga$qfCalRtioMoleDryCo2)
    setQf$rtioMoleDryH2o <- data.frame("qfRngMinRtioMoleDryH2o" = qfInput$irga$qfRngMinRtioMoleDryH2o, 
                                       "qfStepRtioMoleDryH2o" = qfInput$irga$qfStepRtioMoleDryH2o,
                                       "qfPersRtioMoleDryH2o" = qfInput$irga$qfPersRtioMoleDryH2o, 
                                       "qfCalRtioMoleDryH2o" = qfInput$irga$qfCalRtioMoleDryH2o)
    
    setQf$densMoleCo2 <- data.frame("qfRngMinDensMoleCo2" = qfInput$irga$qfRngMinDensMoleCo2, 
                                    "qfStepDensMoleCo2" = qfInput$irga$qfStepDensMoleCo2,
                                    "qfPersDensMoleCo2" = qfInput$irga$qfPersDensMoleCo2, 
                                    "qfCalDensMoleCo2" = qfInput$irga$qfCalDensMoleCo2) 
    
    setQf$densMoleH2o <- data.frame("qfRngMinDensMoleH2o" = qfInput$irga$qfRngMinDensMoleH2o, 
                                    "qfStepDensMoleH2o" = qfInput$irga$qfStepDensMoleH2o,
                                    "qfPersDensMoleH2o" = qfInput$irga$qfPersDensMoleH2o, 
                                    "qfCalDensMoleH2o" = qfInput$irga$qfCalDensMoleH2o)
    
    setQf$presAtm <- data.frame("qfRngMinPresAtm" = qfInput$irga$qfRngMinPresAtm, 
                                "qfStepPresAtm" = qfInput$irga$qfStepPresAtm,
                                "qfPersPresAtm" = qfInput$irga$qfPersPresAtm, 
                                "qfCalPresAtm" = qfInput$irga$qfCalPresAtm)
    
    setQf$presSum <- data.frame("qfRngMinPresSum" = qfInput$irga$qfRngMinPresSum, 
                                "qfStepPresSum" = qfInput$irga$qfStepPresSum,
                                "qfPersPresSum" = qfInput$irga$qfPersPresSum, 
                                "qfCalPresSum" = qfInput$irga$qfCalPresSum)
    
    setQf$tempAve <- data.frame ("qfRngMinTempMean" = qfInput$irga$qfRngMinTempMean, 
                                 "qfStepTempMean" = qfInput$irga$qfStepTempMean,
                                 "qfPersTempMean" = qfInput$irga$qfPersTempMean, 
                                 "qfCalTempMean" = qfInput$irga$qfCalTempMean)
#using irgaMfcSamp during sampling period
 if (MeasType == "samp") {
    if (!is.null(qfInput$irgaMfcSamp)){
      #irgaMfcSamp
      setQf$frt00 <- data.frame("qfRngMinFrt00" = qfInput$irgaMfcSamp$qfRngMinFrt00, 
                                "qfStepFrt00" = qfInput$irgaMfcSamp$qfStepFrt00, 
                                "qfPersFrt00" = qfInput$irgaMfcSamp$qfPersFrt00, 
                                "qfCalFrt00" = qfInput$irgaMfcSamp$qfCalFrt00)
      setQf$frt <- data.frame("qfRngMinFrt" = qfInput$irgaMfcSamp$qfRngMinFrt,
                              "qfStepFrt" = qfInput$irgaMfcSamp$qfStepFrt,
                              "qfPersFrt" = qfInput$irgaMfcSamp$qfPersFrt,
                              "qfCalFrt" = qfInput$irgaMfcSamp$qfCalFrt)
      setQf$mfcPresAtm <- data.frame("qfRngMinPresAtm" = qfInput$irgaMfcSamp$qfRngMinPresAtm, 
                                     "qfStepPresAtm" = qfInput$irgaMfcSamp$qfStepPresAtm,
                                     "qfPersPresAtm" = qfInput$irgaMfcSamp$qfPersPresAtm, 
                                     "qfCalPresAtm" = qfInput$irgaMfcSamp$qfCalPresAtm)
      setQf$mfcTemp <- data.frame("qfRngTemp" = qfInput$irgaMfcSamp$qfRngMinTemp,
                                  "qfStepTemp" = qfInput$irgaMfcSamp$qfStepTemp,
                                  "qfPersTemp" = qfInput$irgaMfcSamp$qfPersTemp,
                                  "qfCalTemp" = qfInput$irgaMfcSamp$qfCalTemp)
      } else {
      #assign qf for irgaMfcSamp to -1 when qf irgaMfcSamp are missing
      setQf$frt00 <- data.frame("qfRngMinFrt00" = -1, 
                                "qfStepFrt00" = -1, 
                                "qfPersFrt00" = -1, 
                                "qfCalFrt00" = -1)
      setQf$frt <- data.frame("qfRngMinFrt" = -1,
                              "qfStepFrt" = -1,
                              "qfPersFrt" = -1,
                              "qfCalFrt" = -1)
      setQf$mfcPresAtm <- data.frame("qfRngMinPresAtm" = -1, 
                                     "qfStepPresAtm" = -1,
                                     "qfPersPresAtm" = -1, 
                                     "qfCalPresAtm" = -1)
      setQf$mfcTemp <- data.frame("qfRngTemp" = -1,
                                  "qfStepTemp" = -1,
                                  "qfPersTemp" = -1,
                                  "qfCalTemp" = -1)
    }
 } # closed MeasType loop
    
#using irgaMfcSamp during sampling period
 if (MeasType == "vali") {
    if (!is.null(qfInput$irgaMfcVali)){
      #irgaMfcVali
      setQf$frt00 <- data.frame("qfRngMinFrt00" = qfInput$irgaMfcVali$qfRngMinFrt00, 
                                "qfStepFrt00" = qfInput$irgaMfcVali$qfStepFrt00, 
                                "qfPersFrt00" = qfInput$irgaMfcVali$qfPersFrt00, 
                                "qfCalFrt00" = qfInput$irgaMfcVali$qfCalFrt00)
      setQf$frt <- data.frame("qfRngMinFrt" = qfInput$irgaMfcVali$qfRngMinFrt,
                              "qfStepFrt" = qfInput$irgaMfcVali$qfStepFrt,
                              "qfPersFrt" = qfInput$irgaMfcVali$qfPersFrt,
                              "qfCalFrt" = qfInput$irgaMfcVali$qfCalFrt)
      setQf$mfcPresAtm <- data.frame("qfRngMinPresAtm" = qfInput$irgaMfcVali$qfRngMinPresAtm, 
                                     "qfStepPresAtm" = qfInput$irgaMfcVali$qfStepPresAtm,
                                     "qfPersPresAtm" = qfInput$irgaMfcVali$qfPersPresAtm, 
                                     "qfCalPresAtm" = qfInput$irgaMfcVali$qfCalPresAtm)
      setQf$mfcTemp <- data.frame("qfRngTemp" = qfInput$irgaMfcVali$qfRngMinTemp,
                                  "qfStepTemp" = qfInput$irgaMfcVali$qfStepTemp,
                                  "qfPersTemp" = qfInput$irgaMfcVali$qfPersTemp,
                                  "qfCalTemp" = qfInput$irgaMfcVali$qfCalTemp)
      } else {
      #assign qf for irgaMfcSamp to -1 when qf irgaMfcSamp are missing
      setQf$frt00 <- data.frame("qfRngMinFrt00" = -1, 
                                "qfStepFrt00" = -1, 
                                "qfPersFrt00" = -1, 
                                "qfCalFrt00" = -1)
      setQf$frt <- data.frame("qfRngMinFrt" = -1,
                              "qfStepFrt" = -1,
                              "qfPersFrt" = -1,
                              "qfCalFrt" = -1)
      setQf$mfcPresAtm <- data.frame("qfRngMinPresAtm" = -1, 
                                     "qfStepPresAtm" = -1,
                                     "qfPersPresAtm" = -1, 
                                     "qfCalPresAtm" = -1)
      setQf$mfcTemp <- data.frame("qfRngTemp" = -1,
                                  "qfStepTemp" = -1,
                                  "qfPersTemp" = -1,
                                  "qfCalTemp" = -1)
      }
 } # closed MeasType loop
    
    #grouping qulity flags that related to irgaCo2 L1 sub-data product
    if (dp01 == "irgaCo2") {
    rpt$rtioMoleDryCo2 <- data.frame(setQf$sens, setQf$asrpCo2, 
                                     setQf$ssiCo2, setQf$rtioMoleDryCo2,
                                     setQf$frt00, setQf$frt,
                                     setQf$mfcPresAtm, setQf$mfcTemp)
    rpt$densMoleCo2 <- data.frame(setQf$sens, setQf$asrpCo2, 
                                  setQf$ssiCo2,setQf$densMoleCo2,
                                  setQf$frt00, setQf$frt,
                                  setQf$mfcPresAtm, setQf$mfcTemp)
    rpt$presAtm <- data.frame(setQf$sens, setQf$presAtm)
    rpt$presSum <- data.frame(setQf$sens, setQf$presSum)
    rpt$frt00Samp <- data.frame (setQf$frt00, setQf$frt00, 
                                 setQf$frt, setQf$mfcPresAtm, 
                                 setQf$mfcTemp)
    rpt$tempAve <- data.frame (setQf$sens, setQf$tempAve)
    }
    #grouping qulity flags that related to irgaH2o L1 sub-data product    
    if (dp01 == "irgaH2o") {
    rpt$rtioMoleDryH2o <- data.frame(setQf$sens, setQf$asrpH2o, 
                                     setQf$ssiH2o, setQf$rtioMoleDryH2o,
                                     setQf$frt00, setQf$frt,
                                     setQf$mfcPresAtm, setQf$mfcTemp)
    rpt$densMoleH2o <- data.frame(setQf$sens, setQf$asrpH2o, 
                                  setQf$ssiH2o,setQf$densMoleH2o,
                                  setQf$frt00, setQf$frt,
                                  setQf$mfcPresAtm, setQf$mfcTemp)
    #need to update which flags will be used for tempDew
    rpt$tempDew <- data.frame(setQf$sens, setQf$asrpH2o,
                              setQf$ssiH2o, setQf$frt00, setQf$frt,
                              setQf$mfcPresAtm, setQf$mfcTemp)
    rpt$presAtm <- data.frame(setQf$sens, setQf$presAtm)
    rpt$presSum <- data.frame(setQf$sens, setQf$presSum)
    rpt$frt00 <- data.frame (setQf$frt00, setQf$frt00, setQf$frt,
                             setQf$mfcPresAtm, setQf$mfcTemp)
    rpt$tempAve <- data.frame (setQf$sens, setQf$tempAve)  
    }
    #remove setQf
    setQf <- NULL
  } #closed loop for dp01 irgaCo2 and irgaH2o
  

#soni#################################################################################  
  if (dp01 == "soni") {
    #organized all quality flags from soni into the set of flags (for frequency use)
    #soni sensor flags
    setQf$sensSoni <- data.frame("qfSoniUnrs" = qfInput$soni$qfSoniUnrs, 
                                 "qfSoniData" = qfInput$soni$qfSoniData,
                                 "qfSoniTrig" = qfInput$soni$qfSoniTrig, 
                                 "qfSoniComm" = qfInput$soni$qfSoniComm,
                                 "qfSoniCode" = qfInput$soni$qfSoniCode, 
                                 "qfSoniTemp" = qfInput$soni$qfSoniTemp,
                                 "qfSoniSgnlPoor" = qfInput$soni$qfSoniSgnlPoor, 
                                 "qfSoniSgnlHigh" = qfInput$soni$qfSoniSgnlHigh,
                                 "qfSoniSgnlLow" = qfInput$soni$qfSoniSgnlLow) 
    #qf for along-axis wind speed
    setQf$veloXaxs <- data.frame("qfRngMinVeloXaxs" = qfInput$soni$qfRngMinVeloXaxs,
                                 "qfStepVeloXaxs" = qfInput$soni$qfStepVeloXaxs,
                                 "qfPersVeloXaxs" = qfInput$soni$qfPersVeloXaxs,
                                 "qfCalVeloXaxs" = qfInput$soni$qfCalVeloXaxs)
    
    #qf for cross-axis wind speed
    setQf$veloYaxs <- data.frame("qfRngMinVeloYaxs" = qfInput$soni$qfRngMinVeloYaxs,
                                 "qfStepVeloYaxs" = qfInput$soni$qfStepVeloYaxs,
                                 "qfPersVeloYaxs" = qfInput$soni$qfPersVeloYaxs,
                                 "qfCalVeloYaxs" = qfInput$soni$qfCalVeloYaxs)
    #qf for vertical-axis wind speed
    setQf$veloZaxs <- data.frame("qfRngMinVeloZaxs" = qfInput$soni$qfRngMinVeloZaxs,
                                 "qfStepVeloZaxs" = qfInput$soni$qfStepVeloZaxs,
                                 "qfPersVeloZaxs" = qfInput$soni$qfPersVeloZaxs,
                                 "qfCalVeloZaxs" = qfInput$soni$qfCalVeloZaxs)
    #qf for soic temperature
    setQf$tempSoni <- data.frame("qfRngMinTempSoni" = qfInput$soni$qfRngMinTempSoni,
                                 "qfStepTempSoni" = qfInput$soni$qfStepTempSoni,
                                 "qfPersTempSoni" = qfInput$soni$qfPersTempSoni,   
                                 "qfCalTempSoni" = qfInput$soni$qfCalTempSoni)
    #irga sensor flags
    setQf$sensIrga <- data.frame("qfIrgaHead" = qfInput$irga$qfIrgaHead, 
                                 "qfIrgaTemp" = qfInput$irga$qfIrgaTemp, 
                                 "qfIrgaTempIn" = qfInput$irga$qfIrgaTempIn,
                                 "qfIrgaAux" = qfInput$irga$qfIrgaAux, 
                                 "qfIrgaPres" = qfInput$irga$qfIrgaPres, 
                                 "qfIrgaChop" = qfInput$irga$qfIrgaChop, 
                                 "qfIrgaDetc" = qfInput$irga$qfIrgaDetc, 
                                 "qfIrgaPll" = qfInput$irga$qfIrgaPll, 
                                 "qfIrgaSync" = qfInput$irga$qfIrgaSync, 
                                 "qfIrgaAgc" = qfInput$irga$qfIrgaAgc)
    
    #grouping qulity flags that related to L1 sub-data product
    rpt$veloXaxsErth <- data.frame(setQf$sensSoni, setQf$veloXaxs)
    rpt$veloYaxsErth <- data.frame(setQf$sensSoni, setQf$veloYaxs)
    rpt$veloZaxsErth <- data.frame(setQf$sensSoni, setQf$veloZaxs)
    rpt$veloXaxsYaxsErth <- data.frame(setQf$sensSoni, setQf$veloXaxs, setQf$veloYaxs)
    rpt$angZaxsErth <- data.frame(setQf$sensSoni, setQf$veloXaxs, setQf$veloYaxs)
    rpt$tempSoni <- data.frame (setQf$sensSoni, setQf$tempSoni)
    #need to update this set of qf once we know for sure which parameters are used to calculate tempAir
    rpt$tempAir <- data.frame (setQf$sensSoni, setQf$tempSoni, setQf$sensIrga)
    #remove setQf
    setQf <- NULL 
  }                        
  
#soniAmrs#################################################################################    
  if (dp01 == "soniAmrs") {
    
    #organized all quality flags from soni into the set of flags (for frequency use)
    #soni sensor flags
    setQf$sens <- data.frame("qfAmrsVal" = qfInput$soniAmrs$qfAmrsVal,
                             "qfAmrsFilt" =  qfInput$soniAmrs$qfAmrsFilt,
                             "qfAmrsVelo" =  qfInput$soniAmrs$qfAmrsVelo,
                             "qfAmrsRng" =  qfInput$soniAmrs$qfAmrsRng)
    setQf$angXaxs <- data.frame("qfRngMinAngXaxs" = qfInput$soniAmrs$qfRngMinAngXaxs,
                                "qfStepAngXaxs" = qfInput$soniAmrs$qfStepAngXaxs,
                                "qfPersAngXaxs" = qfInput$soniAmrs$qfPersAngXaxs,
                                "qfCalAngXaxs" = qfInput$soniAmrs$qfCalAngXaxs)
    setQf$angYaxs <- data.frame("qfRngMinAngYaxs" = qfInput$soniAmrs$qfRngMinAngYaxs,
                                "qfStepAngYaxs" = qfInput$soniAmrs$qfStepAngYaxs,
                                "qfPersAngYaxs" = qfInput$soniAmrs$qfPersAngYaxs,
                                "qfCalAngYaxs" = qfInput$soniAmrs$qfCalAngYaxs)
    setQf$angZaxs <- data.frame("qfRngMinAngZaxs" = qfInput$soniAmrs$qfRngMinAngZaxs,
                                "qfStepAngZaxs" = qfInput$soniAmrs$qfStepAngZaxs,
                                "qfPersAngZaxs" = qfInput$soniAmrs$qfPersAngZaxs,
                                "qfCalAngZaxs" = qfInput$soniAmrs$qfCalAngZaxs)
    
    #grouping qulity flags that related to L1 sub-data product
    rpt$angNedXaxs <- data.frame(setQf$sens, setQf$angXaxs)
    rpt$angNedYaxs <- data.frame(setQf$sens, setQf$angYaxs)
    rpt$angNedZaxs <- data.frame(setQf$sens, setQf$angZaxs)
    
  }
} # closed loop for ecte
  return(rpt)
  
  # end function def.neon.ecte.dp01.qf.grup()
}
