##############################################################################################
#' @title Definition function: Grouping the quality flags for each of NEON ECTE and ECSE L1 data product

#' @author
#' Natchaya Pingintha-Durden \email{ndurden@battelleecology.org}

#' @description Function definition. Grouping the quality flags of each NEON ECTE and ECSE L1 data product into a single dataframe for further use in the calculation of Alpha, Beta, and Final flag.

#' @param \code{qfInput} A list of data frame containing the input quality flag data that related to L1 data products are being grouped. Of class integer". [-] 
#' @param \code{MethMeas} A vector of class "character" containing the name of measurement method (eddy-covariance turbulent exchange or storage exchange), MethMeas = c("ecte", "ecse"). Defaults to "ecse". [-] 
#' @param \code{TypeMeas} A vector of class "character" containing the name of measurement type (sampling or validation), TypeMeas = c("samp", "ecse"). Defaults to "samp". [-]
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
#' qfGrpIrgaCo2 <- def.neon.dp01.qf.grp(qfInput = qf, MethMeas = "ecte", TypeMeas = "vali", dp01="irgaCo2")
#' qfGrpSoni <- def.neon.dp01.qf.grp(qfInput = qf, MethMeas = "ecte", TypeMeas = "samp", dp01="soni")

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
  MethMeas = c("ecte", "ecse")[1],
  TypeMeas = c("samp", "vali")[1], 
  dp01 = c("envHut", "irgaCo2", "irgaH2o", "isoCo2", "isoH2o", "soni", "soniAmrs", "tempAirLvl", "tempAirTop")[1]
){
  
  rpt <- list()
  setQf <- list()
# ecte #######################################################################################
if (MethMeas == "ecte") {
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
 if (TypeMeas == "samp") {
    if (!is.null(qfInput$irgaMfcSamp)){
      #irgaMfcSamp
      setQf$frt00 <- data.frame("qfRngMinFrt00" = qfInput$irgaMfcSamp$qfRngMinFrt00, 
                                "qfStepFrt00" = qfInput$irgaMfcSamp$qfStepFrt00, 
                                "qfPersFrt00" = qfInput$irgaMfcSamp$qfPersFrt00)
      setQf$frt <- data.frame("qfRngMinFrt" = qfInput$irgaMfcSamp$qfRngMinFrt,
                              "qfStepFrt" = qfInput$irgaMfcSamp$qfStepFrt,
                              "qfPersFrt" = qfInput$irgaMfcSamp$qfPersFrt)
      setQf$presAtmMfc <- data.frame("qfRngMinPresAtm" = qfInput$irgaMfcSamp$qfRngMinPresAtm, 
                                     "qfStepPresAtm" = qfInput$irgaMfcSamp$qfStepPresAtm,
                                     "qfPersPresAtm" = qfInput$irgaMfcSamp$qfPersPresAtm)
      setQf$tempMfc <- data.frame("qfRngMinTemp" = qfInput$irgaMfcSamp$qfRngMinTemp,
                                  "qfStepTemp" = qfInput$irgaMfcSamp$qfStepTemp,
                                  "qfPersTemp" = qfInput$irgaMfcSamp$qfPersTemp)
      } else {
      #assign qf for irgaMfcSamp to -1 when qf irgaMfcSamp is missing
      setQf$frt00 <- data.frame("qfRngMinFrt00" = -1, 
                                "qfStepFrt00" = -1, 
                                "qfPersFrt00" = -1)
      setQf$frt <- data.frame("qfRngMinFrt" = -1,
                              "qfStepFrt" = -1,
                              "qfPersFrt" = -1)
      setQf$presAtmMfc <- data.frame("qfRngMinPresAtm" = -1, 
                                     "qfStepPresAtm" = -1,
                                     "qfPersPresAtm" = -1)
      setQf$tempMfc <- data.frame("qfRngMinTemp" = -1,
                                  "qfStepTemp" = -1,
                                  "qfPersTemp" = -1)
    }
 } # closed MeasType loop
    
#using irgaMfcVali during validatio period
 if (TypeMeas == "vali") {
    if (!is.null(qfInput$irgaMfcVali)){
      #irgaMfcVali
      setQf$frt00 <- data.frame("qfRngMinFrt00" = qfInput$irgaMfcVali$qfRngMinFrt00, 
                                "qfStepFrt00" = qfInput$irgaMfcVali$qfStepFrt00, 
                                "qfPersFrt00" = qfInput$irgaMfcVali$qfPersFrt00)
      setQf$frt <- data.frame("qfRngMinFrt" = qfInput$irgaMfcVali$qfRngMinFrt,
                              "qfStepFrt" = qfInput$irgaMfcVali$qfStepFrt,
                              "qfPersFrt" = qfInput$irgaMfcVali$qfPersFrt)
      setQf$presAtmMfc <- data.frame("qfRngMinPresAtm" = qfInput$irgaMfcVali$qfRngMinPresAtm, 
                                     "qfStepPresAtm" = qfInput$irgaMfcVali$qfStepPresAtm,
                                     "qfPersPresAtm" = qfInput$irgaMfcVali$qfPersPresAtm)
      setQf$tempMfc <- data.frame("qfRngMinTemp" = qfInput$irgaMfcVali$qfRngMinTemp,
                                  "qfStepTemp" = qfInput$irgaMfcVali$qfStepTemp,
                                  "qfPersTemp" = qfInput$irgaMfcVali$qfPersTemp)
      } else {
      #assign qf for irgaMfcVali to -1 when qf irgaMfcVali is missing
      setQf$frt00 <- data.frame("qfRngMinFrt00" = -1, 
                                "qfStepFrt00" = -1, 
                                "qfPersFrt00" = -1)
      setQf$frt <- data.frame("qfRngMinFrt" = -1,
                              "qfStepFrt" = -1,
                              "qfPersFrt" = -1)
      setQf$presAtmMfc <- data.frame("qfRngMinPresAtm" = -1, 
                                     "qfStepPresAtm" = -1,
                                     "qfPersPresAtm" = -1)
      setQf$tempMfc <- data.frame("qfRngMinTemp" = -1,
                                  "qfStepTemp" = -1,
                                  "qfPersTemp" = -1)
      }
 } # closed MeasType loop
    
    #grouping qulity flags that related to irgaCo2 L1 sub-data product
    if (dp01 == "irgaCo2") {
    rpt$rtioMoleDryCo2 <- data.frame(setQf$rtioMoleDryCo2, setQf$sens, 
                                setQf$asrpCo2, setQf$ssiCo2,
                                setQf$frt00, setQf$frt,
                                setQf$presAtmMfc, setQf$tempMfc)
    rpt$densMoleCo2 <- data.frame(setQf$densMoleCo2, setQf$sens, 
                             setQf$asrpCo2, setQf$ssiCo2,
                             setQf$frt00, setQf$frt,
                             setQf$presAtmMfc, setQf$tempMfc)
    rpt$presAtm <- data.frame(setQf$presAtm, setQf$sens)
    rpt$presSum <- data.frame(setQf$presSum, setQf$sens)
    rpt$frt00Samp <- data.frame (setQf$frt00, setQf$frt, 
                            setQf$presAtmMfc, setQf$tempMfc)
    rpt$tempAve <- data.frame (setQf$tempAve, setQf$sens)
    }
    #grouping qulity flags that related to irgaH2o L1 sub-data product    
    if (dp01 == "irgaH2o") {
    rpt$rtioMoleDryH2o <- data.frame(setQf$rtioMoleDryH2o, setQf$sens, 
                                setQf$asrpH2o, setQf$ssiH2o, 
                                setQf$frt00, setQf$frt,
                                setQf$presAtmMfc, setQf$tempMfc)
    rpt$densMoleH2o <- data.frame(setQf$densMoleH2o, setQf$sens, 
                             setQf$asrpH2o, setQf$ssiH2o,
                             setQf$frt00, setQf$frt,
                             setQf$presAtmMfc, setQf$tempMfc)
    #need to update which flags will be used for tempDew
    rpt$tempDew <- rpt$rtioMoleDryH2o
    rpt$presAtm <- data.frame(setQf$presAtm, setQf$sens)
    rpt$presSum <- data.frame(setQf$presSum, setQf$sens)
    rpt$frt00 <- data.frame (setQf$frt00, setQf$frt,
                        setQf$presAtmMfc, setQf$tempMfc)
    rpt$tempAve <- data.frame (setQf$tempAve, setQf$sens)  
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
    rpt$veloXaxsErth <- data.frame(setQf$veloXaxs, setQf$sensSoni)
    rpt$veloYaxsErth <- data.frame(setQf$veloYaxs, setQf$sensSoni)
    rpt$veloZaxsErth <- data.frame(setQf$veloZaxs, setQf$sensSoni)
    rpt$veloXaxsYaxsErth <- data.frame(setQf$sensSoni, setQf$veloXaxs, setQf$veloYaxs)
    rpt$angZaxsErth <- data.frame(setQf$sensSoni, setQf$veloXaxs, setQf$veloYaxs)
    rpt$tempSoni <- data.frame (setQf$tempSoni, setQf$sensSoni)
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
    rpt$angNedXaxs <- data.frame(setQf$angXaxs, setQf$sens)
    rpt$angNedYaxs <- data.frame(setQf$angYaxs, setQf$sens)
    rpt$angNedZaxs <- data.frame(setQf$angZaxs, setQf$sens)
    
  }
} # closed loop for ecte

# ecse #######################################################################################
if (MethMeas == "ecse") {  
#irgaCo2 and irgaH2o####################################################################################
  if (dp01 %in% c("irgaCo2", "irgaH2o")) {  
    
    setQf$asrpCo2 <- data.frame("qfRngMinAsrpCo2" = qfInput$irga$qfRngMinAsrpCo2, 
                                "qfStepAsrpCo2" = qfInput$irga$qfStepAsrpCo2, 
                                "qfPersAsrpCo2" = qfInput$irga$qfPersAsrpCo2, 
                                "qfCalAsrpCo2" = qfInput$irga$qfCalAsrpCo2)
    
    setQf$asrpH2o <- data.frame("qfRngMinAsrpH2o" = qfInput$irga$qfRngMinAsrpH2o, 
                                "qfStepAsrpH2o" = qfInput$irga$qfStepAsrpH2o, 
                                "qfPersAsrpH2o" = qfInput$irga$qfPersAsrpH2o, 
                                "qfCalAsrpH2o" = qfInput$irga$qfCalAsrpH2o)
    
    setQf$rtioMoleDryCo2 <- data.frame("qfRngMinRtioMoleDryCo2" = qfInput$irga$qfRngMinRtioMoleDryCo2, 
                                       "qfStepRtioMoleDryCo2" = qfInput$irga$qfStepRtioMoleDryCo2,
                                       "qfPersRtioMoleDryCo2" = qfInput$irga$qfPersRtioMoleDryCo2, 
                                       "qfCalRtioMoleDryCo2" = qfInput$irga$qfCalRtioMoleDryCo2)
    
    setQf$rtioMoleDryH2o <- data.frame("qfRngMinRtioMoleDryH2o" = qfInput$irga$qfRngMinRtioMoleDryH2o, 
                                       "qfStepRtioMoleDryH2o" = qfInput$irga$qfStepRtioMoleDryH2o,
                                       "qfPersRtioMoleDryH2o" = qfInput$irga$qfPersRtioMoleDryH2o, 
                                       "qfCalRtioMoleDryH2o" = qfInput$irga$qfCalRtioMoleDryH2o)
    
    setQf$rtioMoleWetCo2 <- data.frame("qfRngMinRtioMoleWetCo2" = qfInput$irga$qfRngMinRtioMoleWetCo2,
                                       "qfStepRtioMoleWetCo2" = qfInput$irga$qfStepRtioMoleWetCo2,
                                       "qfPersRtioMoleWetCo2" = qfInput$irga$qfPersRtioMoleWetCo2,
                                       "qfCalsRtioMoleWetCo2" = qfInput$irga$qfCalRtioMoleWetCo2)
    
    setQf$rtioMoleWetH2o <- data.frame("qfRngMinRtioMoleWetH2o" = qfInput$irga$qfRngMinRtioMoleWetH2o,
                                       "qfStepRtioMoleWetH2o" = qfInput$irga$qfStepRtioMoleWetH2o,
                                       "qfPersRtioMoleWetH2o" = qfInput$irga$qfPersRtioMoleWetH2o,
                                       "qfCalsRtioMoleWetH2o" = qfInput$irga$qfCalRtioMoleWetH2o)
    
    setQf$presIrga <- data.frame("qfRngMinPresS" = qfInput$irga$qfRngMinPres, 
                                "qfStepPres" = qfInput$irga$qfStepPres,
                                "qfPersPres" = qfInput$irga$qfPersPres, 
                                "qfCalPres" = qfInput$irga$qfCalPres)
    
    setQf$tempIrga <- data.frame ("qfRngMinTemp" = qfInput$irga$qfRngMinTemp, 
                                 "qfStepTemp" = qfInput$irga$qfStepTemp,
                                 "qfPersTemp" = qfInput$irga$qfPersTemp, 
                                 "qfCalTemp" = qfInput$irga$qfCalTemp)
    
    #using irgaMfcSamp during sampling period
    if (TypeMeas == "samp") {
      #set of qf from envHut, heater, and valvAux
      setQf$sensIrga <- data.frame("qfTemp" = ifelse(!is.null(qfInput$envHut), qfInput$envHut$qfTemp, -1),#qf from envHut
                                   "qfHeat" = ifelse(!is.null(qfInput$heatInlt), qfInput$heatInlt$qfHeat, -1),
                                   "qfValvIrga" = ifelse(!is.null(qfInput$valvAux), qfInput$valvAux$qfValvIrga, -1))
      if (!is.null(qfInput$irgaMfcSamp)){
        #irgaMfcSamp
        setQf$frt00 <- data.frame("qfRngMinFrt00" = qfInput$irgaMfcSamp$qfRngMinFrt00, 
                                  "qfStepFrt00" = qfInput$irgaMfcSamp$qfStepFrt00, 
                                  "qfPersFrt00" = qfInput$irgaMfcSamp$qfPersFrt00)
        setQf$frt <- data.frame("qfRngMinFrt" = qfInput$irgaMfcSamp$qfRngMinFrt,
                                "qfStepFrt" = qfInput$irgaMfcSamp$qfStepFrt,
                                "qfPersFrt" = qfInput$irgaMfcSamp$qfPersFrt)
        setQf$presAtmMfc <- data.frame("qfRngMinPresAtm" = qfInput$irgaMfcSamp$qfRngMinPresAtm, 
                                       "qfStepPresAtm" = qfInput$irgaMfcSamp$qfStepPresAtm,
                                       "qfPersPresAtm" = qfInput$irgaMfcSamp$qfPersPresAtm)
        setQf$tempMfc <- data.frame("qfRngMinTemp" = qfInput$irgaMfcSamp$qfRngMinTemp,
                                    "qfStepTemp" = qfInput$irgaMfcSamp$qfStepTemp,
                                    "qfPersTemp" = qfInput$irgaMfcSamp$qfPersTemp)
        setQf$sensMfc <- data.frame("qfFrt00" = qfInput$irgaMfcSamp$qfFrt00)
      } else {
        #assign qf for irgaMfcSamp to -1 when qf irgaMfcSamp are missing
        setQf$frt00 <- data.frame("qfRngMinFrt00" = -1, 
                                  "qfStepFrt00" = -1, 
                                  "qfPersFrt00" = -1)
        setQf$frt <- data.frame("qfRngMinFrt" = -1,
                                "qfStepFrt" = -1,
                                "qfPersFrt" = -1)
        setQf$presAtmMfc <- data.frame("qfRngMinPresAtm" = -1, 
                                       "qfStepPresAtm" = -1,
                                       "qfPersPresAtm" = -1)
        setQf$tempMfc <- data.frame("qfRngMinTemp" = -1,
                                    "qfStepTemp" = -1,
                                    "qfPersTemp" = -1)
        setQf$sensMfc <- data.frame("qfFrt00" = -1)
      }
    } # closed MeasType loop
    
  #using mfcVali during validation period
   if (TypeMeas == "vali") {
     #set of qf from envHut, and valvAux
     setQf$sensIrga <- data.frame("qfTemp" = ifelse(!is.null(qfInput$envHut), qfInput$envHut$qfTemp, -1),#qf from envHut
                                  "qfValvIrga" = ifelse(!is.null(qfInput$valvAux), qfInput$valvAux$qfValvIrga, -1))
     
     if (!is.null(qfInput$mfcVali)){
        #irgaMfcVali
        setQf$frt00 <- data.frame("qfRngMinFrt00" = qfInput$mfcVali$qfRngMinFrt00, 
                                  "qfStepFrt00" = qfInput$mfcVali$qfStepFrt00, 
                                  "qfPersFrt00" = qfInput$mfcVali$qfPersFrt00)
        setQf$frt <- data.frame("qfRngMinFrt" = qfInput$mfcVali$qfRngMinFrt,
                                "qfStepFrt" = qfInput$mfcVali$qfStepFrt,
                                "qfPersFrt" = qfInput$mfcVali$qfPersFrt)
        setQf$presAtmMfc <- data.frame("qfRngMinPresAtm" = qfInput$mfcVali$qfRngMinPresAtm, 
                                       "qfStepPresAtm" = qfInput$mfcVali$qfStepPresAtm,
                                       "qfPersPresAtm" = qfInput$mfcVali$qfPersPresAtm)
        setQf$tempMfc <- data.frame("qfRngMinTemp" = qfInput$mfcVali$qfRngMinTemp,
                                    "qfStepTemp" = qfInput$mfcVali$qfStepTemp,
                                    "qfPersTemp" = qfInput$mfcVali$qfPersTemp)
        setQf$sensMfc <- data.frame("qfFrt00" = qfInput$mfcVali$qfFrt00)
      } else {
        #assign qf for irgaMfcSamp to -1 when qf irgaMfcSamp are missing
        setQf$frt00 <- data.frame("qfRngMinFrt00" = -1, 
                                  "qfStepFrt00" = -1, 
                                  "qfPersFrt00" = -1)
        setQf$frt <- data.frame("qfRngMinFrt" = -1,
                                "qfStepFrt" = -1,
                                "qfPersFrt" = -1)
        setQf$presAtmMfc <- data.frame("qfRngMinPresAtm" = -1, 
                                       "qfStepPresAtm" = -1,
                                       "qfPersPresAtm" = -1)
        setQf$tempMfc <- data.frame("qfRngMinTemp" = -1,
                                    "qfStepTemp" = -1,
                                    "qfPersTemp" = -1)
        setQf$sensMfc <- data.frame("qfFrt00" = -1)
      }
    } # closed MeasType loop
  
#grouping qulity flags that related to irgaCo2 L1 sub-data product
    if (dp01 == "irgaCo2") {
      rpt$rtioMoleDryCo2 <- data.frame(setQf$rtioMoleDryCo2, setQf$sensIrga,
                                       setQf$asrpCo2, setQf$presIrga,
                                       setQf$tempIrga, setQf$frt00,
                                       setQf$frt, setQf$presAtmMfc,
                                       setQf$tempMfc, setQf$sensMfc)
      rpt$rtioMoleWetCo2 <- data.frame(setQf$rtioMoleWetCo2, setQf$sensIrga,
                                       setQf$asrpCo2, setQf$presIrga,
                                       setQf$tempIrga, setQf$frt00,
                                       setQf$frt, setQf$presAtmMfc,
                                       setQf$tempMfc, setQf$sensMfc)
      rpt$pres <- data.frame(setQf$presIrga, setQf$sensIrga)
      rpt$frt00 <- data.frame (setQf$frt00, setQf$frt,
                          setQf$presAtmMfc, setQf$tempMfc,
                          setQf$sensMfc)
      rpt$temp <- data.frame (setQf$tempIrga, setQf$sensIrga)
    }
    #grouping qulity flags that related to irgaH2o L1 sub-data product    
    if (dp01 == "irgaH2o") {
      rpt$rtioMoleDryH2o <- data.frame(setQf$rtioMoleDryH2o, setQf$sensIrga, 
                                  setQf$asrpH2o, setQf$presIrga,
                                  setQf$tempIrga, setQf$frt00, 
                                  setQf$frt, setQf$presAtmMfc,
                                  setQf$tempMfc, setQf$sensMfc)
      rpt$rtioMoleWetH2o <- data.frame(setQf$rtioMoleWetH2o, setQf$sensIrga, 
                                  setQf$asrpH2o, setQf$presIrga,
                                  setQf$tempIrga, setQf$frt00, 
                                  setQf$frt, setQf$presAtmMfc,
                                  setQf$tempMfc, setQf$sensMfc)
      rpt$pres <- data.frame(setQf$presIrga, setQf$sensIrga)
      rpt$frt00 <- data.frame(setQf$frt00, setQf$frt, 
                         setQf$presAtmMfc, setQf$tempMfc,
                         setQf$sensMfc) 
      rpt$temp <- data.frame(setQf$tempIrga, setQf$sensIrga) 
    }
    #remove setQf
    setQf <- NULL
  }#closed loop for irgaCo2 and irgaH2o
  
#isoCo2 ####################################################################################
  if (dp01 == "isoCo2") {
    
    setQf$rtioMoleDryCo2 <- data.frame("qfRngMinRtioMoleDryCo2" = qfInput$crdCo2$qfRngMinRtioMoleDryCo2, 
                                       "qfStepRtioMoleDryCo2" = qfInput$crdCo2$qfStepRtioMoleDryCo2,
                                       "qfPersRtioMoleDryCo2" = qfInput$crdCo2$qfPersRtioMoleDryCo2, 
                                       "qfCalRtioMoleDryCo2" = qfInput$crdCo2$qfCalRtioMoleDryCo2)
    
    setQf$rtioMoleDry12CCo2 <- data.frame("qfRngMinRtioMoleDry12CCo2" = qfInput$crdCo2$qfRngMinRtioMoleDry12CCo2, 
                                          "qfStepRtioMoleDry12CCo2" = qfInput$crdCo2$qfStepRtioMoleDry12CCo2,
                                          "qfPersRtioMoleDry12CCo2" = qfInput$crdCo2$qfPersRtioMoleDry12CCo2, 
                                          "qfCalRtioMoleDry12CCo2" = qfInput$crdCo2$qfCalRtioMoleDry12CCo2)
    
    setQf$rtioMoleDry13CCo2 <- data.frame("qfRngMinRtioMoleDry13CCo2" = qfInput$crdCo2$qfRngMinRtioMoleDry13CCo2, 
                                          "qfStepRtioMoleDry13CCo2" = qfInput$crdCo2$qfStepRtioMoleDry13CCo2,
                                          "qfPersRtioMoleDry13CCo2" = qfInput$crdCo2$qfPersRtioMoleDry13CCo2, 
                                          "qfCalRtioMoleDry13CCo2" = qfInput$crdCo2$qfCalRtioMoleDry13CCo2)
    
    setQf$rtioMoleDryH2o <- data.frame("qfRngMinRtioMoleDryH2o" = qfInput$crdCo2$qfRngMinRtioMoleDryH2o, 
                                       "qfStepRtioMoleDryH2o" = qfInput$crdCo2$qfStepRtioMoleDryH2o,
                                       "qfPersRtioMoleDryH2o" = qfInput$crdCo2$qfPersRtioMoleDryH2o, 
                                       "qfCalRtioMoleDryH2o" = qfInput$crdCo2$qfCalRtioMoleDryH2o)
    
    setQf$rtioMoleWetCo2 <- data.frame("qfRngMinRtioMoleWetCo2" = qfInput$crdCo2$qfRngMinRtioMoleWetCo2, 
                                       "qfStepRtioMoleWetCo2" = qfInput$crdCo2$qfStepRtioMoleWetCo2,
                                       "qfPersRtioMoleWetCo2" = qfInput$crdCo2$qfPersRtioMoleWetCo2, 
                                       "qfCalRtioMoleWetCo2" = qfInput$crdCo2$qfCalRtioMoleWetCo2)
    
    setQf$rtioMoleWet12CCo2 <- data.frame("qfRngMinRtioMoleWet12CCo2" = qfInput$crdCo2$qfRngMinRtioMoleWet12CCo2, 
                                          "qfStepRtioMoleWet12CCo2" = qfInput$crdCo2$qfStepRtioMoleWet12CCo2,
                                          "qfPersRtioMoleWet12CCo2" = qfInput$crdCo2$qfPersRtioMoleWet12CCo2, 
                                          "qfCalRtioMoleWet12CCo2" = qfInput$crdCo2$qfCalRtioMoleWet12CCo2)
    
    setQf$rtioMoleWet13CCo2 <- data.frame("qfRngMinRtioMoleWet13CCo2" = qfInput$crdCo2$qfRngMinRtioMoleWet13CCo2, 
                                          "qfStepRtioMoleWet13CCo2" = qfInput$crdCo2$qfStepRtioMoleWet13CCo2,
                                          "qfPersRtioMoleWet13CCo2" = qfInput$crdCo2$qfPersRtioMoleWet13CCo2, 
                                          "qfCalRtioMoleWet13CCo2 " = qfInput$crdCo2$qfCalRtioMoleWet13CCo2)
    
    setQf$rtioMoleWetH2o <- data.frame("qfRngMinRtioMoleWetH2o" = qfInput$crdCo2$qfRngMinRtioMoleWetH2o, 
                                       "qfStepRtioMoleWetH2o" = qfInput$crdCo2$qfStepRtioMoleWetH2o,
                                       "qfPersRtioMoleWetH2o" = qfInput$crdCo2$qfPersRtioMoleWetH2o, 
                                       "qfCalRtioMoleWetH2o" = qfInput$crdCo2$qfCalRtioMoleWetH2o)
    
    setQf$dlta13CCo2 <- data.frame("qfRngMinDlta13CCo2" = qfInput$crdCo2$qfRngMinDlta13CCo2, 
                                   "qfStepDlta13CCo2" = qfInput$crdCo2$qfStepDlta13CCo2,
                                   "qfPersDlta13CCo2" = qfInput$crdCo2$qfPersDlta13CCo2, 
                                   "qfCalDlta13CCo2" = qfInput$crdCo2$qfCalDlta13CCo2)
    
    setQf$presCrdCo2 <- data.frame("qfRngMinPres" = qfInput$crdCo2$qfRngMinPres, 
                                   "qfStepPres" = qfInput$crdCo2$qfStepPres,
                                   "qfPersPres" = qfInput$crdCo2$qfPersPres, 
                                   "qfCalPres" = qfInput$crdCo2$qfCalPres)
    
    setQf$tempCrdCo2 <- data.frame("qfRngMinTemp" = qfInput$crdCo2$qfRngMinTemp, 
                                   "qfStepTemp" = qfInput$crdCo2$qfStepTemp,
                                   "qfPersTemp" = qfInput$crdCo2$qfPersTemp, 
                                   "qfCalTemp" = qfInput$crdCo2$qfCalTemp)
    
    setQf$tempWbox <- data.frame("qfRngMinTempWbox" = qfInput$crdCo2$qfRngMinTempWbox, 
                                 "qfStepTempWbox" = qfInput$crdCo2$qfStepTempWbox,
                                 "qfPersTempWbox" = qfInput$crdCo2$qfPersTempWbox, 
                                 "qfCalTempWbox" = qfInput$crdCo2$qfCalTempWbox)
    #define qf which use only sampling period
    if (TypeMeas == "samp") {
      
      setQf$sensCrdCo2 <- data.frame("qfSensStus" = qfInput$crdCo2$qfSensStus,
                                     "qfHeat" = ifelse(!is.null(qfInput$heatInlt), qfInput$heatInlt$qfHeat, -1))
      #grouping qulity flags that related to isoCo2 L1 sub-data product  
      rpt$rtioMoleWetCo2 <- data.frame(setQf$rtioMoleWetCo2, setQf$sensCrdCo2)
      rpt$rtioMoleDryCo2 <- data.frame(setQf$rtioMoleDryCo2, setQf$sensCrdCo2)
      rpt$rtioMoleWet12CCo2 <- data.frame(setQf$rtioMoleWet12CCo2, setQf$sensCrdCo2)
      rpt$rtioMoleDry12CCo2 <- data.frame(setQf$rtioMoleDry12CCo2, setQf$sensCrdCo2)
      rpt$rtioMoleWet13CCo2 <- data.frame(setQf$rtioMoleWet13CCo2, setQf$sensCrdCo2)
      rpt$dlta13CCo2 <- data.frame(setQf$dlta13CCo2, setQf$sensCrdCo2)
      rpt$rtioMoleWetH2o <- data.frame(setQf$rtioMoleWetH2o, setQf$sensCrdCo2)
      rpt$rtioMoleDryH2o <- data.frame(setQf$rtioMoleDryH2o, setQf$sensCrdCo2)
      rpt$temp <- data.frame(setQf$tempCrdCo2, setQf$sensCrdCo2)
      rpt$pres <- data.frame(setQf$presCrdCo2, setQf$sensCrdCo2)
      
    }#close loop for sampling
    
    #define qf which use only validation period
    if (TypeMeas == "vali") {
      
      #qf from mfcVali
      if (!is.null(qfInput$mfcVali)){
        #irgaMfcVali
        setQf$frt00 <- data.frame("qfRngMinFrt00" = qfInput$mfcVali$qfRngMinFrt00, 
                                  "qfStepFrt00" = qfInput$mfcVali$qfStepFrt00, 
                                  "qfPersFrt00" = qfInput$mfcVali$qfPersFrt00)
        setQf$frt <- data.frame("qfRngMinFrt" = qfInput$mfcVali$qfRngMinFrt,
                                "qfStepFrt" = qfInput$mfcVali$qfStepFrt,
                                "qfPersFrt" = qfInput$mfcVali$qfPersFrt)
        setQf$presAtmMfc <- data.frame("qfRngMinPresAtm" = qfInput$mfcVali$qfRngMinPresAtm, 
                                       "qfStepPresAtm" = qfInput$mfcVali$qfStepPresAtm,
                                       "qfPersPresAtm" = qfInput$mfcVali$qfPersPresAtm)
        setQf$tempMfc <- data.frame("qfRngTemp" = qfInput$mfcVali$qfRngMinTemp,
                                    "qfStepTemp" = qfInput$mfcVali$qfStepTemp,
                                    "qfPersTemp" = qfInput$mfcVali$qfPersTemp)
        setQf$sensMfc <- data.frame("qfFrt00" = qfInput$mfcVali$qfFrt00)
      } else {
        #assign qf for irgaMfcSamp to -1 when qf irgaMfcSamp are missing
        setQf$frt00 <- data.frame("qfRngMinFrt00" = -1, 
                                  "qfStepFrt00" = -1, 
                                  "qfPersFrt00" = -1)
        setQf$frt <- data.frame("qfRngMinFrt" = -1,
                                "qfStepFrt" = -1,
                                "qfPersFrt" = -1)
        setQf$presAtmMfc <- data.frame("qfRngMinPresAtm" = -1, 
                                       "qfStepPresAtm" = -1,
                                       "qfPersPresAtm" = -1)
        setQf$tempMfc <- data.frame("qfRngTemp" = -1,
                                    "qfStepTemp" = -1,
                                    "qfPersTemp" = -1)
        setQf$sensMfc <- data.frame("qfFrt00" = -1)
      }
      
      setQf$sensCrdCo2 <- data.frame("qfSensStus" = qfInput$crdCo2$qfSensStus,
                                setQf$frt00, setQf$frt,
                                setQf$presAtmMfc, setQf$tempMfc,
                                setQf$sensMfc)
      #grouping qulity flags that related to isoCo2 L1 sub-data product  
      rpt$rtioMoleWetCo2 <- data.frame(setQf$rtioMoleWetCo2, setQf$sensCrdCo2)
      rpt$rtioMoleDryCo2 <- data.frame(setQf$rtioMoleDryCo2, setQf$sensCrdCo2)
      rpt$rtioMoleWet12CCo2 <- data.frame(setQf$rtioMoleWet12CCo2, setQf$sensCrdCo2)
      rpt$rtioMoleDry12CCo2 <- data.frame(setQf$rtioMoleDry12CCo2, setQf$sensCrdCo2)
      rpt$rtioMoleWet13CCo2 <- data.frame(setQf$rtioMoleWet13CCo2, setQf$sensCrdCo2)
      rpt$dlta13CCo2 <- data.frame(setQf$dlta13CCo2, setQf$sensCrdCo2)
      rpt$rtioMoleWetH2o <- data.frame(setQf$rtioMoleWetH2o, setQf$sensCrdCo2)
      rpt$rtioMoleDryH2o <- data.frame(setQf$rtioMoleDryH2o, setQf$sensCrdCo2)
      rpt$temp <- data.frame(setQf$tempCrdCo2, setQf$sensCrdCo2)
      rpt$pres <- data.frame(setQf$presCrdCo2, setQf$sensCrdCo2)
    }#closed loop for vali
    
    }#Closed loop for isoCo2

}# closed loop for ecse
  return(rpt)
  
  # end function def.neon.dp01.qf.grp()
}
