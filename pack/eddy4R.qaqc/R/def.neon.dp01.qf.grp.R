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
  dp01 = c("envHut", "irgaCo2", "irgaH2o", "isopCo2", "isopH2o", "soni", "soniAmrs", "tempAirLvl", "tempAirTop")[1]
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
    
    setQf$asrpCo2 <- data.frame("qfRngAsrpCo2" = qfInput$irga$qfRngAsrpCo2, 
                                "qfStepAsrpCo2" = qfInput$irga$qfStepAsrpCo2, 
                                "qfPersAsrpCo2" = qfInput$irga$qfPersAsrpCo2, 
                                "qfCalAsrpCo2" = qfInput$irga$qfCalAsrpCo2)
    
    setQf$asrpH2o <- data.frame("qfRngAsrpH2o" = qfInput$irga$qfRngAsrpH2o, 
                                "qfStepAsrpH2o" = qfInput$irga$qfStepAsrpH2o, 
                                "qfPersAsrpH2o" = qfInput$irga$qfPersAsrpH2o, 
                                "qfCalAsrpH2o" = qfInput$irga$qfCalAsrpH2o)
    
    setQf$ssiCo2 <- data.frame("qfRngSsiCo2" = qfInput$irga$qfRngSsiCo2, 
                               "qfStepSsiCo2" = qfInput$irga$qfStepSsiCo2,
                               "qfPersSsiCo2" = qfInput$irga$qfPersSsiCo2, 
                               "qfCalSsiCo2" = qfInput$irga$qfCalSsiCo2)
    
    setQf$ssiH2o <- data.frame("qfRngSsiH2o" = qfInput$irga$qfRngSsiH2o, 
                               "qfStepSsiH2o" = qfInput$irga$qfStepSsiH2o,
                               "qfPersSsiH2o" = qfInput$irga$qfPersSsiH2o, 
                               "qfCalSsiH2o" = qfInput$irga$qfCalSsiH2o)
    
    setQf$rtioMoleDryCo2 <- data.frame("qfRngRtioMoleDryCo2" = qfInput$irga$qfRngRtioMoleDryCo2, 
                                       "qfStepRtioMoleDryCo2" = qfInput$irga$qfStepRtioMoleDryCo2,
                                       "qfPersRtioMoleDryCo2" = qfInput$irga$qfPersRtioMoleDryCo2, 
                                       "qfCalRtioMoleDryCo2" = qfInput$irga$qfCalRtioMoleDryCo2)
    
    setQf$rtioMoleDryH2o <- data.frame("qfRngRtioMoleDryH2o" = qfInput$irga$qfRngRtioMoleDryH2o, 
                                       "qfStepRtioMoleDryH2o" = qfInput$irga$qfStepRtioMoleDryH2o,
                                       "qfPersRtioMoleDryH2o" = qfInput$irga$qfPersRtioMoleDryH2o, 
                                       "qfCalRtioMoleDryH2o" = qfInput$irga$qfCalRtioMoleDryH2o)
    
    setQf$densMoleCo2 <- data.frame("qfRngDensMoleCo2" = qfInput$irga$qfRngDensMoleCo2, 
                                    "qfStepDensMoleCo2" = qfInput$irga$qfStepDensMoleCo2,
                                    "qfPersDensMoleCo2" = qfInput$irga$qfPersDensMoleCo2, 
                                    "qfCalDensMoleCo2" = qfInput$irga$qfCalDensMoleCo2) 
    
    setQf$densMoleH2o <- data.frame("qfRngDensMoleH2o" = qfInput$irga$qfRngDensMoleH2o, 
                                    "qfStepDensMoleH2o" = qfInput$irga$qfStepDensMoleH2o,
                                    "qfPersDensMoleH2o" = qfInput$irga$qfPersDensMoleH2o, 
                                    "qfCalDensMoleH2o" = qfInput$irga$qfCalDensMoleH2o)
    
    setQf$presAtm <- data.frame("qfRngPresAtm" = qfInput$irga$qfRngPresAtm, 
                                "qfStepPresAtm" = qfInput$irga$qfStepPresAtm,
                                "qfPersPresAtm" = qfInput$irga$qfPersPresAtm, 
                                "qfCalPresAtm" = qfInput$irga$qfCalPresAtm)
    
    setQf$presSum <- data.frame("qfRngPresSum" = qfInput$irga$qfRngPresSum, 
                                "qfStepPresSum" = qfInput$irga$qfStepPresSum,
                                "qfPersPresSum" = qfInput$irga$qfPersPresSum, 
                                "qfCalPresSum" = qfInput$irga$qfCalPresSum)
    
    setQf$tempAve <- data.frame ("qfRngTempMean" = qfInput$irga$qfRngTempMean, 
                                 "qfStepTempMean" = qfInput$irga$qfStepTempMean,
                                 "qfPersTempMean" = qfInput$irga$qfPersTempMean, 
                                 "qfCalTempMean" = qfInput$irga$qfCalTempMean)
#using irgaMfcSamp during sampling period
 if (TypeMeas == "samp") {
    if (!is.null(qfInput$irgaMfcSamp)){
      #irgaMfcSamp
      setQf$frt00 <- data.frame("qfRngFrt00" = qfInput$irgaMfcSamp$qfRngFrt00, 
                                "qfStepFrt00" = qfInput$irgaMfcSamp$qfStepFrt00, 
                                "qfPersFrt00" = qfInput$irgaMfcSamp$qfPersFrt00)
      setQf$frt <- data.frame("qfRngFrt" = qfInput$irgaMfcSamp$qfRngFrt,
                              "qfStepFrt" = qfInput$irgaMfcSamp$qfStepFrt,
                              "qfPersFrt" = qfInput$irgaMfcSamp$qfPersFrt)
      setQf$presAtmMfc <- data.frame("qfRngPresAtm" = qfInput$irgaMfcSamp$qfRngPresAtm, 
                                     "qfStepPresAtm" = qfInput$irgaMfcSamp$qfStepPresAtm,
                                     "qfPersPresAtm" = qfInput$irgaMfcSamp$qfPersPresAtm)
      setQf$tempMfc <- data.frame("qfRngTemp" = qfInput$irgaMfcSamp$qfRngTemp,
                                  "qfStepTemp" = qfInput$irgaMfcSamp$qfStepTemp,
                                  "qfPersTemp" = qfInput$irgaMfcSamp$qfPersTemp)
      } else {
      #assign qf for irgaMfcSamp to -1 when qf irgaMfcSamp is missing
      setQf$frt00 <- data.frame("qfRngFrt00" = -1, 
                                "qfStepFrt00" = -1, 
                                "qfPersFrt00" = -1)
      setQf$frt <- data.frame("qfRngFrt" = -1,
                              "qfStepFrt" = -1,
                              "qfPersFrt" = -1)
      setQf$presAtmMfc <- data.frame("qfRngPresAtm" = -1, 
                                     "qfStepPresAtm" = -1,
                                     "qfPersPresAtm" = -1)
      setQf$tempMfc <- data.frame("qfRngTemp" = -1,
                                  "qfStepTemp" = -1,
                                  "qfPersTemp" = -1)
    }
 } # closed MeasType loop
    
#using irgaMfcVali during validatio period
 if (TypeMeas == "vali") {
    if (!is.null(qfInput$irgaMfcVali)){
      #irgaMfcVali
      setQf$frt00 <- data.frame("qfRngFrt00" = qfInput$irgaMfcVali$qfRngFrt00, 
                                "qfStepFrt00" = qfInput$irgaMfcVali$qfStepFrt00, 
                                "qfPersFrt00" = qfInput$irgaMfcVali$qfPersFrt00)
      setQf$frt <- data.frame("qfRngFrt" = qfInput$irgaMfcVali$qfRngFrt,
                              "qfStepFrt" = qfInput$irgaMfcVali$qfStepFrt,
                              "qfPersFrt" = qfInput$irgaMfcVali$qfPersFrt)
      setQf$presAtmMfc <- data.frame("qfRngPresAtm" = qfInput$irgaMfcVali$qfRngPresAtm, 
                                     "qfStepPresAtm" = qfInput$irgaMfcVali$qfStepPresAtm,
                                     "qfPersPresAtm" = qfInput$irgaMfcVali$qfPersPresAtm)
      setQf$tempMfc <- data.frame("qfRngTemp" = qfInput$irgaMfcVali$qfRngTemp,
                                  "qfStepTemp" = qfInput$irgaMfcVali$qfStepTemp,
                                  "qfPersTemp" = qfInput$irgaMfcVali$qfPersTemp)
      } else {
      #assign qf for irgaMfcVali to -1 when qf irgaMfcVali is missing
      setQf$frt00 <- data.frame("qfRngFrt00" = -1, 
                                "qfStepFrt00" = -1, 
                                "qfPersFrt00" = -1)
      setQf$frt <- data.frame("qfRngFrt" = -1,
                              "qfStepFrt" = -1,
                              "qfPersFrt" = -1)
      setQf$presAtmMfc <- data.frame("qfRngPresAtm" = -1, 
                                     "qfStepPresAtm" = -1,
                                     "qfPersPresAtm" = -1)
      setQf$tempMfc <- data.frame("qfRngTemp" = -1,
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
    setQf$veloXaxs <- data.frame("qfRngVeloXaxs" = qfInput$soni$qfRngVeloXaxs,
                                 "qfStepVeloXaxs" = qfInput$soni$qfStepVeloXaxs,
                                 "qfPersVeloXaxs" = qfInput$soni$qfPersVeloXaxs,
                                 "qfCalVeloXaxs" = qfInput$soni$qfCalVeloXaxs)
    
    #qf for cross-axis wind speed
    setQf$veloYaxs <- data.frame("qfRngVeloYaxs" = qfInput$soni$qfRngVeloYaxs,
                                 "qfStepVeloYaxs" = qfInput$soni$qfStepVeloYaxs,
                                 "qfPersVeloYaxs" = qfInput$soni$qfPersVeloYaxs,
                                 "qfCalVeloYaxs" = qfInput$soni$qfCalVeloYaxs)
    #qf for vertical-axis wind speed
    setQf$veloZaxs <- data.frame("qfRngVeloZaxs" = qfInput$soni$qfRngVeloZaxs,
                                 "qfStepVeloZaxs" = qfInput$soni$qfStepVeloZaxs,
                                 "qfPersVeloZaxs" = qfInput$soni$qfPersVeloZaxs,
                                 "qfCalVeloZaxs" = qfInput$soni$qfCalVeloZaxs)
    #qf for soic temperature
    setQf$tempSoni <- data.frame("qfRngTempSoni" = qfInput$soni$qfRngTempSoni,
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
    setQf$angXaxs <- data.frame("qfRngAngXaxs" = qfInput$soniAmrs$qfRngAngXaxs,
                                "qfStepAngXaxs" = qfInput$soniAmrs$qfStepAngXaxs,
                                "qfPersAngXaxs" = qfInput$soniAmrs$qfPersAngXaxs,
                                "qfCalAngXaxs" = qfInput$soniAmrs$qfCalAngXaxs)
    setQf$angYaxs <- data.frame("qfRngAngYaxs" = qfInput$soniAmrs$qfRngAngYaxs,
                                "qfStepAngYaxs" = qfInput$soniAmrs$qfStepAngYaxs,
                                "qfPersAngYaxs" = qfInput$soniAmrs$qfPersAngYaxs,
                                "qfCalAngYaxs" = qfInput$soniAmrs$qfCalAngYaxs)
    setQf$angZaxs <- data.frame("qfRngAngZaxs" = qfInput$soniAmrs$qfRngAngZaxs,
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
    
    setQf$asrpCo2 <- data.frame("qfRngAsrpCo2" = qfInput$irga$qfRngAsrpCo2, 
                                "qfStepAsrpCo2" = qfInput$irga$qfStepAsrpCo2, 
                                "qfPersAsrpCo2" = qfInput$irga$qfPersAsrpCo2, 
                                "qfCalAsrpCo2" = qfInput$irga$qfCalAsrpCo2)
    
    setQf$asrpH2o <- data.frame("qfRngAsrpH2o" = qfInput$irga$qfRngAsrpH2o, 
                                "qfStepAsrpH2o" = qfInput$irga$qfStepAsrpH2o, 
                                "qfPersAsrpH2o" = qfInput$irga$qfPersAsrpH2o, 
                                "qfCalAsrpH2o" = qfInput$irga$qfCalAsrpH2o)
    
    setQf$rtioMoleDryCo2 <- data.frame("qfRngRtioMoleDryCo2" = qfInput$irga$qfRngRtioMoleDryCo2, 
                                       "qfStepRtioMoleDryCo2" = qfInput$irga$qfStepRtioMoleDryCo2,
                                       "qfPersRtioMoleDryCo2" = qfInput$irga$qfPersRtioMoleDryCo2, 
                                       "qfCalRtioMoleDryCo2" = qfInput$irga$qfCalRtioMoleDryCo2)
    
    setQf$rtioMoleDryH2o <- data.frame("qfRngRtioMoleDryH2o" = qfInput$irga$qfRngRtioMoleDryH2o, 
                                       "qfStepRtioMoleDryH2o" = qfInput$irga$qfStepRtioMoleDryH2o,
                                       "qfPersRtioMoleDryH2o" = qfInput$irga$qfPersRtioMoleDryH2o, 
                                       "qfCalRtioMoleDryH2o" = qfInput$irga$qfCalRtioMoleDryH2o)
    
    setQf$rtioMoleWetCo2 <- data.frame("qfRngRtioMoleWetCo2" = qfInput$irga$qfRngRtioMoleWetCo2,
                                       "qfStepRtioMoleWetCo2" = qfInput$irga$qfStepRtioMoleWetCo2,
                                       "qfPersRtioMoleWetCo2" = qfInput$irga$qfPersRtioMoleWetCo2,
                                       "qfCalsRtioMoleWetCo2" = qfInput$irga$qfCalRtioMoleWetCo2)
    
    setQf$rtioMoleWetH2o <- data.frame("qfRngRtioMoleWetH2o" = qfInput$irga$qfRngRtioMoleWetH2o,
                                       "qfStepRtioMoleWetH2o" = qfInput$irga$qfStepRtioMoleWetH2o,
                                       "qfPersRtioMoleWetH2o" = qfInput$irga$qfPersRtioMoleWetH2o,
                                       "qfCalsRtioMoleWetH2o" = qfInput$irga$qfCalRtioMoleWetH2o)
    
    setQf$presIrga <- data.frame("qfRngPres" = qfInput$irga$qfRngPres, 
                                "qfStepPres" = qfInput$irga$qfStepPres,
                                "qfPersPres" = qfInput$irga$qfPersPres, 
                                "qfCalPres" = qfInput$irga$qfCalPres)
    
    setQf$tempIrga <- data.frame ("qfRngTemp" = qfInput$irga$qfRngTemp, 
                                 "qfStepTemp" = qfInput$irga$qfStepTemp,
                                 "qfPersTemp" = qfInput$irga$qfPersTemp, 
                                 "qfCalTemp" = qfInput$irga$qfCalTemp)
    setQf$envHut <- data.frame("qfTemp" = ifelse(!is.null(qfInput$envHut), qfInput$envHut$qfTemp, -1))
    setQf$valvAux <- data.frame("qfValvIrga" = ifelse(!is.null(qfInput$valvAux), qfInput$valvAux$qfValvIrga, -1))
    setQf$heatInlt <- data.frame("qfHeat" = ifelse(!is.null(qfInput$heatInlt), qfInput$heatInlt$qfHeat, -1))
  
    
    #using irgaMfcSamp during sampling period
    if (TypeMeas == "samp") {
      
      if (!is.null(qfInput$irgaMfcSamp)){
        #irgaMfcSamp
        setQf$frt00 <- data.frame("qfRngFrt00" = qfInput$irgaMfcSamp$qfRngFrt00, 
                                  "qfStepFrt00" = qfInput$irgaMfcSamp$qfStepFrt00, 
                                  "qfPersFrt00" = qfInput$irgaMfcSamp$qfPersFrt00)
        setQf$frt <- data.frame("qfRngFrt" = qfInput$irgaMfcSamp$qfRngFrt,
                                "qfStepFrt" = qfInput$irgaMfcSamp$qfStepFrt,
                                "qfPersFrt" = qfInput$irgaMfcSamp$qfPersFrt)
        setQf$presAtmMfc <- data.frame("qfRngPresAtm" = qfInput$irgaMfcSamp$qfRngPresAtm, 
                                       "qfStepPresAtm" = qfInput$irgaMfcSamp$qfStepPresAtm,
                                       "qfPersPresAtm" = qfInput$irgaMfcSamp$qfPersPresAtm)
        setQf$tempMfc <- data.frame("qfRngTemp" = qfInput$irgaMfcSamp$qfRngTemp,
                                    "qfStepTemp" = qfInput$irgaMfcSamp$qfStepTemp,
                                    "qfPersTemp" = qfInput$irgaMfcSamp$qfPersTemp)
        setQf$sensMfc <- data.frame("qfFrt00" = qfInput$irgaMfcSamp$qfFrt00)
      } else {
        #assign qf for irgaMfcSamp to -1 when qf irgaMfcSamp are missing
        setQf$frt00 <- data.frame("qfRngFrt00" = -1, 
                                  "qfStepFrt00" = -1, 
                                  "qfPersFrt00" = -1)
        setQf$frt <- data.frame("qfRngFrt" = -1,
                                "qfStepFrt" = -1,
                                "qfPersFrt" = -1)
        setQf$presAtmMfc <- data.frame("qfRngPresAtm" = -1, 
                                       "qfStepPresAtm" = -1,
                                       "qfPersPresAtm" = -1)
        setQf$tempMfc <- data.frame("qfRngTemp" = -1,
                                    "qfStepTemp" = -1,
                                    "qfPersTemp" = -1)
        setQf$sensMfc <- data.frame("qfFrt00" = -1)
      }
#grouping qulity flags that related to irgaCo2 L1 sub-data product
      if (dp01 == "irgaCo2") {
        rpt$rtioMoleDryCo2 <- data.frame(setQf$rtioMoleDryCo2, setQf$asrpCo2,
                                         setQf$asrpH2o, setQf$rtioMoleWetCo2,
                                         setQf$rtioMoleWetH2o, setQf$presIrga,
                                         setQf$tempIrga, setQf$envHut, 
                                         setQf$valvAux, setQf$heatInlt,
                                         setQf$frt00, setQf$frt, 
                                         setQf$presAtmMfc, setQf$tempMfc,
                                         setQf$sensMfc)
        rpt$rtioMoleWetCo2 <- data.frame(setQf$rtioMoleWetCo2, setQf$asrpCo2,
                                         setQf$asrpH2o, setQf$rtioMoleWetH2o, 
                                         setQf$presIrga, setQf$tempIrga, 
                                         setQf$envHut, setQf$valvAux, 
                                         setQf$heatInlt, setQf$frt00, 
                                         setQf$frt, setQf$presAtmMfc, 
                                         setQf$tempMfc, setQf$sensMfc)
        rpt$pres <- data.frame(setQf$presIrga)
        rpt$frt00 <- data.frame (setQf$frt00, setQf$frt,
                                 setQf$presAtmMfc, setQf$tempMfc,
                                 setQf$sensMfc)
        rpt$temp <- data.frame (setQf$tempIrga)
      }
#grouping qulity flags that related to irgaH2o L1 sub-data product    
      if (dp01 == "irgaH2o") {
        rpt$rtioMoleDryH2o <- data.frame(setQf$rtioMoleDryH2o, setQf$asrpH2o,
                                         setQf$rtioMoleWetH2o, setQf$presIrga,
                                         setQf$tempIrga, setQf$envHut, 
                                         setQf$valvAux, setQf$heatInlt, 
                                         setQf$frt00, setQf$frt, 
                                         setQf$presAtmMfc, setQf$tempMfc, 
                                         setQf$sensMfc)
        rpt$rtioMoleWetH2o <- data.frame(setQf$rtioMoleWetH2o, setQf$asrpH2o,
                                         setQf$presIrga, setQf$tempIrga,
                                         setQf$envHut, setQf$valvAux, 
                                         setQf$heatInlt, setQf$frt00, 
                                         setQf$frt, setQf$presAtmMfc, 
                                         setQf$tempMfc, setQf$sensMfc)
        rpt$pres <- data.frame(setQf$presIrga)
        rpt$frt00 <- data.frame(setQf$frt00, setQf$frt,
                                setQf$presAtmMfc, setQf$tempMfc,
                                setQf$sensMfc) 
        rpt$temp <- data.frame(setQf$tempIrga) 
      } 
      
    } # closed MeasType loop
    
  #using mfcVali during validation period
   if (TypeMeas == "vali") {
     if (!is.null(qfInput$mfcVali)){
        #irgaMfcVali
        setQf$frt00 <- data.frame("qfRngFrt00" = qfInput$mfcVali$qfRngFrt00, 
                                  "qfStepFrt00" = qfInput$mfcVali$qfStepFrt00, 
                                  "qfPersFrt00" = qfInput$mfcVali$qfPersFrt00)
        setQf$frt <- data.frame("qfRngFrt" = qfInput$mfcVali$qfRngFrt,
                                "qfStepFrt" = qfInput$mfcVali$qfStepFrt,
                                "qfPersFrt" = qfInput$mfcVali$qfPersFrt)
        setQf$presAtmMfc <- data.frame("qfRngPresAtm" = qfInput$mfcVali$qfRngPresAtm, 
                                       "qfStepPresAtm" = qfInput$mfcVali$qfStepPresAtm,
                                       "qfPersPresAtm" = qfInput$mfcVali$qfPersPresAtm)
        setQf$tempMfc <- data.frame("qfRngTemp" = qfInput$mfcVali$qfRngTemp,
                                    "qfStepTemp" = qfInput$mfcVali$qfStepTemp,
                                    "qfPersTemp" = qfInput$mfcVali$qfPersTemp)
        setQf$sensMfc <- data.frame("qfFrt00" = qfInput$mfcVali$qfFrt00)
      } else {
        #assign qf for irgaMfcSamp to -1 when qf irgaMfcSamp are missing
        setQf$frt00 <- data.frame("qfRngFrt00" = -1, 
                                  "qfStepFrt00" = -1, 
                                  "qfPersFrt00" = -1)
        setQf$frt <- data.frame("qfRngFrt" = -1,
                                "qfStepFrt" = -1,
                                "qfPersFrt" = -1)
        setQf$presAtmMfc <- data.frame("qfRngPresAtm" = -1, 
                                       "qfStepPresAtm" = -1,
                                       "qfPersPresAtm" = -1)
        setQf$tempMfc <- data.frame("qfRngTemp" = -1,
                                    "qfStepTemp" = -1,
                                    "qfPersTemp" = -1)
        setQf$sensMfc <- data.frame("qfFrt00" = -1)
      }
     
#grouping qulity flags that related to irgaCo2 L1 sub-data product
     if (dp01 == "irgaCo2") {
       rpt$rtioMoleDryCo2 <- data.frame(setQf$rtioMoleDryCo2, setQf$asrpCo2,
                                        setQf$asrpH2o, setQf$rtioMoleWetCo2,
                                        setQf$rtioMoleWetH2o, setQf$presIrga,
                                        setQf$tempIrga, setQf$envHut, 
                                        setQf$valvAux, setQf$frt00, 
                                        setQf$frt, setQf$presAtmMfc, 
                                        setQf$tempMfc, setQf$sensMfc)
       rpt$rtioMoleWetCo2 <- data.frame(setQf$rtioMoleWetCo2, setQf$asrpCo2,
                                        setQf$asrpH2o, setQf$rtioMoleWetH2o, 
                                        setQf$presIrga, setQf$tempIrga, 
                                        setQf$envHut, setQf$valvAux, 
                                        setQf$frt00, setQf$frt, 
                                        setQf$presAtmMfc, setQf$tempMfc, 
                                        setQf$sensMfc)
       rpt$pres <- data.frame(setQf$presIrga)
       rpt$frt00 <- data.frame (setQf$frt00, setQf$frt,
                                setQf$presAtmMfc, setQf$tempMfc,
                                setQf$sensMfc)
       rpt$temp <- data.frame (setQf$tempIrga)
     }
#grouping qulity flags that related to irgaH2o L1 sub-data product    
     if (dp01 == "irgaH2o") {
       rpt$rtioMoleDryH2o <- data.frame(setQf$rtioMoleDryH2o, setQf$asrpH2o,
                                        setQf$rtioMoleWetH2o, setQf$presIrga,
                                        setQf$tempIrga, setQf$envHut, 
                                        setQf$valvAux, setQf$frt00, 
                                        setQf$frt, setQf$presAtmMfc, 
                                        setQf$tempMfc, setQf$sensMfc)
       rpt$rtioMoleWetH2o <- data.frame(setQf$rtioMoleWetH2o, setQf$asrpH2o,
                                        setQf$presIrga, setQf$tempIrga, 
                                        setQf$envHut, setQf$valvAux, 
                                        setQf$frt00, setQf$frt, 
                                        setQf$presAtmMfc, setQf$tempMfc, 
                                        setQf$sensMfc)
       rpt$pres <- data.frame(setQf$presIrga)
       rpt$frt00 <- data.frame(setQf$frt00, setQf$frt,
                               setQf$presAtmMfc, setQf$tempMfc,
                               setQf$sensMfc) 
       rpt$temp <- data.frame(setQf$tempIrga) 
     }
    } # closed MeasType loop
  

    #remove setQf
    setQf <- NULL
  }#closed loop for irgaCo2 and irgaH2o
  
#isopCo2 ####################################################################################
  if (dp01 == "isopCo2") {
    
    setQf$rtioMoleDryCo2 <- data.frame("qfRngRtioMoleDryCo2" = qfInput$crdCo2$qfRngRtioMoleDryCo2, 
                                       "qfStepRtioMoleDryCo2" = qfInput$crdCo2$qfStepRtioMoleDryCo2,
                                       "qfPersRtioMoleDryCo2" = qfInput$crdCo2$qfPersRtioMoleDryCo2, 
                                       "qfCalRtioMoleDryCo2" = qfInput$crdCo2$qfCalRtioMoleDryCo2)
    
    setQf$rtioMoleDry12CCo2 <- data.frame("qfRngRtioMoleDry12CCo2" = qfInput$crdCo2$qfRngRtioMoleDry12CCo2, 
                                          "qfStepRtioMoleDry12CCo2" = qfInput$crdCo2$qfStepRtioMoleDry12CCo2,
                                          "qfPersRtioMoleDry12CCo2" = qfInput$crdCo2$qfPersRtioMoleDry12CCo2, 
                                          "qfCalRtioMoleDry12CCo2" = qfInput$crdCo2$qfCalRtioMoleDry12CCo2)
    
    setQf$rtioMoleDry13CCo2 <- data.frame("qfRngRtioMoleDry13CCo2" = qfInput$crdCo2$qfRngRtioMoleDry13CCo2, 
                                          "qfStepRtioMoleDry13CCo2" = qfInput$crdCo2$qfStepRtioMoleDry13CCo2,
                                          "qfPersRtioMoleDry13CCo2" = qfInput$crdCo2$qfPersRtioMoleDry13CCo2, 
                                          "qfCalRtioMoleDry13CCo2" = qfInput$crdCo2$qfCalRtioMoleDry13CCo2)
    
    setQf$rtioMoleDryH2o <- data.frame("qfRngRtioMoleDryH2o" = qfInput$crdCo2$qfRngRtioMoleDryH2o, 
                                       "qfStepRtioMoleDryH2o" = qfInput$crdCo2$qfStepRtioMoleDryH2o,
                                       "qfPersRtioMoleDryH2o" = qfInput$crdCo2$qfPersRtioMoleDryH2o, 
                                       "qfCalRtioMoleDryH2o" = qfInput$crdCo2$qfCalRtioMoleDryH2o)
    
    setQf$rtioMoleWetCo2 <- data.frame("qfRngRtioMoleWetCo2" = qfInput$crdCo2$qfRngRtioMoleWetCo2, 
                                       "qfStepRtioMoleWetCo2" = qfInput$crdCo2$qfStepRtioMoleWetCo2,
                                       "qfPersRtioMoleWetCo2" = qfInput$crdCo2$qfPersRtioMoleWetCo2, 
                                       "qfCalRtioMoleWetCo2" = qfInput$crdCo2$qfCalRtioMoleWetCo2)
    
    setQf$rtioMoleWet12CCo2 <- data.frame("qfRngRtioMoleWet12CCo2" = qfInput$crdCo2$qfRngRtioMoleWet12CCo2, 
                                          "qfStepRtioMoleWet12CCo2" = qfInput$crdCo2$qfStepRtioMoleWet12CCo2,
                                          "qfPersRtioMoleWet12CCo2" = qfInput$crdCo2$qfPersRtioMoleWet12CCo2, 
                                          "qfCalRtioMoleWet12CCo2" = qfInput$crdCo2$qfCalRtioMoleWet12CCo2)
    
    setQf$rtioMoleWet13CCo2 <- data.frame("qfRngRtioMoleWet13CCo2" = qfInput$crdCo2$qfRngRtioMoleWet13CCo2, 
                                          "qfStepRtioMoleWet13CCo2" = qfInput$crdCo2$qfStepRtioMoleWet13CCo2,
                                          "qfPersRtioMoleWet13CCo2" = qfInput$crdCo2$qfPersRtioMoleWet13CCo2, 
                                          "qfCalRtioMoleWet13CCo2 " = qfInput$crdCo2$qfCalRtioMoleWet13CCo2)
    
    setQf$rtioMoleWetH2o <- data.frame("qfRngRtioMoleWetH2o" = qfInput$crdCo2$qfRngRtioMoleWetH2o, 
                                       "qfStepRtioMoleWetH2o" = qfInput$crdCo2$qfStepRtioMoleWetH2o,
                                       "qfPersRtioMoleWetH2o" = qfInput$crdCo2$qfPersRtioMoleWetH2o, 
                                       "qfCalRtioMoleWetH2o" = qfInput$crdCo2$qfCalRtioMoleWetH2o)
    
    setQf$dlta13CCo2 <- data.frame("qfRngDlta13CCo2" = qfInput$crdCo2$qfRngDlta13CCo2, 
                                   "qfStepDlta13CCo2" = qfInput$crdCo2$qfStepDlta13CCo2,
                                   "qfPersDlta13CCo2" = qfInput$crdCo2$qfPersDlta13CCo2, 
                                   "qfCalDlta13CCo2" = qfInput$crdCo2$qfCalDlta13CCo2)
    
    setQf$presCrdCo2 <- data.frame("qfRngPres" = qfInput$crdCo2$qfRngPres, 
                                   "qfStepPres" = qfInput$crdCo2$qfStepPres,
                                   "qfPersPres" = qfInput$crdCo2$qfPersPres, 
                                   "qfCalPres" = qfInput$crdCo2$qfCalPres)
    
    setQf$tempCrdCo2 <- data.frame("qfRngTemp" = qfInput$crdCo2$qfRngTemp, 
                                   "qfStepTemp" = qfInput$crdCo2$qfStepTemp,
                                   "qfPersTemp" = qfInput$crdCo2$qfPersTemp, 
                                   "qfCalTemp" = qfInput$crdCo2$qfCalTemp)
    
    setQf$tempWbox <- data.frame("qfRngTempWbox" = qfInput$crdCo2$qfRngTempWbox, 
                                 "qfStepTempWbox" = qfInput$crdCo2$qfStepTempWbox,
                                 "qfPersTempWbox" = qfInput$crdCo2$qfPersTempWbox, 
                                 "qfCalTempWbox" = qfInput$crdCo2$qfCalTempWbox)
    setQf$sensCrdCo2 <- data.frame("qfSensStus" = qfInput$crdCo2$qfSensStus)
    setQf$heatInlt <- data.frame("qfHeat" = ifelse(!is.null(qfInput$heatInlt), qfInput$heatInlt$qfHeat, -1))
    
    #define qf which use only sampling period
    if (TypeMeas == "samp") {
      #grouping qulity flags that related to isoCo2 L1 sub-data product  
      rpt$rtioMoleWetCo2 <- data.frame(setQf$rtioMoleWetCo2, setQf$dlta13CCo2,
                                       setQf$rtioMoleWet12CCo2, setQf$presCrdCo2, 
                                       setQf$tempCrdCo2, setQf$tempWbox,
                                       setQf$sensCrdCo2, setQf$heatInlt)
      rpt$rtioMoleDryCo2 <- data.frame(setQf$rtioMoleDryCo2, setQf$dlta13CCo2,
                                       setQf$rtioMoleWet12CCo2, setQf$presCrdCo2, 
                                       setQf$tempCrdCo2, setQf$tempWbox,
                                       setQf$sensCrdCo2, setQf$heatInlt)
      rpt$rtioMoleWet12CCo2 <- data.frame(setQf$rtioMoleWet12CCo2, setQf$presCrdCo2, 
                                          setQf$tempCrdCo2, setQf$tempWbox,
                                          setQf$sensCrdCo2, setQf$heatInlt)
      rpt$rtioMoleDry12CCo2 <- data.frame(setQf$rtioMoleDry12CCo2, setQf$rtioMoleWet13CCo2,
                                          setQf$presCrdCo2, setQf$tempCrdCo2, 
                                          setQf$tempWbox, setQf$sensCrdCo2, 
                                          setQf$heatInlt)
      rpt$rtioMoleWet13CCo2 <- data.frame(setQf$rtioMoleWet13CCo2, setQf$presCrdCo2, 
                                          setQf$tempCrdCo2, setQf$tempWbox,
                                          setQf$sensCrdCo2, setQf$heatInlt)
      rpt$rtioMoleDry13CCo2 <- data.frame(setQf$rtioMoleDry13CCo2, setQf$rtioMoleWet13CCo2,
                                          setQf$presCrdCo2, setQf$tempCrdCo2, 
                                          setQf$tempWbox, setQf$sensCrdCo2, 
                                          setQf$heatInlt)
      rpt$dlta13CCo2 <- data.frame(setQf$dlta13CCo2, setQf$presCrdCo2,
                                   setQf$tempCrdCo2, setQf$tempWbox,
                                   setQf$sensCrdCo2, setQf$heatInlt)
      rpt$rtioMoleWetH2o <- data.frame(setQf$rtioMoleWetH2o, setQf$presCrdCo2,
                                       setQf$tempCrdCo2, setQf$tempWbox,
                                       setQf$sensCrdCo2, setQf$heatInlt)
      rpt$rtioMoleDryH2o <- data.frame(setQf$rtioMoleDryH2o, setQf$rtioMoleWetH2o, 
                                       setQf$presCrdCo2, setQf$tempCrdCo2, 
                                       setQf$tempWbox, setQf$sensCrdCo2, 
                                       setQf$heatInlt)
      rpt$temp <- data.frame(setQf$tempCrdCo2, setQf$sensCrdCo2)
      rpt$pres <- data.frame(setQf$presCrdCo2, setQf$sensCrdCo2)
      
    }#close loop for sampling
    
    #define qf which use only validation period
    if (TypeMeas == "vali") {
      
      #qf from mfcVali
      if (!is.null(qfInput$mfcVali)){
        #irgaMfcVali
        setQf$frt00 <- data.frame("qfRngFrt00" = qfInput$mfcVali$qfRngFrt00, 
                                  "qfStepFrt00" = qfInput$mfcVali$qfStepFrt00, 
                                  "qfPersFrt00" = qfInput$mfcVali$qfPersFrt00)
        setQf$frt <- data.frame("qfRngFrt" = qfInput$mfcVali$qfRngFrt,
                                "qfStepFrt" = qfInput$mfcVali$qfStepFrt,
                                "qfPersFrt" = qfInput$mfcVali$qfPersFrt)
        setQf$presAtmMfc <- data.frame("qfRngPresAtm" = qfInput$mfcVali$qfRngPresAtm, 
                                       "qfStepPresAtm" = qfInput$mfcVali$qfStepPresAtm,
                                       "qfPersPresAtm" = qfInput$mfcVali$qfPersPresAtm)
        setQf$tempMfc <- data.frame("qfRngTemp" = qfInput$mfcVali$qfRngTemp,
                                    "qfStepTemp" = qfInput$mfcVali$qfStepTemp,
                                    "qfPersTemp" = qfInput$mfcVali$qfPersTemp)
        setQf$sensMfc <- data.frame("qfFrt00" = qfInput$mfcVali$qfFrt00)
      } else {
        #assign qf for irgaMfcSamp to -1 when qf irgaMfcSamp are missing
        setQf$frt00 <- data.frame("qfRngFrt00" = -1, 
                                  "qfStepFrt00" = -1, 
                                  "qfPersFrt00" = -1)
        setQf$frt <- data.frame("qfRngFrt" = -1,
                                "qfStepFrt" = -1,
                                "qfPersFrt" = -1)
        setQf$presAtmMfc <- data.frame("qfRngPresAtm" = -1, 
                                       "qfStepPresAtm" = -1,
                                       "qfPersPresAtm" = -1)
        setQf$tempMfc <- data.frame("qfRngTemp" = -1,
                                    "qfStepTemp" = -1,
                                    "qfPersTemp" = -1)
        setQf$sensMfc <- data.frame("qfFrt00" = -1)
      }
      
      #grouping qulity flags that related to isoCo2 L1 sub-data product  
      rpt$rtioMoleWetCo2 <- data.frame(setQf$rtioMoleWetCo2, setQf$dlta13CCo2,
                                       setQf$rtioMoleWet12CCo2, setQf$presCrdCo2, 
                                       setQf$tempCrdCo2, setQf$tempWbox,
                                       setQf$sensCrdCo2, setQf$frt00, 
                                       setQf$frt, setQf$presAtmMfc, 
                                       setQf$tempMfc, setQf$sensMfc)
      rpt$rtioMoleDryCo2 <- data.frame(setQf$rtioMoleDryCo2, setQf$dlta13CCo2,
                                       setQf$rtioMoleWet12CCo2, setQf$presCrdCo2, 
                                       setQf$tempCrdCo2, setQf$tempWbox,
                                       setQf$sensCrdCo2, setQf$frt00, 
                                       setQf$frt, setQf$presAtmMfc, 
                                       setQf$tempMfc, setQf$sensMfc)
      rpt$rtioMoleWet12CCo2 <- data.frame(setQf$rtioMoleWet12CCo2, setQf$presCrdCo2, 
                                          setQf$tempCrdCo2, setQf$tempWbox,
                                          setQf$sensCrdCo2, setQf$frt00, 
                                          setQf$frt, setQf$presAtmMfc, 
                                          setQf$tempMfc, setQf$sensMfc)
      rpt$rtioMoleDry12CCo2 <- data.frame(setQf$rtioMoleDry12CCo2, setQf$rtioMoleWet13CCo2,
                                          setQf$presCrdCo2, setQf$tempCrdCo2, 
                                          setQf$tempWbox, setQf$sensCrdCo2, 
                                          setQf$frt00, setQf$frt, 
                                          setQf$presAtmMfc, setQf$tempMfc, 
                                          setQf$sensMfc)
      rpt$rtioMoleWet13CCo2 <- data.frame(setQf$rtioMoleWet13CCo2, setQf$presCrdCo2, 
                                          setQf$tempCrdCo2, setQf$tempWbox,
                                          setQf$sensCrdCo2, setQf$frt00, 
                                          setQf$frt, setQf$presAtmMfc, 
                                          setQf$tempMfc, setQf$sensMfc)
      rpt$rtioMoleDry13CCo2 <- data.frame(setQf$rtioMoleDry13CCo2, setQf$rtioMoleWet13CCo2,
                                          setQf$presCrdCo2, setQf$tempCrdCo2, 
                                          setQf$tempWbox, setQf$sensCrdCo2, 
                                          setQf$frt00, setQf$frt, 
                                          setQf$presAtmMfc, setQf$tempMfc, 
                                          setQf$sensMfc)
      rpt$dlta13CCo2 <- data.frame(setQf$dlta13CCo2, setQf$presCrdCo2,
                                   setQf$tempCrdCo2, setQf$tempWbox,
                                   setQf$sensCrdCo2, setQf$frt00, 
                                   setQf$frt, setQf$presAtmMfc, 
                                   setQf$tempMfc, setQf$sensMfc)
      rpt$rtioMoleWetH2o <- data.frame(setQf$rtioMoleWetH2o, setQf$presCrdCo2,
                                       setQf$tempCrdCo2, setQf$tempWbox,
                                       setQf$sensCrdCo2, setQf$frt00, 
                                       setQf$frt, setQf$presAtmMfc, 
                                       setQf$tempMfc, setQf$sensMfc)
      rpt$rtioMoleDryH2o <- data.frame(setQf$rtioMoleDryH2o, setQf$rtioMoleWetH2o, 
                                       setQf$presCrdCo2, setQf$tempCrdCo2, 
                                       setQf$tempWbox, setQf$sensCrdCo2, 
                                       setQf$frt00, setQf$frt, 
                                       setQf$presAtmMfc, setQf$tempMfc, 
                                       setQf$sensMfc)
      rpt$temp <- data.frame(setQf$tempCrdCo2, setQf$sensCrdCo2)
      rpt$pres <- data.frame(setQf$presCrdCo2, setQf$sensCrdCo2)
    }#closed loop for vali
    #remove setQf
    setQf <- NULL
    }#Closed loop for isopCo2

#isopH2o ####################################################################################
  if (dp01 == "isopH2o") {
    setQf$rtioMoleDryH2o <- data.frame("qfRngRtioMoleDryH2o" = qfInput$crdH2o$qfRngRtioMoleDryH2o, 
                                       "qfStepRtioMoleDryH2o" = qfInput$crdH2o$qfStepRtioMoleDryH2o,
                                       "qfPersRtioMoleDryH2o" = qfInput$crdH2o$qfPersRtioMoleDryH2o, 
                                       "qfCalRtioMoleDryH2o" = qfInput$crdH2o$qfCalRtioMoleDryH2o)
    setQf$rtioMoleWetH2o <- data.frame("qfRngRtioMoleWetH2o" = qfInput$crdH2o$qfRngRtioMoleWetH2o, 
                                       "qfStepRtioMoleWetH2o" = qfInput$crdH2o$qfStepRtioMoleWetH2o,
                                       "qfPersRtioMoleWetH2o" = qfInput$crdH2o$qfPersRtioMoleWetH2o, 
                                       "qfCalRtioMoleWetH2o" = qfInput$crdH2o$qfCalRtioMoleWetH2o)
    setQf$dlta18OH2o <- data.frame("qfRngDlta18OH2o" = qfInput$crdH2o$qfRngDlta18OH2o, 
                                   "qfStepDlta18OH2o" = qfInput$crdH2o$qfStepDlta18OH2o,
                                   "qfPersDlta18OH2o" = qfInput$crdH2o$qfPersDlta18OH2o, 
                                   "qfCalDlta18OH2o" = qfInput$crdH2o$qfCalDlta18OH2o)
    setQf$dlta2HH2o <- data.frame("qfRngDlta2HH2o" = qfInput$crdH2o$qfRngDlta2HH2o, 
                                  "qfStepDlta2HH2o" = qfInput$crdH2o$qfStepDlta2HH2o,
                                  "qfPersDlta2HH2o" = qfInput$crdH2o$qfPersDlta2HH2o, 
                                  "qfCalDlta2HH2o" = qfInput$crdH2o$qfCalDlta2HH2o)
    setQf$presCrdH2o <- data.frame("qfRngPres" = qfInput$crdH2o$qfRngPres, 
                                   "qfStepPres" = qfInput$crdH2o$qfStepPres,
                                   "qfPersPres" = qfInput$crdH2o$qfPersPres, 
                                   "qfCalPres" = qfInput$crdH2o$qfCalPres)
    setQf$tempCrdH2o <- data.frame("qfRngTemp" = qfInput$crdH2o$qfRngTemp, 
                                   "qfStepTemp" = qfInput$crdH2o$qfStepTemp,
                                   "qfPersTemp" = qfInput$crdH2o$qfPersTemp, 
                                   "qfCalTemp" = qfInput$crdH2o$qfCalTemp)
    setQf$tempWbox <- data.frame("qfRngTempWbox" = qfInput$crdH2o$qfRngTempWbox, 
                                 "qfStepTempWbox" = qfInput$crdH2o$qfStepTempWbox,
                                 "qfPersTempWbox" = qfInput$crdH2o$qfPersTempWbox, 
                                 "qfCalTempWbox" = qfInput$crdH2o$qfCalTempWbox)
    setQf$sensCrdH2o <- data.frame("qfSensStus" = qfInput$crdH2o$qfSensStus,
                                   "qfStusN2" = qfInput$crdH2o$qfStusN2)
    setQf$envHut <- data.frame("qfRh" = ifelse(!is.null(qfInput$envHut), qfInput$envHut$qfRh, -1))
    setQf$heatInlt <- data.frame("qfHeat" = ifelse(!is.null(qfInput$heatInlt), qfInput$heatInlt$qfHeat))
    
  #define qf which use only sampling period
    if (TypeMeas == "samp") {     
  #grouping qulity flags that related to isopH2o L1 sub-data product  
    rpt$rtioMoleDryH2o <- data.frame(setQf$rtioMoleDryH2o, setQf$rtioMoleWetH2o,
                                     setQf$presCrdH2o, setQf$tempCrdH2o,
                                     setQf$tempWbox,  setQf$sensCrdH2o,
                                     setQf$envHut, setQf$heatInlt)
    rpt$rtioMoleWetH2o <- data.frame(setQf$rtioMoleWetH2o, setQf$presCrdH2o, 
                                     setQf$tempCrdH2o, setQf$tempWbox,  
                                     setQf$sensCrdH2o, setQf$envHut, 
                                     setQf$heatInlt)
    rpt$dlta18OH2o <- data.frame(setQf$dlta18OH2o, setQf$presCrdH2o, 
                                 setQf$tempCrdH2o, setQf$tempWbox,  
                                 setQf$sensCrdH2o, setQf$envHut, 
                                 setQf$heatInlt)
    rpt$dlta2HH2o <- data.frame(setQf$dlta2HH2o, setQf$presCrdH2o, 
                                setQf$tempCrdH2o, setQf$tempWbox,  
                                setQf$sensCrdH2o, setQf$envHut, 
                                setQf$heatInlt)
    rpt$pres <- data.frame(setQf$presCrdH2o, qfSensStus = setQf$sensCrdH2o$qfSensStus)
    rpt$temp <- data.frame(setQf$tempCrdH2o, qfSensStus = setQf$sensCrdH2o$qfSensStus) 
   }#closed loop for samp
    
    #define qf which use only validation period
    if (TypeMeas == "vali") {     
  #grouping qulity flags that related to isopH2o L1 sub-data product  
      rpt$rtioMoleDryH2o <- data.frame(setQf$rtioMoleDryH2o, setQf$rtioMoleWetH2o,
                                       setQf$presCrdH2o, setQf$tempCrdH2o,
                                       setQf$tempWbox,  setQf$sensCrdH2o,
                                       setQf$envHut)
      rpt$rtioMoleWetH2o <- data.frame(setQf$rtioMoleWetH2o, setQf$presCrdH2o, 
                                       setQf$tempCrdH2o, setQf$tempWbox,  
                                       setQf$sensCrdH2o, setQf$envHut)
      rpt$dlta18OH2o <- data.frame(setQf$dlta18OH2o, setQf$presCrdH2o, 
                                   setQf$tempCrdH2o, setQf$tempWbox,  
                                   setQf$sensCrdH2o, setQf$envHut)
      rpt$dlta2HH2o <- data.frame(setQf$dlta2HH2o, setQf$presCrdH2o, 
                                  setQf$tempCrdH2o, setQf$tempWbox,  
                                  setQf$sensCrdH2o, setQf$envHut)
      rpt$pres <- data.frame(setQf$presCrdH2o, qfSensStus = setQf$sensCrdH2o$qfSensStus)
      rpt$temp <- data.frame(setQf$tempCrdH2o, qfSensStus = setQf$sensCrdH2o$qfSensStus)  
  }#closed loop for vali
    
    #remove setQf
    setQf <- NULL
}#closed loop for isopH2o 

#envHut ####################################################################################
  if (dp01 == "envHut") {
    setQf$tempEnvHut <- data.frame("qfRngTemp" = qfInput$envHut$qfRngTemp, 
                                   "qfStepTemp" = qfInput$envHut$qfStepTemp,
                                   "qfPersTemp" = qfInput$envHut$qfPersTemp)
    setQf$rh <- data.frame("qfRngRh" = qfInput$envHut$qfRng, 
                           "qfStepRh" = qfInput$envHut$qfStep,
                           "qfPersRh" = qfInput$envHut$qfPers)
    setQf$presEnvHut <- data.frame("qfRngPres" = qfInput$envHut$qfRngPres, 
                                   "qfStepPres" = qfInput$envHut$qfStepPres,
                                   "qfPersPres" = qfInput$envHut$qfPersPres)
    setQf$rtioMoleWetH2o <- data.frame("qfRngRtioMoleWetH2o" = qfInput$envHut$qfRngRtioMoleWetH2o, 
                                       "qfStepRtioMoleWetH2o" = qfInput$envHut$qfStepRtioMoleWetH2o,
                                       "qfPersRtioMoleWetH2o" = qfInput$envHut$qfPersRtioMoleWetH2o)
    
    #grouping qulity flags that related to envHut L1 sub-data product
    rpt$temp <- data.frame(setQf$tempEnvHut)
    rpt$rh <- data.frame(setQf$rh)
    rpt$pres <- data.frame(setQf$presEnvHut)
    rpt$rtioMoleWetH2o <- data.frame(setQf$rtioMoleWetH2o)
    
    #remove setQf
    setQf <- NULL
  }#closed loop for envHut

#tempAirLvl ####################################################################################
  if (dp01 == "tempAirLvl") {
    setQf$tempAirLvl <- data.frame("qfRngTemp" = qfInput$tempAirLvl$qfRngTemp, 
                                   "qfStepTemp" = qfInput$tempAirLvl$qfStepTemp,
                                   "qfPersTemp" = qfInput$tempAirLvl$qfPersTemp,
                                   "qfCalTemp" = qfInput$tempAirLvl$qfCalTemp)
    
    setQf$sensTempAirLvl <- data.frame("qfHeat" = qfInput$tempAirLvl$qfHeat,
                                       "qfFlow" = qfInput$tempAirLvl$qfFlow)
    
    #grouping qulity flags that related to tempAirLvl L1 sub-data product
    rpt$temp <- data.frame(setQf$tempAirLvl, setQf$sensTempAirLvl)
    
    #remove setQf
    setQf <- NULL
  }#closed loop for tempAirLvl
  
#tempAirTop ####################################################################################
  if (dp01 == "tempAirTop") {
    setQf$temp01 <- data.frame("qfRngTemp01" = qfInput$tempAirTop$qfRngTemp01, 
                                   "qfStepTemp01" = qfInput$tempAirTop$qfStepTemp01,
                                   "qfPersTemp01" = qfInput$tempAirTop$qfPersTemp01,
                                   "qfCalTemp01" = qfInput$tempAirTop$qfCalTemp01)
    
    setQf$temp02 <- data.frame("qfRngTemp02" = qfInput$tempAirTop$qfRngTemp02, 
                               "qfStepTemp02" = qfInput$tempAirTop$qfStepTemp02,
                               "qfPersTemp02" = qfInput$tempAirTop$qfPersTemp02,
                               "qfCalTemp02" = qfInput$tempAirTop$qfCalTemp02)
    
    setQf$temp03 <- data.frame("qfRngTemp03" = qfInput$tempAirTop$qfRngTemp03, 
                               "qfStepTemp03" = qfInput$tempAirTop$qfStepTemp03,
                               "qfPersTemp03" = qfInput$tempAirTop$qfPersTemp03,
                               "qfCalTemp03" = qfInput$tempAirTop$qfCalTemp03)
    
    setQf$sensTempAirTop <- data.frame("qfHeat" = qfInput$tempAirTop$qfHeat,
                                       "qfFlow" = qfInput$tempAirTop$qfFlow)
    
    #grouping qulity flags that related to tempAirLvl L1 sub-data product
    rpt$temp <- data.frame(setQf$temp01, setQf$temp02,
                           setQf$temp03, setQf$sensTempAirTop)
    
    #remove setQf
    setQf <- NULL
  }#closed loop for tempAirLvl
}# closed loop for ecse
  return(rpt)
  
  # end function def.neon.dp01.qf.grp()
}
