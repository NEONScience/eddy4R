##############################################################################################
#' @title Grouping the quality flags for each of NEON eddy-covariance turbulent exchange L1 data product

#' @author Natchaya Pingintha-Durden \email{ndurden@neoninc.org} \cr
#' 

#' @description Function definition. Grouping the quality flags of each NEON eddy-covariance turbulent exchange L1 data product into a single dataframe for further use in the calculation of Alpha, Beta, and Final flag.

#' @param \code{qfSens00} A dataframe containing the input flag data that related to L1 data products are being grouped. Of class integer". [-] 
#' @param \code{qfSens01} Optional. A dataframe containing the input flag data that related to L1 data products are being grouped. Of class integer". [-] 
#' @param \code{qfSens01} Optional. A dataframe containing the input flag data that related to L1 data products are being grouped. Of class integer". [-]
#' @param \code{dp01} A vector of class "character" containing the name of NEON eddy-covariance turbulent exchange L1 data products which the flags are being grouped, c("irgaCo2","irgaH2o","soni","soniAmrs"). Defaults to "irgaCo2". [-] 

#' @return A list of data frame of the quality flags related to that sub-data product. \cr

#' @references
#' License: Terms of use of the NEON FIU algorithm repository dated 2015-01-16

#' @keywords NEON QAQC, quality flags, quality metrics

#' @examples 
#' #generate the fake quality flags for each sensor
#' TimeBgn <- "2016-04-24 02:00:00.000"
#' TimeEnd <- "2016-04-24 02:29:59.950"
#' inp <- list()
#' inp$qf$irga <- eddy4R.qaqc::def.qf.ecte(TimeBgn = TimeBgn, TimeEnd = TimeEnd, Freq = 20, Sens = "irga", PcntQf = 0.05)
#' inp$qf$irgaMfcSamp <- eddy4R.qaqc::def.qf.ecte(TimeBgn = TimeBgn, TimeEnd = TimeEnd, Freq = 20, Sens = "irgaMfcSamp", PcntQf = 0.05)
#' inp$qf$soni <- eddy4R.qaqc::def.qf.ecte(TimeBgn = TimeBgn, TimeEnd = TimeEnd, Freq = 20, Sens = "soni", PcntQf = 0.05)
#' inp$qf$soniAmrs <- eddy4R.qaqc::def.qf.ecte(TimeBgn = TimeBgn, TimeEnd = TimeEnd, Freq = 40, Sens = "soniAmrs", PcntQf = 0.05)
#' 
#' #grouping the set of the flags
#' qfGrupIrgaCo2 <- def.neon.ecte.dp01.qf.grup(qfSens00 = inp$qf$irga, qfSens01 = inp$qf$irgaMfcSamp, dp01="irgaCo2")
#' qfGrupSoni <- def.neon.ecte.dp01.qf.grup(qfSens00 = inp$qf$soni, qfSens01 = inp$qf$irga, dp01="soni")

#' @seealso Currently none

#' @export

# changelog and author contributions / copyrights
#   Natchaya P-Durden (2016-12-01)
#     original creation
#   Natchaya P-Durden (2016-12-09)
#     added flexibility if some sensor is missing
##############################################################################################

def.neon.ecte.dp01.qf.grup <- function(
  qfSens00, 
  qfSens01 = NULL,
  qfSens02 = NULL,
  dp01 = c("irgaCo2","irgaH2o","soni","soniAmrs")[1]
){
  
  rpt <- list()
  setQf <- list()
  
  #irgaCo2####################################################################################
  if (dp01 == "irgaCo2") {
    #organized all quality flags from irga into the set of flags (for frequency use)
    #irga sensor flags
    setQf$sens <- data.frame("qfIrgaHead" = qfSens00$qfIrgaHead, 
                             "qfIrgaTemp" = qfSens00$qfIrgaTemp, 
                             "qfIrgaTempIn" = qfSens00$qfIrgaTempIn,
                             "qfIrgaAux" = qfSens00$qfIrgaAux, 
                             "qfIrgaPres" = qfSens00$qfIrgaPres, 
                             "qfIrgaChop" = qfSens00$qfIrgaChop, 
                             "qfIrgaDetc" = qfSens00$qfIrgaDetc, 
                             "qfIrgaPll" = qfSens00$qfIrgaPll, 
                             "qfIrgaSync" = qfSens00$qfIrgaSync, 
                             "qfIrgaAgc" = qfSens00$qfIrgaAgc)
    
    setQf$asrpCo2 <- data.frame("qfRngMinAsrpCo2" = qfSens00$qfRngMinAsrpCo2, 
                                "qfStepAsrpCo2" = qfSens00$qfStepAsrpCo2, 
                                "qfPersAsrpCo2" = qfSens00$qfPersAsrpCo2, 
                                "qfCalAsrpCo2" = qfSens00$qfCalAsrpCo2)
    
    setQf$ssiCo2 <- data.frame("qfRngMinSsiCo2" = qfSens00$qfRngMinSsiCo2, 
                               "qfStepSsiCo2" = qfSens00$qfStepSsiCo2,
                               "qfPersSsiCo2" = qfSens00$qfPersSsiCo2, 
                               "qfCalSsiCo2" = qfSens00$qfCalSsiCo2)
    
    setQf$rtioMoleDryCo2 <- data.frame("qfRngMinRtioMoleDryCo2" = qfSens00$qfRngMinRtioMoleDryCo2, 
                                       "qfStepRtioMoleDryCo2" = qfSens00$qfStepRtioMoleDryCo2,
                                       "qfPersRtioMoleDryCo2" = qfSens00$qfPersRtioMoleDryCo2, 
                                       "qfCalRtioMoleDryCo2" = qfSens00$qfCalRtioMoleDryCo2)
    
    setQf$densMoleCo2 <- data.frame("qfRngMinDensMoleCo2" = qfSens00$qfRngMinDensMoleCo2, 
                                    "qfStepDensMoleCo2" = qfSens00$qfStepDensMoleCo2,
                                    "qfPersDensMoleCo2" = qfSens00$qfPersDensMoleCo2, 
                                    "qfCalDensMoleCo2" = qfSens00$qfCalDensMoleCo2) 
    
    setQf$presAtm <- data.frame("qfRngMinPresAtm" = qfSens00$qfRngMinPresAtm, 
                                "qfStepPresAtm" = qfSens00$qfStepPresAtm,
                                "qfPersPresAtm" = qfSens00$qfPersPresAtm, 
                                "qfCalPresAtm" = qfSens00$qfCalPresAtm)
    
    setQf$presSum <- data.frame("qfRngMinPresSum" = qfSens00$qfRngMinPresSum, 
                                "qfStepPresSum" = qfSens00$qfStepPresSum,
                                "qfPersPresSum" = qfSens00$qfPersPresSum, 
                                "qfCalPresSum" = qfSens00$qfCalPresSum)
    
    setQf$tempAve <- data.frame ("qfRngMinTempMean" = qfSens00$qfRngMinTempMean, 
                                 "qfStepTempMean" = qfSens00$qfStepTempMean,
                                 "qfPersTempMean" = qfSens00$qfPersTempMean, 
                                 "qfCalTempMean" = qfSens00$qfCalTempMean)
    
    if (!is.null(qfSens01)){
      #irgaMfcSamp
      setQf$frt00 <- data.frame("qfRngMinFrt00" = qfSens01$qfRngMinFrt00, 
                                "qfStepFrt00" = qfSens01$qfStepFrt00, 
                                "qfPersFrt00" = qfSens01$qfPersFrt00, 
                                "qfCalFrt00" = qfSens01$qfCalFrt00)
    } else {
      #assign qf for irgaMfcSamp to -1 when qf irgaMfcSamp are missing
      setQf$frt00 <- data.frame("qfRngMinFrt00" = -1, 
                                "qfStepFrt00" = -1, 
                                "qfPersFrt00" = -1, 
                                "qfCalFrt00" = -1)
    }

    #gruping qulity flags that related to L1 sub-data product
    rpt$rtioMoleDryCo2 <- data.frame(setQf$sens, setQf$asrpCo2, 
                                     setQf$ssiCo2, setQf$rtioMoleDryCo2)
    rpt$densMoleCo2 <- data.frame(setQf$sens, setQf$asrpCo2, 
                                  setQf$ssiCo2,setQf$densMoleCo2)
    rpt$presAtm <- data.frame(setQf$sens, setQf$presAtm)
    rpt$presSum <- data.frame(setQf$sens, setQf$presSum)
    rpt$frt00Samp <- data.frame (setQf$frt00)
    rpt$tempAve <- data.frame (setQf$sens, setQf$tempAve)
  
    #remove setQf
    setQf <- NULL
  }
  
  #irgaH2o#################################################################################
  if (dp01 == "irgaH2o") {
    #organized all quality flags from irga into the set of flags (for frequency use)
    #irga sensor flags
    setQf$sens <- data.frame("qfIrgaHead" = qfSens00$qfIrgaHead, 
                             "qfIrgaTemp" = qfSens00$qfIrgaTemp, 
                             "qfIrgaTempIn" = qfSens00$qfIrgaTempIn,
                             "qfIrgaAux" = qfSens00$qfIrgaAux, 
                             "qfIrgaPres" = qfSens00$qfIrgaPres, 
                             "qfIrgaChop" = qfSens00$qfIrgaChop, 
                             "qfIrgaDetc" = qfSens00$qfIrgaDetc, 
                             "qfIrgaPll" = qfSens00$qfIrgaPll, 
                             "qfIrgaSync" = qfSens00$qfIrgaSync, 
                             "qfIrgaAgc" = qfSens00$qfIrgaAgc)
    
    setQf$asrpH2o <- data.frame("qfRngMinAsrpH2o" = qfSens00$qfRngMinAsrpH2o, 
                                "qfStepAsrpH2o" = qfSens00$qfStepAsrpH2o, 
                                "qfPersAsrpH2o" = qfSens00$qfPersAsrpH2o, 
                                "qfCalAsrpH2o" = qfSens00$qfCalAsrpH2o)
    
    setQf$ssiH2o <- data.frame("qfRngMinSsiH2o" = qfSens00$qfRngMinSsiH2o, 
                               "qfStepSsiH2o" = qfSens00$qfStepSsiH2o,
                               "qfPersSsiH2o" = qfSens00$qfPersSsiH2o, 
                               "qfCalSsiH2o" = qfSens00$qfCalSsiH2o)
    
    setQf$rtioMoleDryH2o <- data.frame("qfRngMinRtioMoleDryH2o" = qfSens00$qfRngMinRtioMoleDryH2o, 
                                       "qfStepRtioMoleDryH2o" = qfSens00$qfStepRtioMoleDryH2o,
                                       "qfPersRtioMoleDryH2o" = qfSens00$qfPersRtioMoleDryH2o, 
                                       "qfCalRtioMoleDryH2o" = qfSens00$qfCalRtioMoleDryH2o)
    
    setQf$densMoleH2o <- data.frame("qfRngMinDensMoleH2o" = qfSens00$qfRngMinDensMoleH2o, 
                                    "qfStepDensMoleH2o" = qfSens00$qfStepDensMoleH2o,
                                    "qfPersDensMoleH2o" = qfSens00$qfPersDensMoleH2o, 
                                    "qfCalDensMoleH2o" = qfSens00$qfCalDensMoleH2o) 
    
    setQf$presAtm <- data.frame("qfRngMinPresAtm" = qfSens00$qfRngMinPresAtm, 
                                "qfStepPresAtm" = qfSens00$qfStepPresAtm,
                                "qfPersPresAtm" = qfSens00$qfPersPresAtm, 
                                "qfCalPresAtm" = qfSens00$qfCalPresAtm)
    
    setQf$presSum <- data.frame("qfRngMinPresSum" = qfSens00$qfRngMinPresSum, 
                                "qfStepPresSum" = qfSens00$qfStepPresSum,
                                "qfPersPresSum" = qfSens00$qfPersPresSum, 
                                "qfCalPresSum" = qfSens00$qfCalPresSum)
    
    setQf$tempAve <- data.frame ("qfRngMinTempMean" = qfSens00$qfRngMinTempMean, 
                                 "qfStepTempMean" = qfSens00$qfStepTempMean,
                                 "qfPersTempMean" = qfSens00$qfPersTempMean, 
                                 "qfCalTempMean" = qfSens00$qfCalTempMean)
    if (!is.null(qfSens01)){
      #irgaMfcSamp
      setQf$frt00 <- data.frame("qfRngMinFrt00" = qfSens01$qfRngMinFrt00, 
                                "qfStepFrt00" = qfSens01$qfStepFrt00, 
                                "qfPersFrt00" = qfSens01$qfPersFrt00, 
                                "qfCalFrt00" = qfSens01$qfCalFrt00)
    } else {
      #assign qf for irgaMfcSamp to -1 when qf irgaMfcSamp are missing
      setQf$frt00 <- data.frame("qfRngMinFrt00" = -1, 
                                "qfStepFrt00" = -1, 
                                "qfPersFrt00" = -1, 
                                "qfCalFrt00" = -1)
    }
    
    #gruping qulity flags that related to L1 sub-data product
    rpt$rtioMoleDryH2o <- data.frame(setQf$sens, setQf$asrpH2o, 
                                     setQf$ssiH2o, setQf$rtioMoleDryH2o)
    rpt$densMoleH2o <- data.frame(setQf$sens, setQf$asrpH2o, 
                                  setQf$ssiH2o,setQf$densMoleH2o)
    #need to update which flags will be used for tempDew
    rpt$tempDew <- data.frame(setQf$sens, setQf$asrpH2o, 
                              setQf$ssiH2o)
    rpt$presAtm <- data.frame(setQf$sens, setQf$presAtm)
    rpt$presSum <- data.frame(setQf$sens, setQf$presSum)
    rpt$frt00Samp <- data.frame (setQf$frt00)
    rpt$tempAve <- data.frame (setQf$sens, setQf$tempAve)
    #remove setQf
    setQf <- NULL
  }
  
  #soni#################################################################################  
  if (dp01 == "soni") {
    #organized all quality flags from soni into the set of flags (for frequency use)
    #soni sensor flags
    setQf$sensSoni <- data.frame("qfSoniUnrs" = qfSens00$qfSoniUnrs, 
                                 "qfSoniData" = qfSens00$qfSoniData,
                                 "qfSoniTrig" = qfSens00$qfSoniTrig, 
                                 "qfSoniComm" = qfSens00$qfSoniComm,
                                 "qfSoniCode" = qfSens00$qfSoniCode, 
                                 "qfSoniTemp" = qfSens00$qfSoniTemp,
                                 "qfSoniSgnlPoor" = qfSens00$qfSoniSgnlPoor, 
                                 "qfSoniSgnlHigh" = qfSens00$qfSoniSgnlHigh,
                                 "qfSoniSgnlLow" = qfSens00$qfSoniSgnlLow) 
    #qf for along-axis wind speed
    setQf$veloXaxs <- data.frame("qfRngMinVeloXaxs" = qfSens00$qfRngMinVeloXaxs,
                                 "qfStepVeloXaxs" = qfSens00$qfStepVeloXaxs,
                                 "qfPersVeloXaxs" = qfSens00$qfPersVeloXaxs,
                                 "qfCalVeloXaxs" = qfSens00$qfCalVeloXaxs)
    
    #qf for cross-axis wind speed
    setQf$veloYaxs <- data.frame("qfRngMinVeloYaxs" = qfSens00$qfRngMinVeloYaxs,
                                 "qfStepVeloYaxs" = qfSens00$qfStepVeloYaxs,
                                 "qfPersVeloYaxs" = qfSens00$qfPersVeloYaxs,
                                 "qfCalVeloYaxs" = qfSens00$qfCalVeloYaxs)
    #qf for vertical-axis wind speed
    setQf$veloZaxs <- data.frame("qfRngMinVeloZaxs" = qfSens00$qfRngMinVeloZaxs,
                                 "qfStepVeloZaxs" = qfSens00$qfStepVeloZaxs,
                                 "qfPersVeloZaxs" = qfSens00$qfPersVeloZaxs,
                                 "qfCalVeloZaxs" = qfSens00$qfCalVeloZaxs)
    #qf for soic temperature
    setQf$tempSoni <- data.frame("qfRngMinTempSoni" = qfSens00$qfRngMinTempSoni,
                                 "qfStepTempSoni" = qfSens00$qfStepTempSoni,
                                 "qfPersTempSoni" = qfSens00$qfPersTempSoni,   
                                 "qfCalTempSoni" = qfSens00$qfCalTempSoni)
    #irga sensor flags
    setQf$sensIrga <- data.frame("qfIrgaHead" = qfSens01$qfIrgaHead, 
                                 "qfIrgaTemp" = qfSens01$qfIrgaTemp, 
                                 "qfIrgaTempIn" = qfSens01$qfIrgaTempIn,
                                 "qfIrgaAux" = qfSens01$qfIrgaAux, 
                                 "qfIrgaPres" = qfSens01$qfIrgaPres, 
                                 "qfIrgaChop" = qfSens01$qfIrgaChop, 
                                 "qfIrgaDetc" = qfSens01$qfIrgaDetc, 
                                 "qfIrgaPll" = qfSens01$qfIrgaPll, 
                                 "qfIrgaSync" = qfSens01$qfIrgaSync, 
                                 "qfIrgaAgc" = qfSens01$qfIrgaAgc)
    
    #gruping qulity flags that related to L1 sub-data product
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
    setQf$sens <- data.frame("qfAmrsVal" = qfSens00$qfAmrsVal,
                             "qfAmrsFilt" =  qfSens00$qfAmrsFilt,
                             "qfAmrsVelo" =  qfSens00$qfAmrsVelo,
                             "qfAmrsRng" =  qfSens00$qfAmrsRng)
    setQf$angXaxs <- data.frame("qfRngMinAngXaxs" = qfSens00$qfRngMinAngXaxs,
                                "qfStepAngXaxs" = qfSens00$qfStepAngXaxs,
                                "qfPersAngXaxs" = qfSens00$qfPersAngXaxs,
                                "qfCalAngXaxs" = qfSens00$qfCalAngXaxs)
    setQf$angYaxs <- data.frame("qfRngMinAngYaxs" = qfSens00$qfRngMinAngYaxs,
                                "qfStepAngYaxs" = qfSens00$qfStepAngYaxs,
                                "qfPersAngYaxs" = qfSens00$qfPersAngYaxs,
                                "qfCalAngYaxs" = qfSens00$qfCalAngYaxs)
    setQf$angZaxs <- data.frame("qfRngMinAngZaxs" = qfSens00$qfRngMinAngZaxs,
                                "qfStepAngZaxs" = qfSens00$qfStepAngZaxs,
                                "qfPersAngZaxs" = qfSens00$qfPersAngZaxs,
                                "qfCalAngZaxs" = qfSens00$qfCalAngZaxs)
    
    #gruping qulity flags that related to L1 sub-data product
    rpt$angNedXaxs <- data.frame(setQf$sens, setQf$angXaxs)
    rpt$angNedYaxs <- data.frame(setQf$sens, setQf$angYaxs)
    rpt$angNedZaxs <- data.frame(setQf$sens, setQf$angZaxs)
    
  }
  return(rpt)
  
  # end function def.neon.ecte.dp01.qf.grup()
}