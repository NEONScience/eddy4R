##############################################################################################
#' @title Definition function: Grouping the quality flags for each of NEON ECTE and ECSE L1 data product

#' @author
#' Natchaya Pingintha-Durden \email{ndurden@battelleecology.org}

#' @description Function definition. Grouping the quality flags of each NEON ECTE and ECSE L1 data product into a single dataframe for further use in the calculation of Alpha, Beta, and Final flag.

#' @param qfInp A list of data frame containing the input quality flag data that related to L1 data products are being grouped. Of class integer". [-] 
#' @param MethMeas A vector of class "character" containing the name of measurement method (eddy-covariance turbulent exchange or storage exchange), MethMeas = c("ecte", "ecse"). Defaults to "ecse". [-] 
#' @param TypeMeas A vector of class "character" containing the name of measurement type (sampling or validation), TypeMeas = c("samp", "ecse"). Defaults to "samp". [-]
#' @param dp01 A vector of class "character" containing the name of NEON ECTE and ECSE L1 data products which the flags are being grouped, \cr
#' c("envHut", "co2Turb", "h2oTurb", "isoCo2", "isoH2o", "soni", "amrs", "tempAirLvl", "tempAirTop"). Defaults to "co2Turb". [-] 
#' @param idGas A data frame contianing gas ID for isoCo2 measurement. Need to provide when dp01 = "isoCo2". Default to NULL. [-]

#' @return A list of data frame of the quality flags related to that sub-data product. \cr

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

#' @keywords NEON QAQC, quality flags, quality metrics

#' @examples 
#' #generate the fake quality flags for each sensor
#' TimeBgn <- "2016-04-24 02:00:00.000"
#' TimeEnd <- "2016-04-24 02:29:59.950"
#' qf <- list()
#' qf$irgaTurb <- eddy4R.qaqc::def.qf.ecte(TimeBgn = TimeBgn, TimeEnd = TimeEnd, Freq = 20, Sens = "irgaTurb", PcntQf = 0.05)
#' #add qfIrgaTurbvali
#' qf$irgaTurb$qfIrgaTurbVali <- rep(0, 86000)
#' qf$mfcSampTurb <- eddy4R.qaqc::def.qf.ecte(TimeBgn = TimeBgn, TimeEnd = TimeEnd, Freq = 20, Sens = "mfcSampTurb", PcntQf = 0.05)
#' qf$soni <- eddy4R.qaqc::def.qf.ecte(TimeBgn = TimeBgn, TimeEnd = TimeEnd, Freq = 20, Sens = "soni", PcntQf = 0.05)
#' qf$amrs <- eddy4R.qaqc::def.qf.ecte(TimeBgn = TimeBgn, TimeEnd = TimeEnd, Freq = 40, Sens = "amrs", PcntQf = 0.05)
#' 
#' #grouping the set of the flags
#' qfGrpCo2Turb <- eddy4R.qaqc::def.dp01.grp.qf(qfInp = qf, MethMeas = "ecte", TypeMeas = "vali", dp01="co2Turb")
#' qfGrpSoni <- eddy4R.qaqc::def.dp01.grp.qf(qfInp = qf, MethMeas = "ecte", TypeMeas = "samp", dp01="soni")

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
#   Natchaya P-Durden (2017-04-26)
#     commented out the qfCal in irgaTurb and soni
#   Natchaya P-Durden (2017-05-12)
#     added qfIrgaTurbVali and qfIrgaTurbAgc
#   David Durden (2017-06-27)
#     removing qfPersFrt00 flag from processing
#   Natchaya P-Durden (2017-08-02)
#     modified ecse section
#   Natchaya P-Durden (2017-08-17)
#     updated ECSE dp01 term names
#   David Durden (2017-12-12)
#     updated ECTE term names
#   Natchaya P-Durden (2018-01-12)
#     bugs fixed on determination qf for presEnvHut
#   Natchaya P-Durden (2018-02-16)
#     replace -1 if all qfSens in crdCo2 and crdH2o are NaN
#   Natchaya P-Durden (2018-04-03)
#     update @param format
#   Ke Xu (2018-04-19)
#     applied term name convention; replaced qfInput by qfInp
#   Natchaya P-Durden (2018-05-23)
#     rename function from def.neon.dp01.qf.grp() to def.dp01.grp.qf()
#   Natchaya P-Durden (2019-03-20)
#     bug fix to not include qf from other gas speices during grouping qf for isoCo2
#   Natchaya P-Durden (2019-04-24)
#     remove qfStep from rtioMoleDryH2o and rtioMoleWetH2o in isoCo2
#   Natchaya P-Durden (2019-04-29)
#     adding the sensor name to ECSE quality flags 
#   Natchaya P-Durden (2019-05-06)
#     adding the mfm quality flags to ECSE
#   Natchaya P-Durden (2019-05-22)
#     adding the presInlt quality flags to ECSE
#   Natchaya P-Durden (2019-05-23)
#     adding the presStor, presValiRegInStor and and presValiRegOutStor quality flags to ECSE
#   Natchaya P-Durden (2019-05-28)
#     adding qfHeat to ECSE
#   Natchaya P-Durden (2020-03-11)
#     removed qfCal and qfHeat from ECSE
#   David Durden (2020-06-21)
#     removing flags for external sensors from ECSE qfqm
##############################################################################################

def.dp01.grp.qf <- function(
  qfInp = list(),
  MethMeas = c("ecte", "ecse")[1],
  TypeMeas = c("samp", "vali")[1], 
  dp01 = c("envHut", "co2Turb", "h2oTurb", "co2Stor", "h2oStor", "isoCo2", "isoH2o", "soni", "amrs", "tempAirLvl", "tempAirTop")[1],
  idGas =NULL
){
  #check existing input list
    if (dp01 %in% c("co2Turb", "h2oTurb")) {
      if (!("irgaTurb" %in% names(qfInp))) { 
        base::stop("Missing irgaTurb quality flags")
      }
    }# close if statement of dp01
  
    if (dp01 %in% c("co2Stor", "h2oStor")) {
      if (!("irgaStor" %in% names(qfInp))) { 
        base::stop("Missing irgaStor quality flags")
      }
    }# close if statement of dp01
  
    if (dp01 %in% c("soni")) {
      if (!("soni" %in% names(qfInp))){
        base::stop("Missing soni quality flags")
      }
    }# close if statement of dp01
  
    if (dp01 %in% c("amrs")) {
      if (!("amrs" %in% names(qfInp))){
        base::stop("Missing amrs quality flags")
      }
    }# close if statement of dp01
  
    if (dp01 %in% c("isoCo2")) {
      if (!("crdCo2" %in% names(qfInp))){
        base::stop("Missing crdCo2 quality flags")
      }
    }# close if statement of dp01
  
    if (dp01 %in% c("isoH2o")) {
      if (!("crdH2o" %in% names(qfInp))){
        base::stop("Missing crdH2o quality flags")
      }
    }# close if statement of dp01
  
    if (dp01 %in% c("envHut")) {
      if (!("envHut" %in% names(qfInp))){
        base::stop("Missing envHut quality flags")
      }
    }# close if statement of dp01
  
    if (dp01 %in% c("tempAirLvl")) {
      if (!("tempAirLvl" %in% names(qfInp))){
        base::stop("Missing tempAirLvl quality flags")
      }
    }# close if statement of dp01
  
    if (dp01 %in% c("tempAirTop")) {
      if (!("tempAirTop" %in% names(qfInp))){
        base::stop("Missing tempAirTop quality flags")
      }
    }# close if statement of dp01

  rpt <- list()
  setQf <- list()
  
# ecte #######################################################################################
if (MethMeas == "ecte") {
#co2Turb and h2oTurb####################################################################################
  if (dp01 %in% c("co2Turb", "h2oTurb")) {
    #organized all quality flags from irgaTurb into the set of flags (for frequency use)
    #irgaTurb sensor flags
    setQf$sensIrgaTurb <- data.frame("qfIrgaTurbHead" = qfInp$irgaTurb$qfIrgaTurbHead, 
                             "qfIrgaTurbTempOut" = qfInp$irgaTurb$qfIrgaTurbTempOut, 
                             "qfIrgaTurbTempIn" = qfInp$irgaTurb$qfIrgaTurbTempIn,
                             "qfIrgaTurbAux" = qfInp$irgaTurb$qfIrgaTurbAux, 
                             "qfIrgaTurbPres" = qfInp$irgaTurb$qfIrgaTurbPres, 
                             "qfIrgaTurbChop" = qfInp$irgaTurb$qfIrgaTurbChop, 
                             "qfIrgaTurbDetc" = qfInp$irgaTurb$qfIrgaTurbDetc, 
                             "qfIrgaTurbPll" = qfInp$irgaTurb$qfIrgaTurbPll, 
                             "qfIrgaTurbSync" = qfInp$irgaTurb$qfIrgaTurbSync,
                             "qfIrgaTurbAgc" = qfInp$irgaTurb$qfIrgaTurbAgc)
    
    setQf$sensIrgaTurbExt <- data.frame("qfIrgaTurbVali" = qfInp$irgaTurb$qfIrgaTurbVali)
    
    setQf$tempIn <- data.frame("qfRngTempIn" = qfInp$irgaTurb$qfRngTempIn,
                               "qfStepTempIn" = qfInp$irgaTurb$qfStepTempIn,
                               "qfPersTempIn" = qfInp$irgaTurb$qfPersTempIn)
                               #"qfCalTempIn" = qfInp$irgaTurb$qfCalTempIn)
    
    setQf$tempOut <- data.frame("qfRngTempOut" = qfInp$irgaTurb$qfRngTempOut,
                                "qfStepTempOut" = qfInp$irgaTurb$qfStepTempOut,
                                "qfPersTempOut" = qfInp$irgaTurb$qfPersTempOut)
                                #"qfCalTempOut" = qfInp$irgaTurb$qfCalTempOut)
    
    setQf$tempAve <- data.frame ("qfRngTempMean" = qfInp$irgaTurb$qfRngTempMean, 
                                 "qfStepTempMean" = qfInp$irgaTurb$qfStepTempMean,
                                 "qfPersTempMean" = qfInp$irgaTurb$qfPersTempMean) 
                                 #"qfCalTempMean" = qfInp$irgaTurb$qfCalTempMean)
    
    setQf$presAtmIrgaTurb <- data.frame("qfRngPresAtm" = qfInp$irgaTurb$qfRngPresAtm, 
                                "qfStepPresAtm" = qfInp$irgaTurb$qfStepPresAtm,
                                "qfPersPresAtm" = qfInp$irgaTurb$qfPersPresAtm) 
                                #"qfCalPresAtm" = qfInp$irgaTurb$qfCalPresAtm)
    
    setQf$presDiffIrgaTurb <- data.frame("qfRngPresDiff" = qfInp$irgaTurb$qfRngPresDiff,
                                     "qfStepPresDiff" = qfInp$irgaTurb$qfStepPresDiff,
                                     "qfPersPresDiff" = qfInp$irgaTurb$qfPersPresDiff)
                                     #"qfCalPresDiff" = qfInp$irgaTurb$qfCalPresDiff) 
    
    setQf$presSum <- data.frame("qfRngPresSum" = qfInp$irgaTurb$qfRngPresSum,
                                "qfStepPresSum" = qfInp$irgaTurb$qfStepPresSum,
                                "qfPersPresSum" = qfInp$irgaTurb$qfPersPresSum)
                                #"qfCalPresSum" = qfInp$irgaTurb$qfCalPresSum)
    
    setQf$powrH2oSamp <- data.frame ("qfRngPowrH2oSamp" = qfInp$irgaTurb$qfRngPowrH2oSamp,
                                     "qfStepPowrH2oSamp" = qfInp$irgaTurb$qfStepPowrH2oSamp,
                                     "qfPersPowrH2oSamp" = qfInp$irgaTurb$qfPersPowrH2oSamp)
                                     #"qfCalPowrH2oSamp" = qfInp$irgaTurb$qfCalPowrH2oSamp)
    
    setQf$powrH2oRefe <- data.frame ("qfRngPowrH2oRefe" = qfInp$irgaTurb$qfRngPowrH2oRefe,
                                     "qfStepPowrH2oRefe" = qfInp$irgaTurb$qfStepPowrH2oRefe,
                                     "qfPersPowrH2oRefe" = qfInp$irgaTurb$qfPersPowrH2oRefe)
                                     #"qfCalPowrH2oRefe" = qfInp$irgaTurb$qfCalPowrH2oRefe)
    
    setQf$asrpH2o <- data.frame("qfRngAsrpH2o" = qfInp$irgaTurb$qfRngAsrpH2o, 
                                "qfStepAsrpH2o" = qfInp$irgaTurb$qfStepAsrpH2o, 
                                "qfPersAsrpH2o" = qfInp$irgaTurb$qfPersAsrpH2o) 
                                #"qfCalAsrpH2o" = qfInp$irgaTurb$qfCalAsrpH2o)
    
    setQf$densMoleH2o <- data.frame("qfRngDensMoleH2o" = qfInp$irgaTurb$qfRngDensMoleH2o, 
                                    "qfStepDensMoleH2o" = qfInp$irgaTurb$qfStepDensMoleH2o, 
                                    "qfPersDensMoleH2o" = qfInp$irgaTurb$qfPersDensMoleH2o)
                                    #"qfCalDensMoleH2o" = qfInp$irgaTurb$qfCalDensMoleH2o)
    
    setQf$rtioMoleDryH2o <- data.frame("qfRngRtioMoleDryH2o" = qfInp$irgaTurb$qfRngRtioMoleDryH2o,
                                       "qfStepRtioMoleDryH2o" = qfInp$irgaTurb$qfStepRtioMoleDryH2o,
                                       "qfPersRtioMoleDryH2o" = qfInp$irgaTurb$qfPersRtioMoleDryH2o)
                                       #"qfCalRtioMoleDryH2o" = qfInp$irgaTurb$qfCalRtioMoleDryH2o)
    
    setQf$powrCo2Samp <- data.frame("qfRngPowrCo2Samp" = qfInp$irgaTurb$qfRngPowrCo2Samp,
                                    "qfStepPowrCo2Samp" = qfInp$irgaTurb$qfStepPowrCo2Samp,
                                    "qfPersPowrCo2Samp" = qfInp$irgaTurb$qfPersPowrCo2Samp)
                                    #"qfCalPowrCo2Samp" = qfInp$irgaTurb$qfCalPowrCo2Samp)
    
    setQf$powrCo2Refe <- data.frame ("qfRngPowrCo2Refe" = qfInp$irgaTurb$qfRngPowrCo2Refe,
                                     "qfStepPowrCo2Refe" = qfInp$irgaTurb$qfStepPowrCo2Refe,
                                     "qfPersPowrCo2Refe" = qfInp$irgaTurb$qfPersPowrCo2Refe)
                                     #"qfCalPowrCo2Refe" = qfInp$irgaTurb$qfCalPowrCo2Refe)
    
    setQf$asrpCo2 <- data.frame("qfRngAsrpCo2" = qfInp$irgaTurb$qfRngAsrpCo2, 
                                "qfStepAsrpCo2" = qfInp$irgaTurb$qfStepAsrpCo2, 
                                "qfPersAsrpCo2" = qfInp$irgaTurb$qfPersAsrpCo2) 
                                #"qfCalAsrpCo2" = qfInp$irgaTurb$qfCalAsrpCo2)
    
    setQf$densMoleCo2 <- data.frame("qfRngDensMoleCo2" = qfInp$irgaTurb$qfRngDensMoleCo2,
                                    "qfStepDensMoleCo2" = qfInp$irgaTurb$qfStepDensMoleCo2,
                                    "qfPersDensMoleCo2" = qfInp$irgaTurb$qfPersDensMoleCo2) 
                                    #"qfCalDensMoleCo2" = qfInp$irgaTurb$qfCalDensMoleCo2) 
    
    setQf$rtioMoleDryCo2 <- data.frame("qfRngRtioMoleDryCo2" = qfInp$irgaTurb$qfRngRtioMoleDryCo2,
                                       "qfStepRtioMoleDryCo2" = qfInp$irgaTurb$qfStepRtioMoleDryCo2,
                                       "qfPersRtioMoleDryCo2" = qfInp$irgaTurb$qfPersRtioMoleDryCo2)
                                      # "qfCalRtioMoleDryCo2" = qfInp$irgaTurb$qfCalRtioMoleDryCo2)
    
    setQf$ssiCo2 <- data.frame("qfRngSsiCo2" = qfInp$irgaTurb$qfRngSsiCo2, 
                               "qfStepSsiCo2" = qfInp$irgaTurb$qfStepSsiCo2, 
                               "qfPersSsiCo2" = qfInp$irgaTurb$qfPersSsiCo2)
                               #"qfCalSsiCo2" = qfInp$irgaTurb$qfCalSsiCo2)
    
    setQf$ssiH2o <- data.frame("qfRngSsiH2o" = qfInp$irgaTurb$qfRngSsiH2o, 
                               "qfStepSsiH2o" = qfInp$irgaTurb$qfStepSsiH2o, 
                               "qfPersSsiH2o" = qfInp$irgaTurb$qfPersSsiH2o) 
                               #"qfCalSsiH2o" = qfInp$irgaTurb$qfCalSsiH2o)
    
    #external quality flags from mfcSampTurb
    if ("mfcSampTurb" %in% names(qfInp)){
      #mfcSampTurb
      setQf$frt00MfcSampTurb <- data.frame("qfRngFrt00" = qfInp$mfcSampTurb$qfRngFrt00) 
                                #,"qfPersFrt00" = qfInp$mfcSampTurb$qfPersFrt00)
      
      setQf$frtMfcSampTurb <- data.frame("qfRngFrt" = qfInp$mfcSampTurb$qfRngFrt,
                              "qfPersFrt" = qfInp$mfcSampTurb$qfPersFrt)
      
      setQf$presAtmMfcSampTurb <- data.frame("qfRngPresAtm" = qfInp$mfcSampTurb$qfRngPresAtm, 
                                     "qfStepPresAtm" = qfInp$mfcSampTurb$qfStepPresAtm,
                                     "qfPersPresAtm" = qfInp$mfcSampTurb$qfPersPresAtm)
      
      setQf$tempMfcSampTurb <- data.frame("qfRngTemp" = qfInp$mfcSampTurb$qfRngTemp,
                                  "qfStepTemp" = qfInp$mfcSampTurb$qfStepTemp,
                                  "qfPersTemp" = qfInp$mfcSampTurb$qfPersTemp)
      } else {
      #assign qf for mfcSampTurb to -1 when qf mfcSampTurb is missing
      setQf$frt00MfcSampTurb <- data.frame("qfRngFrt00" = -1)
                                           #,"qfPersFrt00" = -1)
      
      setQf$frtMfcSampTurb <- data.frame("qfRngFrt" = -1,
                              "qfPersFrt" = -1)
      
      setQf$presAtmMfcSampTurb <- data.frame("qfRngPresAtm" = -1, 
                                     "qfStepPresAtm" = -1,
                                     "qfPersPresAtm" = -1)
      
      setQf$tempMfcSampTurb <- data.frame("qfRngTemp" = -1,
                                  "qfStepTemp" = -1,
                                  "qfPersTemp" = -1)
    }

    #external quality flags from mfcValiTurb
    if ("mfcValiTurb" %in% names(qfInp)){
      #mfcValiTurb
      setQf$frt00MfcValiTurb <- data.frame("qfRngFrt00" = qfInp$mfcValiTurb$qfRngFrt00) 
                                # "qfPersFrt00" = qfInp$mfcValiTurb$qfPersFrt00)
      
      setQf$frtMfcValiTurb <- data.frame("qfRngFrt" = qfInp$mfcValiTurb$qfRngFrt,
                              "qfPersFrt" = qfInp$mfcValiTurb$qfPersFrt)
      
      setQf$presAtmMfcValiTurb <- data.frame("qfRngPresAtm" = qfInp$mfcValiTurb$qfRngPresAtm, 
                                     "qfStepPresAtm" = qfInp$mfcValiTurb$qfStepPresAtm,
                                     "qfPersPresAtm" = qfInp$mfcValiTurb$qfPersPresAtm)
      
      setQf$tempMfcValiTurb <- data.frame("qfRngTemp" = qfInp$mfcValiTurb$qfRngTemp,
                                  "qfStepTemp" = qfInp$mfcValiTurb$qfStepTemp,
                                  "qfPersTemp" = qfInp$mfcValiTurb$qfPersTemp)
      } else {
      #assign qf for mfcValiTurb to -1 when qf mfcValiTurb is missing
      setQf$frt00MfcValiTurb <- data.frame("qfRngFrt00" = -1) 
                                #"qfPersFrt00" = -1)
      
      setQf$frtMfcValiTurb <- data.frame("qfRngFrt" = -1,
                              "qfPersFrt" = -1)
      
      setQf$presAtmMfcValiTurb <- data.frame("qfRngPresAtm" = -1, 
                                     "qfStepPresAtm" = -1,
                                     "qfPersPresAtm" = -1)
      
      setQf$tempMfcValiTurb <- data.frame("qfRngTemp" = -1,
                                  "qfStepTemp" = -1,
                                  "qfPersTemp" = -1)
      }

    #quality flags from soni for grouping qf of tempDew 
    if ("soni" %in% names(qfInp)){
      setQf$soni <- data.frame(#"qfCalVeloSoni" = qfInp$soni$qfCalVeloSoni,
                               "qfPersVeloSoni" = qfInp$soni$qfPersVeloSoni,
                               "qfRngVeloSoni" = qfInp$soni$qfRngVeloSoni,
                               "qfStepVeloSoni" = qfInp$soni$qfStepVeloSoni,
                               #"qfCalTempSoni" = qfInp$soni$qfCalTempSoni,
                               "qfPersTempSoni" = qfInp$soni$qfPersTempSoni,
                               "qfRngTempSoni" = qfInp$soni$qfRngTempSoni,
                               "qfStepTempSoni" = qfInp$soni$qfStepTempSoni,
                               "qfSoniUnrs" = qfInp$soni$qfSoniUnrs, 
                               "qfSoniData" = qfInp$soni$qfSoniData,
                               "qfSoniTrig" = qfInp$soni$qfSoniTrig, 
                               "qfSoniComm" = qfInp$soni$qfSoniComm,
                               "qfSoniCode" = qfInp$soni$qfSoniCode, 
                               "qfSoniTemp" = qfInp$soni$qfSoniTemp,
                               "qfSoniSgnlPoor" = qfInp$soni$qfSoniSgnlPoor, 
                               "qfSoniSgnlHigh" = qfInp$soni$qfSoniSgnlHigh,
                               "qfSoniSgnlLow" = qfInp$soni$qfSoniSgnlLow
                               )
    } else {
      setQf$soni <- data.frame(#"qfCalVeloSoni" = -1,
                               "qfPersVeloSoni" = -1,
                               "qfRngVeloSoni" = -1,
                               "qfStepVeloSoni" = -1,
                               #"qfCalTempSoni" = -1,
                               "qfPersTempSoni" = -1,
                               "qfRngTempSoni" = -1,
                               "qfStepTempSoni" = -1,
                               "qfSoniUnrs" = -1, 
                               "qfSoniData" = -1,
                               "qfSoniTrig" = -1, 
                               "qfSoniComm" = -1,
                               "qfSoniCode" = -1, 
                               "qfSoniTemp" = -1,
                               "qfSoniSgnlPoor" = -1, 
                               "qfSoniSgnlHigh" = -1,
                               "qfSoniSgnlLow" = -1
      )
      
    }#close if statment of soni
    
    #grouping qulity flags that related to co2Turb L1 sub-data product
    if (dp01 == "co2Turb"){
      if (TypeMeas == "samp"){
      rpt$densMoleCo2 <- data.frame(setQf$sensIrgaTurb, setQf$sensIrgaTurbExt,
                                    setQf$tempAve, setQf$presDiffIrgaTurb, 
                                    setQf$powrCo2Samp, setQf$powrCo2Refe, 
                                    setQf$asrpCo2, setQf$densMoleCo2, 
                                    setQf$rtioMoleDryCo2, setQf$ssiCo2, 
                                    "qfRngFrt00" = setQf$frt00MfcSampTurb$qfRngFrt00, setQf$presAtmMfcSampTurb, 
                                    setQf$tempMfcSampTurb)
      
      rpt$rtioMoleDryCo2 <- data.frame(setQf$sensIrgaTurb, setQf$sensIrgaTurbExt,
                                       setQf$tempAve, setQf$presDiffIrgaTurb, 
                                       setQf$powrH2oSamp, setQf$powrH2oRefe, 
                                       setQf$asrpH2o, setQf$rtioMoleDryH2o, 
                                       setQf$powrCo2Samp, setQf$powrCo2Refe, 
                                       setQf$asrpCo2, setQf$rtioMoleDryCo2, 
                                       setQf$ssiCo2, setQf$ssiH2o, 
                                       "qfRngFrt00" = setQf$frt00MfcSampTurb$qfRngFrt00, setQf$presAtmMfcSampTurb, 
                                       setQf$tempMfcSampTurb)
      
      rpt$presAtm <- data.frame(setQf$sensIrgaTurb, setQf$sensIrgaTurbExt, setQf$presAtmIrgaTurb)
      
      rpt$presSum <- data.frame(setQf$sensIrgaTurb, setQf$sensIrgaTurbExt, setQf$presSum)
      
      rpt$tempAve <- data.frame(setQf$sensIrgaTurb, setQf$sensIrgaTurbExt, 
                                setQf$tempIn, setQf$tempOut, 
                                setQf$tempAve)
      }#close if statement of TypeMeas == "samp"
      
      if (TypeMeas == "vali"){
        rpt$densMoleCo2 <- data.frame(setQf$sensIrgaTurb, setQf$tempAve,
                                      setQf$presDiffIrgaTurb, setQf$powrCo2Samp, 
                                      setQf$powrCo2Refe, setQf$asrpCo2 , 
                                      setQf$densMoleCo2, setQf$rtioMoleDryCo2, 
                                      setQf$ssiCo2, "qfRngFrt00" = setQf$frt00MfcSampTurb$qfRngFrt00,
                                      setQf$presAtmMfcSampTurb, setQf$tempMfcSampTurb,
                                      "qfRngFrt00" = setQf$frt00MfcValiTurb$qfRngFrt00, setQf$presAtmMfcValiTurb, 
                                      setQf$tempMfcValiTurb)
        
        rpt$rtioMoleDryCo2 <- data.frame(setQf$sensIrgaTurb, setQf$tempAve,
                                         setQf$presDiffIrgaTurb, setQf$powrH2oSamp, 
                                         setQf$powrH2oRefe, setQf$asrpH2o, 
                                         setQf$rtioMoleDryH2o, setQf$powrCo2Samp, 
                                         setQf$powrCo2Refe, setQf$asrpCo2,
                                         setQf$rtioMoleDryCo2, setQf$ssiCo2, 
                                         setQf$ssiH2o, "qfRngFrt00" = setQf$frt00MfcSampTurb$qfRngFrt00,
                                         setQf$presAtmMfcSampTurb, setQf$tempMfcSampTurb,
                                         "qfRngFrt00" = setQf$frt00MfcValiTurb$qfRngFrt00, setQf$presAtmMfcValiTurb, 
                                         setQf$tempMfcValiTurb)
        
        rpt$presAtm <- data.frame(setQf$sensIrgaTurb, setQf$presAtmIrgaTurb)
        
        rpt$presSum <- data.frame(setQf$sensIrgaTurb, setQf$presSum)
        
        rpt$tempAve <- data.frame(setQf$sensIrgaTurb, setQf$tempIn, 
                                  setQf$tempOut, setQf$tempAve)
      }#close if statement of TypeMeas == "vali"
      
      rpt$frt00Samp <- data.frame(setQf$frt00MfcSampTurb, setQf$frtMfcSampTurb, 
                                    setQf$presAtmMfcSampTurb, setQf$tempMfcSampTurb)
      
    }
    #grouping qulity flags that related to h2oTurb L1 sub-data product    
    if (dp01 == "h2oTurb") {
      if (TypeMeas == "samp"){
      rpt$densMoleH2o <- data.frame(setQf$sensIrgaTurb, setQf$sensIrgaTurbExt,
                                    setQf$tempAve, setQf$presDiffIrgaTurb, 
                                    setQf$powrH2oSamp, setQf$powrH2oRefe, 
                                    setQf$asrpH2o, setQf$densMoleH2o, 
                                    setQf$ssiH2o, "qfRngFrt00" = setQf$frt00MfcSampTurb$qfRngFrt00, 
                                    setQf$presAtmMfcSampTurb, setQf$tempMfcSampTurb)
      
      rpt$rtioMoleDryH2o <- data.frame(setQf$sensIrgaTurb, setQf$sensIrgaTurbExt,
                                       setQf$tempAve, setQf$presDiffIrgaTurb, 
                                       setQf$powrH2oSamp, setQf$powrH2oRefe, 
                                       setQf$asrpH2o, setQf$rtioMoleDryH2o, 
                                       setQf$ssiH2o, "qfRngFrt00" = setQf$frt00MfcSampTurb$qfRngFrt00, 
                                       setQf$presAtmMfcSampTurb, setQf$tempMfcSampTurb)
      
      rpt$presAtm <- data.frame(setQf$sensIrgaTurb, setQf$sensIrgaTurbExt, setQf$presAtmIrgaTurb)
      
      rpt$presSum <- data.frame(setQf$sensIrgaTurb, setQf$sensIrgaTurbExt, setQf$presSum)
      
      rpt$tempAve <- data.frame(setQf$sensIrgaTurb, setQf$sensIrgaTurbExt, 
                                setQf$tempIn, setQf$tempOut, 
                                setQf$tempAve)
      
      rpt$tempDew <- data.frame(setQf$sensIrgaTurb, setQf$sensIrgaTurbExt,
                                setQf$tempAve, setQf$presDiffIrgaTurb, 
                                setQf$presSum, setQf$powrH2oSamp,
                                setQf$powrH2oRefe, setQf$asrpH2o, 
                                setQf$rtioMoleDryH2o, setQf$ssiH2o)#,setQf$soni) Remove until we can deal with differance length
      }#close if statement of TypeMeas == "samp"
      
      if (TypeMeas == "vali"){
        rpt$densMoleH2o <- data.frame(setQf$sensIrgaTurb, setQf$tempAve,
                                      setQf$presDiffIrgaTurb, setQf$powrH2oSamp, 
                                      setQf$powrH2oRefe, setQf$asrpH2o, 
                                      setQf$densMoleH2o, setQf$ssiH2o,
                                      "qfRngFrt00" = setQf$frt00MfcSampTurb$qfRngFrt00, setQf$presAtmMfcSampTurb, 
                                      setQf$tempMfcSampTurb, "qfRngFrt00" = setQf$frt00MfcValiTurb$qfRngFrt00, 
                                      setQf$presAtmMfcValiTurb, setQf$tempMfcValiTurb)
        
        rpt$rtioMoleDryH2o <- data.frame(setQf$sensIrgaTurb, setQf$tempAve,
                                         setQf$presDiffIrgaTurb, setQf$powrH2oSamp, 
                                         setQf$powrH2oRefe, setQf$asrpH2o, 
                                         setQf$rtioMoleDryH2o, setQf$ssiH2o,
                                         "qfRngFrt00" = setQf$frt00MfcSampTurb$qfRngFrt00, setQf$presAtmMfcSampTurb, 
                                         setQf$tempMfcSampTurb, "qfRngFrt00" = setQf$frt00MfcValiTurb$qfRngFrt00, 
                                         setQf$presAtmMfcValiTurb, setQf$tempMfcValiTurb)
        
        rpt$presAtm <- data.frame(setQf$sensIrgaTurb, setQf$presAtmIrgaTurb)
        
        rpt$presSum <- data.frame(setQf$sensIrgaTurb, setQf$presSum)
        
        rpt$tempAve <- data.frame(setQf$sensIrgaTurb, setQf$tempIn, 
                                  setQf$tempOut, setQf$tempAve)
        
        rpt$tempDew <- data.frame(setQf$sensIrgaTurb, setQf$tempAve,
                                  setQf$presDiffIrgaTurb, setQf$presSum,
                                  setQf$powrH2oSamp,setQf$powrH2oRefe, 
                                  setQf$asrpH2o, setQf$rtioMoleDryH2o, 
                                  setQf$ssiH2o) #, setQf$soni) Remove until we can deal with the difference length
      }#close if statement of TypeMeas == "vali"
      
      rpt$frt00Samp <- data.frame(setQf$frt00MfcSampTurb, setQf$frtMfcSampTurb, 
                              setQf$presAtmMfcSampTurb, setQf$tempMfcSampTurb)
      
    }
    #remove setQf
    setQf <- NULL
  } #closed loop for dp01 co2Turb and h2oTurb
  
#soni#################################################################################  
  if (dp01 == "soni") {
    if (TypeMeas %in% c("samp", "vali")) {
    #organized all quality flags from soni into the set of flags (for frequency use)
    #soni sensor flags
    setQf$sensSoni <- data.frame("qfSoniUnrs" = qfInp$soni$qfSoniUnrs, 
                                 "qfSoniData" = qfInp$soni$qfSoniData, 
                                 "qfSoniTrig" = qfInp$soni$qfSoniTrig, 
                                 "qfSoniComm" = qfInp$soni$qfSoniComm, 
                                 "qfSoniCode" = qfInp$soni$qfSoniCode, 
                                 "qfSoniTemp" = qfInp$soni$qfSoniTemp, 
                                 "qfSoniSgnlPoor" = qfInp$soni$qfSoniSgnlPoor, 
                                 "qfSoniSgnlHigh" = qfInp$soni$qfSoniSgnlHigh, 
                                 "qfSoniSgnlLow" = qfInp$soni$qfSoniSgnlLow) 
    #qf for along-axis wind speed
    setQf$veloXaxs <- data.frame("qfRngVeloXaxs" = qfInp$soni$qfRngVeloXaxs, 
                                 "qfStepVeloXaxs" = qfInp$soni$qfStepVeloXaxs, 
                                 "qfPersVeloXaxs" = qfInp$soni$qfPersVeloXaxs) 
                                 #"qfCalVeloXaxs" = qfInp$soni$qfCalVeloXaxs)
    #qf for cross-axis wind speed
    setQf$veloYaxs <- data.frame("qfRngVeloYaxs" = qfInp$soni$qfRngVeloYaxs, 
                                 "qfStepVeloYaxs" = qfInp$soni$qfStepVeloYaxs, 
                                 "qfPersVeloYaxs" = qfInp$soni$qfPersVeloYaxs) 
                                 #"qfCalVeloYaxs" = qfInp$soni$qfCalVeloYaxs)
    #qf for vertical-axis wind speed
    setQf$veloZaxs <- data.frame("qfRngVeloZaxs" = qfInp$soni$qfRngVeloZaxs, 
                                 "qfStepVeloZaxs" = qfInp$soni$qfStepVeloZaxs, 
                                 "qfPersVeloZaxs" = qfInp$soni$qfPersVeloZaxs) 
                                 #"qfCalVeloZaxs" = qfInp$soni$qfCalVeloZaxs)
    #qf for sonic velocity
    setQf$veloSoni <- data.frame("qfRngVeloSoni" = qfInp$soni$qfRngVeloSoni, 
                                 "qfStepVeloSoni" = qfInp$soni$qfStepVeloSoni, 
                                 "qfPersVeloSoni" = qfInp$soni$qfPersVeloSoni) 
                                 #"qfCalVeloSoni" = qfInp$soni$qfCalVeloSoni)
    #qf for soic temperature
    setQf$tempSoni <- data.frame("qfRngTempSoni" = qfInp$soni$qfRngTempSoni, 
                                 "qfStepTempSoni" = qfInp$soni$qfStepTempSoni, 
                                 "qfPersTempSoni" = qfInp$soni$qfPersTempSoni) 
                                 #"qfCalTempSoni" = qfInp$soni$qfCalTempSoni)
    
    #external quality flags from irgaTurb for grouping qf of tempAir
    if ("irgaTurb" %in% names(qfInp)){
      setQf$irgaTurb <- data.frame("qfIrgaTurbHead" = qfInp$irgaTurb$qfIrgaTurbHead,
                               "qfIrgaTurbTempOut" = qfInp$irgaTurb$qfIrgaTurbTempOut, 
                               "qfIrgaTurbTempIn" = qfInp$irgaTurb$qfIrgaTurbTempIn,
                               "qfIrgaTurbAux" = qfInp$irgaTurb$qfIrgaTurbAux, 
                               "qfIrgaTurbPres" = qfInp$irgaTurb$qfIrgaTurbPres,
                               "qfIrgaTurbChop" = qfInp$irgaTurb$qfIrgaTurbChop, 
                               "qfIrgaTurbDetc" = qfInp$irgaTurb$qfIrgaTurbDetc,
                               "qfIrgaTurbPll" = qfInp$irgaTurb$qfIrgaTurbPll, 
                               "qfIrgaTurbSync" = qfInp$irgaTurb$qfIrgaTurbSync,
                               "qfIrgaTurbAgc" = qfInp$irgaTurb$qfIrgaTurbAgc,
                               "qfIrgaTurbVali" = qfInp$irgaTurb$qfIrgaTurbVali,
                               "qfRngTempMean" = qfInp$irgaTurb$qfRngTempMean, 
                               "qfStepTempMean" = qfInp$irgaTurb$qfStepTempMean,
                               "qfPersTempMean" = qfInp$irgaTurb$qfPersTempMean, 
                               #"qfCalTempMean" = qfInp$irgaTurb$qfCalTempMean,
                               "qfRngPresDiff" = qfInp$irgaTurb$qfRngPresDiff,
                               "qfStepPresDiff" = qfInp$irgaTurb$qfStepPresDiff,
                               "qfPersPresDiff" = qfInp$irgaTurb$qfPersPresDiff,
                               #"qfCalPresDiff" = qfInp$irgaTurb$qfCalPresDiff,
                               "qfRngPowrH2oSamp" = qfInp$irgaTurb$qfRngPowrH2oSamp,
                               "qfStepPowrH2oSamp" = qfInp$irgaTurb$qfStepPowrH2oSamp,
                               "qfPersPowrH2oSamp" = qfInp$irgaTurb$qfPersPowrH2oSamp,
                               #"qfCalPowrH2oSamp" = qfInp$irgaTurb$qfCalPowrH2oSamp,
                               "qfRngPowrH2oRefe" = qfInp$irgaTurb$qfRngPowrH2oRefe,
                               "qfStepPowrH2oRefe" = qfInp$irgaTurb$qfStepPowrH2oRefe,
                               "qfPersPowrH2oRefe" = qfInp$irgaTurb$qfPersPowrH2oRefe,
                               #"qfCalPowrH2oRefe" = qfInp$irgaTurb$qfCalPowrH2oRefe,
                               "qfRngAsrpH2o" = qfInp$irgaTurb$qfRngAsrpH2o, 
                               "qfStepAsrpH2o" = qfInp$irgaTurb$qfStepAsrpH2o, 
                               "qfPersAsrpH2o" = qfInp$irgaTurb$qfPersAsrpH2o, 
                               #"qfCalAsrpH2o" = qfInp$irgaTurb$qfCalAsrpH2o,
                               "qfRngDensMoleH2o" = qfInp$irgaTurb$qfRngDensMoleH2o, 
                               "qfStepDensMoleH2o" = qfInp$irgaTurb$qfStepDensMoleH2o, 
                               "qfPersDensMoleH2o" = qfInp$irgaTurb$qfPersDensMoleH2o, 
                               #"qfCalDensMoleH2o" = qfInp$irgaTurb$qfCalDensMoleH2o,
                               "qfRngSsiH2o" = qfInp$irgaTurb$qfRngSsiH2o, 
                               "qfStepSsiH2o" = qfInp$irgaTurb$qfStepSsiH2o, 
                               "qfPersSsiH2o" = qfInp$irgaTurb$qfPersSsiH2o) 
                               #"qfCalSsiH2o" = qfInp$irgaTurb$qfCalSsiH2o)
      } else {
      setQf$irgaTurb <- data.frame("qfIrgaTurbHead" = -1,
                               "qfIrgaTurbTempOut" = -1, 
                               "qfIrgaTurbTempIn" = -1,
                               "qfIrgaTurbAux" = -1, 
                               "qfIrgaTurbPres" = -1,
                               "qfIrgaTurbChop" = -1, 
                               "qfIrgaTurbDetc" = -1,
                               "qfIrgaTurbPll" = -1, 
                               "qfIrgaTurbSync" = -1,
                               "qfIrgaTurbAgc" = -1,
                               "qfIrgaTurbVali" = -1,
                               "qfRngTempMean" = -1, 
                               "qfStepTempMean" = -1,
                               "qfPersTempMean" = -1, 
                               #"qfCalTempMean" = -1,
                               "qfRngPresDiff" = -1,
                               "qfStepPresDiff" = -1,
                               "qfPersPresDiff" = -1,
                               #"qfCalPresDiff" = -1,
                               "qfRngPowrH2oSamp" = -1,
                               "qfStepPowrH2oSamp" = -1,
                               "qfPersPowrH2oSamp" = -1,
                               #"qfCalPowrH2oSamp" = -1,
                               "qfRngPowrH2oRefe" = -1,
                               "qfStepPowrH2oRefe" = -1,
                               "qfPersPowrH2oRefe" = -1,
                               #"qfCalPowrH2oRefe" = -1,
                               "qfRngAsrpH2o" = -1, 
                               "qfStepAsrpH2o" = -1, 
                               "qfPersAsrpH2o" = -1, 
                               #"qfCalAsrpH2o" = -1,
                               "qfRngDensMoleH2o" = -1, 
                               "qfStepDensMoleH2o" = -1, 
                               "qfPersDensMoleH2o" = -1, 
                               #"qfCalDensMoleH2o" = -1,
                               "qfRngSsiH2o" = -1, 
                               "qfStepSsiH2o" = -1, 
                               "qfPersSsiH2o" = -1) 
                               #"qfCalSsiH2o" = -1)   
    }#close if else statement for irgaTurb
    ##TO DO##Considering later when the AMRS is collaborating to correct the SONI data
    # #external quality flags from irgaTurb for grouping qf of tempAir
    # if ("amrs" %in% names(qfInp)){
    #   #subset only odd row to match with soni frequency at 20Hz
    #   qfColNames <- colnames(qfInp$amrs)
    #   qfInp$amrs <- data.frame(qfInp$amrs[seq(1,nrow(qfInp$amrs),2),])
    #   colnames(qfInp$amrs) <- qfColNames
    #   
    #   setQf$amrs <- data.frame("qfAmrsVal" = qfInp$amrs$qfAmrsVal,
    #                                "qfAmrsFilt" = qfInp$amrs$qfAmrsFilt,
    #                                "qfAmrsVelo" = qfInp$amrs$qfAmrsVelo,
    #                                "qfAmrsRng" = qfInp$amrs$qfAmrsRng,
    #                                "qfRngAngXaxs" = qfInp$amrs$qfRngAngXaxs,
    #                                "qfStepAngXaxs" = qfInp$amrs$qfStepAngXaxs,
    #                                "qfPersAngXaxs" = qfInp$amrs$qfPersAngXaxs,
    #                                "qfRngAngYaxs" = qfInp$amrs$qfRngAngYaxs,
    #                                "qfStepAngYaxs" = qfInp$amrs$qfStepAngYaxs,
    #                                "qfPersAngYaxs" = qfInp$amrs$qfPersAngYaxs,
    #                                "qfRngAngZaxs" = qfInp$amrs$qfRngAngZaxs,
    #                                "qfStepAngZaxs" = qfInp$amrs$qfStepAngZaxs,
    #                                "qfPersAngZaxs" = qfInp$amrs$qfPersAngZaxs)
    #   
    #   setQf$accXaxsDiff <- data.frame("qfRngAccXaxsDiff" = qfInp$amrs$qfRngAccXaxsDiff,
    #                                   "qfStepAccXaxsDiff" = qfInp$amrs$qfStepAccXaxsDiff,
    #                                   "qfPersAccXaxsDiff" = qfInp$amrs$qfPersAccXaxsDiff)
    #   
    #   setQf$accYaxsDiff <- data.frame("qfRngAccYaxsDiff" = qfInp$amrs$qfRngAccYaxsDiff,
    #                                   "qfStepAccYaxsDiff" = qfInp$amrs$qfStepAccYaxsDiff,
    #                                   "qfPersAccYaxsDiff" = qfInp$amrs$qfPersAccYaxsDiff)
    #   
    #   setQf$accZaxsDiff <- data.frame("qfRngAccZaxsDiff" = qfInp$amrs$qfRngAccZaxsDiff,
    #                                   "qfStepAccZaxsDiff" = qfInp$amrs$qfStepAccZaxsDiff,
    #                                   "qfPersAccZaxsDiff" = qfInp$amrs$qfPersAccZaxsDiff)
    # } else {
    #   setQf$amrs <- data.frame("qfAmrsVal" = -1,
    #                                "qfAmrsFilt" = -1,
    #                                "qfAmrsVelo" = -1,
    #                                "qfAmrsRng" = -1,
    #                                "qfRngAngXaxs" = -1,
    #                                "qfStepAngXaxs" = -1,
    #                                "qfPersAngXaxs" = -1,
    #                                "qfRngAngYaxs" = -1,
    #                                "qfStepAngYaxs" = -1,
    #                                "qfPersAngYaxs" = -1,
    #                                "qfRngAngZaxs" =-1,
    #                                "qfStepAngZaxs" = -1,
    #                                "qfPersAngZaxs" = -1)
    #   
    #   setQf$accXaxsDiff <- data.frame("qfRngAccXaxsDiff" = -1,
    #                                   "qfStepAccXaxsDiff" = -1,
    #                                   "qfPersAccXaxsDiff" = -1)
    #   
    #   setQf$accYaxsDiff <- data.frame("qfRngAccYaxsDiff" = -1,
    #                                   "qfStepAccYaxsDiff" = -1,
    #                                   "qfPersAccYaxsDiff" = -1)
    #   
    #   setQf$accZaxsDiff <- data.frame("qfRngAccZaxsDiff" = -1,
    #                                   "qfStepAccZaxsDiff" = -1,
    #                                   "qfPersAccZaxsDiff" = -1) 
    # }#close if else statement for qf amrs
    
    #grouping qulity flags that related to L1 sub-data product
    rpt$angZaxsErth <- data.frame(setQf$sensSoni, setQf$veloXaxs,
                                  setQf$veloYaxs, setQf$veloZaxs,
                                  setQf$veloSoni)
    ##TO DO##Considering later when the AMRS is collaborating to correct the SONI data 
    # rpt$angZaxsErth <- data.frame(setQf$sensSoni, setQf$veloXaxs,
    #                               setQf$veloYaxs, setQf$veloZaxs,
    #                               setQf$veloSoni, setQf$amrs)
    
    rpt$tempAir <- data.frame(setQf$sensSoni, setQf$veloSoni, 
                              setQf$tempSoni) #setQf$irgaTurb) # Removing until we can handle flags of different lengths
    
    rpt$tempSoni <- data.frame(setQf$sensSoni, setQf$veloSoni, 
                               setQf$tempSoni)
    
    rpt$veloXaxsErth <- data.frame(setQf$sensSoni, setQf$veloXaxs,
                                setQf$veloSoni)
    
    rpt$veloXaxsYaxsErth <- data.frame(setQf$sensSoni, setQf$veloXaxs,
                                       setQf$veloYaxs, setQf$veloSoni)
    
    rpt$veloYaxsErth <- data.frame(setQf$sensSoni, setQf$veloYaxs,
                                   setQf$veloSoni)
    
    rpt$veloZaxsErth <- data.frame(setQf$sensSoni, setQf$veloZaxs,
                                   setQf$veloSoni)
    ##TO DO##Considering later when the AMRS is collaborating to correct the SONI data
    # rpt$veloXaxsErth <- data.frame(setQf$sensSoni, setQf$veloXaxs,
    #                                setQf$veloSoni, setQf$amrs,
    #                                setQf$accXaxsDiff)
    # 
    # rpt$veloXaxsYaxsErth <- data.frame(setQf$sensSoni, setQf$veloXaxs,
    #                                    setQf$veloYaxs, setQf$veloSoni, 
    #                                    setQf$amrs, setQf$accXaxsDiff,
    #                                    setQf$accYaxsDiff)
    # 
    # rpt$veloYaxsErth <- data.frame(setQf$sensSoni, setQf$veloYaxs,
    #                                setQf$veloSoni, setQf$amrs,
    #                                setQf$accYaxsDiff)
    # 
    # rpt$veloZaxsErth <- data.frame(setQf$sensSoni, setQf$veloZaxs,
    #                                setQf$veloSoni, setQf$amrs,
    #                                setQf$accZaxsDiff)
    #remove setQf
    setQf <- NULL 
    }# close if statement of TypeMeas %in% c("samp", "vali")
  }#close if statement of dp01 == "soni"                       
  
#amrs#################################################################################    
  if (dp01 == "amrs") {
    if (TypeMeas %in% c("samp", "vali")) {
    #organized all quality flags from soni into the set of flags (for frequency use)
    #soni sensor flags
      setQf$sensAmrs <- data.frame("qfAmrsVal" = qfInp$amrs$qfAmrsVal,
                                       "qfAmrsFilt" = qfInp$amrs$qfAmrsFilt,
                                       "qfAmrsVelo" = qfInp$amrs$qfAmrsVelo,
                                       "qfAmrsRng" = qfInp$amrs$qfAmrsRng)
      
      setQf$accXaxs <- data.frame("qfRngAccXaxs" = qfInp$amrs$qfRngAccXaxs,
                                  "qfStepAccXaxs" = qfInp$amrs$qfStepAccXaxs,
                                  "qfPersAccXaxs" = qfInp$amrs$qfPersAccXaxs)
      
      setQf$accYaxs <- data.frame("qfRngAccYaxs" = qfInp$amrs$qfRngAccYaxs,
                                  "qfStepAccYaxs" = qfInp$amrs$qfStepAccYaxs,
                                  "qfPersAccYaxs" = qfInp$amrs$qfPersAccYaxs)
      
      setQf$accZaxs <- data.frame("qfRngAccZaxs" = qfInp$amrs$qfRngAccZaxs,
                                  "qfStepAccZaxs" = qfInp$amrs$qfStepAccZaxs,
                                  "qfPersAccZaxs" = qfInp$amrs$qfPersAccZaxs)
      
      setQf$accXaxsDiff <- data.frame("qfRngAccXaxsDiff" = qfInp$amrs$qfRngAccXaxsDiff,
                                      "qfStepAccXaxsDiff" = qfInp$amrs$qfStepAccXaxsDiff,
                                      "qfPersAccXaxsDiff" = qfInp$amrs$qfPersAccXaxsDiff)
      
      setQf$accYaxsDiff <- data.frame("qfRngAccYaxsDiff" = qfInp$amrs$qfRngAccYaxsDiff,
                                      "qfStepAccYaxsDiff" = qfInp$amrs$qfStepAccYaxsDiff,
                                      "qfPersAccYaxsDiff" = qfInp$amrs$qfPersAccYaxsDiff)
      
      setQf$accZaxsDiff <- data.frame("qfRngAccZaxsDiff" = qfInp$amrs$qfRngAccZaxsDiff,
                                      "qfStepAccZaxsDiff" = qfInp$amrs$qfStepAccZaxsDiff,
                                      "qfPersAccZaxsDiff" = qfInp$amrs$qfPersAccZaxsDiff)
      
      setQf$avelXaxs <- data.frame("qfRngAvelXaxs" = qfInp$amrs$qfRngAvelXaxs,
                                   "qfStepAvelXaxs" = qfInp$amrs$qfStepAvelXaxs,
                                   "qfPersAvelXaxs" = qfInp$amrs$qfPersAvelXaxs)
      
      setQf$avelYaxs <- data.frame("qfRngAvelYaxs" = qfInp$amrs$qfRngAvelYaxs,
                                   "qfStepAvelYaxs" = qfInp$amrs$qfStepAvelYaxs,
                                   "qfPersAvelYaxs" = qfInp$amrs$qfPersAvelYaxs)
      
      setQf$avelZaxs <- data.frame("qfRngAvelZaxs" = qfInp$amrs$qfRngAvelZaxs,
                                   "qfStepAvelZaxs" = qfInp$amrs$qfStepAvelZaxs,
                                   "qfPersAvelZaxs" = qfInp$amrs$qfPersAvelZaxs)
      
      setQf$angXaxs <- data.frame("qfRngAngXaxs" = qfInp$amrs$qfRngAngXaxs,
                                  "qfStepAngXaxs" = qfInp$amrs$qfStepAngXaxs,
                                  "qfPersAngXaxs" = qfInp$amrs$qfPersAngXaxs)
      
      setQf$angYaxs <- data.frame("qfRngAngYaxs" = qfInp$amrs$qfRngAngYaxs,
                                  "qfStepAngYaxs" = qfInp$amrs$qfStepAngYaxs,
                                  "qfPersAngYaxs" = qfInp$amrs$qfPersAngYaxs)
      
      setQf$angZaxs <- data.frame("qfRngAngZaxs" = qfInp$amrs$qfRngAngZaxs,
                                  "qfStepAngZaxs" = qfInp$amrs$qfStepAngZaxs,
                                  "qfPersAngZaxs" = qfInp$amrs$qfPersAngZaxs)
    
    #grouping qulity flags that related to L1 sub-data product
      rpt$angNedXaxs <- data.frame(setQf$sensAmrs, setQf$angXaxs)
      rpt$angNedYaxs <- data.frame(setQf$sensAmrs, setQf$angYaxs)
      rpt$angNedZaxs <- data.frame(setQf$sensAmrs, setQf$angZaxs)
    }#close if statement of TypeMeas %in% c("samp", "vali")
  } #close if statement of dp01 == "amrs"
} #close if statement of  MethMeas == "ecse"

# ecse #######################################################################################
if (MethMeas == "ecse") {  
#co2Turb and h2oStor####################################################################################
  if (dp01 %in% c("co2Stor", "h2oStor")) { 
    #check if data are exist
    #external quality flags from envHut
    if (!("envHut" %in% names(qfInp)) || length(which(!is.na(qfInp$irgaStor$qfRngTemp))) == 0){
      qfInp$envHut <- as.data.frame(matrix(-1, ncol = 14, nrow = length(qfInp$irgaStor$qfRngAsrpCo2)))
      names(qfInp$envHut) <- c("qfRngPres", "qfStepPres", "qfPersPres", "qfRngRh", "qfStepRh", "qfPersRh",
                                 "qfRngRtioMoleWetH2o", "qfStepRtioMoleWetH2o", "qfPersRtioMoleWetH2o", 
                                 "qfRngTemp", "qfStepTemp", "qfPersTemp", "qfTemp", "qfRh")}
    #external quality flags from valvAux
    if (!("valvAux" %in% names(qfInp)) || length(which(!is.na(qfInp$irgaStor$qfRngTemp))) == 0){
      qfInp$valvAux <- as.data.frame(matrix(-1, ncol = 1, nrow = length(qfInp$irgaStor$qfRngAsrpCo2)))
      names(qfInp$valvAux) <- "qfValvIrga"}
    
    #heater flag
    if (!("qfHeat" %in% names(qfInp$irgaStor))){
      qfInp$irgaStor$qfHeat <- -1}
    
    #external quality flags from mfcSampStor
    if (!("mfcSampStor" %in% names(qfInp)) || length(which(!is.na(qfInp$irgaStor$qfRngTemp))) == 0){
      qfInp$mfcSampStor <- as.data.frame(matrix(-1, ncol = 12, nrow = length(qfInp$irgaStor$qfRngAsrpCo2)))
      names(qfInp$mfcSampStor) <- c("qfRngFrt00", "qfStepFrt00", "qfRngFrt", "qfStepFrt", "qfPersFrt",
                                      "qfRngPresAtm", "qfStepPresAtm", "qfPersPresAtm", "qfRngTemp", "qfStepTemp", "qfPersTemp",
                                      "qfFrt00")}
    #external quality flags from mfcValiStor
    if (!("mfcValiStor" %in% names(qfInp)) || length(which(!is.na(qfInp$irgaStor$qfRngTemp))) == 0){
      qfInp$mfcValiStor <- as.data.frame(matrix(-1, ncol = 12, nrow = length(qfInp$irgaStor$qfRngAsrpCo2)))
      names(qfInp$mfcValiStor) <- c("qfRngFrt00", "qfStepFrt00", "qfRngFrt", "qfStepFrt", "qfPersFrt",
                                  "qfRngPresAtm", "qfStepPresAtm", "qfPersPresAtm", "qfRngTemp", "qfStepTemp", "qfPersTemp",
                                  "qfFrt00")}
    #external quality flags from mfm
    if (!("mfm" %in% names(qfInp)) || length(which(!is.na(qfInp$irgaStor$qfRngTemp))) == 0){
      qfInp$mfm <- as.data.frame(matrix(-1, ncol = 13, nrow = length(qfInp$irgaStor$qfRngAsrpCo2)))
      names(qfInp$mfm) <- c("qfRngFrt00", "qfStepFrt00", "qfPersFrt00", "qfRngFrt", "qfStepFrt", "qfPersFrt",
                                    "qfRngPresAtm", "qfStepPresAtm", "qfPersPresAtm", "qfRngTemp", "qfStepTemp", "qfPersTemp",
                                    "qfFrt00")}
    
    #external quality flags from presInlt
    if (!("presInlt" %in% names(qfInp)) || length(which(!is.na(qfInp$irgaStor$qfRngTemp))) == 0){
      qfInp$presInlt <- as.data.frame(matrix(-1, ncol = 4, nrow = length(qfInp$irgaStor$qfRngAsrpCo2)))
      names(qfInp$presInlt) <- c("qfPersPresDiff", "qfPresDiff", "qfRngPresDiff", "qfStepPresDiff")}
    
    #external quality flags from pumpStor
    if (!("pumpStor" %in% names(qfInp)) || length(which(!is.na(qfInp$irgaStor$qfRngTemp))) == 0){
      qfInp$pumpStor <- as.data.frame(matrix(-1, ncol = 3, nrow = length(qfInp$irgaStor$qfRngAsrpCo2)))
      names(qfInp$pumpStor) <- c("qfPersPumpVolt", "qfRngPumpVolt", "qfStepPumpVolt")}
    
    #external quality flags from pumpStor
    if (!("pumpIrgaStor" %in% names(qfInp)) || length(which(!is.na(qfInp$irgaStor$qfRngTemp))) == 0){
      qfInp$pumpIrgaStor <- as.data.frame(matrix(-1, ncol = 3, nrow = length(qfInp$irgaStor$qfRngAsrpCo2)))
      names(qfInp$pumpIrgaStor) <- c("qfPersPumpVolt", "qfRngPumpVolt", "qfStepPumpVolt")}
    
    #external quality flags from presValiRegInStor
    if (!("presValiRegInStor" %in% names(qfInp)) || length(which(!is.na(qfInp$irgaStor$qfRngTemp))) == 0){
      qfInp$presValiRegInStor <- as.data.frame(matrix(-1, ncol = 4, nrow = length(qfInp$irgaStor$qfRngAsrpCo2)))
      names(qfInp$presValiRegInStor) <- c("qfPersPresDiff", "qfPresDiff", "qfRngPresDiff", "qfStepPresDiff")}
    
    #replace -1 if all qf in irga are NA
    if (length(which(!is.na(qfInp$irgaStor$qfRngTemp))) == 0){
      qfInp$irgaStor[,1:length(qfInp$irgaStor)] <- -1
    }
    
    #grouping the flags
    setQf$asrpCo2 <- data.frame("qfRngAsrpCo2" = qfInp$irgaStor$qfRngAsrpCo2, 
                                "qfStepAsrpCo2" = qfInp$irgaStor$qfStepAsrpCo2, 
                                "qfPersAsrpCo2" = qfInp$irgaStor$qfPersAsrpCo2) 
                                #"qfCalAsrpCo2" = qfInp$irgaStor$qfCalAsrpCo2)
    
    setQf$asrpH2o <- data.frame("qfRngAsrpH2o" = qfInp$irgaStor$qfRngAsrpH2o, 
                                "qfStepAsrpH2o" = qfInp$irgaStor$qfStepAsrpH2o, 
                                "qfPersAsrpH2o" = qfInp$irgaStor$qfPersAsrpH2o) 
                                #"qfCalAsrpH2o" = qfInp$irgaStor$qfCalAsrpH2o)
    
    setQf$rtioMoleDryCo2 <- data.frame("qfRngRtioMoleDryCo2" = qfInp$irgaStor$qfRngRtioMoleDryCo2, 
                                       "qfStepRtioMoleDryCo2" = qfInp$irgaStor$qfStepRtioMoleDryCo2,
                                       "qfPersRtioMoleDryCo2" = qfInp$irgaStor$qfPersRtioMoleDryCo2) 
                                       #"qfCalRtioMoleDryCo2" = qfInp$irgaStor$qfCalRtioMoleDryCo2)
    
    setQf$rtioMoleDryH2o <- data.frame("qfRngRtioMoleDryH2o" = qfInp$irgaStor$qfRngRtioMoleDryH2o, 
                                       "qfStepRtioMoleDryH2o" = qfInp$irgaStor$qfStepRtioMoleDryH2o,
                                       "qfPersRtioMoleDryH2o" = qfInp$irgaStor$qfPersRtioMoleDryH2o) 
                                       #"qfCalRtioMoleDryH2o" = qfInp$irgaStor$qfCalRtioMoleDryH2o)
    
    setQf$rtioMoleWetCo2 <- data.frame("qfRngRtioMoleWetCo2" = qfInp$irgaStor$qfRngRtioMoleWetCo2,
                                       "qfStepRtioMoleWetCo2" = qfInp$irgaStor$qfStepRtioMoleWetCo2,
                                       "qfPersRtioMoleWetCo2" = qfInp$irgaStor$qfPersRtioMoleWetCo2)
                                       #"qfCalRtioMoleWetCo2" = qfInp$irgaStor$qfCalRtioMoleWetCo2)
    
    setQf$rtioMoleWetH2o <- data.frame("qfRngRtioMoleWetH2o" = qfInp$irgaStor$qfRngRtioMoleWetH2o,
                                       "qfStepRtioMoleWetH2o" = qfInp$irgaStor$qfStepRtioMoleWetH2o,
                                       "qfPersRtioMoleWetH2o" = qfInp$irgaStor$qfPersRtioMoleWetH2o)
                                       #"qfCalRtioMoleWetH2o" = qfInp$irgaStor$qfCalRtioMoleWetH2o)
    
    setQf$presIrga <- data.frame("qfRngPres" = qfInp$irgaStor$qfRngPres, 
                                 "qfStepPres" = qfInp$irgaStor$qfStepPres,
                                 "qfPersPres" = qfInp$irgaStor$qfPersPres)
                                 #"qfCalPres" = qfInp$irgaStor$qfCalPres)
    
    setQf$tempIrga <- data.frame ("qfRngTemp" = qfInp$irgaStor$qfRngTemp, 
                                  "qfStepTemp" = qfInp$irgaStor$qfStepTemp,
                                  "qfPersTemp" = qfInp$irgaStor$qfPersTemp)
                                  #"qfCalTemp" = qfInp$irgaStor$qfCalTemp)
    
    #change column names
    names(setQf$asrpCo2) <- paste0(colnames(setQf$asrpCo2), "IrgaStor")
    names(setQf$asrpH2o) <- paste0(colnames(setQf$asrpH2o), "IrgaStor")
    names(setQf$rtioMoleDryCo2) <- paste0(colnames(setQf$rtioMoleDryCo2), "IrgaStor")
    names(setQf$rtioMoleDryH2o) <- paste0(colnames(setQf$rtioMoleDryH2o), "IrgaStor")
    names(setQf$rtioMoleWetCo2) <- paste0(colnames(setQf$rtioMoleWetCo2), "IrgaStor")
    names(setQf$rtioMoleWetH2o) <- paste0(colnames(setQf$rtioMoleWetH2o), "IrgaStor")
    names(setQf$presIrga) <- paste0(colnames(setQf$presIrga), "IrgaStor")
    names(setQf$tempIrga) <- paste0(colnames(setQf$tempIrga), "IrgaStor")
    
    #external quality flags from envHut
    setQf$envHut <- data.frame("qfTemp" = qfInp$envHut$qfTemp)
    setQf$presEnvHut <- data.frame("qfRngPres" = qfInp$envHut$qfRngPres, 
                                   "qfStepPres" = qfInp$envHut$qfStepPres,
                                   "qfPersPres" = qfInp$envHut$qfPersPres)
    
    setQf$rhEnvHut <- data.frame("qfRngRh" = qfInp$envHut$qfRngRh, 
                                 "qfStepRh" = qfInp$envHut$qfStepRh,
                                 "qfPersRh" = qfInp$envHut$qfPersRh)
    
    setQf$rtioMoleWetH2oEnvHut <- data.frame("qfRngRtioMoleWetH2o" = qfInp$envHut$qfRngRtioMoleWetH2o, 
                                             "qfStepRtioMoleWetH2o" = qfInp$envHut$qfStepRtioMoleWetH2o,
                                             "qfPersRtioMoleWetH2o" = qfInp$envHut$qfPersRtioMoleWetH2o)
    
    setQf$tempEnvHut <- data.frame("qfRngTemp" = qfInp$envHut$qfRngTemp, 
                                   "qfStepTemp" = qfInp$envHut$qfStepTemp,
                                   "qfPersTemp" = qfInp$envHut$qfPersTemp)
    
    #change column names
    names(setQf$envHut) <- paste0(colnames(setQf$envHut), "EnvHut")
    names(setQf$presEnvHut) <- paste0(colnames(setQf$presEnvHut), "EnvHut")
    names(setQf$rhEnvHut) <- paste0(colnames(setQf$rhEnvHut), "EnvHut")
    names(setQf$rtioMoleWetH2oEnvHut) <- paste0(colnames(setQf$rtioMoleWetH2oEnvHut), "EnvHut")
    names(setQf$tempEnvHut) <- paste0(colnames(setQf$tempEnvHut), "EnvHut")
    
    #external quality flags from valvAux
    setQf$valvAux <- data.frame("qfValvIrga" = qfInp$valvAux$qfValvIrga)
    
    #heater flag
    setQf$heatInlt <- data.frame("qfHeat" = qfInp$irgaStor$qfHeat)
    
    #external quality flags from mfcSampStor
    setQf$frt00MfcSampStor <- data.frame("qfRngFrt00" = qfInp$mfcSampStor$qfRngFrt00,
                                         "qfStepFrt00" = qfInp$mfcSampStor$qfStepFrt00)
    
    setQf$frtMfcSampStor <- data.frame("qfRngFrt" = qfInp$mfcSampStor$qfRngFrt,
                                       "qfStepFrt" = qfInp$mfcSampStor$qfStepFrt,
                                       "qfPersFrt" = qfInp$mfcSampStor$qfPersFrt)
    
    setQf$presAtmMfcSampStor <- data.frame("qfRngPresAtm" = qfInp$mfcSampStor$qfRngPresAtm,
                                           "qfStepPresAtm" = qfInp$mfcSampStor$qfStepPresAtm,
                                           "qfPersPresAtm" = qfInp$mfcSampStor$qfPersPresAtm)
    
    setQf$tempMfcSampStor <- data.frame("qfRngTemp" = qfInp$mfcSampStor$qfRngTemp,
                                        "qfStepTemp" = qfInp$mfcSampStor$qfStepTemp,
                                        "qfPersTemp" = qfInp$mfcSampStor$qfPersTemp)
    
    setQf$sensMfcSampStor <- data.frame("qfFrt00" = qfInp$mfcSampStor$qfFrt00)
    
    #change column names
    names(setQf$frt00MfcSampStor) <- paste0(colnames(setQf$frt00MfcSampStor), "MfcSampStor")
    names(setQf$frtMfcSampStor) <- paste0(colnames(setQf$frtMfcSampStor), "MfcSampStor")
    names(setQf$presAtmMfcSampStor) <- paste0(colnames(setQf$presAtmMfcSampStor), "MfcSampStor")
    names(setQf$tempMfcSampStor) <- paste0(colnames(setQf$tempMfcSampStor), "MfcSampStor")
    names(setQf$sensMfcSampStor) <- paste0(colnames(setQf$sensMfcSampStor), "MfcSampStor")
    
    #external quality flags from mfcValiStor
    setQf$frt00MfcVali <- data.frame("qfRngFrt00" = qfInp$mfcValiStor$qfRngFrt00,
                                     "qfStepFrt00" = qfInp$mfcValiStor$qfStepFrt00)
    
    setQf$frtMfcVali <- data.frame("qfRngFrt" = qfInp$mfcValiStor$qfRngFrt,
                                   "qfStepFrt" = qfInp$mfcValiStor$qfStepFrt,
                                   "qfPersFrt" = qfInp$mfcValiStor$qfPersFrt)
    
    setQf$presAtmMfcVali <- data.frame("qfRngPresAtm" = qfInp$mfcValiStor$qfRngPresAtm,
                                       "qfStepPresAtm" = qfInp$mfcValiStor$qfStepPresAtm,
                                       "qfPersPresAtm" = qfInp$mfcValiStor$qfPersPresAtm)
    
    setQf$tempMfcVali <- data.frame("qfRngTemp" = qfInp$mfcValiStor$qfRngTemp,
                                    "qfStepTemp" = qfInp$mfcValiStor$qfStepTemp,
                                    "qfPersTemp" = qfInp$mfcValiStor$qfPersTemp)
    
    setQf$sensMfcVali <- data.frame("qfFrt00" = qfInp$mfcValiStor$qfFrt00)
    
    #change column names
    names(setQf$frt00MfcVali) <- paste0(colnames(setQf$frt00MfcVali), "MfcValiStor")
    names(setQf$frtMfcVali) <- paste0(colnames(setQf$frtMfcVali), "MfcValiStor")
    names(setQf$presAtmMfcVali) <- paste0(colnames(setQf$presAtmMfcVali), "MfcValiStor")
    names(setQf$tempMfcVali) <- paste0(colnames(setQf$tempMfcVali), "MfcValiStor")
    names(setQf$sensMfcVali) <- paste0(colnames(setQf$sensMfcVali), "MfcValiStor")
    
    #external quality flags from mfm
    setQf$frt00Mfm<- data.frame("qfRngFrt00" = qfInp$mfm$qfRngFrt00,
                                "qfStepFrt00" = qfInp$mfm$qfStepFrt00,
                                "qfPersFrt00" = qfInp$mfm$qfPersFrt00)
    
    setQf$frtMfm <- data.frame("qfRngFrt" = qfInp$mfm$qfRngFrt,
                               "qfStepFrt" = qfInp$mfm$qfStepFrt,
                               "qfPersFrt" = qfInp$mfm$qfPersFrt)
    
    setQf$presAtmMfm <- data.frame("qfRngPresAtm" = qfInp$mfm$qfRngPresAtm,
                                   "qfStepPresAtm" = qfInp$mfm$qfStepPresAtm,
                                   "qfPersPresAtm" = qfInp$mfm$qfPersPresAtm)
    
    setQf$tempMfm<- data.frame("qfRngTemp" = qfInp$mfm$qfRngTemp,
                               "qfStepTemp" = qfInp$mfm$qfStepTemp,
                               "qfPersTemp" = qfInp$mfm$qfPersTemp)
    
    setQf$sensMfm <- data.frame("qfFrt00" = qfInp$mfm$qfFrt00)
    
    #change column names
    names(setQf$frt00Mfm) <- paste0(colnames(setQf$frt00Mfm), "Mfm")
    names(setQf$frtMfm) <- paste0(colnames(setQf$frtMfm), "Mfm")
    names(setQf$presAtmMfm) <- paste0(colnames(setQf$presAtmMfm), "Mfm")
    names(setQf$tempMfm) <- paste0(colnames(setQf$tempMfm), "Mfm")
    names(setQf$sensMfm) <- paste0(colnames(setQf$sensMfm), "Mfm")
    
    #external quality flags from presInlt
    setQf$presInlt <- data.frame("qfPresDiff" = qfInp$presInlt$qfPresDiff)
    #change column names
    names(setQf$presInlt) <- paste0(colnames(setQf$presInlt), "PresInlt")
    
    #external quality flags from pumpStor
    setQf$pumpStor <- data.frame("qfPersPumpVolt" = qfInp$pumpStor$qfPersPumpVolt,
                                 "qfRngPumpVolt" = qfInp$pumpStor$qfRngPumpVolt,
                                 "qfStepPumpVolt" = qfInp$pumpStor$qfStepPumpVolt)
    #change column names
    names(setQf$pumpStor) <- paste0(colnames(setQf$pumpStor), "PumpStor")
    
    #external quality flags from pumpIrgaStor
    setQf$pumpIrgaStor <- data.frame("qfPersPumpVolt" = qfInp$pumpIrgaStor$qfPersPumpVolt,
                                 "qfRngPumpVolt" = qfInp$pumpIrgaStor$qfRngPumpVolt,
                                 "qfStepPumpVolt" = qfInp$pumpIrgaStor$qfStepPumpVolt)
    #change column names
    names(setQf$pumpIrgaStor) <- paste0(colnames(setQf$pumpIrgaStor), "PumpIrgaStor")
    
    #external quality flags from presValiRegInStor
    setQf$presValiRegInStor <- data.frame("qfPresDiff" = qfInp$presValiRegInStor$qfPresDiff)
    #change column names
    names(setQf$presValiRegInStor) <- paste0(colnames(setQf$presValiRegInStor), "PresValiRegInStor")
    
    #grouping qulity flags that related to co2Stor L1 sub-data product
    if (dp01 == "co2Stor"){
      if (TypeMeas == "samp"){
        rpt$rtioMoleDryCo2 <- na.omit(data.frame(setQf$rtioMoleDryCo2, setQf$asrpCo2,
                                                 setQf$asrpH2o, setQf$rtioMoleWetCo2,
                                                 setQf$rtioMoleWetH2o, setQf$presIrga,
                                                 setQf$tempIrga, #setQf$envHut, 
                                                 #etQf$valvAux, 
                                                 #setQf$heatInlt,
                                                 #setQf$frt00MfcSampStor, setQf$frtMfcSampStor, 
                                                 #setQf$presAtmMfcSampStor, setQf$tempMfcSampStor,
                                                 setQf$sensMfcSampStor,
                                                 #setQf$frt00Mfm, setQf$frtMfm, 
                                                 #setQf$presAtmMfm, setQf$tempMfm,
                                                 setQf$sensMfm#, setQf$presInl,
                                                 #setQf$pumpStor, setQf$pumpIrgaStor
                                                 ))
        
        rpt$rtioMoleWetCo2 <- na.omit(data.frame(setQf$rtioMoleWetCo2, setQf$asrpCo2,
                                                 setQf$asrpH2o, setQf$rtioMoleWetH2o, 
                                                 setQf$presIrga, setQf$tempIrga, 
                                                 #setQf$envHut, setQf$valvAux, 
                                                 #setQf$heatInlt, 
                                                 #setQf$frt00MfcSampStor, 
                                                 #setQf$frtMfcSampStor, setQf$presAtmMfcSampStor, 
                                                 #setQf$tempMfcSampStor, 
                                                 setQf$sensMfcSampStor,
                                                 #setQf$frt00Mfm, setQf$frtMfm, 
                                                 #setQf$presAtmMfm, setQf$tempMfm,
                                                 setQf$sensMfm #, setQf$presInl,
                                                 #setQf$pumpStor, setQf$pumpIrgaStor
                                                 ))
      }#close if statement of TypeMeas == "samp"
      
      if (TypeMeas == "vali"){
        rpt$rtioMoleDryCo2 <- na.omit(data.frame(setQf$rtioMoleDryCo2, setQf$asrpCo2,
                                                 setQf$asrpH2o, setQf$rtioMoleWetCo2,
                                                 setQf$rtioMoleWetH2o, setQf$presIrga,
                                                 setQf$tempIrga, #setQf$envHut, 
                                                 #setQf$valvAux, setQf$frt00MfcSampStor, 
                                                # setQf$frtMfcSampStor, setQf$presAtmMfcSampStor, 
                                                 #setQf$tempMfcSampStor, setQf$sensMfcSampStor,
                                                 #setQf$frt00MfcVali, setQf$frtMfcVali, 
                                                 #setQf$presAtmMfcVali, setQf$tempMfcVali, 
                                                 setQf$sensMfcVali#, setQf$pumpIrgaStor,
                                                # setQf$presValiRegInStor
                                                 ))
        
        rpt$rtioMoleWetCo2 <- na.omit(data.frame(setQf$rtioMoleWetCo2, setQf$asrpCo2,
                                                 setQf$asrpH2o, setQf$rtioMoleWetH2o, 
                                                 setQf$presIrga, setQf$tempIrga, 
                                                 #setQf$envHut, setQf$valvAux, 
                                                 #setQf$frt00MfcSampStor, setQf$frtMfcSampStor, 
                                                 #setQf$presAtmMfcSampStor, setQf$tempMfcSampStor, 
                                                 #setQf$sensMfcSampStor,setQf$frt00MfcVali, 
                                                 #setQf$frtMfcVali, setQf$presAtmMfcVali, 
                                                 #setQf$tempMfcVali, 
                                                 setQf$sensMfcVali#,
                                                 #setQf$pumpIrgaStor, setQf$presValiRegInStor
                                                 ))
      }#close if statement of TypeMeas == "vali"
      
      rpt$pres <- na.omit(data.frame(setQf$presIrga))
      rpt$frt00 <- na.omit(data.frame (setQf$frt00MfcSampStor, setQf$frtMfcSampStor,
                                       setQf$presAtmMfcSampStor, setQf$tempMfcSampStor,
                                       setQf$sensMfcSampStor))
      rpt$temp <- na.omit(data.frame (setQf$tempIrga))
      rpt$presEnvHut <- na.omit(data.frame(setQf$presEnvHut))
      rpt$rhEnvHut <- na.omit(data.frame (setQf$rhEnvHut))
      rpt$tempEnvHut <- na.omit(data.frame (setQf$tempEnvHut))
      rpt$rtioMoleWetH2oEnvHut <- na.omit(data.frame (setQf$rtioMoleWetH2oEnvHut))
      
    }#close if statement of dp01 == "co2Stor"
    #grouping qulity flags that related to h2oStor L1 sub-data product    
    if (dp01 == "h2oStor") {
      if (TypeMeas == "samp"){
        rpt$rtioMoleDryH2o <- na.omit(data.frame(setQf$rtioMoleDryH2o, setQf$asrpH2o,
                                                 setQf$rtioMoleWetH2o, setQf$presIrga,
                                                 setQf$tempIrga, #setQf$envHut, 
                                                 #setQf$valvAux, 
                                                 #setQf$heatInlt, 
                                                 #setQf$frt00MfcSampStor, setQf$frtMfcSampStor, 
                                                 #setQf$presAtmMfcSampStor, setQf$tempMfcSampStor, 
                                                 #setQf$sensMfcSampStor,
                                                 #setQf$frt00Mfm, setQf$frtMfm, 
                                                 #setQf$presAtmMfm, setQf$tempMfm,
                                                 setQf$sensMfm#, setQf$presInl,
                                                 #setQf$pumpStor, setQf$pumpIrgaStor
                                                 ))
        
        rpt$rtioMoleWetH2o <- na.omit(data.frame(setQf$rtioMoleWetH2o, setQf$asrpH2o,
                                                 setQf$presIrga, setQf$tempIrga,
                                                 #setQf$envHut, setQf$valvAux, 
                                                 #setQf$heatInlt, 
                                                 #setQf$frt00MfcSampStor, 
                                                 #setQf$frtMfcSampStor, setQf$presAtmMfcSampStor, 
                                                 #setQf$tempMfcSampStor, setQf$sensMfcSampStor,
                                                 #setQf$frt00Mfm, setQf$frtMfm, 
                                                 #setQf$presAtmMfm, setQf$tempMfm,
                                                 setQf$sensMfm #, setQf$presInl,
                                                 #setQf$pumpStor, setQf$pumpIrgaStor
                                                 ))
      }#close if statement of TypeMeas == "samp"
      
      if (TypeMeas == "vali"){
        rpt$rtioMoleDryH2o <- na.omit(data.frame(setQf$rtioMoleDryH2o, setQf$asrpH2o,
                                                 setQf$rtioMoleWetH2o, setQf$presIrga,
                                                 setQf$tempIrga, #setQf$envHut, 
                                                 #setQf$valvAux, setQf$frt00MfcSampStor, 
                                                 #setQf$frtMfcSampStor, setQf$presAtmMfcSampStor, 
                                                 #setQf$tempMfcSampStor, setQf$sensMfcSampStor,
                                                 #setQf$frt00MfcVali, setQf$frtMfcVali, 
                                                 #setQf$presAtmMfcVali, setQf$tempMfcVali, 
                                                 setQf$sensMfcVali#, setQf$pumpIrgaStor,
                                                 #setQf$presValiRegInStor
                                                 ))
        
        rpt$rtioMoleWetH2o <- na.omit(data.frame(setQf$rtioMoleWetH2o, setQf$asrpH2o,
                                                 setQf$presIrga, setQf$tempIrga, 
                                                 #setQf$envHut, setQf$valvAux, 
                                                 #setQf$frt00MfcSampStor, setQf$frtMfcSampStor, 
                                                 #setQf$presAtmMfcSampStor, setQf$tempMfcSampStor, 
                                                 #setQf$sensMfcSampStor,setQf$frt00MfcVali, 
                                                 #setQf$frtMfcVali, setQf$presAtmMfcVali, 
                                                 #setQf$tempMfcVali, 
                                                 setQf$sensMfcVali#,
                                                 #setQf$pumpIrgaStor, setQf$presValiRegInStor
                                                 ))
        
      }#close if statement of TypeMeas == "vali"
      rpt$pres <- na.omit(data.frame(setQf$presIrga))
      rpt$frt00 <- na.omit(data.frame(setQf$frt00MfcSampStor, setQf$frtMfcSampStor, 
                                      setQf$presAtmMfcSampStor, setQf$tempMfcSampStor, 
                                      setQf$sensMfcSampStor)) 
      rpt$temp <- na.omit(data.frame(setQf$tempIrga))
      rpt$presEnvHut <- na.omit(data.frame(setQf$presEnvHut))
      rpt$rhEnvHut <- na.omit(data.frame (setQf$rhEnvHut))
      rpt$tempEnvHut <- na.omit(data.frame (setQf$tempEnvHut))
      rpt$rtioMoleWetH2oEnvHut <- na.omit(data.frame (setQf$rtioMoleWetH2oEnvHut))
    }#close if statement of dp01 == "h2oStor"
    #remove setQf
    setQf <- NULL
  }##close if statement of dp01 %in% c("co2Stor", "h2oStor")
  
#isoCo2 ####################################################################################
  if (dp01 == "isoCo2") {
    #check if data are exist
    #external quality flags from envHut
    if (!("envHut" %in% names(qfInp)) || length(which(!is.na(qfInp$crdCo2$qfRngTemp))) == 0){
      qfInp$envHut <- as.data.frame(matrix(-1, ncol = 14, nrow = length(qfInp$crdCo2$qfRngRtioMoleDryCo2)))
      names(qfInp$envHut) <- c("qfRngPres", "qfStepPres", "qfPersPres", "qfRngRh", "qfStepRh", "qfPersRh",
                                 "qfRngRtioMoleWetH2o", "qfStepRtioMoleWetH2o", "qfPersRtioMoleWetH2o", 
                                 "qfRngTemp", "qfStepTemp", "qfPersTemp", "qfTemp", "qfRh")}
    
    #heater flag
    if (!("qfHeat" %in% names(qfInp$crdCo2))){
      qfInp$crdCo2$qfHeat <- -1}
    
    #external quality flags from mfcValiStor
    if (!("mfcValiStor" %in% names(qfInp)) || length(which(!is.na(qfInp$crdCo2$qfRngTemp))) == 0){
      qfInp$mfcValiStor <- as.data.frame(matrix(-1, ncol = 12, nrow = length(qfInp$crdCo2$qfRngRtioMoleDryCo2)))
      names(qfInp$mfcValiStor) <- c("qfRngFrt00", "qfStepFrt00", "qfRngFrt", "qfStepFrt", "qfPersFrt",
                                  "qfRngPresAtm", "qfStepPresAtm", "qfPersPresAtm", "qfRngTemp", "qfStepTemp", "qfPersTemp",
                                  "qfFrt00")}
    #external quality flags from mfm
    if (!("mfm" %in% names(qfInp)) || length(which(!is.na(qfInp$crdCo2$qfRngTemp))) == 0){
      qfInp$mfm <- as.data.frame(matrix(-1, ncol = 13, nrow = length(qfInp$crdCo2$qfRngRtioMoleDryCo2)))
      names(qfInp$mfm) <- c("qfRngFrt00", "qfStepFrt00", "qfPersFrt00", "qfRngFrt", "qfStepFrt", "qfPersFrt",
                            "qfRngPresAtm", "qfStepPresAtm", "qfPersPresAtm", "qfRngTemp", "qfStepTemp", "qfPersTemp",
                            "qfFrt00")}
    #external quality flags from presInlt
    if (!("presInlt" %in% names(qfInp)) || length(which(!is.na(qfInp$crdCo2$qfRngTemp))) == 0){
      qfInp$presInlt <- as.data.frame(matrix(-1, ncol = 4, nrow = length(qfInp$crdCo2$qfRngRtioMoleDryCo2)))
      names(qfInp$presInlt) <- c("qfPersPresDiff", "qfPresDiff", "qfRngPresDiff", "qfStepPresDiff")}
    
    #external quality flags from pumpStor
    if (!("pumpStor" %in% names(qfInp)) || length(which(!is.na(qfInp$crdCo2$qfRngTemp))) == 0){
      qfInp$pumpStor <- as.data.frame(matrix(-1, ncol = 3, nrow = length(qfInp$crdCo2$qfRngRtioMoleDryCo2)))
      names(qfInp$pumpStor) <- c("qfPersPumpVolt", "qfRngPumpVolt", "qfStepPumpVolt")}
    
    #external quality flags from presValiRegInStor
    if (!("presValiRegInStor" %in% names(qfInp)) || length(which(!is.na(qfInp$crdCo2$qfRngTemp))) == 0){
      qfInp$presValiRegInStor <- as.data.frame(matrix(-1, ncol = 4, nrow = length(qfInp$crdCo2$qfRngRtioMoleDryCo2)))
      names(qfInp$presValiRegInStor) <- c("qfPersPresDiff", "qfPresDiff", "qfRngPresDiff", "qfStepPresDiff")}
    
    #replace -1 if all qf in crdCo2 are NA
    if (length(which(!is.na(qfInp$crdCo2$qfRngTemp))) == 0){
      qfInp$crdCo2[,1:length(qfInp$crdCo2)] <- -1
    }
    #replace -1 if all qfSens in crdCo2 are NA
    if (length(which(!is.na(qfInp$crdCo2$qfSensStus))) == 0){
      qfInp$crdCo2$qfSensStus <- -1
    }
    #setQf for crdCo2
    setQf$rtioMoleDryCo2 <- data.frame("qfRngRtioMoleDryCo2" = qfInp$crdCo2$qfRngRtioMoleDryCo2, 
                                       "qfStepRtioMoleDryCo2" = qfInp$crdCo2$qfStepRtioMoleDryCo2,
                                       "qfPersRtioMoleDryCo2" = qfInp$crdCo2$qfPersRtioMoleDryCo2) 
                                       #"qfCalRtioMoleDryCo2" = qfInp$crdCo2$qfCalRtioMoleDryCo2)
    
    setQf$rtioMoleDry12CCo2 <- data.frame("qfRngRtioMoleDry12CCo2" = qfInp$crdCo2$qfRngRtioMoleDry12CCo2, 
                                          "qfStepRtioMoleDry12CCo2" = qfInp$crdCo2$qfStepRtioMoleDry12CCo2,
                                          "qfPersRtioMoleDry12CCo2" = qfInp$crdCo2$qfPersRtioMoleDry12CCo2) 
                                          #"qfCalRtioMoleDry12CCo2" = qfInp$crdCo2$qfCalRtioMoleDry12CCo2)
    
    setQf$rtioMoleDry13CCo2 <- data.frame("qfRngRtioMoleDry13CCo2" = qfInp$crdCo2$qfRngRtioMoleDry13CCo2, 
                                          "qfStepRtioMoleDry13CCo2" = qfInp$crdCo2$qfStepRtioMoleDry13CCo2,
                                          "qfPersRtioMoleDry13CCo2" = qfInp$crdCo2$qfPersRtioMoleDry13CCo2) 
                                          #"qfCalRtioMoleDry13CCo2" = qfInp$crdCo2$qfCalRtioMoleDry13CCo2)
    
    setQf$rtioMoleDryH2o <- data.frame("qfRngRtioMoleDryH2o" = qfInp$crdCo2$qfRngRtioMoleDryH2o, 
                                       #"qfStepRtioMoleDryH2o" = qfInp$crdCo2$qfStepRtioMoleDryH2o,
                                       "qfPersRtioMoleDryH2o" = qfInp$crdCo2$qfPersRtioMoleDryH2o) 
                                       #"qfCalRtioMoleDryH2o" = qfInp$crdCo2$qfCalRtioMoleDryH2o)
    
    setQf$rtioMoleWetCo2 <- data.frame("qfRngRtioMoleWetCo2" = qfInp$crdCo2$qfRngRtioMoleWetCo2, 
                                       "qfStepRtioMoleWetCo2" = qfInp$crdCo2$qfStepRtioMoleWetCo2,
                                       "qfPersRtioMoleWetCo2" = qfInp$crdCo2$qfPersRtioMoleWetCo2) 
                                       #"qfCalRtioMoleWetCo2" = qfInp$crdCo2$qfCalRtioMoleWetCo2)
    
    setQf$rtioMoleWet12CCo2 <- data.frame("qfRngRtioMoleWet12CCo2" = qfInp$crdCo2$qfRngRtioMoleWet12CCo2, 
                                          "qfStepRtioMoleWet12CCo2" = qfInp$crdCo2$qfStepRtioMoleWet12CCo2,
                                          "qfPersRtioMoleWet12CCo2" = qfInp$crdCo2$qfPersRtioMoleWet12CCo2) 
                                          #"qfCalRtioMoleWet12CCo2" = qfInp$crdCo2$qfCalRtioMoleWet12CCo2)
    
    setQf$rtioMoleWet13CCo2 <- data.frame("qfRngRtioMoleWet13CCo2" = qfInp$crdCo2$qfRngRtioMoleWet13CCo2, 
                                          "qfStepRtioMoleWet13CCo2" = qfInp$crdCo2$qfStepRtioMoleWet13CCo2,
                                          "qfPersRtioMoleWet13CCo2" = qfInp$crdCo2$qfPersRtioMoleWet13CCo2) 
                                          #"qfCalRtioMoleWet13CCo2 " = qfInp$crdCo2$qfCalRtioMoleWet13CCo2)
    
    setQf$rtioMoleWetH2o <- data.frame("qfRngRtioMoleWetH2o" = qfInp$crdCo2$qfRngRtioMoleWetH2o, 
                                       #"qfStepRtioMoleWetH2o" = qfInp$crdCo2$qfStepRtioMoleWetH2o,
                                       "qfPersRtioMoleWetH2o" = qfInp$crdCo2$qfPersRtioMoleWetH2o) 
                                       #"qfCalRtioMoleWetH2o" = qfInp$crdCo2$qfCalRtioMoleWetH2o)
    
    setQf$dlta13CCo2 <- data.frame("qfRngDlta13CCo2" = qfInp$crdCo2$qfRngDlta13CCo2, 
                                   "qfStepDlta13CCo2" = qfInp$crdCo2$qfStepDlta13CCo2,
                                   "qfPersDlta13CCo2" = qfInp$crdCo2$qfPersDlta13CCo2)
                                   #"qfCalDlta13CCo2" = qfInp$crdCo2$qfCalDlta13CCo2)
    
    setQf$presCrdCo2 <- data.frame("qfRngPres" = qfInp$crdCo2$qfRngPres, 
                                   "qfStepPres" = qfInp$crdCo2$qfStepPres,
                                   "qfPersPres" = qfInp$crdCo2$qfPersPres) 
                                   #"qfCalPres" = qfInp$crdCo2$qfCalPres)
    
    setQf$tempCrdCo2 <- data.frame("qfRngTemp" = qfInp$crdCo2$qfRngTemp, 
                                   "qfStepTemp" = qfInp$crdCo2$qfStepTemp,
                                   "qfPersTemp" = qfInp$crdCo2$qfPersTemp) 
                                   #"qfCalTemp" = qfInp$crdCo2$qfCalTemp)
    
    setQf$tempWbox <- data.frame("qfRngTempWbox" = qfInp$crdCo2$qfRngTempWbox, 
                                 "qfStepTempWbox" = qfInp$crdCo2$qfStepTempWbox,
                                 "qfPersTempWbox" = qfInp$crdCo2$qfPersTempWbox) 
                                 #"qfCalTempWbox" = qfInp$crdCo2$qfCalTempWbox)
    setQf$sensCrdCo2 <- data.frame("qfSensStus" = qfInp$crdCo2$qfSensStus)
    
    #change column names
    names(setQf$rtioMoleDryCo2) <- paste0(colnames(setQf$rtioMoleDryCo2), "CrdCo2")
    names(setQf$rtioMoleDry12CCo2) <- paste0(colnames(setQf$rtioMoleDry12CCo2), "CrdCo2")
    names(setQf$rtioMoleDry13CCo2) <- paste0(colnames(setQf$rtioMoleDry13CCo2), "CrdCo2")
    names(setQf$rtioMoleDryH2o) <- paste0(colnames(setQf$rtioMoleDryH2o), "CrdCo2")
    names(setQf$rtioMoleWetCo2) <- paste0(colnames(setQf$rtioMoleWetCo2), "CrdCo2")
    names(setQf$rtioMoleWet12CCo2) <- paste0(colnames(setQf$rtioMoleWet12CCo2), "CrdCo2")
    names(setQf$rtioMoleWet13CCo2) <- paste0(colnames(setQf$rtioMoleWet13CCo2), "CrdCo2")
    names(setQf$rtioMoleWetH2o) <- paste0(colnames(setQf$rtioMoleWetH2o), "CrdCo2")
    names(setQf$dlta13CCo2) <- paste0(colnames(setQf$dlta13CCo2), "CrdCo2")
    names(setQf$presCrdCo2) <- paste0(colnames(setQf$presCrdCo2), "CrdCo2")
    names(setQf$tempCrdCo2) <- paste0(colnames(setQf$tempCrdCo2), "CrdCo2")
    names(setQf$tempWbox) <- paste0(colnames(setQf$tempWbox), "CrdCo2")
    names(setQf$sensCrdCo2) <- paste0(colnames(setQf$sensCrdCo2), "CrdCo2")
    
    #external quality flags from envHut
    setQf$presEnvHut <- data.frame("qfRngPres" = qfInp$envHut$qfRngPres, 
                                   "qfStepPres" = qfInp$envHut$qfStepPres,
                                   "qfPersPres" = qfInp$envHut$qfPersPres)
    
    setQf$rhEnvHut <- data.frame("qfRngRh" = qfInp$envHut$qfRngRh, 
                                 "qfStepRh" = qfInp$envHut$qfStepRh,
                                 "qfPersRh" = qfInp$envHut$qfPersRh)
    
    setQf$rtioMoleWetH2oEnvHut <- data.frame("qfRngRtioMoleWetH2o" = qfInp$envHut$qfRngRtioMoleWetH2o, 
                                             "qfStepRtioMoleWetH2o" = qfInp$envHut$qfStepRtioMoleWetH2o,
                                             "qfPersRtioMoleWetH2o" = qfInp$envHut$qfPersRtioMoleWetH2o)
    
    setQf$tempEnvHut <- data.frame("qfRngTemp" = qfInp$envHut$qfRngTemp, 
                                   "qfStepTemp" = qfInp$envHut$qfStepTemp,
                                   "qfPersTemp" = qfInp$envHut$qfPersTemp)
    
    #change column names
    names(setQf$presEnvHut) <- paste0(colnames(setQf$presEnvHut), "EnvHut")
    names(setQf$rhEnvHut) <- paste0(colnames(setQf$rhEnvHut), "EnvHut")
    names(setQf$rtioMoleWetH2oEnvHut) <- paste0(colnames(setQf$rtioMoleWetH2oEnvHut), "EnvHut")
    names(setQf$tempEnvHut) <- paste0(colnames(setQf$tempEnvHut), "EnvHut")
    
    #heater flag
    setQf$heatInlt <- data.frame("qfHeat" = qfInp$crdCo2$qfHeat)
    
    #setQf from mfcValiStor
    setQf$frt00MfcVali <- data.frame("qfRngFrt00" = qfInp$mfcValiStor$qfRngFrt00, 
                                     "qfStepFrt00" = qfInp$mfcValiStor$qfStepFrt00)
    
    setQf$frtMfcVali <- data.frame("qfRngFrt" = qfInp$mfcValiStor$qfRngFrt,
                                   "qfStepFrt" = qfInp$mfcValiStor$qfStepFrt,
                                   "qfPersFrt" = qfInp$mfcValiStor$qfPersFrt)
    
    setQf$presAtmMfcVali <- data.frame("qfRngPresAtm" = qfInp$mfcValiStor$qfRngPresAtm, 
                                       "qfStepPresAtm" = qfInp$mfcValiStor$qfStepPresAtm,
                                       "qfPersPresAtm" = qfInp$mfcValiStor$qfPersPresAtm)
    
    setQf$tempMfcVali <- data.frame("qfRngTemp" = qfInp$mfcValiStor$qfRngTemp,
                                    "qfStepTemp" = qfInp$mfcValiStor$qfStepTemp,
                                    "qfPersTemp" = qfInp$mfcValiStor$qfPersTemp)
    
    setQf$sensMfcVali <- data.frame("qfFrt00" = qfInp$mfcValiStor$qfFrt00)
    
    #change column names
    names(setQf$frt00MfcVali) <- paste0(colnames(setQf$frt00MfcVali), "MfcValiStor")
    names(setQf$frtMfcVali) <- paste0(colnames(setQf$frtMfcVali), "MfcValiStor")
    names(setQf$presAtmMfcVali) <- paste0(colnames(setQf$presAtmMfcVali), "MfcValiStor")
    names(setQf$tempMfcVali) <- paste0(colnames(setQf$tempMfcVali), "MfcValiStor")
    names(setQf$sensMfcVali) <- paste0(colnames(setQf$sensMfcVali), "MfcValiStor")
    
    #external quality flags from mfm
    setQf$frt00Mfm<- data.frame("qfRngFrt00" = qfInp$mfm$qfRngFrt00,
                                "qfStepFrt00" = qfInp$mfm$qfStepFrt00,
                                "qfPersFrt00" = qfInp$mfm$qfPersFrt00)
    
    setQf$frtMfm <- data.frame("qfRngFrt" = qfInp$mfm$qfRngFrt,
                               "qfStepFrt" = qfInp$mfm$qfStepFrt,
                               "qfPersFrt" = qfInp$mfm$qfPersFrt)
    
    setQf$presAtmMfm <- data.frame("qfRngPresAtm" = qfInp$mfm$qfRngPresAtm,
                                   "qfStepPresAtm" = qfInp$mfm$qfStepPresAtm,
                                   "qfPersPresAtm" = qfInp$mfm$qfPersPresAtm)
    
    setQf$tempMfm<- data.frame("qfRngTemp" = qfInp$mfm$qfRngTemp,
                               "qfStepTemp" = qfInp$mfm$qfStepTemp,
                               "qfPersTemp" = qfInp$mfm$qfPersTemp)
    
    setQf$sensMfm <- data.frame("qfFrt00" = qfInp$mfm$qfFrt00)
    
    #change column names
    names(setQf$frt00Mfm) <- paste0(colnames(setQf$frt00Mfm), "Mfm")
    names(setQf$frtMfm) <- paste0(colnames(setQf$frtMfm), "Mfm")
    names(setQf$presAtmMfm) <- paste0(colnames(setQf$presAtmMfm), "Mfm")
    names(setQf$tempMfm) <- paste0(colnames(setQf$tempMfm), "Mfm")
    names(setQf$sensMfm) <- paste0(colnames(setQf$sensMfm), "Mfm")
    
    #external quality flags from presInlt
    setQf$presInlt <- data.frame("qfPresDiff" = qfInp$presInlt$qfPresDiff)
    #change column names
    names(setQf$presInlt) <- paste0(colnames(setQf$presInlt), "PresInlt")
    
    #external quality flags from pumpStor
    setQf$pumpStor <- data.frame("qfPersPumpVolt" = qfInp$pumpStor$qfPersPumpVolt,
                                 "qfRngPumpVolt" = qfInp$pumpStor$qfRngPumpVolt,
                                 "qfStepPumpVolt" = qfInp$pumpStor$qfStepPumpVolt)
    #change column names
    names(setQf$pumpStor) <- paste0(colnames(setQf$pumpStor), "PumpStor")
    
    #external quality flags from presValiRegInStor
    setQf$presValiRegInStor <- data.frame("qfPresDiff" = qfInp$presValiRegInStor$qfPresDiff)
    #change column names
    names(setQf$presValiRegInStor) <- paste0(colnames(setQf$presValiRegInStor), "PresValiRegInStor")
    
    #define qf which use only sampling period
    if (TypeMeas == "samp") {
      #temporary list
      tmp <- list()
      #if not all idGas = NA or all qfRngTemp = NA
      if (length(which(!is.na(idGas))) > 0){
        #grouping qulity flags that related to isoCo2 L1 sub-data product  
        tmp$rtioMoleWetCo2 <- data.frame(setQf$rtioMoleWetCo2, setQf$rtioMoleWet13CCo2, #setQf$dlta13CCo2,
                                         setQf$rtioMoleWet12CCo2, setQf$presCrdCo2, 
                                         setQf$tempCrdCo2, setQf$tempWbox,
                                         setQf$sensCrdCo2,
                                         #setQf$frt00Mfm, 
                                         #setQf$frtMfm, setQf$presAtmMfm, 
                                         #setQf$tempMfm,
                                         setQf$sensMfm,
                                         #setQf$presInlt, setQf$pumpStor,
                                         #setQf$heatInlt, 
                                         idGas = idGas
        )
        rpt$rtioMoleWetCo2 <- na.omit(tmp$rtioMoleWetCo2[which(tmp$rtioMoleWetCo2$idGas == 105 | (is.na(tmp$rtioMoleWetCo2$idGas) & tmp$rtioMoleWetCo2$qfSensStus == -1)),])
        
        tmp$rtioMoleDryCo2 <- data.frame(setQf$rtioMoleDryCo2, #setQf$dlta13CCo2,
                                         setQf$rtioMoleDry12CCo2, setQf$rtioMoleDry13CCo2, setQf$presCrdCo2, 
                                         setQf$tempCrdCo2, setQf$tempWbox,
                                         setQf$sensCrdCo2, 
                                         #setQf$frt00Mfm, 
                                         #setQf$frtMfm, setQf$presAtmMfm, 
                                         #setQf$tempMfm, 
                                         setQf$sensMfm,
                                         #setQf$presInlt, setQf$pumpStor,
                                         #setQf$heatInlt, 
                                         idGas = idGas
                                          
        )
        rpt$rtioMoleDryCo2 <- na.omit(tmp$rtioMoleDryCo2[which(tmp$rtioMoleDryCo2$idGas == 105 | (is.na(tmp$rtioMoleDryCo2$idGas) & tmp$rtioMoleDryCo2$qfSensStus == -1)),])
        
        tmp$rtioMoleWet12CCo2 <-data.frame(setQf$rtioMoleWet12CCo2, setQf$presCrdCo2, 
                                           setQf$tempCrdCo2, setQf$tempWbox,
                                           setQf$sensCrdCo2,
                                           #, setQf$frt00Mfm, 
                                           #setQf$frtMfm, setQf$presAtmMfm, 
                                           #setQf$tempMfm, 
                                           setQf$sensMfm,
                                           #setQf$presInlt, setQf$pumpStor,
                                           #setQf$heatInlt, 
                                           idGas = idGas
                                            
        )
        rpt$rtioMoleWet12CCo2 <- na.omit(tmp$rtioMoleWet12CCo2[which(tmp$rtioMoleWet12CCo2$idGas == 105 | (is.na(tmp$rtioMoleWet12CCo2$idGas) & tmp$rtioMoleWet12CCo2$qfSensStus == -1)),])
        
        tmp$rtioMoleDry12CCo2 <- data.frame(setQf$rtioMoleDry12CCo2, setQf$rtioMoleWet12CCo2,
                                            setQf$presCrdCo2, setQf$tempCrdCo2, 
                                            setQf$tempWbox, setQf$sensCrdCo2,
                                            #setQf$frt00Mfm, setQf$frtMfm, 
                                            #setQf$presAtmMfm, setQf$tempMfm,
                                            setQf$sensMfm,
                                            #, setQf$presInlt,
                                            #setQf$pumpStor, 
                                            #setQf$heatInlt,
                                            idGas = idGas
        )
        rpt$rtioMoleDry12CCo2 <- na.omit(tmp$rtioMoleDry12CCo2[which(tmp$rtioMoleDry12CCo2$idGas == 105 | (is.na(tmp$rtioMoleDry12CCo2$idGas) & tmp$rtioMoleDry12CCo2$qfSensStus == -1)),])
        
        tmp$rtioMoleWet13CCo2 <- data.frame(setQf$rtioMoleWet13CCo2, setQf$presCrdCo2, 
                                          setQf$tempCrdCo2, setQf$tempWbox,
                                          setQf$sensCrdCo2, 
                                          #setQf$frt00Mfm, 
                                          #setQf$frtMfm, setQf$presAtmMfm, 
                                          #setQf$tempMfm, 
                                          setQf$sensMfm,
                                          #setQf$presInlt, setQf$pumpStor,
                                          #setQf$heatInlt, 
                                          idGas = idGas
                                          
        )
        rpt$rtioMoleWet13CCo2 <- na.omit(tmp$rtioMoleWet13CCo2[which(tmp$rtioMoleWet13CCo2$idGas == 105 | (is.na(tmp$rtioMoleWet13CCo2$idGas) & tmp$rtioMoleWet13CCo2$qfSensStus == -1)),])
        
        tmp$rtioMoleDry13CCo2<- data.frame(setQf$rtioMoleDry13CCo2, setQf$rtioMoleWet13CCo2,
                                          setQf$presCrdCo2, setQf$tempCrdCo2, 
                                          setQf$tempWbox, setQf$sensCrdCo2,
                                          #setQf$frt00Mfm, setQf$frtMfm, 
                                          #setQf$presAtmMfm, setQf$tempMfm,
                                          setQf$sensMfm, 
                                          #setQf$presInlt,
                                          #setQf$pumpStor, 
                                          #setQf$heatInlt,
                                          idGas = idGas
                                          
        )
        rpt$rtioMoleDry13CCo2 <- na.omit(tmp$rtioMoleDry13CCo2[which(tmp$rtioMoleDry13CCo2$idGas == 105 | (is.na(tmp$rtioMoleDry13CCo2$idGas) & tmp$rtioMoleDry13CCo2$qfSensStus == -1)),])
        
        tmp$dlta13CCo2 <- data.frame(setQf$dlta13CCo2, setQf$rtioMoleWet12CCo2,
                                     setQf$rtioMoleWet13CCo2, setQf$presCrdCo2,
                                     setQf$tempCrdCo2, setQf$tempWbox,
                                     setQf$sensCrdCo2,
                                     #setQf$frt00Mfm, 
                                     #setQf$frtMfm, setQf$presAtmMfm, 
                                     #setQf$tempMfm, 
                                     setQf$sensMfm,
                                     #setQf$presInlt, setQf$pumpStor,
                                     #setQf$heatInlt, 
                                     idGas = idGas
                                      
        )
        rpt$dlta13CCo2 <- na.omit(tmp$dlta13CCo2[which(tmp$dlta13CCo2$idGas == 105 | (is.na(tmp$dlta13CCo2$idGas) & tmp$dlta13CCo2$qfSensStus == -1)),])
        
        tmp$rtioMoleWetH2o <- data.frame(setQf$rtioMoleWetH2o, setQf$presCrdCo2,
                                         setQf$tempCrdCo2, setQf$tempWbox,
                                         setQf$sensCrdCo2,
                                         #setQf$frt00Mfm, 
                                         #setQf$frtMfm, setQf$presAtmMfm, 
                                         #setQf$tempMfm,
                                         setQf$sensMfm,
                                         #setQf$presInlt, setQf$pumpStor,
                                         #setQf$heatInlt, 
                                         idGas = idGas
                                          
        )
        rpt$rtioMoleWetH2o <- na.omit(tmp$rtioMoleWetH2o[which(tmp$rtioMoleWetH2o$idGas == 11 | (is.na(tmp$rtioMoleWetH2o$idGas) & tmp$rtioMoleWetH2o$qfSensStus == -1)),])
        
        tmp$rtioMoleDryH2o <- data.frame(setQf$rtioMoleDryH2o, setQf$rtioMoleWetH2o, 
                                          setQf$presCrdCo2, setQf$tempCrdCo2, 
                                          setQf$tempWbox, setQf$sensCrdCo2,
                                         #setQf$frt00Mfm, setQf$frtMfm, 
                                         #setQf$presAtmMfm, setQf$tempMfm,
                                         setQf$sensMfm,
                                         #setQf$presInlt,
                                         #setQf$pumpStor, 
                                         #setQf$heatInlt,
                                         idGas = idGas
        )                             
        rpt$rtioMoleDryH2o <- na.omit(tmp$rtioMoleDryH2o[which(tmp$rtioMoleDryH2o$idGas == 11 | (is.na(tmp$rtioMoleDryH2o$idGas) & tmp$rtioMoleDryH2o$qfSensStus == -1)),])
        
        rpt$temp <- na.omit(data.frame(setQf$tempCrdCo2, setQf$sensCrdCo2))
        rpt$pres <- na.omit(data.frame(setQf$presCrdCo2, setQf$sensCrdCo2))
        rpt$presEnvHut <- na.omit(data.frame(setQf$presEnvHut))
        rpt$rhEnvHut <- na.omit(data.frame (setQf$rhEnvHut))
        rpt$tempEnvHut <- na.omit(data.frame (setQf$tempEnvHut))
        rpt$rtioMoleWetH2oEnvHut <- na.omit(data.frame (setQf$rtioMoleWetH2oEnvHut))
        #remove idGas column
        lapply(names(rpt), function(x){
          rpt[[x]] <<- rpt[[x]][, ! names(rpt[[x]]) %in% "idGas", drop = F]
        })
        #remove tmp
        rm(tmp)
      } else {
        #grouping qulity flags that related to isoCo2 L1 sub-data product  
        rpt$rtioMoleWetCo2 <- na.omit(data.frame(setQf$rtioMoleWetCo2, setQf$rtioMoleWet12CCo2, #setQf$dlta13CCo2,
                                                 setQf$rtioMoleWet13CCo2, setQf$presCrdCo2, 
                                                 setQf$tempCrdCo2, setQf$tempWbox,
                                                 setQf$sensCrdCo2, 
                                                 #setQf$frt00Mfm, 
                                                 #setQf$frtMfm, setQf$presAtmMfm, 
                                                 #setQf$tempMfm,
                                                 setQf$sensMfm#,
                                                 #setQf$presInlt, setQf$pumpStor
                                                 #setQf$heatInlt
                                                 ))
        
        rpt$rtioMoleDryCo2 <- na.omit(data.frame(setQf$rtioMoleDryCo2, setQf$rtioMoleDry12CCo2, #setQf$dlta13CCo2,
                                                 setQf$rtioMoleDry13CCo2, setQf$presCrdCo2, 
                                                 setQf$tempCrdCo2, setQf$tempWbox,
                                                 setQf$sensCrdCo2, 
                                                 #setQf$frt00Mfm, 
                                                 #setQf$frtMfm, setQf$presAtmMfm, 
                                                 #setQf$tempMfm, 
                                                 setQf$sensMfm#,
                                                 #setQf$presInlt, setQf$pumpStor
                                                 #setQf$heatInlt
                                                 ))
        
        rpt$rtioMoleWet12CCo2 <- na.omit(data.frame(setQf$rtioMoleWet12CCo2, setQf$presCrdCo2, 
                                                    setQf$tempCrdCo2, setQf$tempWbox,
                                                    setQf$sensCrdCo2, 
                                                    #setQf$frt00Mfm, 
                                                    #setQf$frtMfm, setQf$presAtmMfm, 
                                                    #setQf$tempMfm, 
                                                    setQf$sensMfm#,
                                                    #setQf$presInlt, setQf$pumpStor
                                                    #setQf$heatInlt
                                                    ))
        
        rpt$rtioMoleDry12CCo2 <- na.omit(data.frame(setQf$rtioMoleDry12CCo2, setQf$rtioMoleWet12CCo2,
                                                    setQf$presCrdCo2, setQf$tempCrdCo2, 
                                                    setQf$tempWbox, setQf$sensCrdCo2,
                                                    #setQf$frt00Mfm, setQf$frtMfm, 
                                                    #setQf$presAtmMfm, setQf$tempMfm,
                                                    setQf$sensMfm#, setQf$presInlt,
                                                    #setQf$pumpStor
                                                    #setQf$heatInlt
                                                    ))
        
        rpt$rtioMoleWet13CCo2 <- na.omit(data.frame(setQf$rtioMoleWet13CCo2, setQf$presCrdCo2, 
                                                    setQf$tempCrdCo2, setQf$tempWbox,
                                                    setQf$sensCrdCo2, #setQf$frt00Mfm, 
                                                    #setQf$frtMfm, setQf$presAtmMfm, 
                                                    #setQf$tempMfm,
                                                    setQf$sensMfm#,
                                                    #setQf$presInlt, setQf$pumpStor
                                                    #setQf$heatInlt
                                                    ))
        
        rpt$rtioMoleDry13CCo2 <- na.omit(data.frame(setQf$rtioMoleDry13CCo2, setQf$rtioMoleWet13CCo2,
                                                    setQf$presCrdCo2, setQf$tempCrdCo2, 
                                                    setQf$tempWbox, setQf$sensCrdCo2,
                                                    #setQf$frt00Mfm, setQf$frtMfm, 
                                                    #setQf$presAtmMfm, setQf$tempMfm,
                                                    setQf$sensMfm#, setQf$presInlt,
                                                    #setQf$pumpStor
                                                    #setQf$heatInlt
                                                    ))
        
        rpt$dlta13CCo2 <- na.omit(data.frame(setQf$dlta13CCo2, setQf$rtioMoleWet12CCo2,
                                             setQf$rtioMoleWet13CCo2, setQf$presCrdCo2,
                                             setQf$tempCrdCo2, setQf$tempWbox,
                                             setQf$sensCrdCo2,# setQf$frt00Mfm, 
                                             #setQf$frtMfm, setQf$presAtmMfm, 
                                             #setQf$tempMfm, 
                                             setQf$sensMfm#,
                                             #setQf$presInlt, setQf$pumpStor
                                             #setQf$heatInlt
                                             ))
        
        rpt$rtioMoleWetH2o <- na.omit(data.frame(setQf$rtioMoleWetH2o, setQf$presCrdCo2,
                                                 setQf$tempCrdCo2, setQf$tempWbox,
                                                 setQf$sensCrdCo2, #setQf$frt00Mfm, 
                                                 #setQf$frtMfm, setQf$presAtmMfm, 
                                                 #setQf$tempMfm, 
                                                 setQf$sensMfm#,
                                                 #setQf$presInlt, setQf$pumpStor
                                                 #setQf$heatInlt
                                                 ))
        
        rpt$rtioMoleDryH2o <- na.omit(data.frame(setQf$rtioMoleDryH2o, setQf$rtioMoleWetH2o, 
                                                 setQf$presCrdCo2, setQf$tempCrdCo2, 
                                                 setQf$tempWbox, setQf$sensCrdCo2,
                                                 #setQf$frt00Mfm, setQf$frtMfm, 
                                                 #setQf$presAtmMfm, setQf$tempMfm,
                                                 setQf$sensMfm#, setQf$presInlt,
                                                 #setQf$pumpStor
                                                 #setQf$heatInlt
                                                 ))
        
        rpt$temp <- na.omit(data.frame(setQf$tempCrdCo2, setQf$sensCrdCo2))
        rpt$pres <- na.omit(data.frame(setQf$presCrdCo2, setQf$sensCrdCo2)) 
        rpt$presEnvHut <- na.omit(data.frame(setQf$presEnvHut))
        rpt$rhEnvHut <- na.omit(data.frame (setQf$rhEnvHut))
        rpt$tempEnvHut <- na.omit(data.frame (setQf$tempEnvHut))
        rpt$rtioMoleWetH2oEnvHut <- na.omit(data.frame (setQf$rtioMoleWetH2oEnvHut))
      }# close else statement
      
    }#close if statement of TypeMeas == "samp"
    
    #define qf which use only validation period
    if (TypeMeas == "vali") { 
      #temporary list
      tmp <- list()
      #if not all idGas = NA or all qfRngTemp = NA
      if (length(which(!is.na(idGas))) > 0){
        #grouping qulity flags that related to isoCo2 L1 sub-data product
        tmp$rtioMoleWetCo2 <- data.frame(setQf$rtioMoleWetCo2, setQf$rtioMoleWet12CCo2, #setQf$dlta13CCo2,
                                          setQf$rtioMoleWet13CCo2, setQf$presCrdCo2, 
                                          setQf$tempCrdCo2, setQf$tempWbox,
                                          setQf$sensCrdCo2,# setQf$frt00MfcVali, 
                                          #setQf$frtMfcVali, setQf$presAtmMfcVali, 
                                          #setQf$tempMfcVali,
                                         setQf$sensMfcVali,
                                          #setQf$presValiRegInStor,
                                         idGas = idGas)
        rpt$rtioMoleWetCo2 <- na.omit(tmp$rtioMoleWetCo2[which(tmp$rtioMoleWetCo2$idGas == 105 | (is.na(tmp$rtioMoleWetCo2$idGas) & tmp$rtioMoleWetCo2$qfSensStus == -1)),])
        
        tmp$rtioMoleDryCo2<- data.frame(setQf$rtioMoleDryCo2, setQf$rtioMoleDry12CCo2, #setQf$dlta13CCo2,
                                       setQf$rtioMoleDry13CCo2, setQf$presCrdCo2, 
                                       setQf$tempCrdCo2, setQf$tempWbox,
                                       setQf$sensCrdCo2, #setQf$frt00MfcVali, 
                                       #setQf$frtMfcVali, setQf$presAtmMfcVali, 
                                       #setQf$tempMfcVali, 
                                       setQf$sensMfcVali,
                                       #setQf$presValiRegInStor, 
                                       idGas = idGas)
        rpt$rtioMoleDryCo2 <- na.omit(tmp$rtioMoleDryCo2[which(tmp$rtioMoleDryCo2$idGas == 105 | (is.na(tmp$rtioMoleDryCo2$idGas) & tmp$rtioMoleDryCo2$qfSensStus == -1)),])
        
        tmp$rtioMoleWet12CCo2 <- data.frame(setQf$rtioMoleWet12CCo2, setQf$presCrdCo2, 
                                             setQf$tempCrdCo2, setQf$tempWbox,
                                             setQf$sensCrdCo2, #setQf$frt00MfcVali, 
                                             #setQf$frtMfcVali, setQf$presAtmMfcVali, 
                                             #setQf$tempMfcVali, 
                                            setQf$sensMfcVali,
                                             #setQf$presValiRegInStor, 
                                            idGas = idGas)
        rpt$rtioMoleWet12CCo2 <- na.omit(tmp$rtioMoleWet12CCo2[which(tmp$rtioMoleWet12CCo2$idGas == 105 | (is.na(tmp$rtioMoleWet12CCo2$idGas) & tmp$rtioMoleWet12CCo2$qfSensStus == -1)),])
        
        tmp$rtioMoleDry12CCo2 <- data.frame(setQf$rtioMoleDry12CCo2, setQf$rtioMoleWet12CCo2,
                                            setQf$presCrdCo2, setQf$tempCrdCo2, 
                                            setQf$tempWbox, setQf$sensCrdCo2, 
                                            #setQf$frt00MfcVali, setQf$frtMfcVali, 
                                            #setQf$presAtmMfcVali, setQf$tempMfcVali, 
                                            setQf$sensMfcVali, #setQf$presValiRegInStor,
                                            idGas = idGas)
        rpt$rtioMoleDry12CCo2 <- na.omit(tmp$rtioMoleDry12CCo2[which(tmp$rtioMoleDry12CCo2$idGas == 105 | (is.na(tmp$rtioMoleDry12CCo2$idGas) & tmp$rtioMoleDry12CCo2$qfSensStus == -1)),])
        
        tmp$rtioMoleWet13CCo2 <- data.frame(setQf$rtioMoleWet13CCo2, setQf$presCrdCo2, 
                                            setQf$tempCrdCo2, setQf$tempWbox,
                                            setQf$sensCrdCo2, #setQf$frt00MfcVali, 
                                            #setQf$frtMfcVali, setQf$presAtmMfcVali, 
                                            #setQf$tempMfcVali,
                                            setQf$sensMfcVali,
                                            #setQf$presValiRegInStor, 
                                            idGas = idGas)
        rpt$rtioMoleWet13CCo2 <- na.omit(tmp$rtioMoleWet13CCo2[which(tmp$rtioMoleWet13CCo2$idGas == 105 | (is.na(tmp$rtioMoleWet13CCo2$idGas) & tmp$rtioMoleWet13CCo2$qfSensStus == -1)),])
        
        tmp$rtioMoleDry13CCo2 <- data.frame(setQf$rtioMoleDry13CCo2, setQf$rtioMoleWet13CCo2,
                                            setQf$presCrdCo2, setQf$tempCrdCo2, 
                                            setQf$tempWbox, setQf$sensCrdCo2, 
                                            #setQf$frt00MfcVali, setQf$frtMfcVali, 
                                            #setQf$presAtmMfcVali, setQf$tempMfcVali, 
                                            setQf$sensMfcVali, #setQf$presValiRegInStor,
                                            idGas = idGas)
        rpt$rtioMoleDry13CCo2 <- na.omit(tmp$rtioMoleDry13CCo2[which(tmp$rtioMoleDry13CCo2$idGas == 105 | (is.na(tmp$rtioMoleDry13CCo2$idGas) & tmp$rtioMoleDry13CCo2$qfSensStus == -1)),])
        
        tmp$dlta13CCo2 <- data.frame(setQf$dlta13CCo2, setQf$rtioMoleWet12CCo2,
                                     setQf$rtioMoleWet13CCo2, setQf$presCrdCo2,
                                     setQf$tempCrdCo2, setQf$tempWbox,
                                     setQf$sensCrdCo2, #setQf$frt00MfcVali, 
                                     #setQf$frtMfcVali, setQf$presAtmMfcVali, 
                                     #setQf$tempMfcVali, 
                                     setQf$sensMfcVali,
                                     #setQf$presValiRegInStor, 
                                     idGas = idGas)
        rpt$dlta13CCo2 <- na.omit(tmp$dlta13CCo2[which(tmp$dlta13CCo2$idGas == 105 | (is.na(tmp$dlta13CCo2$idGas) & tmp$dlta13CCo2$qfSensStus == -1)),])
        
        tmp$rtioMoleWetH2o <- data.frame(setQf$rtioMoleWetH2o, setQf$presCrdCo2,
                                         setQf$tempCrdCo2, setQf$tempWbox,
                                         setQf$sensCrdCo2, #setQf$frt00MfcVali, 
                                         #setQf$frtMfcVali, setQf$presAtmMfcVali, 
                                         #setQf$tempMfcVali, 
                                         setQf$sensMfcVali,
                                         #setQf$presValiRegInStor, 
                                         idGas = idGas)
        rpt$rtioMoleWetH2o <- na.omit(tmp$rtioMoleWetH2o[which(tmp$rtioMoleWetH2o$idGas == 11 | (is.na(tmp$rtioMoleWetH2o$idGas) & tmp$rtioMoleWetH2o$qfSensStus == -1)),])
        
        tmp$rtioMoleDryH2o <- data.frame(setQf$rtioMoleDryH2o, setQf$rtioMoleWetH2o, 
                                         setQf$presCrdCo2, setQf$tempCrdCo2, 
                                         setQf$tempWbox, setQf$sensCrdCo2, 
                                         #setQf$frt00MfcVali, setQf$frtMfcVali, 
                                         #setQf$presAtmMfcVali, setQf$tempMfcVali, 
                                         setQf$sensMfcVali, #setQf$presValiRegInStor,
                                         idGas = idGas)
        rpt$rtioMoleDryH2o <- na.omit(tmp$rtioMoleDryH2o[which(tmp$rtioMoleDryH2o$idGas == 11 | (is.na(tmp$rtioMoleDryH2o$idGas) & tmp$rtioMoleDryH2o$qfSensStus == -1)),])
        
        rpt$temp <- na.omit(data.frame(setQf$tempCrdCo2, setQf$sensCrdCo2))
        
        rpt$pres <- na.omit(data.frame(setQf$presCrdCo2, setQf$sensCrdCo2))
        rpt$presEnvHut <- na.omit(data.frame(setQf$presEnvHut))
        rpt$rhEnvHut <- na.omit(data.frame (setQf$rhEnvHut))
        rpt$tempEnvHut <- na.omit(data.frame (setQf$tempEnvHut))
        rpt$rtioMoleWetH2oEnvHut <- na.omit(data.frame (setQf$rtioMoleWetH2oEnvHut))
        #remove idGas column
        lapply(names(rpt), function(x){
          rpt[[x]] <<- rpt[[x]][, ! names(rpt[[x]]) %in% "idGas", drop = F]
        })
        #remove tmp
        rm(tmp)
      } else {
        #grouping qulity flags that related to isoCo2 L1 sub-data product  
        rpt$rtioMoleWetCo2 <- na.omit(data.frame(setQf$rtioMoleWetCo2, setQf$rtioMoleWet12CCo2,#setQf$dlta13CCo2,
                                                 setQf$rtioMoleWet13CCo2, setQf$presCrdCo2, 
                                                 setQf$tempCrdCo2, setQf$tempWbox,
                                                 setQf$sensCrdCo2, #setQf$frt00MfcVali, 
                                                 #setQf$frtMfcVali, setQf$presAtmMfcVali, 
                                                 #setQf$tempMfcVali,
                                                 setQf$sensMfcVali#,
                                                 #setQf$presValiRegInStor
                                                 ))
        
        rpt$rtioMoleDryCo2 <- na.omit(data.frame(setQf$rtioMoleDryCo2, setQf$rtioMoleDry12CCo2,#setQf$dlta13CCo2,
                                                 setQf$rtioMoleDry13CCo2, setQf$presCrdCo2, 
                                                 setQf$tempCrdCo2, setQf$tempWbox,
                                                 setQf$sensCrdCo2, #setQf$frt00MfcVali, 
                                                 #setQf$frtMfcVali, setQf$presAtmMfcVali, 
                                                 #setQf$tempMfcVali, 
                                                 setQf$sensMfcVali#,
                                                 #setQf$presValiRegInStor
                                                 ))
        
        rpt$rtioMoleWet12CCo2 <- na.omit(data.frame(setQf$rtioMoleWet12CCo2, setQf$presCrdCo2, 
                                                    setQf$tempCrdCo2, setQf$tempWbox,
                                                    setQf$sensCrdCo2, #setQf$frt00MfcVali, 
                                                    #setQf$frtMfcVali, setQf$presAtmMfcVali, 
                                                    #setQf$tempMfcVali, 
                                                    setQf$sensMfcVali#,
                                                    #setQf$presValiRegInStor
                                                    ))
        
        rpt$rtioMoleDry12CCo2 <- na.omit(data.frame(setQf$rtioMoleDry12CCo2, setQf$rtioMoleWet12CCo2,
                                                    setQf$presCrdCo2, setQf$tempCrdCo2, 
                                                    setQf$tempWbox, setQf$sensCrdCo2, 
                                                    #setQf$frt00MfcVali, setQf$frtMfcVali, 
                                                    #setQf$presAtmMfcVali, setQf$tempMfcVali, 
                                                    setQf$sensMfcVali#, setQf$presValiRegInStor
                                                    ))
        
        rpt$rtioMoleWet13CCo2 <- na.omit(data.frame(setQf$rtioMoleWet13CCo2, setQf$presCrdCo2, 
                                                    setQf$tempCrdCo2, setQf$tempWbox,
                                                    setQf$sensCrdCo2, #setQf$frt00MfcVali, 
                                                    #setQf$frtMfcVali, setQf$presAtmMfcVali, 
                                                    #setQf$tempMfcVali, 
                                                    setQf$sensMfcVali#,
                                                    #setQf$presValiRegInStor
                                                    ))
        
        rpt$rtioMoleDry13CCo2 <- na.omit(data.frame(setQf$rtioMoleDry13CCo2, setQf$rtioMoleWet13CCo2,
                                                    setQf$presCrdCo2, setQf$tempCrdCo2, 
                                                    setQf$tempWbox, setQf$sensCrdCo2, 
                                                    #setQf$frt00MfcVali, setQf$frtMfcVali, 
                                                    #setQf$presAtmMfcVali, setQf$tempMfcVali, 
                                                    setQf$sensMfcVali#, setQf$presValiRegInStor
                                                    ))
        
        rpt$dlta13CCo2 <- na.omit(data.frame(setQf$dlta13CCo2, setQf$rtioMoleWet12CCo2,
                                             setQf$rtioMoleWet13CCo2, setQf$presCrdCo2,
                                             setQf$tempCrdCo2, setQf$tempWbox,
                                             setQf$sensCrdCo2, #setQf$frt00MfcVali, 
                                             #setQf$frtMfcVali, setQf$presAtmMfcVali, 
                                             #setQf$tempMfcVali, 
                                             setQf$sensMfcVali#,
                                             #setQf$presValiRegInStor
                                             ))
        
        rpt$rtioMoleWetH2o <- na.omit(data.frame(setQf$rtioMoleWetH2o, setQf$presCrdCo2,
                                                 setQf$tempCrdCo2, setQf$tempWbox,
                                                 setQf$sensCrdCo2, #setQf$frt00MfcVali, 
                                                 #setQf$frtMfcVali, setQf$presAtmMfcVali, 
                                                 #setQf$tempMfcVali, 
                                                 setQf$sensMfcVali#,
                                                 #setQf$presValiRegInStor
                                                 ))
        
        rpt$rtioMoleDryH2o <- na.omit(data.frame(setQf$rtioMoleDryH2o, setQf$rtioMoleWetH2o, 
                                                 setQf$presCrdCo2, setQf$tempCrdCo2, 
                                                 setQf$tempWbox, setQf$sensCrdCo2, 
                                                 #setQf$frt00MfcVali, setQf$frtMfcVali, 
                                                 #setQf$presAtmMfcVali, setQf$tempMfcVali, 
                                                 setQf$sensMfcVali#, setQf$presValiRegInStor
                                                 ))
        
        rpt$temp <- na.omit(data.frame(setQf$tempCrdCo2, setQf$sensCrdCo2))
        
        rpt$pres <- na.omit(data.frame(setQf$presCrdCo2, setQf$sensCrdCo2))  
        rpt$presEnvHut <- na.omit(data.frame(setQf$presEnvHut))
        rpt$rhEnvHut <- na.omit(data.frame (setQf$rhEnvHut))
        rpt$tempEnvHut <- na.omit(data.frame (setQf$tempEnvHut))
        rpt$rtioMoleWetH2oEnvHut <- na.omit(data.frame (setQf$rtioMoleWetH2oEnvHut))
      }#close else statement
    }##close if statement of TypeMeas == "vali"
    #remove setQf
    setQf <- NULL
  }##close if statement of dp01 == isoCo2

#isoH2o ####################################################################################
  if (dp01 == "isoH2o") {
    #check if data are exist
    #external quality flags from envHut
    if (!("envHut" %in% names(qfInp)) || length(which(!is.na(qfInp$crdH2o$qfRngTemp))) == 0){
      qfInp$envHut <- as.data.frame(matrix(-1, ncol = 14, nrow = length(qfInp$crdH2o$qfRngRtioMoleDryH2o)))
      names(qfInp$envHut) <- c("qfRngPres", "qfStepPres", "qfPersPres", "qfRngRh", "qfStepRh", "qfPersRh",
                                 "qfRngRtioMoleWetH2o", "qfStepRtioMoleWetH2o", "qfPersRtioMoleWetH2o", 
                                 "qfRngTemp", "qfStepTemp", "qfPersTemp", "qfTemp", "qfRh")}
    #external quality flags from mfm
    if (!("mfm" %in% names(qfInp)) || length(which(!is.na(qfInp$crdH2o$qfRngTemp))) == 0){
      qfInp$mfm <- as.data.frame(matrix(-1, ncol = 13, nrow = length(qfInp$crdH2o$qfRngRtioMoleDryH2o)))
      names(qfInp$mfm) <- c("qfRngFrt00", "qfStepFrt00", "qfPersFrt00", "qfRngFrt", "qfStepFrt", "qfPersFrt",
                            "qfRngPresAtm", "qfStepPresAtm", "qfPersPresAtm", "qfRngTemp", "qfStepTemp", "qfPersTemp",
                            "qfFrt00")}
    #external quality flags from presInlt
    if (!("presInlt" %in% names(qfInp)) || length(which(!is.na(qfInp$crdH2o$qfRngTemp))) == 0){
      qfInp$presInlt <- as.data.frame(matrix(-1, ncol = 4, nrow = length(qfInp$crdH2o$qfRngRtioMoleDryH2o)))
      names(qfInp$presInlt) <- c("qfPersPresDiff", "qfPresDiff", "qfRngPresDiff", "qfStepPresDiff")}
    
    #external quality flags from pumpStor
    if (!("pumpStor" %in% names(qfInp)) || length(which(!is.na(qfInp$crdH2o$qfRngTemp))) == 0){
      qfInp$pumpStor <- as.data.frame(matrix(-1, ncol = 3, nrow = length(qfInp$crdH2o$qfRngRtioMoleDryH2o)))
      names(qfInp$pumpStor) <- c("qfPersPumpVolt", "qfRngPumpVolt", "qfStepPumpVolt")}
    
    #external quality flags from presValiRegInStor
    if (!("presValiRegInStor" %in% names(qfInp)) || length(which(!is.na(qfInp$crdH2o$qfRngTemp))) == 0){
      qfInp$presValiRegInStor <- as.data.frame(matrix(-1, ncol = 4, nrow = length(qfInp$crdH2o$qfRngRtioMoleDryH2o)))
      names(qfInp$presValiRegInStor) <- c("qfPersPresDiff", "qfPresDiff", "qfRngPresDiff", "qfStepPresDiff")}
    
    #external quality flags from presValiRegOutStor
    if (!("presValiRegOutStor" %in% names(qfInp)) || length(which(!is.na(qfInp$crdH2o$qfRngTemp))) == 0){
      qfInp$presValiRegOutStor <- as.data.frame(matrix(-1, ncol = 4, nrow = length(qfInp$crdH2o$qfRngRtioMoleDryH2o)))
      names(qfInp$presValiRegOutStor) <- c("qfPersPresDiff", "qfPresDiff", "qfRngPresDiff", "qfStepPresDiff")}
    
    #heater flag
    if (!("qfHeat" %in% names(qfInp$crdH2o))){
      qfInp$crdH2o$qfHeat <- -1}
    
    #replace -1 if all qf in crdH2o are NA
    if (length(which(!is.na(qfInp$crdH2o$qfRngTemp))) == 0){
      qfInp$crdH2o[,1:length(qfInp$crdH2o)] <- -1
    }
    
    #replace -1 if all qf in crdCH2o are NA
    qfName <- c("qfSensStus", "qfStusN2", "qfValiH2o")
    for (idx in 1:length(qfName)){
    if (length(which(!is.na(qfInp$crdH2o[[qfName[idx]]]))) == 0){
      qfInp$crdH2o[[qfName[idx]]] <- -1
    }}
    
    #setQf for crdH2o
    setQf$rtioMoleDryH2o <- data.frame("qfRngRtioMoleDryH2o" = qfInp$crdH2o$qfRngRtioMoleDryH2o, 
                                       "qfStepRtioMoleDryH2o" = qfInp$crdH2o$qfStepRtioMoleDryH2o,
                                       "qfPersRtioMoleDryH2o" = qfInp$crdH2o$qfPersRtioMoleDryH2o) 
                                       #"qfCalRtioMoleDryH2o" = qfInp$crdH2o$qfCalRtioMoleDryH2o)
    
    setQf$rtioMoleWetH2o <- data.frame("qfRngRtioMoleWetH2o" = qfInp$crdH2o$qfRngRtioMoleWetH2o, 
                                       "qfStepRtioMoleWetH2o" = qfInp$crdH2o$qfStepRtioMoleWetH2o,
                                       "qfPersRtioMoleWetH2o" = qfInp$crdH2o$qfPersRtioMoleWetH2o) 
                                       #"qfCalRtioMoleWetH2o" = qfInp$crdH2o$qfCalRtioMoleWetH2o)
    
    setQf$dlta18OH2o <- data.frame("qfRngDlta18OH2o" = qfInp$crdH2o$qfRngDlta18OH2o, 
                                   "qfStepDlta18OH2o" = qfInp$crdH2o$qfStepDlta18OH2o,
                                   "qfPersDlta18OH2o" = qfInp$crdH2o$qfPersDlta18OH2o) 
                                   #"qfCalDlta18OH2o" = qfInp$crdH2o$qfCalDlta18OH2o)
    
    setQf$dlta2HH2o <- data.frame("qfRngDlta2HH2o" = qfInp$crdH2o$qfRngDlta2HH2o, 
                                  "qfStepDlta2HH2o" = qfInp$crdH2o$qfStepDlta2HH2o,
                                  "qfPersDlta2HH2o" = qfInp$crdH2o$qfPersDlta2HH2o) 
                                  #"qfCalDlta2HH2o" = qfInp$crdH2o$qfCalDlta2HH2o)
    
    setQf$presCrdH2o <- data.frame("qfRngPres" = qfInp$crdH2o$qfRngPres, 
                                   "qfStepPres" = qfInp$crdH2o$qfStepPres,
                                   "qfPersPres" = qfInp$crdH2o$qfPersPres) 
                                   #"qfCalPres" = qfInp$crdH2o$qfCalPres)
    
    setQf$tempCrdH2o <- data.frame("qfRngTemp" = qfInp$crdH2o$qfRngTemp, 
                                   "qfStepTemp" = qfInp$crdH2o$qfStepTemp,
                                   "qfPersTemp" = qfInp$crdH2o$qfPersTemp) 
                                   #"qfCalTemp" = qfInp$crdH2o$qfCalTemp)
    
    setQf$tempWbox <- data.frame("qfRngTempWbox" = qfInp$crdH2o$qfRngTempWbox, 
                                 "qfStepTempWbox" = qfInp$crdH2o$qfStepTempWbox,
                                 "qfPersTempWbox" = qfInp$crdH2o$qfPersTempWbox) 
                                 #"qfCalTempWbox" = qfInp$crdH2o$qfCalTempWbox)
    
    setQf$sensCrdH2o <- data.frame("qfSensStus" = qfInp$crdH2o$qfSensStus,
                                   "qfStusN2" = qfInp$crdH2o$qfStusN2)
    setQf$valiCrdH2o <- data.frame("qfValiH2o" = qfInp$crdH2o$qfValiH2o)
    
    #change column names
    names(setQf$rtioMoleDryH2o) <- paste0(colnames(setQf$rtioMoleDryH2o), "CrdH2o")
    names(setQf$rtioMoleWetH2o) <- paste0(colnames(setQf$rtioMoleWetH2o), "CrdH2o")
    names(setQf$dlta18OH2o) <- paste0(colnames(setQf$dlta18OH2o), "CrdH2o")
    names(setQf$dlta2HH2o) <- paste0(colnames(setQf$dlta2HH2o), "CrdH2o")
    names(setQf$presCrdH2o) <- paste0(colnames(setQf$presCrdH2o), "CrdH2o")
    names(setQf$tempCrdH2o) <- paste0(colnames(setQf$tempCrdH2o), "CrdH2o")
    names(setQf$tempWbox) <- paste0(colnames(setQf$tempWbox), "CrdH2o")
    names(setQf$sensCrdH2o) <- paste0(colnames(setQf$sensCrdH2o), "CrdH2o")
    names(setQf$valiCrdH2o) <- paste0(colnames(setQf$valiCrdH2o), "CrdH2o")
    
    #setQf of envHut
    setQf$envHut <- data.frame("qfRh" = qfInp$envHut$qfRh)
    setQf$presEnvHut <- data.frame("qfRngPres" = qfInp$envHut$qfRngPres, 
                                   "qfStepPres" = qfInp$envHut$qfStepPres,
                                   "qfPersPres" = qfInp$envHut$qfPersPres)
    
    setQf$rhEnvHut <- data.frame("qfRngRh" = qfInp$envHut$qfRngRh, 
                                 "qfStepRh" = qfInp$envHut$qfStepRh,
                                 "qfPersRh" = qfInp$envHut$qfPersRh)
    
    setQf$rtioMoleWetH2oEnvHut <- data.frame("qfRngRtioMoleWetH2o" = qfInp$envHut$qfRngRtioMoleWetH2o, 
                                             "qfStepRtioMoleWetH2o" = qfInp$envHut$qfStepRtioMoleWetH2o,
                                             "qfPersRtioMoleWetH2o" = qfInp$envHut$qfPersRtioMoleWetH2o)
    
    setQf$tempEnvHut <- data.frame("qfRngTemp" = qfInp$envHut$qfRngTemp, 
                                   "qfStepTemp" = qfInp$envHut$qfStepTemp,
                                   "qfPersTemp" = qfInp$envHut$qfPersTemp)
    #change column names
    names(setQf$envHut) <- paste0(colnames(setQf$envHut), "EnvHut")
    names(setQf$presEnvHut) <- paste0(colnames(setQf$presEnvHut), "EnvHut")
    names(setQf$rhEnvHut) <- paste0(colnames(setQf$rhEnvHut), "EnvHut")
    names(setQf$rtioMoleWetH2oEnvHut) <- paste0(colnames(setQf$rtioMoleWetH2oEnvHut), "EnvHut")
    names(setQf$tempEnvHut) <- paste0(colnames(setQf$tempEnvHut), "EnvHut")
    
    #external quality flags from mfm
    setQf$frt00Mfm<- data.frame("qfRngFrt00" = qfInp$mfm$qfRngFrt00,
                                "qfStepFrt00" = qfInp$mfm$qfStepFrt00,
                                "qfPersFrt00" = qfInp$mfm$qfPersFrt00)
    
    setQf$frtMfm <- data.frame("qfRngFrt" = qfInp$mfm$qfRngFrt,
                               "qfStepFrt" = qfInp$mfm$qfStepFrt,
                               "qfPersFrt" = qfInp$mfm$qfPersFrt)
    
    setQf$presAtmMfm <- data.frame("qfRngPresAtm" = qfInp$mfm$qfRngPresAtm,
                                   "qfStepPresAtm" = qfInp$mfm$qfStepPresAtm,
                                   "qfPersPresAtm" = qfInp$mfm$qfPersPresAtm)
    
    setQf$tempMfm<- data.frame("qfRngTemp" = qfInp$mfm$qfRngTemp,
                               "qfStepTemp" = qfInp$mfm$qfStepTemp,
                               "qfPersTemp" = qfInp$mfm$qfPersTemp)
    
    setQf$sensMfm <- data.frame("qfFrt00" = qfInp$mfm$qfFrt00)
    
    #change column names
    names(setQf$frt00Mfm) <- paste0(colnames(setQf$frt00Mfm), "Mfm")
    names(setQf$frtMfm) <- paste0(colnames(setQf$frtMfm), "Mfm")
    names(setQf$presAtmMfm) <- paste0(colnames(setQf$presAtmMfm), "Mfm")
    names(setQf$tempMfm) <- paste0(colnames(setQf$tempMfm), "Mfm")
    names(setQf$sensMfm) <- paste0(colnames(setQf$sensMfm), "Mfm")
    
    #external quality flags from presInlt
    setQf$presInlt <- data.frame("qfPresDiff" = qfInp$presInlt$qfPresDiff)
    #change column names
    names(setQf$presInlt) <- paste0(colnames(setQf$presInlt), "PresInlt")
    
    #external quality flags from pumpStor
    setQf$pumpStor <- data.frame("qfPersPumpVolt" = qfInp$pumpStor$qfPersPumpVolt,
                                 "qfRngPumpVolt" = qfInp$pumpStor$qfRngPumpVolt,
                                 "qfStepPumpVolt" = qfInp$pumpStor$qfStepPumpVolt)
    #change column names
    names(setQf$pumpStor) <- paste0(colnames(setQf$pumpStor), "PumpStor")
    
    #external quality flags from presValiRegInStor
    setQf$presValiRegInStor <- data.frame("qfPresDiff" = qfInp$presValiRegInStor$qfPresDiff)
    #change column names
    names(setQf$presValiRegInStor) <- paste0(colnames(setQf$presValiRegInStor), "PresValiRegInStor")
    
    #external quality flags from presValiRegOutStor
    setQf$presValiRegOutStor <- data.frame("qfPresDiff" = qfInp$presValiRegOutStor$qfPresDiff)
    #change column names
    names(setQf$presValiRegOutStor) <- paste0(colnames(setQf$presValiRegOutStor), "PresValiRegOutStor")
    
    #heater flag
    setQf$heatInlt <- data.frame("qfHeat" = qfInp$crdH2o$qfHeat)
    
    #define qf which use only sampling period
    if (TypeMeas == "samp") {     
      #grouping qulity flags that related to isopH2o L1 sub-data product  
      rpt$rtioMoleDryH2o <- na.omit(data.frame(setQf$rtioMoleDryH2o, setQf$rtioMoleWetH2o,
                                               setQf$presCrdH2o, setQf$tempCrdH2o,
                                               setQf$tempWbox,  setQf$sensCrdH2o,
                                               #setQf$envHut, setQf$frt00Mfm, 
                                               #setQf$frtMfm, setQf$presAtmMfm, 
                                               #setQf$tempMfm, 
                                               setQf$sensMfm#,
                                               #setQf$presInlt, setQf$pumpStor
                                               #setQf$heatInlt
                                               ))
      
      rpt$rtioMoleWetH2o <- na.omit(data.frame(setQf$rtioMoleWetH2o, setQf$presCrdH2o, 
                                               setQf$tempCrdH2o, setQf$tempWbox,  
                                               setQf$sensCrdH2o, #setQf$envHut,
                                               #setQf$frt00Mfm, setQf$frtMfm, 
                                               #setQf$presAtmMfm, setQf$tempMfm,
                                               setQf$sensMfm#, setQf$presInlt,
                                               #setQf$pumpStor
                                               #setQf$heatInlt
                                               ))
      
      rpt$dlta18OH2o <- na.omit(data.frame(setQf$dlta18OH2o, setQf$presCrdH2o, 
                                           setQf$tempCrdH2o, setQf$tempWbox,  
                                           setQf$sensCrdH2o, #setQf$envHut,
                                           #setQf$frt00Mfm, setQf$frtMfm, 
                                           #setQf$presAtmMfm, setQf$tempMfm,
                                           setQf$sensMfm#, setQf$presInlt,
                                           #setQf$pumpStor
                                           #setQf$heatInlt
                                           ))
      
      rpt$dlta2HH2o <- na.omit(data.frame(setQf$dlta2HH2o, setQf$presCrdH2o, 
                                          setQf$tempCrdH2o, setQf$tempWbox,  
                                          setQf$sensCrdH2o, #setQf$envHut,
                                          #setQf$frt00Mfm, setQf$frtMfm, 
                                          #setQf$presAtmMfm, setQf$tempMfm,
                                          setQf$sensMfm#, setQf$presInlt,
                                          #setQf$pumpStor
                                          #setQf$heatInlt
                                          ))
      
      rpt$pres <- na.omit(data.frame(setQf$presCrdH2o, setQf$sensCrdH2o$qfSensStus))
      rpt$temp <- na.omit(data.frame(setQf$tempCrdH2o, setQf$sensCrdH2o$qfSensStus)) 
      rpt$presEnvHut <- na.omit(data.frame(setQf$presEnvHut))
      rpt$rhEnvHut <- na.omit(data.frame (setQf$rhEnvHut))
      rpt$tempEnvHut <- na.omit(data.frame (setQf$tempEnvHut))
      rpt$rtioMoleWetH2oEnvHut <- na.omit(data.frame (setQf$rtioMoleWetH2oEnvHut))
    }#close if statement of TypeMeas == "samp"
    
    #define qf which use only validation period
    if (TypeMeas == "vali") {     
      #grouping qulity flags that related to isopH2o L1 sub-data product  
      rpt$rtioMoleDryH2o <- na.omit(data.frame(setQf$rtioMoleDryH2o, setQf$rtioMoleWetH2o,
                                               setQf$presCrdH2o, setQf$tempCrdH2o,
                                               setQf$tempWbox,  setQf$sensCrdH2o,
                                               #setQf$envHut, 
                                               setQf$valiCrdH2o#,
                                               #setQf$presValiRegInStor, setQf$presValiRegOutStor
                                               ))
      
      rpt$rtioMoleWetH2o <- na.omit(data.frame(setQf$rtioMoleWetH2o, setQf$presCrdH2o, 
                                               setQf$tempCrdH2o, setQf$tempWbox,  
                                               setQf$sensCrdH2o, #setQf$envHut,
                                               setQf$valiCrdH2o#, setQf$presValiRegInStor, 
                                               #setQf$presValiRegOutStor
                                               ))
      
      rpt$dlta18OH2o <- na.omit(data.frame(setQf$dlta18OH2o, setQf$presCrdH2o, 
                                           setQf$tempCrdH2o, setQf$tempWbox,  
                                           setQf$sensCrdH2o, #setQf$envHut,
                                           setQf$valiCrdH2o#, setQf$presValiRegInStor, 
                                           #setQf$presValiRegOutStor
                                           ))
      
      rpt$dlta2HH2o <- na.omit(data.frame(setQf$dlta2HH2o, setQf$presCrdH2o, 
                                          setQf$tempCrdH2o, setQf$tempWbox,  
                                          setQf$sensCrdH2o, #setQf$envHut,
                                          setQf$valiCrdH2o#, setQf$presValiRegInStor, 
                                          #setQf$presValiRegOutStor
                                          ))
      
      rpt$pres <- na.omit(data.frame(setQf$presCrdH2o, setQf$sensCrdH2o$qfSensStus, setQf$valiCrdH2o))
      rpt$temp <- na.omit(data.frame(setQf$tempCrdH2o, setQf$sensCrdH2o$qfSensStus, setQf$valiCrdH2o))  
      rpt$presEnvHut <- na.omit(data.frame(setQf$presEnvHut))
      rpt$rhEnvHut <- na.omit(data.frame (setQf$rhEnvHut))
      rpt$tempEnvHut <- na.omit(data.frame (setQf$tempEnvHut))
      rpt$rtioMoleWetH2oEnvHut <- na.omit(data.frame (setQf$rtioMoleWetH2oEnvHut))
    }#close if statement of TypeMeas == "vali"
    
    #remove setQf
    setQf <- NULL
}##close if statement of dp01 == "isoH2o" 

#envHut ####################################################################################
  if (dp01 == "envHut") {
    if (TypeMeas %in% c("samp", "vali")) {
    setQf$tempEnvHut <- data.frame("qfRngTemp" = qfInp$envHut$qfRngTemp, 
                                   "qfStepTemp" = qfInp$envHut$qfStepTemp,
                                   "qfPersTemp" = qfInp$envHut$qfPersTemp)
    
    setQf$rh <- data.frame("qfRngRh" = qfInp$envHut$qfRngRh, 
                           "qfStepRh" = qfInp$envHut$qfStepRh,
                           "qfPersRh" = qfInp$envHut$qfPersRh)
    
    setQf$presEnvHut <- data.frame("qfRngPres" = qfInp$envHut$qfRngPres, 
                                   "qfStepPres" = qfInp$envHut$qfStepPres,
                                   "qfPersPres" = qfInp$envHut$qfPersPres)
    
    setQf$rtioMoleWetH2o <- data.frame("qfRngRtioMoleWetH2o" = qfInp$envHut$qfRngRtioMoleWetH2o, 
                                       "qfStepRtioMoleWetH2o" = qfInp$envHut$qfStepRtioMoleWetH2o,
                                       "qfPersRtioMoleWetH2o" = qfInp$envHut$qfPersRtioMoleWetH2o)
    
    #change column names
    names(setQf$tempEnvHut) <- paste0(colnames(setQf$tempEnvHut), "EnvHut")
    names(setQf$rh) <- paste0(colnames(setQf$rh), "EnvHut")
    names(setQf$presEnvHut) <- paste0(colnames(setQf$presEnvHut), "EnvHut")
    names(setQf$rtioMoleWetH2o) <- paste0(colnames(setQf$rtioMoleWetH2o), "EnvHut")
    
    #grouping qulity flags that related to envHut L1 sub-data product
    rpt$temp <- data.frame(setQf$tempEnvHut)
    
    rpt$rh <- data.frame(setQf$rh)
    
    rpt$pres <- data.frame(setQf$presEnvHut)
    
    rpt$rtioMoleWetH2o <- data.frame(setQf$rtioMoleWetH2o)
    
    #remove setQf
    setQf <- NULL
    }#close if statement of TypeMeas %in% c("samp", "vali")
  }#close if statement of dp01 == "envHut"

#tempAirLvl ####################################################################################
  if (dp01 == "tempAirLvl") {
    if (TypeMeas %in% c("samp", "vali")) {
    setQf$tempAirLvl <- data.frame("qfRngTemp" = qfInp$tempAirLvl$qfRngTemp, 
                                   "qfStepTemp" = qfInp$tempAirLvl$qfStepTemp,
                                   "qfPersTemp" = qfInp$tempAirLvl$qfPersTemp,
                                   "qfCalTemp" = qfInp$tempAirLvl$qfCalTemp)
    
    setQf$sensTempAirLvl <- data.frame("qfHeat" = qfInp$tempAirLvl$qfHeat,
                                       "qfFlow" = qfInp$tempAirLvl$qfFlow)
    
    #grouping qulity flags that related to tempAirLvl L1 sub-data product
    rpt$temp <- data.frame(setQf$tempAirLvl, setQf$sensTempAirLvl)
    
    #remove setQf
    setQf <- NULL
    }#close if statement of TypeMeas %in% c("samp", "vali")
  }#close if statement of dp01 == tempAirLvl
  
#tempAirTop ####################################################################################
  if (dp01 == "tempAirTop") {
    if (TypeMeas %in% c("samp", "vali")) {
    setQf$temp01 <- data.frame("qfRngTemp01" = qfInp$tempAirTop$qfRngTemp01,
                               "qfStepTemp01" = qfInp$tempAirTop$qfStepTemp01,
                               "qfPersTemp01" = qfInp$tempAirTop$qfPersTemp01,
                               "qfCalTemp01" = qfInp$tempAirTop$qfCalTemp01)
    
    setQf$temp02 <- data.frame("qfRngTemp02" = qfInp$tempAirTop$qfRngTemp02, 
                               "qfStepTemp02" = qfInp$tempAirTop$qfStepTemp02,
                               "qfPersTemp02" = qfInp$tempAirTop$qfPersTemp02,
                               "qfCalTemp02" = qfInp$tempAirTop$qfCalTemp02)
    
    setQf$temp03 <- data.frame("qfRngTemp03" = qfInp$tempAirTop$qfRngTemp03, 
                               "qfStepTemp03" = qfInp$tempAirTop$qfStepTemp03,
                               "qfPersTemp03" = qfInp$tempAirTop$qfPersTemp03,
                               "qfCalTemp03" = qfInp$tempAirTop$qfCalTemp03)
    
    setQf$sensTempAirTop <- data.frame("qfHeat" = qfInp$tempAirTop$qfHeat,
                                       "qfFlow" = qfInp$tempAirTop$qfFlow)
    
    #grouping qulity flags that related to tempAirLvl L1 sub-data product
    rpt$temp <- data.frame(setQf$temp01, setQf$temp02,
                           setQf$temp03, setQf$sensTempAirTop)
    
    #remove setQf
    setQf <- NULL
    }#close if statement of TypeMeas %in% c("samp", "vali")
  }#close if statement of dp01 == tempAirLvl
}# closed if statement of MethMeas == "ecse"

#return values
return(rpt)
  
#end function def.dp01.grp.qf()
}
