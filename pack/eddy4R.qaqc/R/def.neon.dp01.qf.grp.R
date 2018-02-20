##############################################################################################
#' @title Definition function: Grouping the quality flags for each of NEON ECTE and ECSE L1 data product

#' @author
#' Natchaya Pingintha-Durden \email{ndurden@battelleecology.org}

#' @description Function definition. Grouping the quality flags of each NEON ECTE and ECSE L1 data product into a single dataframe for further use in the calculation of Alpha, Beta, and Final flag.

#' @param \code{qfInput} A list of data frame containing the input quality flag data that related to L1 data products are being grouped. Of class integer". [-] 
#' @param \code{MethMeas} A vector of class "character" containing the name of measurement method (eddy-covariance turbulent exchange or storage exchange), MethMeas = c("ecte", "ecse"). Defaults to "ecse". [-] 
#' @param \code{TypeMeas} A vector of class "character" containing the name of measurement type (sampling or validation), TypeMeas = c("samp", "ecse"). Defaults to "samp". [-]
#' @param \code{dp01} A vector of class "character" containing the name of NEON ECTE and ECSE L1 data products which the flags are being grouped, \cr
#' c("envHut", "co2Turb", "h2oTurb", "isoCo2", "isoH2o", "soni", "amrs", "tempAirLvl", "tempAirTop"). Defaults to "co2Turb". [-] 
#' @param \code{idGas} A data frame contianing gas ID for isoCo2 measurement. Need to provide when dp01 = "isoCo2". Default to NULL. [-]

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
#' qfGrpCo2Turb <- eddy4R.qaqc::def.neon.dp01.qf.grp(qfInput = qf, MethMeas = "ecte", TypeMeas = "vali", dp01="co2Turb")
#' qfGrpSoni <- eddy4R.qaqc::def.neon.dp01.qf.grp(qfInput = qf, MethMeas = "ecte", TypeMeas = "samp", dp01="soni")

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
##############################################################################################

def.neon.dp01.qf.grp <- function(
  qfInput = list(),
  MethMeas = c("ecte", "ecse")[1],
  TypeMeas = c("samp", "vali")[1], 
  dp01 = c("envHut", "co2Turb", "h2oTurb", "co2Stor", "h2oStor", "isoCo2", "isoH2o", "soni", "amrs", "tempAirLvl", "tempAirTop")[1],
  idGas =NULL
){
  #check existing input list
    if (dp01 %in% c("co2Turb", "h2oTurb")) {
      if (!("irgaTurb" %in% names(qfInput))) { 
        base::stop("Missing irgaTurb quality flags")
      }
    }# close if statement of dp01
  
    if (dp01 %in% c("co2Stor", "h2oStor")) {
      if (!("irgaStor" %in% names(qfInput))) { 
        base::stop("Missing irgaStor quality flags")
      }
    }# close if statement of dp01
  
    if (dp01 %in% c("soni")) {
      if (!("soni" %in% names(qfInput))){
        base::stop("Missing soni quality flags")
      }
    }# close if statement of dp01
  
    if (dp01 %in% c("amrs")) {
      if (!("amrs" %in% names(qfInput))){
        base::stop("Missing amrs quality flags")
      }
    }# close if statement of dp01
  
    if (dp01 %in% c("isoCo2")) {
      if (!("crdCo2" %in% names(qfInput))){
        base::stop("Missing crdCo2 quality flags")
      }
    }# close if statement of dp01
  
    if (dp01 %in% c("isoH2o")) {
      if (!("crdH2o" %in% names(qfInput))){
        base::stop("Missing crdH2o quality flags")
      }
    }# close if statement of dp01
  
    if (dp01 %in% c("envHut")) {
      if (!("envHut" %in% names(qfInput))){
        base::stop("Missing envHut quality flags")
      }
    }# close if statement of dp01
  
    if (dp01 %in% c("tempAirLvl")) {
      if (!("tempAirLvl" %in% names(qfInput))){
        base::stop("Missing tempAirLvl quality flags")
      }
    }# close if statement of dp01
  
    if (dp01 %in% c("tempAirTop")) {
      if (!("tempAirTop" %in% names(qfInput))){
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
    setQf$sensIrgaTurb <- data.frame("qfIrgaTurbHead" = qfInput$irgaTurb$qfIrgaTurbHead, 
                             "qfIrgaTurbTempOut" = qfInput$irgaTurb$qfIrgaTurbTempOut, 
                             "qfIrgaTurbTempIn" = qfInput$irgaTurb$qfIrgaTurbTempIn,
                             "qfIrgaTurbAux" = qfInput$irgaTurb$qfIrgaTurbAux, 
                             "qfIrgaTurbPres" = qfInput$irgaTurb$qfIrgaTurbPres, 
                             "qfIrgaTurbChop" = qfInput$irgaTurb$qfIrgaTurbChop, 
                             "qfIrgaTurbDetc" = qfInput$irgaTurb$qfIrgaTurbDetc, 
                             "qfIrgaTurbPll" = qfInput$irgaTurb$qfIrgaTurbPll, 
                             "qfIrgaTurbSync" = qfInput$irgaTurb$qfIrgaTurbSync,
                             "qfIrgaTurbAgc" = qfInput$irgaTurb$qfIrgaTurbAgc)
    
    setQf$sensIrgaTurbExt <- data.frame("qfIrgaTurbVali" = qfInput$irgaTurb$qfIrgaTurbVali)
    
    setQf$tempIn <- data.frame("qfRngTempIn" = qfInput$irgaTurb$qfRngTempIn,
                               "qfStepTempIn" = qfInput$irgaTurb$qfStepTempIn,
                               "qfPersTempIn" = qfInput$irgaTurb$qfPersTempIn)
                               #"qfCalTempIn" = qfInput$irgaTurb$qfCalTempIn)
    
    setQf$tempOut <- data.frame("qfRngTempOut" = qfInput$irgaTurb$qfRngTempOut,
                                "qfStepTempOut" = qfInput$irgaTurb$qfStepTempOut,
                                "qfPersTempOut" = qfInput$irgaTurb$qfPersTempOut)
                                #"qfCalTempOut" = qfInput$irgaTurb$qfCalTempOut)
    
    setQf$tempAve <- data.frame ("qfRngTempMean" = qfInput$irgaTurb$qfRngTempMean, 
                                 "qfStepTempMean" = qfInput$irgaTurb$qfStepTempMean,
                                 "qfPersTempMean" = qfInput$irgaTurb$qfPersTempMean) 
                                 #"qfCalTempMean" = qfInput$irgaTurb$qfCalTempMean)
    
    setQf$presAtmIrgaTurb <- data.frame("qfRngPresAtm" = qfInput$irgaTurb$qfRngPresAtm, 
                                "qfStepPresAtm" = qfInput$irgaTurb$qfStepPresAtm,
                                "qfPersPresAtm" = qfInput$irgaTurb$qfPersPresAtm) 
                                #"qfCalPresAtm" = qfInput$irgaTurb$qfCalPresAtm)
    
    setQf$presDiffIrgaTurb <- data.frame("qfRngPresDiff" = qfInput$irgaTurb$qfRngPresDiff,
                                     "qfStepPresDiff" = qfInput$irgaTurb$qfStepPresDiff,
                                     "qfPersPresDiff" = qfInput$irgaTurb$qfPersPresDiff)
                                     #"qfCalPresDiff" = qfInput$irgaTurb$qfCalPresDiff) 
    
    setQf$presSum <- data.frame("qfRngPresSum" = qfInput$irgaTurb$qfRngPresSum,
                                "qfStepPresSum" = qfInput$irgaTurb$qfStepPresSum,
                                "qfPersPresSum" = qfInput$irgaTurb$qfPersPresSum)
                                #"qfCalPresSum" = qfInput$irgaTurb$qfCalPresSum)
    
    setQf$powrH2oSamp <- data.frame ("qfRngPowrH2oSamp" = qfInput$irgaTurb$qfRngPowrH2oSamp,
                                     "qfStepPowrH2oSamp" = qfInput$irgaTurb$qfStepPowrH2oSamp,
                                     "qfPersPowrH2oSamp" = qfInput$irgaTurb$qfPersPowrH2oSamp)
                                     #"qfCalPowrH2oSamp" = qfInput$irgaTurb$qfCalPowrH2oSamp)
    
    setQf$powrH2oRefe <- data.frame ("qfRngPowrH2oRefe" = qfInput$irgaTurb$qfRngPowrH2oRefe,
                                     "qfStepPowrH2oRefe" = qfInput$irgaTurb$qfStepPowrH2oRefe,
                                     "qfPersPowrH2oRefe" = qfInput$irgaTurb$qfPersPowrH2oRefe)
                                     #"qfCalPowrH2oRefe" = qfInput$irgaTurb$qfCalPowrH2oRefe)
    
    setQf$asrpH2o <- data.frame("qfRngAsrpH2o" = qfInput$irgaTurb$qfRngAsrpH2o, 
                                "qfStepAsrpH2o" = qfInput$irgaTurb$qfStepAsrpH2o, 
                                "qfPersAsrpH2o" = qfInput$irgaTurb$qfPersAsrpH2o) 
                                #"qfCalAsrpH2o" = qfInput$irgaTurb$qfCalAsrpH2o)
    
    setQf$densMoleH2o <- data.frame("qfRngDensMoleH2o" = qfInput$irgaTurb$qfRngDensMoleH2o, 
                                    "qfStepDensMoleH2o" = qfInput$irgaTurb$qfStepDensMoleH2o, 
                                    "qfPersDensMoleH2o" = qfInput$irgaTurb$qfPersDensMoleH2o)
                                    #"qfCalDensMoleH2o" = qfInput$irgaTurb$qfCalDensMoleH2o)
    
    setQf$rtioMoleDryH2o <- data.frame("qfRngRtioMoleDryH2o" = qfInput$irgaTurb$qfRngRtioMoleDryH2o,
                                       "qfStepRtioMoleDryH2o" = qfInput$irgaTurb$qfStepRtioMoleDryH2o,
                                       "qfPersRtioMoleDryH2o" = qfInput$irgaTurb$qfPersRtioMoleDryH2o)
                                       #"qfCalRtioMoleDryH2o" = qfInput$irgaTurb$qfCalRtioMoleDryH2o)
    
    setQf$powrCo2Samp <- data.frame("qfRngPowrCo2Samp" = qfInput$irgaTurb$qfRngPowrCo2Samp,
                                    "qfStepPowrCo2Samp" = qfInput$irgaTurb$qfStepPowrCo2Samp,
                                    "qfPersPowrCo2Samp" = qfInput$irgaTurb$qfPersPowrCo2Samp)
                                    #"qfCalPowrCo2Samp" = qfInput$irgaTurb$qfCalPowrCo2Samp)
    
    setQf$powrCo2Refe <- data.frame ("qfRngPowrCo2Refe" = qfInput$irgaTurb$qfRngPowrCo2Refe,
                                     "qfStepPowrCo2Refe" = qfInput$irgaTurb$qfStepPowrCo2Refe,
                                     "qfPersPowrCo2Refe" = qfInput$irgaTurb$qfPersPowrCo2Refe)
                                     #"qfCalPowrCo2Refe" = qfInput$irgaTurb$qfCalPowrCo2Refe)
    
    setQf$asrpCo2 <- data.frame("qfRngAsrpCo2" = qfInput$irgaTurb$qfRngAsrpCo2, 
                                "qfStepAsrpCo2" = qfInput$irgaTurb$qfStepAsrpCo2, 
                                "qfPersAsrpCo2" = qfInput$irgaTurb$qfPersAsrpCo2) 
                                #"qfCalAsrpCo2" = qfInput$irgaTurb$qfCalAsrpCo2)
    
    setQf$densMoleCo2 <- data.frame("qfRngDensMoleCo2" = qfInput$irgaTurb$qfRngDensMoleCo2,
                                    "qfStepDensMoleCo2" = qfInput$irgaTurb$qfStepDensMoleCo2,
                                    "qfPersDensMoleCo2" = qfInput$irgaTurb$qfPersDensMoleCo2) 
                                    #"qfCalDensMoleCo2" = qfInput$irgaTurb$qfCalDensMoleCo2) 
    
    setQf$rtioMoleDryCo2 <- data.frame("qfRngRtioMoleDryCo2" = qfInput$irgaTurb$qfRngRtioMoleDryCo2,
                                       "qfStepRtioMoleDryCo2" = qfInput$irgaTurb$qfStepRtioMoleDryCo2,
                                       "qfPersRtioMoleDryCo2" = qfInput$irgaTurb$qfPersRtioMoleDryCo2)
                                      # "qfCalRtioMoleDryCo2" = qfInput$irgaTurb$qfCalRtioMoleDryCo2)
    
    setQf$ssiCo2 <- data.frame("qfRngSsiCo2" = qfInput$irgaTurb$qfRngSsiCo2, 
                               "qfStepSsiCo2" = qfInput$irgaTurb$qfStepSsiCo2, 
                               "qfPersSsiCo2" = qfInput$irgaTurb$qfPersSsiCo2)
                               #"qfCalSsiCo2" = qfInput$irgaTurb$qfCalSsiCo2)
    
    setQf$ssiH2o <- data.frame("qfRngSsiH2o" = qfInput$irgaTurb$qfRngSsiH2o, 
                               "qfStepSsiH2o" = qfInput$irgaTurb$qfStepSsiH2o, 
                               "qfPersSsiH2o" = qfInput$irgaTurb$qfPersSsiH2o) 
                               #"qfCalSsiH2o" = qfInput$irgaTurb$qfCalSsiH2o)
    
    #external quality flags from mfcSampTurb
    if ("mfcSampTurb" %in% names(qfInput)){
      #mfcSampTurb
      setQf$frt00MfcSampTurb <- data.frame("qfRngFrt00" = qfInput$mfcSampTurb$qfRngFrt00) 
                                #,"qfPersFrt00" = qfInput$mfcSampTurb$qfPersFrt00)
      
      setQf$frtMfcSampTurb <- data.frame("qfRngFrt" = qfInput$mfcSampTurb$qfRngFrt,
                              "qfPersFrt" = qfInput$mfcSampTurb$qfPersFrt)
      
      setQf$presAtmMfcSampTurb <- data.frame("qfRngPresAtm" = qfInput$mfcSampTurb$qfRngPresAtm, 
                                     "qfStepPresAtm" = qfInput$mfcSampTurb$qfStepPresAtm,
                                     "qfPersPresAtm" = qfInput$mfcSampTurb$qfPersPresAtm)
      
      setQf$tempMfcSampTurb <- data.frame("qfRngTemp" = qfInput$mfcSampTurb$qfRngTemp,
                                  "qfStepTemp" = qfInput$mfcSampTurb$qfStepTemp,
                                  "qfPersTemp" = qfInput$mfcSampTurb$qfPersTemp)
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
    if ("mfcValiTurb" %in% names(qfInput)){
      #mfcValiTurb
      setQf$frt00MfcValiTurb <- data.frame("qfRngFrt00" = qfInput$mfcValiTurb$qfRngFrt00) 
                                # "qfPersFrt00" = qfInput$mfcValiTurb$qfPersFrt00)
      
      setQf$frtMfcValiTurb <- data.frame("qfRngFrt" = qfInput$mfcValiTurb$qfRngFrt,
                              "qfPersFrt" = qfInput$mfcValiTurb$qfPersFrt)
      
      setQf$presAtmMfcValiTurb <- data.frame("qfRngPresAtm" = qfInput$mfcValiTurb$qfRngPresAtm, 
                                     "qfStepPresAtm" = qfInput$mfcValiTurb$qfStepPresAtm,
                                     "qfPersPresAtm" = qfInput$mfcValiTurb$qfPersPresAtm)
      
      setQf$tempMfcValiTurb <- data.frame("qfRngTemp" = qfInput$mfcValiTurb$qfRngTemp,
                                  "qfStepTemp" = qfInput$mfcValiTurb$qfStepTemp,
                                  "qfPersTemp" = qfInput$mfcValiTurb$qfPersTemp)
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
    if ("soni" %in% names(qfInput)){
      setQf$soni <- data.frame(#"qfCalVeloSoni" = qfInput$soni$qfCalVeloSoni,
                               "qfPersVeloSoni" = qfInput$soni$qfPersVeloSoni,
                               "qfRngVeloSoni" = qfInput$soni$qfRngVeloSoni,
                               "qfStepVeloSoni" = qfInput$soni$qfStepVeloSoni,
                               #"qfCalTempSoni" = qfInput$soni$qfCalTempSoni,
                               "qfPersTempSoni" = qfInput$soni$qfPersTempSoni,
                               "qfRngTempSoni" = qfInput$soni$qfRngTempSoni,
                               "qfStepTempSoni" = qfInput$soni$qfStepTempSoni,
                               "qfSoniUnrs" = qfInput$soni$qfSoniUnrs, 
                               "qfSoniData" = qfInput$soni$qfSoniData,
                               "qfSoniTrig" = qfInput$soni$qfSoniTrig, 
                               "qfSoniComm" = qfInput$soni$qfSoniComm,
                               "qfSoniCode" = qfInput$soni$qfSoniCode, 
                               "qfSoniTemp" = qfInput$soni$qfSoniTemp,
                               "qfSoniSgnlPoor" = qfInput$soni$qfSoniSgnlPoor, 
                               "qfSoniSgnlHigh" = qfInput$soni$qfSoniSgnlHigh,
                               "qfSoniSgnlLow" = qfInput$soni$qfSoniSgnlLow
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
                                 "qfPersVeloXaxs" = qfInput$soni$qfPersVeloXaxs) 
                                 #"qfCalVeloXaxs" = qfInput$soni$qfCalVeloXaxs)
    #qf for cross-axis wind speed
    setQf$veloYaxs <- data.frame("qfRngVeloYaxs" = qfInput$soni$qfRngVeloYaxs, 
                                 "qfStepVeloYaxs" = qfInput$soni$qfStepVeloYaxs, 
                                 "qfPersVeloYaxs" = qfInput$soni$qfPersVeloYaxs) 
                                 #"qfCalVeloYaxs" = qfInput$soni$qfCalVeloYaxs)
    #qf for vertical-axis wind speed
    setQf$veloZaxs <- data.frame("qfRngVeloZaxs" = qfInput$soni$qfRngVeloZaxs, 
                                 "qfStepVeloZaxs" = qfInput$soni$qfStepVeloZaxs, 
                                 "qfPersVeloZaxs" = qfInput$soni$qfPersVeloZaxs) 
                                 #"qfCalVeloZaxs" = qfInput$soni$qfCalVeloZaxs)
    #qf for sonic velocity
    setQf$veloSoni <- data.frame("qfRngVeloSoni" = qfInput$soni$qfRngVeloSoni, 
                                 "qfStepVeloSoni" = qfInput$soni$qfStepVeloSoni, 
                                 "qfPersVeloSoni" = qfInput$soni$qfPersVeloSoni) 
                                 #"qfCalVeloSoni" = qfInput$soni$qfCalVeloSoni)
    #qf for soic temperature
    setQf$tempSoni <- data.frame("qfRngTempSoni" = qfInput$soni$qfRngTempSoni, 
                                 "qfStepTempSoni" = qfInput$soni$qfStepTempSoni, 
                                 "qfPersTempSoni" = qfInput$soni$qfPersTempSoni) 
                                 #"qfCalTempSoni" = qfInput$soni$qfCalTempSoni)
    
    #external quality flags from irgaTurb for grouping qf of tempAir
    if ("irgaTurb" %in% names(qfInput)){
      setQf$irgaTurb <- data.frame("qfIrgaTurbHead" = qfInput$irgaTurb$qfIrgaTurbHead,
                               "qfIrgaTurbTempOut" = qfInput$irgaTurb$qfIrgaTurbTempOut, 
                               "qfIrgaTurbTempIn" = qfInput$irgaTurb$qfIrgaTurbTempIn,
                               "qfIrgaTurbAux" = qfInput$irgaTurb$qfIrgaTurbAux, 
                               "qfIrgaTurbPres" = qfInput$irgaTurb$qfIrgaTurbPres,
                               "qfIrgaTurbChop" = qfInput$irgaTurb$qfIrgaTurbChop, 
                               "qfIrgaTurbDetc" = qfInput$irgaTurb$qfIrgaTurbDetc,
                               "qfIrgaTurbPll" = qfInput$irgaTurb$qfIrgaTurbPll, 
                               "qfIrgaTurbSync" = qfInput$irgaTurb$qfIrgaTurbSync,
                               "qfIrgaTurbAgc" = qfInput$irgaTurb$qfIrgaTurbAgc,
                               "qfIrgaTurbVali" = qfInput$irgaTurb$qfIrgaTurbVali,
                               "qfRngTempMean" = qfInput$irgaTurb$qfRngTempMean, 
                               "qfStepTempMean" = qfInput$irgaTurb$qfStepTempMean,
                               "qfPersTempMean" = qfInput$irgaTurb$qfPersTempMean, 
                               #"qfCalTempMean" = qfInput$irgaTurb$qfCalTempMean,
                               "qfRngPresDiff" = qfInput$irgaTurb$qfRngPresDiff,
                               "qfStepPresDiff" = qfInput$irgaTurb$qfStepPresDiff,
                               "qfPersPresDiff" = qfInput$irgaTurb$qfPersPresDiff,
                               #"qfCalPresDiff" = qfInput$irgaTurb$qfCalPresDiff,
                               "qfRngPowrH2oSamp" = qfInput$irgaTurb$qfRngPowrH2oSamp,
                               "qfStepPowrH2oSamp" = qfInput$irgaTurb$qfStepPowrH2oSamp,
                               "qfPersPowrH2oSamp" = qfInput$irgaTurb$qfPersPowrH2oSamp,
                               #"qfCalPowrH2oSamp" = qfInput$irgaTurb$qfCalPowrH2oSamp,
                               "qfRngPowrH2oRefe" = qfInput$irgaTurb$qfRngPowrH2oRefe,
                               "qfStepPowrH2oRefe" = qfInput$irgaTurb$qfStepPowrH2oRefe,
                               "qfPersPowrH2oRefe" = qfInput$irgaTurb$qfPersPowrH2oRefe,
                               #"qfCalPowrH2oRefe" = qfInput$irgaTurb$qfCalPowrH2oRefe,
                               "qfRngAsrpH2o" = qfInput$irgaTurb$qfRngAsrpH2o, 
                               "qfStepAsrpH2o" = qfInput$irgaTurb$qfStepAsrpH2o, 
                               "qfPersAsrpH2o" = qfInput$irgaTurb$qfPersAsrpH2o, 
                               #"qfCalAsrpH2o" = qfInput$irgaTurb$qfCalAsrpH2o,
                               "qfRngDensMoleH2o" = qfInput$irgaTurb$qfRngDensMoleH2o, 
                               "qfStepDensMoleH2o" = qfInput$irgaTurb$qfStepDensMoleH2o, 
                               "qfPersDensMoleH2o" = qfInput$irgaTurb$qfPersDensMoleH2o, 
                               #"qfCalDensMoleH2o" = qfInput$irgaTurb$qfCalDensMoleH2o,
                               "qfRngSsiH2o" = qfInput$irgaTurb$qfRngSsiH2o, 
                               "qfStepSsiH2o" = qfInput$irgaTurb$qfStepSsiH2o, 
                               "qfPersSsiH2o" = qfInput$irgaTurb$qfPersSsiH2o) 
                               #"qfCalSsiH2o" = qfInput$irgaTurb$qfCalSsiH2o)
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
    # if ("amrs" %in% names(qfInput)){
    #   #subset only odd row to match with soni frequency at 20Hz
    #   qfColNames <- colnames(qfInput$amrs)
    #   qfInput$amrs <- data.frame(qfInput$amrs[seq(1,nrow(qfInput$amrs),2),])
    #   colnames(qfInput$amrs) <- qfColNames
    #   
    #   setQf$amrs <- data.frame("qfAmrsVal" = qfInput$amrs$qfAmrsVal,
    #                                "qfAmrsFilt" = qfInput$amrs$qfAmrsFilt,
    #                                "qfAmrsVelo" = qfInput$amrs$qfAmrsVelo,
    #                                "qfAmrsRng" = qfInput$amrs$qfAmrsRng,
    #                                "qfRngAngXaxs" = qfInput$amrs$qfRngAngXaxs,
    #                                "qfStepAngXaxs" = qfInput$amrs$qfStepAngXaxs,
    #                                "qfPersAngXaxs" = qfInput$amrs$qfPersAngXaxs,
    #                                "qfRngAngYaxs" = qfInput$amrs$qfRngAngYaxs,
    #                                "qfStepAngYaxs" = qfInput$amrs$qfStepAngYaxs,
    #                                "qfPersAngYaxs" = qfInput$amrs$qfPersAngYaxs,
    #                                "qfRngAngZaxs" = qfInput$amrs$qfRngAngZaxs,
    #                                "qfStepAngZaxs" = qfInput$amrs$qfStepAngZaxs,
    #                                "qfPersAngZaxs" = qfInput$amrs$qfPersAngZaxs)
    #   
    #   setQf$accXaxsDiff <- data.frame("qfRngAccXaxsDiff" = qfInput$amrs$qfRngAccXaxsDiff,
    #                                   "qfStepAccXaxsDiff" = qfInput$amrs$qfStepAccXaxsDiff,
    #                                   "qfPersAccXaxsDiff" = qfInput$amrs$qfPersAccXaxsDiff)
    #   
    #   setQf$accYaxsDiff <- data.frame("qfRngAccYaxsDiff" = qfInput$amrs$qfRngAccYaxsDiff,
    #                                   "qfStepAccYaxsDiff" = qfInput$amrs$qfStepAccYaxsDiff,
    #                                   "qfPersAccYaxsDiff" = qfInput$amrs$qfPersAccYaxsDiff)
    #   
    #   setQf$accZaxsDiff <- data.frame("qfRngAccZaxsDiff" = qfInput$amrs$qfRngAccZaxsDiff,
    #                                   "qfStepAccZaxsDiff" = qfInput$amrs$qfStepAccZaxsDiff,
    #                                   "qfPersAccZaxsDiff" = qfInput$amrs$qfPersAccZaxsDiff)
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
      setQf$sensAmrs <- data.frame("qfAmrsVal" = qfInput$amrs$qfAmrsVal,
                                       "qfAmrsFilt" = qfInput$amrs$qfAmrsFilt,
                                       "qfAmrsVelo" = qfInput$amrs$qfAmrsVelo,
                                       "qfAmrsRng" = qfInput$amrs$qfAmrsRng)
      
      setQf$accXaxs <- data.frame("qfRngAccXaxs" = qfInput$amrs$qfRngAccXaxs,
                                  "qfStepAccXaxs" = qfInput$amrs$qfStepAccXaxs,
                                  "qfPersAccXaxs" = qfInput$amrs$qfPersAccXaxs)
      
      setQf$accYaxs <- data.frame("qfRngAccYaxs" = qfInput$amrs$qfRngAccYaxs,
                                  "qfStepAccYaxs" = qfInput$amrs$qfStepAccYaxs,
                                  "qfPersAccYaxs" = qfInput$amrs$qfPersAccYaxs)
      
      setQf$accZaxs <- data.frame("qfRngAccZaxs" = qfInput$amrs$qfRngAccZaxs,
                                  "qfStepAccZaxs" = qfInput$amrs$qfStepAccZaxs,
                                  "qfPersAccZaxs" = qfInput$amrs$qfPersAccZaxs)
      
      setQf$accXaxsDiff <- data.frame("qfRngAccXaxsDiff" = qfInput$amrs$qfRngAccXaxsDiff,
                                      "qfStepAccXaxsDiff" = qfInput$amrs$qfStepAccXaxsDiff,
                                      "qfPersAccXaxsDiff" = qfInput$amrs$qfPersAccXaxsDiff)
      
      setQf$accYaxsDiff <- data.frame("qfRngAccYaxsDiff" = qfInput$amrs$qfRngAccYaxsDiff,
                                      "qfStepAccYaxsDiff" = qfInput$amrs$qfStepAccYaxsDiff,
                                      "qfPersAccYaxsDiff" = qfInput$amrs$qfPersAccYaxsDiff)
      
      setQf$accZaxsDiff <- data.frame("qfRngAccZaxsDiff" = qfInput$amrs$qfRngAccZaxsDiff,
                                      "qfStepAccZaxsDiff" = qfInput$amrs$qfStepAccZaxsDiff,
                                      "qfPersAccZaxsDiff" = qfInput$amrs$qfPersAccZaxsDiff)
      
      setQf$avelXaxs <- data.frame("qfRngAvelXaxs" = qfInput$amrs$qfRngAvelXaxs,
                                   "qfStepAvelXaxs" = qfInput$amrs$qfStepAvelXaxs,
                                   "qfPersAvelXaxs" = qfInput$amrs$qfPersAvelXaxs)
      
      setQf$avelYaxs <- data.frame("qfRngAvelYaxs" = qfInput$amrs$qfRngAvelYaxs,
                                   "qfStepAvelYaxs" = qfInput$amrs$qfStepAvelYaxs,
                                   "qfPersAvelYaxs" = qfInput$amrs$qfPersAvelYaxs)
      
      setQf$avelZaxs <- data.frame("qfRngAvelZaxs" = qfInput$amrs$qfRngAvelZaxs,
                                   "qfStepAvelZaxs" = qfInput$amrs$qfStepAvelZaxs,
                                   "qfPersAvelZaxs" = qfInput$amrs$qfPersAvelZaxs)
      
      setQf$angXaxs <- data.frame("qfRngAngXaxs" = qfInput$amrs$qfRngAngXaxs,
                                  "qfStepAngXaxs" = qfInput$amrs$qfStepAngXaxs,
                                  "qfPersAngXaxs" = qfInput$amrs$qfPersAngXaxs)
      
      setQf$angYaxs <- data.frame("qfRngAngYaxs" = qfInput$amrs$qfRngAngYaxs,
                                  "qfStepAngYaxs" = qfInput$amrs$qfStepAngYaxs,
                                  "qfPersAngYaxs" = qfInput$amrs$qfPersAngYaxs)
      
      setQf$angZaxs <- data.frame("qfRngAngZaxs" = qfInput$amrs$qfRngAngZaxs,
                                  "qfStepAngZaxs" = qfInput$amrs$qfStepAngZaxs,
                                  "qfPersAngZaxs" = qfInput$amrs$qfPersAngZaxs)
    
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
    if (!("envHut" %in% names(qfInput)) || length(which(!is.na(qfInput$irgaStor$qfRngTemp))) == 0){
      qfInput$envHut <- as.data.frame(matrix(-1, ncol = 14, nrow = length(qfInput$irgaStor$qfRngAsrpCo2)))
      names(qfInput$envHut) <- c("qfRngPres", "qfStepPres", "qfPersPres", "qfRngRh", "qfStepRh", "qfPersRh",
                                 "qfRngRtioMoleWetH2o", "qfStepRtioMoleWetH2o", "qfPersRtioMoleWetH2o", 
                                 "qfRngTemp", "qfStepTemp", "qfPersTemp", "qfTemp", "qfRh")}
    #external quality flags from valvAux
    if (!("valvAux" %in% names(qfInput)) || length(which(!is.na(qfInput$irgaStor$qfRngTemp))) == 0){
      qfInput$valvAux <- as.data.frame(matrix(-1, ncol = 1, nrow = length(qfInput$irgaStor$qfRngAsrpCo2)))
      names(qfInput$valvAux) <- "qfValvIrga"}
    # #external quality flags from heatInlt
    # if (!("heatInlt" %in% names(qfInput)) || length(which(!is.na(qfInput$irgaStor$qfRngTemp))) == 0){
    #   qfInput$heatInlt <- as.data.frame(matrix(-1, ncol = 1, nrow = length(qfInput$irgaStor$qfRngAsrpCo2)))
    #   names(qfInput$heatInlt) <- "qfHeat"}
    #external quality flags from mfcSampStor
    if (!("mfcSampStor" %in% names(qfInput)) || length(which(!is.na(qfInput$irgaStor$qfRngTemp))) == 0){
      qfInput$mfcSampStor <- as.data.frame(matrix(-1, ncol = 12, nrow = length(qfInput$irgaStor$qfRngAsrpCo2)))
      names(qfInput$mfcSampStor) <- c("qfRngFrt00", "qfStepFrt00", "qfRngFrt", "qfStepFrt", "qfPersFrt",
                                      "qfRngPresAtm", "qfStepPresAtm", "qfPersPresAtm", "qfRngTemp", "qfStepTemp", "qfPersTemp",
                                      "qfFrt00")}
    #external quality flags from mfcValiStor
    if (!("mfcValiStor" %in% names(qfInput)) || length(which(!is.na(qfInput$irgaStor$qfRngTemp))) == 0){
      qfInput$mfcValiStor <- as.data.frame(matrix(-1, ncol = 12, nrow = length(qfInput$irgaStor$qfRngAsrpCo2)))
      names(qfInput$mfcValiStor) <- c("qfRngFrt00", "qfStepFrt00", "qfRngFrt", "qfStepFrt", "qfPersFrt",
                                  "qfRngPresAtm", "qfStepPresAtm", "qfPersPresAtm", "qfRngTemp", "qfStepTemp", "qfPersTemp",
                                  "qfFrt00")}
    #replace -1 if all qf in irga are NA
    if (length(which(!is.na(qfInput$irgaStor$qfRngTemp))) == 0){
      qfInput$irgaStor[,1:length(qfInput$irgaStor)] <- -1
    }
    
    #grouping the flags
    
    setQf$asrpCo2 <- data.frame("qfRngAsrpCo2" = qfInput$irgaStor$qfRngAsrpCo2, 
                                "qfStepAsrpCo2" = qfInput$irgaStor$qfStepAsrpCo2, 
                                "qfPersAsrpCo2" = qfInput$irgaStor$qfPersAsrpCo2, 
                                "qfCalAsrpCo2" = qfInput$irgaStor$qfCalAsrpCo2)
    
    setQf$asrpH2o <- data.frame("qfRngAsrpH2o" = qfInput$irgaStor$qfRngAsrpH2o, 
                                "qfStepAsrpH2o" = qfInput$irgaStor$qfStepAsrpH2o, 
                                "qfPersAsrpH2o" = qfInput$irgaStor$qfPersAsrpH2o, 
                                "qfCalAsrpH2o" = qfInput$irgaStor$qfCalAsrpH2o)
    
    setQf$rtioMoleDryCo2 <- data.frame("qfRngRtioMoleDryCo2" = qfInput$irgaStor$qfRngRtioMoleDryCo2, 
                                       "qfStepRtioMoleDryCo2" = qfInput$irgaStor$qfStepRtioMoleDryCo2,
                                       "qfPersRtioMoleDryCo2" = qfInput$irgaStor$qfPersRtioMoleDryCo2, 
                                       "qfCalRtioMoleDryCo2" = qfInput$irgaStor$qfCalRtioMoleDryCo2)
    
    setQf$rtioMoleDryH2o <- data.frame("qfRngRtioMoleDryH2o" = qfInput$irgaStor$qfRngRtioMoleDryH2o, 
                                       "qfStepRtioMoleDryH2o" = qfInput$irgaStor$qfStepRtioMoleDryH2o,
                                       "qfPersRtioMoleDryH2o" = qfInput$irgaStor$qfPersRtioMoleDryH2o, 
                                       "qfCalRtioMoleDryH2o" = qfInput$irgaStor$qfCalRtioMoleDryH2o)
    
    setQf$rtioMoleWetCo2 <- data.frame("qfRngRtioMoleWetCo2" = qfInput$irgaStor$qfRngRtioMoleWetCo2,
                                       "qfStepRtioMoleWetCo2" = qfInput$irgaStor$qfStepRtioMoleWetCo2,
                                       "qfPersRtioMoleWetCo2" = qfInput$irgaStor$qfPersRtioMoleWetCo2,
                                       "qfCalRtioMoleWetCo2" = qfInput$irgaStor$qfCalRtioMoleWetCo2)
    
    setQf$rtioMoleWetH2o <- data.frame("qfRngRtioMoleWetH2o" = qfInput$irgaStor$qfRngRtioMoleWetH2o,
                                       "qfStepRtioMoleWetH2o" = qfInput$irgaStor$qfStepRtioMoleWetH2o,
                                       "qfPersRtioMoleWetH2o" = qfInput$irgaStor$qfPersRtioMoleWetH2o,
                                       "qfCalRtioMoleWetH2o" = qfInput$irgaStor$qfCalRtioMoleWetH2o)
    
    setQf$presIrga <- data.frame("qfRngPres" = qfInput$irgaStor$qfRngPres, 
                                 "qfStepPres" = qfInput$irgaStor$qfStepPres,
                                 "qfPersPres" = qfInput$irgaStor$qfPersPres, 
                                 "qfCalPres" = qfInput$irgaStor$qfCalPres)
    
    setQf$tempIrga <- data.frame ("qfRngTemp" = qfInput$irgaStor$qfRngTemp, 
                                  "qfStepTemp" = qfInput$irgaStor$qfStepTemp,
                                  "qfPersTemp" = qfInput$irgaStor$qfPersTemp, 
                                  "qfCalTemp" = qfInput$irgaStor$qfCalTemp)
    #external quality flags from envHut
    setQf$envHut <- data.frame("qfTemp" = qfInput$envHut$qfTemp)
    setQf$presEnvHut <- data.frame("qfRngPres" = qfInput$envHut$qfRngPres, 
                                   "qfStepPres" = qfInput$envHut$qfStepPres,
                                   "qfPersPres" = qfInput$envHut$qfPersPres)
    
    setQf$rhEnvHut <- data.frame("qfRngRh" = qfInput$envHut$qfRngRh, 
                                 "qfStepRh" = qfInput$envHut$qfStepRh,
                                 "qfPersRh" = qfInput$envHut$qfPersRh)
    
    setQf$rtioMoleWetH2oEnvHut <- data.frame("qfRngRtioMoleWetH2o" = qfInput$envHut$qfRngRtioMoleWetH2o, 
                                             "qfStepRtioMoleWetH2o" = qfInput$envHut$qfStepRtioMoleWetH2o,
                                             "qfPersRtioMoleWetH2o" = qfInput$envHut$qfPersRtioMoleWetH2o)
    
    setQf$tempEnvHut <- data.frame("qfRngTemp" = qfInput$envHut$qfRngTemp, 
                                   "qfStepTemp" = qfInput$envHut$qfStepTemp,
                                   "qfPersTemp" = qfInput$envHut$qfPersTemp)
    
    #external quality flags from valvAux
    setQf$valvAux <- data.frame("qfValvIrga" = qfInput$valvAux$qfValvIrga)
    ##external quality flags from heatInlt
    # setQf$heatInlt <- data.frame("qfHeat" = qfInput$heatInlt$qfHeat)
    #external quality flags from mfcSampStor
    setQf$frt00MfcSampTurb <- data.frame("qfRngFrt00" = qfInput$mfcSampStor$qfRngFrt00,
                                         "qfStepFrt00" = qfInput$mfcSampStor$qfStepFrt00)
    
    setQf$frtMfcSampTurb <- data.frame("qfRngFrt" = qfInput$mfcSampStor$qfRngFrt,
                                       "qfStepFrt" = qfInput$mfcSampStor$qfStepFrt,
                                       "qfPersFrt" = qfInput$mfcSampStor$qfPersFrt)
    
    setQf$presAtmMfcSampTurb <- data.frame("qfRngPresAtm" = qfInput$mfcSampStor$qfRngPresAtm,
                                           "qfStepPresAtm" = qfInput$mfcSampStor$qfStepPresAtm,
                                           "qfPersPresAtm" = qfInput$mfcSampStor$qfPersPresAtm)
    
    setQf$tempMfcSampTurb <- data.frame("qfRngTemp" = qfInput$mfcSampStor$qfRngTemp,
                                        "qfStepTemp" = qfInput$mfcSampStor$qfStepTemp,
                                        "qfPersTemp" = qfInput$mfcSampStor$qfPersTemp)
    
    setQf$sensMfcSampTurb <- data.frame("qfFrt00" = qfInput$mfcSampStor$qfFrt00)
    
    #external quality flags from mfcValiStor
    setQf$frt00MfcVali <- data.frame("qfRngFrt00" = qfInput$mfcValiStor$qfRngFrt00,
                                     "qfStepFrt00" = qfInput$mfcValiStor$qfStepFrt00)
    
    setQf$frtMfcVali <- data.frame("qfRngFrt" = qfInput$mfcValiStor$qfRngFrt,
                                   "qfStepFrt" = qfInput$mfcValiStor$qfStepFrt,
                                   "qfPersFrt" = qfInput$mfcValiStor$qfPersFrt)
    
    setQf$presAtmMfcVali <- data.frame("qfRngPresAtm" = qfInput$mfcValiStor$qfRngPresAtm,
                                       "qfStepPresAtm" = qfInput$mfcValiStor$qfStepPresAtm,
                                       "qfPersPresAtm" = qfInput$mfcValiStor$qfPersPresAtm)
    
    setQf$tempMfcVali <- data.frame("qfRngTemp" = qfInput$mfcValiStor$qfRngTemp,
                                    "qfStepTemp" = qfInput$mfcValiStor$qfStepTemp,
                                    "qfPersTemp" = qfInput$mfcValiStor$qfPersTemp)
    
    setQf$sensMfcVali <- data.frame("qfFrt00" = qfInput$mfcValiStor$qfFrt00)
    
    
    #grouping qulity flags that related to co2Stor L1 sub-data product
    if (dp01 == "co2Stor"){
      if (TypeMeas == "samp"){
        rpt$rtioMoleDryCo2 <- na.omit(data.frame(setQf$rtioMoleDryCo2, setQf$asrpCo2,
                                                 setQf$asrpH2o, setQf$rtioMoleWetCo2,
                                                 setQf$rtioMoleWetH2o, setQf$presIrga,
                                                 setQf$tempIrga, setQf$envHut, 
                                                 setQf$valvAux, 
                                                 #setQf$heatInlt,
                                                 setQf$frt00MfcSampTurb, setQf$frtMfcSampTurb, 
                                                 setQf$presAtmMfcSampTurb, setQf$tempMfcSampTurb,
                                                 setQf$sensMfcSampTurb))
        
        rpt$rtioMoleWetCo2 <- na.omit(data.frame(setQf$rtioMoleWetCo2, setQf$asrpCo2,
                                                 setQf$asrpH2o, setQf$rtioMoleWetH2o, 
                                                 setQf$presIrga, setQf$tempIrga, 
                                                 setQf$envHut, setQf$valvAux, 
                                                 #setQf$heatInlt, 
                                                 setQf$frt00MfcSampTurb, 
                                                 setQf$frtMfcSampTurb, setQf$presAtmMfcSampTurb, 
                                                 setQf$tempMfcSampTurb, setQf$sensMfcSampTurb))
      }#close if statement of TypeMeas == "samp"
      
      if (TypeMeas == "vali"){
        rpt$rtioMoleDryCo2 <- na.omit(data.frame(setQf$rtioMoleDryCo2, setQf$asrpCo2,
                                                 setQf$asrpH2o, setQf$rtioMoleWetCo2,
                                                 setQf$rtioMoleWetH2o, setQf$presIrga,
                                                 setQf$tempIrga, setQf$envHut, 
                                                 setQf$valvAux, setQf$frt00MfcSampTurb, 
                                                 setQf$frtMfcSampTurb, setQf$presAtmMfcSampTurb, 
                                                 setQf$tempMfcSampTurb, setQf$sensMfcSampTurb,
                                                 setQf$frt00MfcVali, setQf$frtMfcVali, 
                                                 setQf$presAtmMfcVali, setQf$tempMfcVali, 
                                                 setQf$sensMfcVali))
        
        rpt$rtioMoleWetCo2 <- na.omit(data.frame(setQf$rtioMoleWetCo2, setQf$asrpCo2,
                                                 setQf$asrpH2o, setQf$rtioMoleWetH2o, 
                                                 setQf$presIrga, setQf$tempIrga, 
                                                 setQf$envHut, setQf$valvAux, 
                                                 setQf$frt00MfcSampTurb, setQf$frtMfcSampTurb, 
                                                 setQf$presAtmMfcSampTurb, setQf$tempMfcSampTurb, 
                                                 setQf$sensMfcSampTurb,setQf$frt00MfcVali, 
                                                 setQf$frtMfcVali, setQf$presAtmMfcVali, 
                                                 setQf$tempMfcVali, setQf$sensMfcVali))
      }#close if statement of TypeMeas == "vali"
      
      rpt$pres <- na.omit(data.frame(setQf$presIrga))
      rpt$frt00 <- na.omit(data.frame (setQf$frt00MfcSampTurb, setQf$frtMfcSampTurb,
                                       setQf$presAtmMfcSampTurb, setQf$tempMfcSampTurb,
                                       setQf$sensMfcSampTurb))
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
                                                 setQf$tempIrga, setQf$envHut, 
                                                 setQf$valvAux, 
                                                 #setQf$heatInlt, 
                                                 setQf$frt00MfcSampTurb, setQf$frtMfcSampTurb, 
                                                 setQf$presAtmMfcSampTurb, setQf$tempMfcSampTurb, 
                                                 setQf$sensMfcSampTurb))
        
        rpt$rtioMoleWetH2o <- na.omit(data.frame(setQf$rtioMoleWetH2o, setQf$asrpH2o,
                                                 setQf$presIrga, setQf$tempIrga,
                                                 setQf$envHut, setQf$valvAux, 
                                                 #setQf$heatInlt, 
                                                 setQf$frt00MfcSampTurb, 
                                                 setQf$frtMfcSampTurb, setQf$presAtmMfcSampTurb, 
                                                 setQf$tempMfcSampTurb, setQf$sensMfcSampTurb))
      }#close if statement of TypeMeas == "samp"
      
      if (TypeMeas == "vali"){
        rpt$rtioMoleDryH2o <- na.omit(data.frame(setQf$rtioMoleDryH2o, setQf$asrpH2o,
                                                 setQf$rtioMoleWetH2o, setQf$presIrga,
                                                 setQf$tempIrga, setQf$envHut, 
                                                 setQf$valvAux, setQf$frt00MfcSampTurb, 
                                                 setQf$frtMfcSampTurb, setQf$presAtmMfcSampTurb, 
                                                 setQf$tempMfcSampTurb, setQf$sensMfcSampTurb,
                                                 setQf$frt00MfcVali, setQf$frtMfcVali, 
                                                 setQf$presAtmMfcVali, setQf$tempMfcVali, 
                                                 setQf$sensMfcVali))
        
        rpt$rtioMoleWetH2o <- na.omit(data.frame(setQf$rtioMoleWetH2o, setQf$asrpH2o,
                                                 setQf$presIrga, setQf$tempIrga, 
                                                 setQf$envHut, setQf$valvAux, 
                                                 setQf$frt00MfcSampTurb, setQf$frtMfcSampTurb, 
                                                 setQf$presAtmMfcSampTurb, setQf$tempMfcSampTurb, 
                                                 setQf$sensMfcSampTurb,setQf$frt00MfcVali, 
                                                 setQf$frtMfcVali, setQf$presAtmMfcVali, 
                                                 setQf$tempMfcVali, setQf$sensMfcVali))
        
      }#close if statement of TypeMeas == "vali"
      rpt$pres <- na.omit(data.frame(setQf$presIrga))
      rpt$frt00 <- na.omit(data.frame(setQf$frt00MfcSampTurb, setQf$frtMfcSampTurb, 
                                      setQf$presAtmMfcSampTurb, setQf$tempMfcSampTurb, 
                                      setQf$sensMfcSampTurb)) 
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
    if (!("envHut" %in% names(qfInput)) || length(which(!is.na(qfInput$crdCo2$qfRngTemp))) == 0){
      qfInput$envHut <- as.data.frame(matrix(-1, ncol = 14, nrow = length(qfInput$crdCo2$qfRngRtioMoleDryCo2)))
      names(qfInput$envHut) <- c("qfRngPres", "qfStepPres", "qfPersPres", "qfRngRh", "qfStepRh", "qfPersRh",
                                 "qfRngRtioMoleWetH2o", "qfStepRtioMoleWetH2o", "qfPersRtioMoleWetH2o", 
                                 "qfRngTemp", "qfStepTemp", "qfPersTemp", "qfTemp", "qfRh")}
    # #external quality flags from heatInlt
    # if (!("heatInlt" %in% names(qfInput)) || length(which(!is.na(qfInput$crdCo2$qfRngTemp))) == 0){
    #   qfInput$heatInlt <- as.data.frame(matrix(-1, ncol = 1, nrow = length(qfInput$crdCo2$qfRngRtioMoleDryCo2)))
    #   names(qfInput$heatInlt) <- "qfHeat"}
    
    #external quality flags from mfcValiStor
    if (!("mfcValiStor" %in% names(qfInput)) || length(which(!is.na(qfInput$crdCo2$qfRngTemp))) == 0){
      qfInput$mfcValiStor <- as.data.frame(matrix(-1, ncol = 12, nrow = length(qfInput$crdCo2$qfRngRtioMoleDryCo2)))
      names(qfInput$mfcValiStor) <- c("qfRngFrt00", "qfStepFrt00", "qfRngFrt", "qfStepFrt", "qfPersFrt",
                                  "qfRngPresAtm", "qfStepPresAtm", "qfPersPresAtm", "qfRngTemp", "qfStepTemp", "qfPersTemp",
                                  "qfFrt00")}
    #replace -1 if all qf in crdCo2 are NA
    if (length(which(!is.na(qfInput$crdCo2$qfRngTemp))) == 0){
      qfInput$crdCo2[,1:length(qfInput$crdCo2)] <- -1
    }
    #replace -1 if all qfSens in crdCo2 are NA
    if (length(which(!is.na(qfInput$crdCo2$qfSensStus))) == 0){
      qfInput$crdCo2$qfSensStus <- -1
    }
    #setQf for crdCo2
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
    
    #external quality flags from envHut
    setQf$presEnvHut <- data.frame("qfRngPres" = qfInput$envHut$qfRngPres, 
                                   "qfStepPres" = qfInput$envHut$qfStepPres,
                                   "qfPersPres" = qfInput$envHut$qfPersPres)
    
    setQf$rhEnvHut <- data.frame("qfRngRh" = qfInput$envHut$qfRngRh, 
                                 "qfStepRh" = qfInput$envHut$qfStepRh,
                                 "qfPersRh" = qfInput$envHut$qfPersRh)
    
    setQf$rtioMoleWetH2oEnvHut <- data.frame("qfRngRtioMoleWetH2o" = qfInput$envHut$qfRngRtioMoleWetH2o, 
                                             "qfStepRtioMoleWetH2o" = qfInput$envHut$qfStepRtioMoleWetH2o,
                                             "qfPersRtioMoleWetH2o" = qfInput$envHut$qfPersRtioMoleWetH2o)
    
    setQf$tempEnvHut <- data.frame("qfRngTemp" = qfInput$envHut$qfRngTemp, 
                                   "qfStepTemp" = qfInput$envHut$qfStepTemp,
                                   "qfPersTemp" = qfInput$envHut$qfPersTemp)
    # #setQf from heatInlt
    # setQf$heatInlt <- data.frame("qfHeat" = qfInput$heatInlt$qfHeat)
    
    #setQf from mfcValiStor
    setQf$frt00MfcVali <- data.frame("qfRngFrt00" = qfInput$mfcValiStor$qfRngFrt00, 
                                     "qfStepFrt00" = qfInput$mfcValiStor$qfStepFrt00)
    
    setQf$frtMfcVali <- data.frame("qfRngFrt" = qfInput$mfcValiStor$qfRngFrt,
                                   "qfStepFrt" = qfInput$mfcValiStor$qfStepFrt,
                                   "qfPersFrt" = qfInput$mfcValiStor$qfPersFrt)
    
    setQf$presAtmMfcVali <- data.frame("qfRngPresAtm" = qfInput$mfcValiStor$qfRngPresAtm, 
                                       "qfStepPresAtm" = qfInput$mfcValiStor$qfStepPresAtm,
                                       "qfPersPresAtm" = qfInput$mfcValiStor$qfPersPresAtm)
    
    setQf$tempMfcVali <- data.frame("qfRngTemp" = qfInput$mfcValiStor$qfRngTemp,
                                    "qfStepTemp" = qfInput$mfcValiStor$qfStepTemp,
                                    "qfPersTemp" = qfInput$mfcValiStor$qfPersTemp)
    
    setQf$sensMfcVali <- data.frame("qfFrt00" = qfInput$mfcValiStor$qfFrt00)
    
    
    #define qf which use only sampling period
    if (TypeMeas == "samp") {
      #if not all idGas = NA or all qfRngTemp = NA
      if (length(which(!is.na(qfInput$crdCo2$qfRngTemp))) == 0){
        #grouping qulity flags that related to isoCo2 L1 sub-data product  
        rpt$rtioMoleWetCo2 <- na.omit(data.frame(setQf$rtioMoleWetCo2, setQf$dlta13CCo2,
                                                 setQf$rtioMoleWet12CCo2, setQf$presCrdCo2, 
                                                 setQf$tempCrdCo2, setQf$tempWbox,
                                                 setQf$sensCrdCo2
                                                 #, setQf$heatInlt
                                                 )[which(idGas == 105 | (is.na(idGas) & setQf$sensCrdCo2$qfSensStus == -1)),])
        
        rpt$rtioMoleDryCo2 <- na.omit(data.frame(setQf$rtioMoleDryCo2, setQf$dlta13CCo2,
                                                 setQf$rtioMoleWet12CCo2, setQf$presCrdCo2, 
                                                 setQf$tempCrdCo2, setQf$tempWbox,
                                                 setQf$sensCrdCo2
                                                 #, setQf$heatInlt
                                                 )[which(idGas == 105 | (is.na(idGas) & setQf$sensCrdCo2$qfSensStus == -1)),])
        
        rpt$rtioMoleWet12CCo2 <- na.omit(data.frame(setQf$rtioMoleWet12CCo2, setQf$presCrdCo2, 
                                                    setQf$tempCrdCo2, setQf$tempWbox,
                                                    setQf$sensCrdCo2
                                                    #, setQf$heatInlt
                                                    )[which(idGas == 105 | (is.na(idGas) & setQf$sensCrdCo2$qfSensStus == -1)),])
        
        rpt$rtioMoleDry12CCo2 <- na.omit(data.frame(setQf$rtioMoleDry12CCo2, setQf$rtioMoleWet13CCo2,
                                                    setQf$presCrdCo2, setQf$tempCrdCo2, 
                                                    setQf$tempWbox, setQf$sensCrdCo2 
                                                    #, setQf$heatInlt
                                                    )[which(idGas == 105 | (is.na(idGas) & setQf$sensCrdCo2$qfSensStus == -1)),])
        
        rpt$rtioMoleWet13CCo2 <- na.omit(data.frame(setQf$rtioMoleWet13CCo2, setQf$presCrdCo2, 
                                                    setQf$tempCrdCo2, setQf$tempWbox,
                                                    setQf$sensCrdCo2
                                                    #, setQf$heatInlt
                                                    )[which(idGas == 105 | (is.na(idGas) & setQf$sensCrdCo2$qfSensStus == -1)),])
        
        rpt$rtioMoleDry13CCo2 <- na.omit(data.frame(setQf$rtioMoleDry13CCo2, setQf$rtioMoleWet13CCo2,
                                                    setQf$presCrdCo2, setQf$tempCrdCo2, 
                                                    setQf$tempWbox, setQf$sensCrdCo2 
                                                    #setQf$heatInlt
                                                    )[which(idGas == 105 | (is.na(idGas) & setQf$sensCrdCo2$qfSensStus == -1)),])
        
        rpt$dlta13CCo2 <- na.omit(data.frame(setQf$dlta13CCo2, setQf$presCrdCo2,
                                             setQf$tempCrdCo2, setQf$tempWbox,
                                             setQf$sensCrdCo2
                                             #, setQf$heatInlt
                                             )[which(idGas == 105 | (is.na(idGas) & setQf$sensCrdCo2$qfSensStus == -1)),])
        
        rpt$rtioMoleWetH2o <- na.omit(data.frame(setQf$rtioMoleWetH2o, setQf$presCrdCo2,
                                                 setQf$tempCrdCo2, setQf$tempWbox,
                                                 setQf$sensCrdCo2
                                                 #, setQf$heatInlt
                                                 )[which(idGas == 11 | (is.na(idGas) & setQf$sensCrdCo2$qfSensStus == -1)),])
        
        rpt$rtioMoleDryH2o <- na.omit(data.frame(setQf$rtioMoleDryH2o, setQf$rtioMoleWetH2o, 
                                                 setQf$presCrdCo2, setQf$tempCrdCo2, 
                                                 setQf$tempWbox, setQf$sensCrdCo2 
                                                 #, setQf$heatInlt
                                                 )[which(idGas == 11 | (is.na(idGas) & setQf$sensCrdCo2$qfSensStus == -1)),])
        
        rpt$temp <- na.omit(data.frame(setQf$tempCrdCo2, setQf$sensCrdCo2))
        rpt$pres <- na.omit(data.frame(setQf$presCrdCo2, setQf$sensCrdCo2))
        rpt$presEnvHut <- na.omit(data.frame(setQf$presEnvHut))
        rpt$rhEnvHut <- na.omit(data.frame (setQf$rhEnvHut))
        rpt$tempEnvHut <- na.omit(data.frame (setQf$tempEnvHut))
        rpt$rtioMoleWetH2oEnvHut <- na.omit(data.frame (setQf$rtioMoleWetH2oEnvHut))
      } else {
        #grouping qulity flags that related to isoCo2 L1 sub-data product  
        rpt$rtioMoleWetCo2 <- na.omit(data.frame(setQf$rtioMoleWetCo2, setQf$dlta13CCo2,
                                                 setQf$rtioMoleWet12CCo2, setQf$presCrdCo2, 
                                                 setQf$tempCrdCo2, setQf$tempWbox,
                                                 setQf$sensCrdCo2
                                                 #, setQf$heatInlt
                                                 ))
        
        rpt$rtioMoleDryCo2 <- na.omit(data.frame(setQf$rtioMoleDryCo2, setQf$dlta13CCo2,
                                                 setQf$rtioMoleWet12CCo2, setQf$presCrdCo2, 
                                                 setQf$tempCrdCo2, setQf$tempWbox,
                                                 setQf$sensCrdCo2
                                                 #, setQf$heatInlt
                                                 ))
        
        rpt$rtioMoleWet12CCo2 <- na.omit(data.frame(setQf$rtioMoleWet12CCo2, setQf$presCrdCo2, 
                                                    setQf$tempCrdCo2, setQf$tempWbox,
                                                    setQf$sensCrdCo2
                                                    #, setQf$heatInlt
                                                    ))
        
        rpt$rtioMoleDry12CCo2 <- na.omit(data.frame(setQf$rtioMoleDry12CCo2, setQf$rtioMoleWet13CCo2,
                                                    setQf$presCrdCo2, setQf$tempCrdCo2, 
                                                    setQf$tempWbox, setQf$sensCrdCo2 
                                                    #, setQf$heatInlt,
                                                    ))
        
        rpt$rtioMoleWet13CCo2 <- na.omit(data.frame(setQf$rtioMoleWet13CCo2, setQf$presCrdCo2, 
                                                    setQf$tempCrdCo2, setQf$tempWbox,
                                                    setQf$sensCrdCo2
                                                    #, setQf$heatInlt
                                                    ))
        
        rpt$rtioMoleDry13CCo2 <- na.omit(data.frame(setQf$rtioMoleDry13CCo2, setQf$rtioMoleWet13CCo2,
                                                    setQf$presCrdCo2, setQf$tempCrdCo2, 
                                                    setQf$tempWbox, setQf$sensCrdCo2 
                                                    #, setQf$heatInlt
                                                    ))
        
        rpt$dlta13CCo2 <- na.omit(data.frame(setQf$dlta13CCo2, setQf$presCrdCo2,
                                             setQf$tempCrdCo2, setQf$tempWbox,
                                             setQf$sensCrdCo2
                                             #, setQf$heatInlt
                                             ))
        
        rpt$rtioMoleWetH2o <- na.omit(data.frame(setQf$rtioMoleWetH2o, setQf$presCrdCo2,
                                                 setQf$tempCrdCo2, setQf$tempWbox,
                                                 setQf$sensCrdCo2
                                                 #, setQf$heatInlt
                                                 ))
        
        rpt$rtioMoleDryH2o <- na.omit(data.frame(setQf$rtioMoleDryH2o, setQf$rtioMoleWetH2o, 
                                                 setQf$presCrdCo2, setQf$tempCrdCo2, 
                                                 setQf$tempWbox, setQf$sensCrdCo2 
                                                 #, setQf$heatInlt
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
      #if not all idGas = NA or all qfRngTemp = NA
      if (length(which(!is.na(qfInput$crdCo2$qfRngTemp))) == 0){
        #grouping qulity flags that related to isoCo2 L1 sub-data product  
        rpt$rtioMoleWetCo2 <- na.omit(data.frame(setQf$rtioMoleWetCo2, setQf$dlta13CCo2,
                                                 setQf$rtioMoleWet12CCo2, setQf$presCrdCo2, 
                                                 setQf$tempCrdCo2, setQf$tempWbox,
                                                 setQf$sensCrdCo2, setQf$frt00MfcVali, 
                                                 setQf$frtMfcVali, setQf$presAtmMfcVali, 
                                                 setQf$tempMfcVali,setQf$sensMfcVali)[which(idGas == 105 | (is.na(idGas) & setQf$sensCrdCo2$qfSensStus == -1)),])
        
        rpt$rtioMoleDryCo2 <- na.omit(data.frame(setQf$rtioMoleDryCo2, setQf$dlta13CCo2,
                                                 setQf$rtioMoleWet12CCo2, setQf$presCrdCo2, 
                                                 setQf$tempCrdCo2, setQf$tempWbox,
                                                 setQf$sensCrdCo2, setQf$frt00MfcVali, 
                                                 setQf$frtMfcVali, setQf$presAtmMfcVali, 
                                                 setQf$tempMfcVali, setQf$sensMfcVali)[which(idGas == 105 | (is.na(idGas) & setQf$sensCrdCo2$qfSensStus == -1)),])
        
        rpt$rtioMoleWet12CCo2 <- na.omit(data.frame(setQf$rtioMoleWet12CCo2, setQf$presCrdCo2, 
                                                    setQf$tempCrdCo2, setQf$tempWbox,
                                                    setQf$sensCrdCo2, setQf$frt00MfcVali, 
                                                    setQf$frtMfcVali, setQf$presAtmMfcVali, 
                                                    setQf$tempMfcVali, setQf$sensMfcVali)[which(idGas == 105 | (is.na(idGas) & setQf$sensCrdCo2$qfSensStus == -1)),])
        
        rpt$rtioMoleDry12CCo2 <- na.omit(data.frame(setQf$rtioMoleDry12CCo2, setQf$rtioMoleWet13CCo2,
                                                    setQf$presCrdCo2, setQf$tempCrdCo2, 
                                                    setQf$tempWbox, setQf$sensCrdCo2, 
                                                    setQf$frt00MfcVali, setQf$frtMfcVali, 
                                                    setQf$presAtmMfcVali, setQf$tempMfcVali, 
                                                    setQf$sensMfcVali)[which(idGas == 105 | (is.na(idGas) & setQf$sensCrdCo2$qfSensStus == -1)),])
        
        rpt$rtioMoleWet13CCo2 <- na.omit(data.frame(setQf$rtioMoleWet13CCo2, setQf$presCrdCo2, 
                                                    setQf$tempCrdCo2, setQf$tempWbox,
                                                    setQf$sensCrdCo2, setQf$frt00MfcVali, 
                                                    setQf$frtMfcVali, setQf$presAtmMfcVali, 
                                                    setQf$tempMfcVali, setQf$sensMfcVali)[which(idGas == 105 | (is.na(idGas) & setQf$sensCrdCo2$qfSensStus == -1)),])
        
        rpt$rtioMoleDry13CCo2 <- na.omit(data.frame(setQf$rtioMoleDry13CCo2, setQf$rtioMoleWet13CCo2,
                                                    setQf$presCrdCo2, setQf$tempCrdCo2, 
                                                    setQf$tempWbox, setQf$sensCrdCo2, 
                                                    setQf$frt00MfcVali, setQf$frtMfcVali, 
                                                    setQf$presAtmMfcVali, setQf$tempMfcVali, 
                                                    setQf$sensMfcVali)[which(idGas == 105 | (is.na(idGas) & setQf$sensCrdCo2$qfSensStus == -1)),])
        
        rpt$dlta13CCo2 <- na.omit(data.frame(setQf$dlta13CCo2, setQf$presCrdCo2,
                                             setQf$tempCrdCo2, setQf$tempWbox,
                                             setQf$sensCrdCo2, setQf$frt00MfcVali, 
                                             setQf$frtMfcVali, setQf$presAtmMfcVali, 
                                             setQf$tempMfcVali, setQf$sensMfcVali)[which(idGas == 105 | (is.na(idGas) & setQf$sensCrdCo2$qfSensStus == -1)),])
        
        rpt$rtioMoleWetH2o <- na.omit(data.frame(setQf$rtioMoleWetH2o, setQf$presCrdCo2,
                                                 setQf$tempCrdCo2, setQf$tempWbox,
                                                 setQf$sensCrdCo2, setQf$frt00MfcVali, 
                                                 setQf$frtMfcVali, setQf$presAtmMfcVali, 
                                                 setQf$tempMfcVali, setQf$sensMfcVali)[which(idGas == 11 | (is.na(idGas) & setQf$sensCrdCo2$qfSensStus == -1)),])
        
        rpt$rtioMoleDryH2o <- na.omit(data.frame(setQf$rtioMoleDryH2o, setQf$rtioMoleWetH2o, 
                                                 setQf$presCrdCo2, setQf$tempCrdCo2, 
                                                 setQf$tempWbox, setQf$sensCrdCo2, 
                                                 setQf$frt00MfcVali, setQf$frtMfcVali, 
                                                 setQf$presAtmMfcVali, setQf$tempMfcVali, 
                                                 setQf$sensMfcVali)[which(idGas == 11 | (is.na(idGas) & setQf$sensCrdCo2$qfSensStus == -1)),])
        
        rpt$temp <- na.omit(data.frame(setQf$tempCrdCo2, setQf$sensCrdCo2))
        
        rpt$pres <- na.omit(data.frame(setQf$presCrdCo2, setQf$sensCrdCo2))
        rpt$presEnvHut <- na.omit(data.frame(setQf$presEnvHut))
        rpt$rhEnvHut <- na.omit(data.frame (setQf$rhEnvHut))
        rpt$tempEnvHut <- na.omit(data.frame (setQf$tempEnvHut))
        rpt$rtioMoleWetH2oEnvHut <- na.omit(data.frame (setQf$rtioMoleWetH2oEnvHut))
      } else {
        #grouping qulity flags that related to isoCo2 L1 sub-data product  
        rpt$rtioMoleWetCo2 <- na.omit(data.frame(setQf$rtioMoleWetCo2, setQf$dlta13CCo2,
                                                 setQf$rtioMoleWet12CCo2, setQf$presCrdCo2, 
                                                 setQf$tempCrdCo2, setQf$tempWbox,
                                                 setQf$sensCrdCo2, setQf$frt00MfcVali, 
                                                 setQf$frtMfcVali, setQf$presAtmMfcVali, 
                                                 setQf$tempMfcVali,setQf$sensMfcVali))
        
        rpt$rtioMoleDryCo2 <- na.omit(data.frame(setQf$rtioMoleDryCo2, setQf$dlta13CCo2,
                                                 setQf$rtioMoleWet12CCo2, setQf$presCrdCo2, 
                                                 setQf$tempCrdCo2, setQf$tempWbox,
                                                 setQf$sensCrdCo2, setQf$frt00MfcVali, 
                                                 setQf$frtMfcVali, setQf$presAtmMfcVali, 
                                                 setQf$tempMfcVali, setQf$sensMfcVali))
        
        rpt$rtioMoleWet12CCo2 <- na.omit(data.frame(setQf$rtioMoleWet12CCo2, setQf$presCrdCo2, 
                                                    setQf$tempCrdCo2, setQf$tempWbox,
                                                    setQf$sensCrdCo2, setQf$frt00MfcVali, 
                                                    setQf$frtMfcVali, setQf$presAtmMfcVali, 
                                                    setQf$tempMfcVali, setQf$sensMfcVali))
        
        rpt$rtioMoleDry12CCo2 <- na.omit(data.frame(setQf$rtioMoleDry12CCo2, setQf$rtioMoleWet13CCo2,
                                                    setQf$presCrdCo2, setQf$tempCrdCo2, 
                                                    setQf$tempWbox, setQf$sensCrdCo2, 
                                                    setQf$frt00MfcVali, setQf$frtMfcVali, 
                                                    setQf$presAtmMfcVali, setQf$tempMfcVali, 
                                                    setQf$sensMfcVali))
        
        rpt$rtioMoleWet13CCo2 <- na.omit(data.frame(setQf$rtioMoleWet13CCo2, setQf$presCrdCo2, 
                                                    setQf$tempCrdCo2, setQf$tempWbox,
                                                    setQf$sensCrdCo2, setQf$frt00MfcVali, 
                                                    setQf$frtMfcVali, setQf$presAtmMfcVali, 
                                                    setQf$tempMfcVali, setQf$sensMfcVali))
        
        rpt$rtioMoleDry13CCo2 <- na.omit(data.frame(setQf$rtioMoleDry13CCo2, setQf$rtioMoleWet13CCo2,
                                                    setQf$presCrdCo2, setQf$tempCrdCo2, 
                                                    setQf$tempWbox, setQf$sensCrdCo2, 
                                                    setQf$frt00MfcVali, setQf$frtMfcVali, 
                                                    setQf$presAtmMfcVali, setQf$tempMfcVali, 
                                                    setQf$sensMfcVali))
        
        rpt$dlta13CCo2 <- na.omit(data.frame(setQf$dlta13CCo2, setQf$presCrdCo2,
                                             setQf$tempCrdCo2, setQf$tempWbox,
                                             setQf$sensCrdCo2, setQf$frt00MfcVali, 
                                             setQf$frtMfcVali, setQf$presAtmMfcVali, 
                                             setQf$tempMfcVali, setQf$sensMfcVali))
        
        rpt$rtioMoleWetH2o <- na.omit(data.frame(setQf$rtioMoleWetH2o, setQf$presCrdCo2,
                                                 setQf$tempCrdCo2, setQf$tempWbox,
                                                 setQf$sensCrdCo2, setQf$frt00MfcVali, 
                                                 setQf$frtMfcVali, setQf$presAtmMfcVali, 
                                                 setQf$tempMfcVali, setQf$sensMfcVali))
        
        rpt$rtioMoleDryH2o <- na.omit(data.frame(setQf$rtioMoleDryH2o, setQf$rtioMoleWetH2o, 
                                                 setQf$presCrdCo2, setQf$tempCrdCo2, 
                                                 setQf$tempWbox, setQf$sensCrdCo2, 
                                                 setQf$frt00MfcVali, setQf$frtMfcVali, 
                                                 setQf$presAtmMfcVali, setQf$tempMfcVali, 
                                                 setQf$sensMfcVali))
        
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
    if (!("envHut" %in% names(qfInput)) || length(which(!is.na(qfInput$crdH2o$qfRngTemp))) == 0){
      qfInput$envHut <- as.data.frame(matrix(-1, ncol = 14, nrow = length(qfInput$crdH2o$qfRngRtioMoleDryH2o)))
      names(qfInput$envHut) <- c("qfRngPres", "qfStepPres", "qfPersPres", "qfRngRh", "qfStepRh", "qfPersRh",
                                 "qfRngRtioMoleWetH2o", "qfStepRtioMoleWetH2o", "qfPersRtioMoleWetH2o", 
                                 "qfRngTemp", "qfStepTemp", "qfPersTemp", "qfTemp", "qfRh")}
    
    # #external quality flags from heatInlt
    # if (!("heatInlt" %in% names(qfInput)) || length(which(!is.na(qfInput$crdH2o$qfRngTemp))) == 0){
    #   qfInput$heatInlt <- as.data.frame(matrix(-1, ncol = 1, nrow = length(qfInput$crdH2o$qfRngRtioMoleDryH2o)))
    #   names(qfInput$heatInlt) <- "qfHeat"}
    
    #replace -1 if all qf in crdH2o are NA
    if (length(which(!is.na(qfInput$crdH2o$qfRngTemp))) == 0){
      qfInput$crdH2o[,1:length(qfInput$crdH2o)] <- -1
    }
    
    #replace -1 if all qf in crdCH2o are NA
    qfName <- c("qfSensStus", "qfStusN2", "qfValiH2o")
    for (idx in 1:length(qfName)){
    if (length(which(!is.na(qfInput$crdH2o[[qfName[idx]]]))) == 0){
      qfInput$crdH2o[[qfName[idx]]] <- -1
    }}
    
    #setQf for crdH2o
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
    setQf$valiCrdH2o <- data.frame("qfValiH2o" = qfInput$crdH2o$qfValiH2o)
    #setQf of envHut
    setQf$envHut <- data.frame("qfRh" = qfInput$envHut$qfRh)
    setQf$presEnvHut <- data.frame("qfRngPres" = qfInput$envHut$qfRngPres, 
                                   "qfStepPres" = qfInput$envHut$qfStepPres,
                                   "qfPersPres" = qfInput$envHut$qfPersPres)
    
    setQf$rhEnvHut <- data.frame("qfRngRh" = qfInput$envHut$qfRngRh, 
                                 "qfStepRh" = qfInput$envHut$qfStepRh,
                                 "qfPersRh" = qfInput$envHut$qfPersRh)
    
    setQf$rtioMoleWetH2oEnvHut <- data.frame("qfRngRtioMoleWetH2o" = qfInput$envHut$qfRngRtioMoleWetH2o, 
                                             "qfStepRtioMoleWetH2o" = qfInput$envHut$qfStepRtioMoleWetH2o,
                                             "qfPersRtioMoleWetH2o" = qfInput$envHut$qfPersRtioMoleWetH2o)
    
    setQf$tempEnvHut <- data.frame("qfRngTemp" = qfInput$envHut$qfRngTemp, 
                                   "qfStepTemp" = qfInput$envHut$qfStepTemp,
                                   "qfPersTemp" = qfInput$envHut$qfPersTemp)
    
    # #setQf of heatInlt
    # setQf$heatInlt <- data.frame("qfHeat" = qfInput$heatInlt$qfHeat)
    
    #define qf which use only sampling period
    if (TypeMeas == "samp") {     
      #grouping qulity flags that related to isopH2o L1 sub-data product  
      rpt$rtioMoleDryH2o <- na.omit(data.frame(setQf$rtioMoleDryH2o, setQf$rtioMoleWetH2o,
                                               setQf$presCrdH2o, setQf$tempCrdH2o,
                                               setQf$tempWbox,  setQf$sensCrdH2o,
                                               setQf$envHut
                                               #, setQf$heatInlt
                                               ))
      
      rpt$rtioMoleWetH2o <- na.omit(data.frame(setQf$rtioMoleWetH2o, setQf$presCrdH2o, 
                                               setQf$tempCrdH2o, setQf$tempWbox,  
                                               setQf$sensCrdH2o, setQf$envHut 
                                               #, setQf$heatInlt
                                               ))
      
      rpt$dlta18OH2o <- na.omit(data.frame(setQf$dlta18OH2o, setQf$presCrdH2o, 
                                           setQf$tempCrdH2o, setQf$tempWbox,  
                                           setQf$sensCrdH2o, setQf$envHut 
                                           #, setQf$heatInlt
                                           ))
      
      rpt$dlta2HH2o <- na.omit(data.frame(setQf$dlta2HH2o, setQf$presCrdH2o, 
                                          setQf$tempCrdH2o, setQf$tempWbox,  
                                          setQf$sensCrdH2o, setQf$envHut 
                                          #, setQf$heatInlt
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
                                               setQf$envHut, setQf$valiCrdH2o))
      
      rpt$rtioMoleWetH2o <- na.omit(data.frame(setQf$rtioMoleWetH2o, setQf$presCrdH2o, 
                                               setQf$tempCrdH2o, setQf$tempWbox,  
                                               setQf$sensCrdH2o, setQf$envHut,
                                               setQf$valiCrdH2o))
      
      rpt$dlta18OH2o <- na.omit(data.frame(setQf$dlta18OH2o, setQf$presCrdH2o, 
                                           setQf$tempCrdH2o, setQf$tempWbox,  
                                           setQf$sensCrdH2o, setQf$envHut,
                                           setQf$valiCrdH2o))
      
      rpt$dlta2HH2o <- na.omit(data.frame(setQf$dlta2HH2o, setQf$presCrdH2o, 
                                          setQf$tempCrdH2o, setQf$tempWbox,  
                                          setQf$sensCrdH2o, setQf$envHut,
                                          setQf$valiCrdH2o))
      
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
    setQf$tempEnvHut <- data.frame("qfRngTemp" = qfInput$envHut$qfRngTemp, 
                                   "qfStepTemp" = qfInput$envHut$qfStepTemp,
                                   "qfPersTemp" = qfInput$envHut$qfPersTemp)
    
    setQf$rh <- data.frame("qfRngRh" = qfInput$envHut$qfRngRh, 
                           "qfStepRh" = qfInput$envHut$qfStepRh,
                           "qfPersRh" = qfInput$envHut$qfPersRh)
    
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
    }#close if statement of TypeMeas %in% c("samp", "vali")
  }#close if statement of dp01 == "envHut"

#tempAirLvl ####################################################################################
  if (dp01 == "tempAirLvl") {
    if (TypeMeas %in% c("samp", "vali")) {
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
    }#close if statement of TypeMeas %in% c("samp", "vali")
  }#close if statement of dp01 == tempAirLvl
  
#tempAirTop ####################################################################################
  if (dp01 == "tempAirTop") {
    if (TypeMeas %in% c("samp", "vali")) {
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
    }#close if statement of TypeMeas %in% c("samp", "vali")
  }#close if statement of dp01 == tempAirLvl
}# closed if statement of MethMeas == "ecse"

#return values
return(rpt)
  
#end function def.neon.dp01.qf.grp()
}
