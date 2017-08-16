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
#' qf$irga <- eddy4R.qaqc::def.qf.ecte(TimeBgn = TimeBgn, TimeEnd = TimeEnd, Freq = 20, Sens = "irga", PcntQf = 0.05)
#' #add qfIrgavali
#' qf$irga$qfIrgaVali <- rep(0, 86000)
#' qf$irgaMfcSamp <- eddy4R.qaqc::def.qf.ecte(TimeBgn = TimeBgn, TimeEnd = TimeEnd, Freq = 20, Sens = "irgaMfcSamp", PcntQf = 0.05)
#' qf$soni <- eddy4R.qaqc::def.qf.ecte(TimeBgn = TimeBgn, TimeEnd = TimeEnd, Freq = 20, Sens = "soni", PcntQf = 0.05)
#' qf$soniAmrs <- eddy4R.qaqc::def.qf.ecte(TimeBgn = TimeBgn, TimeEnd = TimeEnd, Freq = 40, Sens = "soniAmrs", PcntQf = 0.05)
#' 
#' #grouping the set of the flags
#' qfGrpIrgaCo2 <- eddy4R.qaqc::def.neon.dp01.qf.grp(qfInput = qf, MethMeas = "ecte", TypeMeas = "vali", dp01="irgaCo2")
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
#     commented out the qfCal in irga and soni
#   Natchaya P-Durden (2017-05-12)
#     added qfIrgaVali and qfIrgaAgc
#   David Durden (2017-06-27)
#     removing qfPersFrt00 flag from processing
#   Natchaya P-Durden (2017-08-02)
#     modified ecse section
##############################################################################################

def.neon.dp01.qf.grp <- function(
  qfInput = list(),
  MethMeas = c("ecte", "ecse")[1],
  TypeMeas = c("samp", "vali")[1], 
  dp01 = c("envHut", "irgaCo2", "irgaH2o", "isoCo2", "isoH2o", "soni", "soniAmrs", "tempAirLvl", "tempAirTop")[1],
  idGas =NULL
){
  #check existing input list
    if (dp01 %in% c("irgaCo2", "irgaH2o")) {
      if (!("irga" %in% names(qfInput))) { 
        base::stop("Missing irga quality flags")
      }
    }# close if statement of dp01
  
    if (dp01 %in% c("soni")) {
      if (!("soni" %in% names(qfInput))){
        base::stop("Missing soni quality flags")
      }
    }# close if statement of dp01
  
    if (dp01 %in% c("soniAmrs")) {
      if (!("soniAmrs" %in% names(qfInput))){
        base::stop("Missing soniAmrs quality flags")
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
#irgaCo2 and irgaH2o####################################################################################
  if (dp01 %in% c("irgaCo2", "irgaH2o")) {
    #organized all quality flags from irga into the set of flags (for frequency use)
    #irga sensor flags
    setQf$sensIrga <- data.frame("qfIrgaHead" = qfInput$irga$qfIrgaHead, 
                             "qfIrgaTempOut" = qfInput$irga$qfIrgaTempOut, 
                             "qfIrgaTempIn" = qfInput$irga$qfIrgaTempIn,
                             "qfIrgaAux" = qfInput$irga$qfIrgaAux, 
                             "qfIrgaPres" = qfInput$irga$qfIrgaPres, 
                             "qfIrgaChop" = qfInput$irga$qfIrgaChop, 
                             "qfIrgaDetc" = qfInput$irga$qfIrgaDetc, 
                             "qfIrgaPll" = qfInput$irga$qfIrgaPll, 
                             "qfIrgaSync" = qfInput$irga$qfIrgaSync,
                             "qfIrgaAgc" = qfInput$irga$qfIrgaAgc)
    
    setQf$sensIrgaExt <- data.frame("qfIrgaVali" = qfInput$irga$qfIrgaVali)
    
    setQf$tempIn <- data.frame("qfRngTempIn" = qfInput$irga$qfRngTempIn,
                               "qfStepTempIn" = qfInput$irga$qfStepTempIn,
                               "qfPersTempIn" = qfInput$irga$qfPersTempIn)
                               #"qfCalTempIn" = qfInput$irga$qfCalTempIn)
    
    setQf$tempOut <- data.frame("qfRngTempOut" = qfInput$irga$qfRngTempOut,
                                "qfStepTempOut" = qfInput$irga$qfStepTempOut,
                                "qfPersTempOut" = qfInput$irga$qfPersTempOut)
                                #"qfCalTempOut" = qfInput$irga$qfCalTempOut)
    
    setQf$tempAve <- data.frame ("qfRngTempMean" = qfInput$irga$qfRngTempMean, 
                                 "qfStepTempMean" = qfInput$irga$qfStepTempMean,
                                 "qfPersTempMean" = qfInput$irga$qfPersTempMean) 
                                 #"qfCalTempMean" = qfInput$irga$qfCalTempMean)
    
    setQf$presAtmIrga <- data.frame("qfRngPresAtm" = qfInput$irga$qfRngPresAtm, 
                                "qfStepPresAtm" = qfInput$irga$qfStepPresAtm,
                                "qfPersPresAtm" = qfInput$irga$qfPersPresAtm) 
                                #"qfCalPresAtm" = qfInput$irga$qfCalPresAtm)
    
    setQf$presDiffIrga <- data.frame("qfRngPresDiff" = qfInput$irga$qfRngPresDiff,
                                     "qfStepPresDiff" = qfInput$irga$qfStepPresDiff,
                                     "qfPersPresDiff" = qfInput$irga$qfPersPresDiff)
                                     #"qfCalPresDiff" = qfInput$irga$qfCalPresDiff) 
    
    setQf$presSum <- data.frame("qfRngPresSum" = qfInput$irga$qfRngPresSum,
                                "qfStepPresSum" = qfInput$irga$qfStepPresSum,
                                "qfPersPresSum" = qfInput$irga$qfPersPresSum)
                                #"qfCalPresSum" = qfInput$irga$qfCalPresSum)
    
    setQf$powrH2oSamp <- data.frame ("qfRngPowrH2oSamp" = qfInput$irga$qfRngPowrH2oSamp,
                                     "qfStepPowrH2oSamp" = qfInput$irga$qfStepPowrH2oSamp,
                                     "qfPersPowrH2oSamp" = qfInput$irga$qfPersPowrH2oSamp)
                                     #"qfCalPowrH2oSamp" = qfInput$irga$qfCalPowrH2oSamp)
    
    setQf$powrH2oRefe <- data.frame ("qfRngPowrH2oRefe" = qfInput$irga$qfRngPowrH2oRefe,
                                     "qfStepPowrH2oRefe" = qfInput$irga$qfStepPowrH2oRefe,
                                     "qfPersPowrH2oRefe" = qfInput$irga$qfPersPowrH2oRefe)
                                     #"qfCalPowrH2oRefe" = qfInput$irga$qfCalPowrH2oRefe)
    
    setQf$asrpH2o <- data.frame("qfRngAsrpH2o" = qfInput$irga$qfRngAsrpH2o, 
                                "qfStepAsrpH2o" = qfInput$irga$qfStepAsrpH2o, 
                                "qfPersAsrpH2o" = qfInput$irga$qfPersAsrpH2o) 
                                #"qfCalAsrpH2o" = qfInput$irga$qfCalAsrpH2o)
    
    setQf$densMoleH2o <- data.frame("qfRngDensMoleH2o" = qfInput$irga$qfRngDensMoleH2o, 
                                    "qfStepDensMoleH2o" = qfInput$irga$qfStepDensMoleH2o, 
                                    "qfPersDensMoleH2o" = qfInput$irga$qfPersDensMoleH2o)
                                    #"qfCalDensMoleH2o" = qfInput$irga$qfCalDensMoleH2o)
    
    setQf$rtioMoleDryH2o <- data.frame("qfRngRtioMoleDryH2o" = qfInput$irga$qfRngRtioMoleDryH2o,
                                       "qfStepRtioMoleDryH2o" = qfInput$irga$qfStepRtioMoleDryH2o,
                                       "qfPersRtioMoleDryH2o" = qfInput$irga$qfPersRtioMoleDryH2o)
                                       #"qfCalRtioMoleDryH2o" = qfInput$irga$qfCalRtioMoleDryH2o)
    
    setQf$powrCo2Samp <- data.frame("qfRngPowrCo2Samp" = qfInput$irga$qfRngPowrCo2Samp,
                                    "qfStepPowrCo2Samp" = qfInput$irga$qfStepPowrCo2Samp,
                                    "qfPersPowrCo2Samp" = qfInput$irga$qfPersPowrCo2Samp)
                                    #"qfCalPowrCo2Samp" = qfInput$irga$qfCalPowrCo2Samp)
    
    setQf$powrCo2Refe <- data.frame ("qfRngPowrCo2Refe" = qfInput$irga$qfRngPowrCo2Refe,
                                     "qfStepPowrCo2Refe" = qfInput$irga$qfStepPowrCo2Refe,
                                     "qfPersPowrCo2Refe" = qfInput$irga$qfPersPowrCo2Refe)
                                     #"qfCalPowrCo2Refe" = qfInput$irga$qfCalPowrCo2Refe)
    
    setQf$asrpCo2 <- data.frame("qfRngAsrpCo2" = qfInput$irga$qfRngAsrpCo2, 
                                "qfStepAsrpCo2" = qfInput$irga$qfStepAsrpCo2, 
                                "qfPersAsrpCo2" = qfInput$irga$qfPersAsrpCo2) 
                                #"qfCalAsrpCo2" = qfInput$irga$qfCalAsrpCo2)
    
    setQf$densMoleCo2 <- data.frame("qfRngDensMoleCo2" = qfInput$irga$qfRngDensMoleCo2,
                                    "qfStepDensMoleCo2" = qfInput$irga$qfStepDensMoleCo2,
                                    "qfPersDensMoleCo2" = qfInput$irga$qfPersDensMoleCo2) 
                                    #"qfCalDensMoleCo2" = qfInput$irga$qfCalDensMoleCo2) 
    
    setQf$rtioMoleDryCo2 <- data.frame("qfRngRtioMoleDryCo2" = qfInput$irga$qfRngRtioMoleDryCo2,
                                       "qfStepRtioMoleDryCo2" = qfInput$irga$qfStepRtioMoleDryCo2,
                                       "qfPersRtioMoleDryCo2" = qfInput$irga$qfPersRtioMoleDryCo2)
                                      # "qfCalRtioMoleDryCo2" = qfInput$irga$qfCalRtioMoleDryCo2)
    
    setQf$ssiCo2 <- data.frame("qfRngSsiCo2" = qfInput$irga$qfRngSsiCo2, 
                               "qfStepSsiCo2" = qfInput$irga$qfStepSsiCo2, 
                               "qfPersSsiCo2" = qfInput$irga$qfPersSsiCo2)
                               #"qfCalSsiCo2" = qfInput$irga$qfCalSsiCo2)
    
    setQf$ssiH2o <- data.frame("qfRngSsiH2o" = qfInput$irga$qfRngSsiH2o, 
                               "qfStepSsiH2o" = qfInput$irga$qfStepSsiH2o, 
                               "qfPersSsiH2o" = qfInput$irga$qfPersSsiH2o) 
                               #"qfCalSsiH2o" = qfInput$irga$qfCalSsiH2o)
    
    #external quality flags from irgaMfcSamp
    if ("irgaMfcSamp" %in% names(qfInput)){
      #irgaMfcSamp
      setQf$frt00IrgaMfcSamp <- data.frame("qfRngFrt00" = qfInput$irgaMfcSamp$qfRngFrt00) 
                                #,"qfPersFrt00" = qfInput$irgaMfcSamp$qfPersFrt00)
      
      setQf$frtIrgaMfcSamp <- data.frame("qfRngFrt" = qfInput$irgaMfcSamp$qfRngFrt,
                              "qfPersFrt" = qfInput$irgaMfcSamp$qfPersFrt)
      
      setQf$presAtmIrgaMfcSamp <- data.frame("qfRngPresAtm" = qfInput$irgaMfcSamp$qfRngPresAtm, 
                                     "qfStepPresAtm" = qfInput$irgaMfcSamp$qfStepPresAtm,
                                     "qfPersPresAtm" = qfInput$irgaMfcSamp$qfPersPresAtm)
      
      setQf$tempIrgaMfcSamp <- data.frame("qfRngTemp" = qfInput$irgaMfcSamp$qfRngTemp,
                                  "qfStepTemp" = qfInput$irgaMfcSamp$qfStepTemp,
                                  "qfPersTemp" = qfInput$irgaMfcSamp$qfPersTemp)
      } else {
      #assign qf for irgaMfcSamp to -1 when qf irgaMfcSamp is missing
      setQf$frt00IrgaMfcSamp <- data.frame("qfRngFrt00" = -1)
                                           #,"qfPersFrt00" = -1)
      
      setQf$frtIrgaMfcSamp <- data.frame("qfRngFrt" = -1,
                              "qfPersFrt" = -1)
      
      setQf$presAtmIrgaMfcSamp <- data.frame("qfRngPresAtm" = -1, 
                                     "qfStepPresAtm" = -1,
                                     "qfPersPresAtm" = -1)
      
      setQf$tempIrgaMfcSamp <- data.frame("qfRngTemp" = -1,
                                  "qfStepTemp" = -1,
                                  "qfPersTemp" = -1)
    }

    #external quality flags from irgaMfcVali
    if ("irgaMfcVali" %in% names(qfInput)){
      #irgaMfcVali
      setQf$frt00IrgaMfcVali <- data.frame("qfRngFrt00" = qfInput$irgaMfcVali$qfRngFrt00) 
                                # "qfPersFrt00" = qfInput$irgaMfcVali$qfPersFrt00)
      
      setQf$frtIrgaMfcVali <- data.frame("qfRngFrt" = qfInput$irgaMfcVali$qfRngFrt,
                              "qfPersFrt" = qfInput$irgaMfcVali$qfPersFrt)
      
      setQf$presAtmIrgaMfcVali <- data.frame("qfRngPresAtm" = qfInput$irgaMfcVali$qfRngPresAtm, 
                                     "qfStepPresAtm" = qfInput$irgaMfcVali$qfStepPresAtm,
                                     "qfPersPresAtm" = qfInput$irgaMfcVali$qfPersPresAtm)
      
      setQf$tempIrgaMfcVali <- data.frame("qfRngTemp" = qfInput$irgaMfcVali$qfRngTemp,
                                  "qfStepTemp" = qfInput$irgaMfcVali$qfStepTemp,
                                  "qfPersTemp" = qfInput$irgaMfcVali$qfPersTemp)
      } else {
      #assign qf for irgaMfcVali to -1 when qf irgaMfcVali is missing
      setQf$frt00IrgaMfcVali <- data.frame("qfRngFrt00" = -1) 
                                #"qfPersFrt00" = -1)
      
      setQf$frtIrgaMfcVali <- data.frame("qfRngFrt" = -1,
                              "qfPersFrt" = -1)
      
      setQf$presAtmIrgaMfcVali <- data.frame("qfRngPresAtm" = -1, 
                                     "qfStepPresAtm" = -1,
                                     "qfPersPresAtm" = -1)
      
      setQf$tempIrgaMfcVali <- data.frame("qfRngTemp" = -1,
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
    
    #grouping qulity flags that related to irgaCo2 L1 sub-data product
    if (dp01 == "irgaCo2"){
      if (TypeMeas == "samp"){
      rpt$densMoleCo2 <- data.frame(setQf$sensIrga, setQf$sensIrgaExt,
                                    setQf$tempAve, setQf$presDiffIrga, 
                                    setQf$powrCo2Samp, setQf$powrCo2Refe, 
                                    setQf$asrpCo2, setQf$densMoleCo2, 
                                    setQf$rtioMoleDryCo2, setQf$ssiCo2, 
                                    "qfRngFrt00" = setQf$frt00IrgaMfcSamp$qfRngFrt00, setQf$presAtmIrgaMfcSamp, 
                                    setQf$tempIrgaMfcSamp)
      
      rpt$rtioMoleDryCo2 <- data.frame(setQf$sensIrga, setQf$sensIrgaExt,
                                       setQf$tempAve, setQf$presDiffIrga, 
                                       setQf$powrH2oSamp, setQf$powrH2oRefe, 
                                       setQf$asrpH2o, setQf$rtioMoleDryH2o, 
                                       setQf$powrCo2Samp, setQf$powrCo2Refe, 
                                       setQf$asrpCo2, setQf$rtioMoleDryCo2, 
                                       setQf$ssiCo2, setQf$ssiH2o, 
                                       "qfRngFrt00" = setQf$frt00IrgaMfcSamp$qfRngFrt00, setQf$presAtmIrgaMfcSamp, 
                                       setQf$tempIrgaMfcSamp)
      
      rpt$presAtm <- data.frame(setQf$sensIrga, setQf$sensIrgaExt, setQf$presAtmIrga)
      
      rpt$presSum <- data.frame(setQf$sensIrga, setQf$sensIrgaExt, setQf$presSum)
      
      rpt$tempAve <- data.frame(setQf$sensIrga, setQf$sensIrgaExt, 
                                setQf$tempIn, setQf$tempOut, 
                                setQf$tempAve)
      }#close if statement of TypeMeas == "samp"
      
      if (TypeMeas == "vali"){
        rpt$densMoleCo2 <- data.frame(setQf$sensIrga, setQf$tempAve,
                                      setQf$presDiffIrga, setQf$powrCo2Samp, 
                                      setQf$powrCo2Refe, setQf$asrpCo2 , 
                                      setQf$densMoleCo2, setQf$rtioMoleDryCo2, 
                                      setQf$ssiCo2, "qfRngFrt00" = setQf$frt00IrgaMfcSamp$qfRngFrt00,
                                      setQf$presAtmIrgaMfcSamp, setQf$tempIrgaMfcSamp,
                                      "qfRngFrt00" = setQf$frt00IrgaMfcVali$qfRngFrt00, setQf$presAtmIrgaMfcVali, 
                                      setQf$tempIrgaMfcVali)
        
        rpt$rtioMoleDryCo2 <- data.frame(setQf$sensIrga, setQf$tempAve,
                                         setQf$presDiffIrga, setQf$powrH2oSamp, 
                                         setQf$powrH2oRefe, setQf$asrpH2o, 
                                         setQf$rtioMoleDryH2o, setQf$powrCo2Samp, 
                                         setQf$powrCo2Refe, setQf$asrpCo2,
                                         setQf$rtioMoleDryCo2, setQf$ssiCo2, 
                                         setQf$ssiH2o, "qfRngFrt00" = setQf$frt00IrgaMfcSamp$qfRngFrt00,
                                         setQf$presAtmIrgaMfcSamp, setQf$tempIrgaMfcSamp,
                                         "qfRngFrt00" = setQf$frt00IrgaMfcVali$qfRngFrt00, setQf$presAtmIrgaMfcVali, 
                                         setQf$tempIrgaMfcVali)
        
        rpt$presAtm <- data.frame(setQf$sensIrga, setQf$presAtmIrga)
        
        rpt$presSum <- data.frame(setQf$sensIrga, setQf$presSum)
        
        rpt$tempAve <- data.frame(setQf$sensIrga, setQf$tempIn, 
                                  setQf$tempOut, setQf$tempAve)
      }#close if statement of TypeMeas == "vali"
      
      rpt$frt00Samp <- data.frame(setQf$frt00IrgaMfcSamp, setQf$frtIrgaMfcSamp, 
                                    setQf$presAtmIrgaMfcSamp, setQf$tempIrgaMfcSamp)
      
    }
    #grouping qulity flags that related to irgaH2o L1 sub-data product    
    if (dp01 == "irgaH2o") {
      if (TypeMeas == "samp"){
      rpt$densMoleH2o <- data.frame(setQf$sensIrga, setQf$sensIrgaExt,
                                    setQf$tempAve, setQf$presDiffIrga, 
                                    setQf$powrH2oSamp, setQf$powrH2oRefe, 
                                    setQf$asrpH2o, setQf$densMoleH2o, 
                                    setQf$ssiH2o, "qfRngFrt00" = setQf$frt00IrgaMfcSamp$qfRngFrt00, 
                                    setQf$presAtmIrgaMfcSamp, setQf$tempIrgaMfcSamp)
      
      rpt$rtioMoleDryH2o <- data.frame(setQf$sensIrga, setQf$sensIrgaExt,
                                       setQf$tempAve, setQf$presDiffIrga, 
                                       setQf$powrH2oSamp, setQf$powrH2oRefe, 
                                       setQf$asrpH2o, setQf$rtioMoleDryH2o, 
                                       setQf$ssiH2o, "qfRngFrt00" = setQf$frt00IrgaMfcSamp$qfRngFrt00, 
                                       setQf$presAtmIrgaMfcSamp, setQf$tempIrgaMfcSamp)
      
      rpt$presAtm <- data.frame(setQf$sensIrga, setQf$sensIrgaExt, setQf$presAtmIrga)
      
      rpt$presSum <- data.frame(setQf$sensIrga, setQf$sensIrgaExt, setQf$presSum)
      
      rpt$tempAve <- data.frame(setQf$sensIrga, setQf$sensIrgaExt, 
                                setQf$tempIn, setQf$tempOut, 
                                setQf$tempAve)
      
      rpt$tempDew <- data.frame(setQf$sensIrga, setQf$sensIrgaExt,
                                setQf$tempAve, setQf$presDiffIrga, 
                                setQf$presSum, setQf$powrH2oSamp,
                                setQf$powrH2oRefe, setQf$asrpH2o, 
                                setQf$rtioMoleDryH2o, setQf$ssiH2o)#,setQf$soni) Remove until we can deal with differance length
      }#close if statement of TypeMeas == "samp"
      
      if (TypeMeas == "vali"){
        rpt$densMoleH2o <- data.frame(setQf$sensIrga, setQf$tempAve,
                                      setQf$presDiffIrga, setQf$powrH2oSamp, 
                                      setQf$powrH2oRefe, setQf$asrpH2o, 
                                      setQf$densMoleH2o, setQf$ssiH2o,
                                      "qfRngFrt00" = setQf$frt00IrgaMfcSamp$qfRngFrt00, setQf$presAtmIrgaMfcSamp, 
                                      setQf$tempIrgaMfcSamp, "qfRngFrt00" = setQf$frt00IrgaMfcVali$qfRngFrt00, 
                                      setQf$presAtmIrgaMfcVali, setQf$tempIrgaMfcVali)
        
        rpt$rtioMoleDryH2o <- data.frame(setQf$sensIrga, setQf$tempAve,
                                         setQf$presDiffIrga, setQf$powrH2oSamp, 
                                         setQf$powrH2oRefe, setQf$asrpH2o, 
                                         setQf$rtioMoleDryH2o, setQf$ssiH2o,
                                         "qfRngFrt00" = setQf$frt00IrgaMfcSamp$qfRngFrt00, setQf$presAtmIrgaMfcSamp, 
                                         setQf$tempIrgaMfcSamp, "qfRngFrt00" = setQf$frt00IrgaMfcVali$qfRngFrt00, 
                                         setQf$presAtmIrgaMfcVali, setQf$tempIrgaMfcVali)
        
        rpt$presAtm <- data.frame(setQf$sensIrga, setQf$presAtmIrga)
        
        rpt$presSum <- data.frame(setQf$sensIrga, setQf$presSum)
        
        rpt$tempAve <- data.frame(setQf$sensIrga, setQf$tempIn, 
                                  setQf$tempOut, setQf$tempAve)
        
        rpt$tempDew <- data.frame(setQf$sensIrga, setQf$tempAve,
                                  setQf$presDiffIrga, setQf$presSum,
                                  setQf$powrH2oSamp,setQf$powrH2oRefe, 
                                  setQf$asrpH2o, setQf$rtioMoleDryH2o, 
                                  setQf$ssiH2o) #, setQf$soni) Remove until we can deal with the difference length
      }#close if statement of TypeMeas == "vali"
      
      rpt$frt00Samp <- data.frame(setQf$frt00IrgaMfcSamp, setQf$frtIrgaMfcSamp, 
                              setQf$presAtmIrgaMfcSamp, setQf$tempIrgaMfcSamp)
      
    }
    #remove setQf
    setQf <- NULL
  } #closed loop for dp01 irgaCo2 and irgaH2o
  
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
    
    #external quality flags from irga for grouping qf of tempAir
    if ("irga" %in% names(qfInput)){
      setQf$irga <- data.frame("qfIrgaHead" = qfInput$irga$qfIrgaHead,
                               "qfIrgaTempOut" = qfInput$irga$qfIrgaTempOut, 
                               "qfIrgaTempIn" = qfInput$irga$qfIrgaTempIn,
                               "qfIrgaAux" = qfInput$irga$qfIrgaAux, 
                               "qfIrgaPres" = qfInput$irga$qfIrgaPres,
                               "qfIrgaChop" = qfInput$irga$qfIrgaChop, 
                               "qfIrgaDetc" = qfInput$irga$qfIrgaDetc,
                               "qfIrgaPll" = qfInput$irga$qfIrgaPll, 
                               "qfIrgaSync" = qfInput$irga$qfIrgaSync,
                               "qfIrgaAgc" = qfInput$irga$qfIrgaAgc,
                               "qfIrgaVali" = qfInput$irga$qfIrgaVali,
                               "qfRngTempMean" = qfInput$irga$qfRngTempMean, 
                               "qfStepTempMean" = qfInput$irga$qfStepTempMean,
                               "qfPersTempMean" = qfInput$irga$qfPersTempMean, 
                               #"qfCalTempMean" = qfInput$irga$qfCalTempMean,
                               "qfRngPresDiff" = qfInput$irga$qfRngPresDiff,
                               "qfStepPresDiff" = qfInput$irga$qfStepPresDiff,
                               "qfPersPresDiff" = qfInput$irga$qfPersPresDiff,
                               #"qfCalPresDiff" = qfInput$irga$qfCalPresDiff,
                               "qfRngPowrH2oSamp" = qfInput$irga$qfRngPowrH2oSamp,
                               "qfStepPowrH2oSamp" = qfInput$irga$qfStepPowrH2oSamp,
                               "qfPersPowrH2oSamp" = qfInput$irga$qfPersPowrH2oSamp,
                               #"qfCalPowrH2oSamp" = qfInput$irga$qfCalPowrH2oSamp,
                               "qfRngPowrH2oRefe" = qfInput$irga$qfRngPowrH2oRefe,
                               "qfStepPowrH2oRefe" = qfInput$irga$qfStepPowrH2oRefe,
                               "qfPersPowrH2oRefe" = qfInput$irga$qfPersPowrH2oRefe,
                               #"qfCalPowrH2oRefe" = qfInput$irga$qfCalPowrH2oRefe,
                               "qfRngAsrpH2o" = qfInput$irga$qfRngAsrpH2o, 
                               "qfStepAsrpH2o" = qfInput$irga$qfStepAsrpH2o, 
                               "qfPersAsrpH2o" = qfInput$irga$qfPersAsrpH2o, 
                               #"qfCalAsrpH2o" = qfInput$irga$qfCalAsrpH2o,
                               "qfRngDensMoleH2o" = qfInput$irga$qfRngDensMoleH2o, 
                               "qfStepDensMoleH2o" = qfInput$irga$qfStepDensMoleH2o, 
                               "qfPersDensMoleH2o" = qfInput$irga$qfPersDensMoleH2o, 
                               #"qfCalDensMoleH2o" = qfInput$irga$qfCalDensMoleH2o,
                               "qfRngSsiH2o" = qfInput$irga$qfRngSsiH2o, 
                               "qfStepSsiH2o" = qfInput$irga$qfStepSsiH2o, 
                               "qfPersSsiH2o" = qfInput$irga$qfPersSsiH2o) 
                               #"qfCalSsiH2o" = qfInput$irga$qfCalSsiH2o)
      } else {
      setQf$irga <- data.frame("qfIrgaHead" = -1,
                               "qfIrgaTempOut" = -1, 
                               "qfIrgaTempIn" = -1,
                               "qfIrgaAux" = -1, 
                               "qfIrgaPres" = -1,
                               "qfIrgaChop" = -1, 
                               "qfIrgaDetc" = -1,
                               "qfIrgaPll" = -1, 
                               "qfIrgaSync" = -1,
                               "qfIrgaAgc" = -1,
                               "qfIrgaVali" = -1,
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
    }#close if else statement for irga
    ##TO DO##Considering later when the AMRS is collaborating to correct the SONI data
    # #external quality flags from irga for grouping qf of tempAir
    # if ("soniAmrs" %in% names(qfInput)){
    #   #subset only odd row to match with soni frequency at 20Hz
    #   qfColNames <- colnames(qfInput$soniAmrs)
    #   qfInput$soniAmrs <- data.frame(qfInput$soniAmrs[seq(1,nrow(qfInput$soniAmrs),2),])
    #   colnames(qfInput$soniAmrs) <- qfColNames
    #   
    #   setQf$soniAmrs <- data.frame("qfAmrsVal" = qfInput$soniAmrs$qfAmrsVal,
    #                                "qfAmrsFilt" = qfInput$soniAmrs$qfAmrsFilt,
    #                                "qfAmrsVelo" = qfInput$soniAmrs$qfAmrsVelo,
    #                                "qfAmrsRng" = qfInput$soniAmrs$qfAmrsRng,
    #                                "qfRngAngXaxs" = qfInput$soniAmrs$qfRngAngXaxs,
    #                                "qfStepAngXaxs" = qfInput$soniAmrs$qfStepAngXaxs,
    #                                "qfPersAngXaxs" = qfInput$soniAmrs$qfPersAngXaxs,
    #                                "qfRngAngYaxs" = qfInput$soniAmrs$qfRngAngYaxs,
    #                                "qfStepAngYaxs" = qfInput$soniAmrs$qfStepAngYaxs,
    #                                "qfPersAngYaxs" = qfInput$soniAmrs$qfPersAngYaxs,
    #                                "qfRngAngZaxs" = qfInput$soniAmrs$qfRngAngZaxs,
    #                                "qfStepAngZaxs" = qfInput$soniAmrs$qfStepAngZaxs,
    #                                "qfPersAngZaxs" = qfInput$soniAmrs$qfPersAngZaxs)
    #   
    #   setQf$accXaxsDiff <- data.frame("qfRngAccXaxsDiff" = qfInput$soniAmrs$qfRngAccXaxsDiff,
    #                                   "qfStepAccXaxsDiff" = qfInput$soniAmrs$qfStepAccXaxsDiff,
    #                                   "qfPersAccXaxsDiff" = qfInput$soniAmrs$qfPersAccXaxsDiff)
    #   
    #   setQf$accYaxsDiff <- data.frame("qfRngAccYaxsDiff" = qfInput$soniAmrs$qfRngAccYaxsDiff,
    #                                   "qfStepAccYaxsDiff" = qfInput$soniAmrs$qfStepAccYaxsDiff,
    #                                   "qfPersAccYaxsDiff" = qfInput$soniAmrs$qfPersAccYaxsDiff)
    #   
    #   setQf$accZaxsDiff <- data.frame("qfRngAccZaxsDiff" = qfInput$soniAmrs$qfRngAccZaxsDiff,
    #                                   "qfStepAccZaxsDiff" = qfInput$soniAmrs$qfStepAccZaxsDiff,
    #                                   "qfPersAccZaxsDiff" = qfInput$soniAmrs$qfPersAccZaxsDiff)
    # } else {
    #   setQf$soniAmrs <- data.frame("qfAmrsVal" = -1,
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
    # }#close if else statement for qf soniAmrs
    
    #grouping qulity flags that related to L1 sub-data product
    rpt$angZaxsErth <- data.frame(setQf$sensSoni, setQf$veloXaxs,
                                  setQf$veloYaxs, setQf$veloZaxs,
                                  setQf$veloSoni)
    ##TO DO##Considering later when the AMRS is collaborating to correct the SONI data 
    # rpt$angZaxsErth <- data.frame(setQf$sensSoni, setQf$veloXaxs,
    #                               setQf$veloYaxs, setQf$veloZaxs,
    #                               setQf$veloSoni, setQf$soniAmrs)
    
    rpt$tempAir <- data.frame(setQf$sensSoni, setQf$veloSoni, 
                              setQf$tempSoni) #setQf$irga) # Removing until we can handle flags of different lengths
    
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
    #                                setQf$veloSoni, setQf$soniAmrs,
    #                                setQf$accXaxsDiff)
    # 
    # rpt$veloXaxsYaxsErth <- data.frame(setQf$sensSoni, setQf$veloXaxs,
    #                                    setQf$veloYaxs, setQf$veloSoni, 
    #                                    setQf$soniAmrs, setQf$accXaxsDiff,
    #                                    setQf$accYaxsDiff)
    # 
    # rpt$veloYaxsErth <- data.frame(setQf$sensSoni, setQf$veloYaxs,
    #                                setQf$veloSoni, setQf$soniAmrs,
    #                                setQf$accYaxsDiff)
    # 
    # rpt$veloZaxsErth <- data.frame(setQf$sensSoni, setQf$veloZaxs,
    #                                setQf$veloSoni, setQf$soniAmrs,
    #                                setQf$accZaxsDiff)
    #remove setQf
    setQf <- NULL 
    }# close if statement of TypeMeas %in% c("samp", "vali")
  }#close if statement of dp01 == "soni"                       
  
#soniAmrs#################################################################################    
  if (dp01 == "soniAmrs") {
    if (TypeMeas %in% c("samp", "vali")) {
    #organized all quality flags from soni into the set of flags (for frequency use)
    #soni sensor flags
      setQf$sensSoniAmrs <- data.frame("qfAmrsVal" = qfInput$soniAmrs$qfAmrsVal,
                                       "qfAmrsFilt" = qfInput$soniAmrs$qfAmrsFilt,
                                       "qfAmrsVelo" = qfInput$soniAmrs$qfAmrsVelo,
                                       "qfAmrsRng" = qfInput$soniAmrs$qfAmrsRng)
      
      setQf$accXaxs <- data.frame("qfRngAccXaxs" = qfInput$soniAmrs$qfRngAccXaxs,
                                  "qfStepAccXaxs" = qfInput$soniAmrs$qfStepAccXaxs,
                                  "qfPersAccXaxs" = qfInput$soniAmrs$qfPersAccXaxs)
      
      setQf$accYaxs <- data.frame("qfRngAccYaxs" = qfInput$soniAmrs$qfRngAccYaxs,
                                  "qfStepAccYaxs" = qfInput$soniAmrs$qfStepAccYaxs,
                                  "qfPersAccYaxs" = qfInput$soniAmrs$qfPersAccYaxs)
      
      setQf$accZaxs <- data.frame("qfRngAccZaxs" = qfInput$soniAmrs$qfRngAccZaxs,
                                  "qfStepAccZaxs" = qfInput$soniAmrs$qfStepAccZaxs,
                                  "qfPersAccZaxs" = qfInput$soniAmrs$qfPersAccZaxs)
      
      setQf$accXaxsDiff <- data.frame("qfRngAccXaxsDiff" = qfInput$soniAmrs$qfRngAccXaxsDiff,
                                      "qfStepAccXaxsDiff" = qfInput$soniAmrs$qfStepAccXaxsDiff,
                                      "qfPersAccXaxsDiff" = qfInput$soniAmrs$qfPersAccXaxsDiff)
      
      setQf$accYaxsDiff <- data.frame("qfRngAccYaxsDiff" = qfInput$soniAmrs$qfRngAccYaxsDiff,
                                      "qfStepAccYaxsDiff" = qfInput$soniAmrs$qfStepAccYaxsDiff,
                                      "qfPersAccYaxsDiff" = qfInput$soniAmrs$qfPersAccYaxsDiff)
      
      setQf$accZaxsDiff <- data.frame("qfRngAccZaxsDiff" = qfInput$soniAmrs$qfRngAccZaxsDiff,
                                      "qfStepAccZaxsDiff" = qfInput$soniAmrs$qfStepAccZaxsDiff,
                                      "qfPersAccZaxsDiff" = qfInput$soniAmrs$qfPersAccZaxsDiff)
      
      setQf$avelXaxs <- data.frame("qfRngAvelXaxs" = qfInput$soniAmrs$qfRngAvelXaxs,
                                   "qfStepAvelXaxs" = qfInput$soniAmrs$qfStepAvelXaxs,
                                   "qfPersAvelXaxs" = qfInput$soniAmrs$qfPersAvelXaxs)
      
      setQf$avelYaxs <- data.frame("qfRngAvelYaxs" = qfInput$soniAmrs$qfRngAvelYaxs,
                                   "qfStepAvelYaxs" = qfInput$soniAmrs$qfStepAvelYaxs,
                                   "qfPersAvelYaxs" = qfInput$soniAmrs$qfPersAvelYaxs)
      
      setQf$avelZaxs <- data.frame("qfRngAvelZaxs" = qfInput$soniAmrs$qfRngAvelZaxs,
                                   "qfStepAvelZaxs" = qfInput$soniAmrs$qfStepAvelZaxs,
                                   "qfPersAvelZaxs" = qfInput$soniAmrs$qfPersAvelZaxs)
      
      setQf$angXaxs <- data.frame("qfRngAngXaxs" = qfInput$soniAmrs$qfRngAngXaxs,
                                  "qfStepAngXaxs" = qfInput$soniAmrs$qfStepAngXaxs,
                                  "qfPersAngXaxs" = qfInput$soniAmrs$qfPersAngXaxs)
      
      setQf$angYaxs <- data.frame("qfRngAngYaxs" = qfInput$soniAmrs$qfRngAngYaxs,
                                  "qfStepAngYaxs" = qfInput$soniAmrs$qfStepAngYaxs,
                                  "qfPersAngYaxs" = qfInput$soniAmrs$qfPersAngYaxs)
      
      setQf$angZaxs <- data.frame("qfRngAngZaxs" = qfInput$soniAmrs$qfRngAngZaxs,
                                  "qfStepAngZaxs" = qfInput$soniAmrs$qfStepAngZaxs,
                                  "qfPersAngZaxs" = qfInput$soniAmrs$qfPersAngZaxs)
    
    #grouping qulity flags that related to L1 sub-data product
      rpt$angNedXaxs <- data.frame(setQf$sensSoniAmrs, setQf$angXaxs)
      rpt$angNedYaxs <- data.frame(setQf$sensSoniAmrs, setQf$angYaxs)
      rpt$angNedZaxs <- data.frame(setQf$sensSoniAmrs, setQf$angZaxs)
    }#close if statement of TypeMeas %in% c("samp", "vali")
  } #close if statement of dp01 == "soniAmrs"
} #close if statement of  MethMeas == "ecse"

# ecse #######################################################################################
if (MethMeas == "ecse") {  
#irgaCo2 and irgaH2o####################################################################################
  if (dp01 %in% c("irgaCo2", "irgaH2o")) { 
    #check if data are exist
    #external quality flags from envHut
    if (!("envHut" %in% names(qfInput)) || length(which(!is.na(qfInput$irga$qfRngTemp))) == 0){
      qfInput$envHut <- as.data.frame(matrix(-1, ncol = 1, nrow = length(qfInput$irga$qfRngAsrpCo2)))
      names(qfInput$envHut) <- "qfTemp"}
    #external quality flags from valvAux
    if (!("valvAux" %in% names(qfInput)) || length(which(!is.na(qfInput$irga$qfRngTemp))) == 0){
      qfInput$valvAux <- as.data.frame(matrix(-1, ncol = 1, nrow = length(qfInput$irga$qfRngAsrpCo2)))
      names(qfInput$valvAux) <- "qfValvIrga"}
    #external quality flags from heatInlt
    if (!("heatInlt" %in% names(qfInput)) || length(which(!is.na(qfInput$irga$qfRngTemp))) == 0){
      qfInput$heatInlt <- as.data.frame(matrix(-1, ncol = 1, nrow = length(qfInput$irga$qfRngAsrpCo2)))
      names(qfInput$heatInlt) <- "qfHeat"}
    #external quality flags from irgaMfcSamp
    if (!("irgaMfcSamp" %in% names(qfInput)) || length(which(!is.na(qfInput$irga$qfRngTemp))) == 0){
      qfInput$irgaMfcSamp <- as.data.frame(matrix(-1, ncol = 13, nrow = length(qfInput$irga$qfRngAsrpCo2)))
      names(qfInput$irgaMfcSamp) <- c("qfRngFrt00", "qfStepFrt00", "qfPersFrt00", "qfRngFrt", "qfStepFrt", "qfPersFrt",
                                      "qfRngPresAtm", "qfStepPresAtm", "qfPersPresAtm", "qfRngTemp", "qfStepTemp", "qfPersTemp",
                                      "qfFrt00")}
    #external quality flags from mfcVali
    if (!("mfcVali" %in% names(qfInput)) || length(which(!is.na(qfInput$irga$qfRngTemp))) == 0){
      qfInput$mfcVali <- as.data.frame(matrix(-1, ncol = 13, nrow = length(qfInput$irga$qfRngAsrpCo2)))
      names(qfInput$mfcVali) <- c("qfRngFrt00", "qfStepFrt00", "qfPersFrt00", "qfRngFrt", "qfStepFrt", "qfPersFrt",
                                  "qfRngPresAtm", "qfStepPresAtm", "qfPersPresAtm", "qfRngTemp", "qfStepTemp", "qfPersTemp",
                                  "qfFrt00")}
    #replace -1 if all qf in irga are NA
    if (length(which(!is.na(qfInput$irga$qfRngTemp))) == 0){
      qfInput$irga[,1:length(qfInput$irga)] <- -1
    }
    # #get the flag names
    # qfName <- list()
    # qfName$irga <- names(qfInput$irga)
    # qfName$irgaMfcSamp <- names(qfInput$irgaMfcSamp)
    # qfName$envHut <- names(qfInput$envHut)
    # qfName$valvAux <- names(qfInput$valvAux)
    # qfName$heatInlt <- names(qfInput$heatInlt)
    # qfName$mfcVali <- names(qfInput$mfcVali)
    # 
    # #combine qf dataframe and get rid of NA
    # if (TypeMeas == "samp"){
    #   qfComb <- na.omit(data.frame(qfInput$irga, qfInput$irgaMfcSamp, 
    #                                qfInput$envHut, qfInput$valvAux,
    #                                qfInput$heatInlt))
    #   #put the data back to its own list
    #   qfInput$irga <- data.frame(qfComb[,1:length(qfInput$irga)])
    #   qfInput$irgaMfcSamp <- data.frame(qfComb[,(length(qfInput$irga)+1):(length(qfInput$irga)+length(qfInput$irga))])
    #   qfInput$envHut <- data.frame(qfComb[,(length(qfInput$irga)+length(qfInput$irga)+1):
    #                                         (length(qfInput$irga)+length(qfInput$irga)+length(qfInput$envHut))])
    #   qfInput$valvAux <- data.frame(qfComb[,(length(qfInput$irga)+length(qfInput$irga)+length(qfInput$envHut)+1):
    #                                         (length(qfInput$irga)+length(qfInput$irga)+length(qfInput$envHut)+length(qfInput$valvAux))])
    #   qfInput$heatInlt <- data.frame(qfComb[,(length(qfInput$irga)+length(qfInput$irga)+length(qfInput$envHut)+length(qfInput$valvAux)+1):
    #                                          (length(qfInput$irga)+length(qfInput$irga)+length(qfInput$envHut)+length(qfInput$valvAux)+length(qfInput$heatInlt))])
    #   #assign names
    #   names(qfInput$irga) <- qfName$irga
    #   names(qfInput$irgaMfcSamp) <- qfName$irgaMfcSamp
    #   names(qfInput$envHut) <- qfName$envHut
    #   names(qfInput$valvAux) <- qfName$valvAux
    #   names(qfInput$heatInlt) <- qfName$heatInlt
    #   
    # }#close if statement
    # if (TypeMeas == "vali"){
    #   qfComb <- na.omit(data.frame(qfInput$irga, qfInput$irgaMfcSamp, 
    #                                qfInput$envHut, qfInput$valvAux,
    #                                qfInput$heatInlt))
    #   #put the data back to its own list
    #   qfInput$irga <- data.frame(qfComb[,1:length(qfInput$irga)])
    #   qfInput$irgaMfcSamp <- data.frame(qfComb[,(length(qfInput$irga)+1):(length(qfInput$irga)+length(qfInput$irga))])
    #   qfInput$envHut <- data.frame(qfComb[,(length(qfInput$irga)+length(qfInput$irga)+1):
    #                                         (length(qfInput$irga)+length(qfInput$irga)+length(qfInput$envHut))])
    #   qfInput$valvAux <- data.frame(qfComb[,(length(qfInput$irga)+length(qfInput$irga)+length(qfInput$envHut)+1):
    #                                          (length(qfInput$irga)+length(qfInput$irga)+length(qfInput$envHut)+length(qfInput$valvAux))])
    #   qfInput$heatInlt <- data.frame(qfComb[,(length(qfInput$irga)+length(qfInput$irga)+length(qfInput$envHut)+length(qfInput$valvAux)+1):
    #                                           (length(qfInput$irga)+length(qfInput$irga)+length(qfInput$envHut)+length(qfInput$valvAux)+length(qfInput$heatInlt))])
    #   #assign names
    #   names(qfInput$irga) <- qfName$irga
    #   names(qfInput$irgaMfcSamp) <- qfName$irgaMfcSamp
    #   names(qfInput$envHut) <- qfName$envHut
    #   names(qfInput$valvAux) <- qfName$valvAux
    #   names(qfInput$heatInlt) <- qfName$heatInlt
    #   
    # }#close if statement
    #grouping the flags
    
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
                                       "qfCalRtioMoleWetCo2" = qfInput$irga$qfCalRtioMoleWetCo2)
    
    setQf$rtioMoleWetH2o <- data.frame("qfRngRtioMoleWetH2o" = qfInput$irga$qfRngRtioMoleWetH2o,
                                       "qfStepRtioMoleWetH2o" = qfInput$irga$qfStepRtioMoleWetH2o,
                                       "qfPersRtioMoleWetH2o" = qfInput$irga$qfPersRtioMoleWetH2o,
                                       "qfCalRtioMoleWetH2o" = qfInput$irga$qfCalRtioMoleWetH2o)
    
    setQf$presIrga <- data.frame("qfRngPres" = qfInput$irga$qfRngPres, 
                                 "qfStepPres" = qfInput$irga$qfStepPres,
                                 "qfPersPres" = qfInput$irga$qfPersPres, 
                                 "qfCalPres" = qfInput$irga$qfCalPres)
    
    setQf$tempIrga <- data.frame ("qfRngTemp" = qfInput$irga$qfRngTemp, 
                                  "qfStepTemp" = qfInput$irga$qfStepTemp,
                                  "qfPersTemp" = qfInput$irga$qfPersTemp, 
                                  "qfCalTemp" = qfInput$irga$qfCalTemp)
    #external quality flags from envHut
    setQf$envHut <- data.frame("qfTemp" = qfInput$envHut$qfTemp)
    #external quality flags from valvAux
    setQf$valvAux <- data.frame("qfValvIrga" = qfInput$valvAux$qfValvIrga)
    #external quality flags from heatInlt
    setQf$heatInlt <- data.frame("qfHeat" = qfInput$heatInlt$qfHeat)
    #external quality flags from irgaMfcSamp
    setQf$frt00IrgaMfcSamp <- data.frame("qfRngFrt00" = qfInput$irgaMfcSamp$qfRngFrt00,
                                         "qfStepFrt00" = qfInput$irgaMfcSamp$qfStepFrt00,
                                         "qfPersFrt00" = qfInput$irgaMfcSamp$qfPersFrt00)
    
    setQf$frtIrgaMfcSamp <- data.frame("qfRngFrt" = qfInput$irgaMfcSamp$qfRngFrt,
                                       "qfStepFrt" = qfInput$irgaMfcSamp$qfStepFrt,
                                       "qfPersFrt" = qfInput$irgaMfcSamp$qfPersFrt)
    
    setQf$presAtmIrgaMfcSamp <- data.frame("qfRngPresAtm" = qfInput$irgaMfcSamp$qfRngPresAtm,
                                           "qfStepPresAtm" = qfInput$irgaMfcSamp$qfStepPresAtm,
                                           "qfPersPresAtm" = qfInput$irgaMfcSamp$qfPersPresAtm)
    
    setQf$tempIrgaMfcSamp <- data.frame("qfRngTemp" = qfInput$irgaMfcSamp$qfRngTemp,
                                        "qfStepTemp" = qfInput$irgaMfcSamp$qfStepTemp,
                                        "qfPersTemp" = qfInput$irgaMfcSamp$qfPersTemp)
    
    setQf$sensIrgaMfcSamp <- data.frame("qfFrt00" = qfInput$irgaMfcSamp$qfFrt00)
    
    #external quality flags from mfcVali
    setQf$frt00MfcVali <- data.frame("qfRngFrt00" = qfInput$mfcVali$qfRngFrt00,
                                     "qfStepFrt00" = qfInput$mfcVali$qfStepFrt00,
                                     "qfPersFrt00" = qfInput$mfcVali$qfPersFrt00)
    
    setQf$frtMfcVali <- data.frame("qfRngFrt" = qfInput$mfcVali$qfRngFrt,
                                   "qfStepFrt" = qfInput$mfcVali$qfStepFrt,
                                   "qfPersFrt" = qfInput$mfcVali$qfPersFrt)
    
    setQf$presAtmMfcVali <- data.frame("qfRngPresAtm" = qfInput$mfcVali$qfRngPresAtm,
                                       "qfStepPresAtm" = qfInput$mfcVali$qfStepPresAtm,
                                       "qfPersPresAtm" = qfInput$mfcVali$qfPersPresAtm)
    
    setQf$tempMfcVali <- data.frame("qfRngTemp" = qfInput$mfcVali$qfRngTemp,
                                    "qfStepTemp" = qfInput$mfcVali$qfStepTemp,
                                    "qfPersTemp" = qfInput$mfcVali$qfPersTemp)
    
    setQf$sensMfcVali <- data.frame("qfFrt00" = qfInput$mfcVali$qfFrt00)
    
    
    #grouping qulity flags that related to irgaCo2 L1 sub-data product
    if (dp01 == "irgaCo2"){
      if (TypeMeas == "samp"){
        rpt$rtioMoleDryCo2 <- na.omit(data.frame(setQf$rtioMoleDryCo2, setQf$asrpCo2,
                                                 setQf$asrpH2o, setQf$rtioMoleWetCo2,
                                                 setQf$rtioMoleWetH2o, setQf$presIrga,
                                                 setQf$tempIrga, setQf$envHut, 
                                                 setQf$valvAux, setQf$heatInlt,
                                                 setQf$frt00IrgaMfcSamp, setQf$frtIrgaMfcSamp, 
                                                 setQf$presAtmIrgaMfcSamp, setQf$tempIrgaMfcSamp,
                                                 setQf$sensIrgaMfcSamp))
        
        rpt$rtioMoleWetCo2 <- na.omit(data.frame(setQf$rtioMoleWetCo2, setQf$asrpCo2,
                                                 setQf$asrpH2o, setQf$rtioMoleWetH2o, 
                                                 setQf$presIrga, setQf$tempIrga, 
                                                 setQf$envHut, setQf$valvAux, 
                                                 setQf$heatInlt, setQf$frt00IrgaMfcSamp, 
                                                 setQf$frtIrgaMfcSamp, setQf$presAtmIrgaMfcSamp, 
                                                 setQf$tempIrgaMfcSamp, setQf$sensIrgaMfcSamp))
      }#close if statement of TypeMeas == "samp"
      
      if (TypeMeas == "vali"){
        rpt$rtioMoleDryCo2 <- na.omit(data.frame(setQf$rtioMoleDryCo2, setQf$asrpCo2,
                                                 setQf$asrpH2o, setQf$rtioMoleWetCo2,
                                                 setQf$rtioMoleWetH2o, setQf$presIrga,
                                                 setQf$tempIrga, setQf$envHut, 
                                                 setQf$valvAux, setQf$frt00IrgaMfcSamp, 
                                                 setQf$frtIrgaMfcSamp, setQf$presAtmIrgaMfcSamp, 
                                                 setQf$tempIrgaMfcSamp, setQf$sensIrgaMfcSamp,
                                                 setQf$frt00MfcVali, setQf$frtMfcVali, 
                                                 setQf$presAtmMfcVali, setQf$tempMfcVali, 
                                                 setQf$sensMfcVali))
        
        rpt$rtioMoleWetCo2 <- na.omit(data.frame(setQf$rtioMoleWetCo2, setQf$asrpCo2,
                                                 setQf$asrpH2o, setQf$rtioMoleWetH2o, 
                                                 setQf$presIrga, setQf$tempIrga, 
                                                 setQf$envHut, setQf$valvAux, 
                                                 setQf$frt00IrgaMfcSamp, setQf$frtIrgaMfcSamp, 
                                                 setQf$presAtmIrgaMfcSamp, setQf$tempIrgaMfcSamp, 
                                                 setQf$sensIrgaMfcSamp,setQf$frt00MfcVali, 
                                                 setQf$frtMfcVali, setQf$presAtmMfcVali, 
                                                 setQf$tempMfcVali, setQf$sensMfcVali))
      }#close if statement of TypeMeas == "vali"
      
      rpt$pres <- na.omit(data.frame(setQf$presIrga))
      
      rpt$frt00 <- na.omit(data.frame (setQf$frt00IrgaMfcSamp, setQf$frtIrgaMfcSamp,
                                       setQf$presAtmIrgaMfcSamp, setQf$tempIrgaMfcSamp,
                                       setQf$sensIrgaMfcSamp))
      
      rpt$temp <- na.omit(data.frame (setQf$tempIrga))
    }#close if statement of dp01 == "irgaCo2"
    #grouping qulity flags that related to irgaH2o L1 sub-data product    
    if (dp01 == "irgaH2o") {
      if (TypeMeas == "samp"){
        rpt$rtioMoleDryH2o <- na.omit(data.frame(setQf$rtioMoleDryH2o, setQf$asrpH2o,
                                                 setQf$rtioMoleWetH2o, setQf$presIrga,
                                                 setQf$tempIrga, setQf$envHut, 
                                                 setQf$valvAux, setQf$heatInlt, 
                                                 setQf$frt00IrgaMfcSamp, setQf$frtIrgaMfcSamp, 
                                                 setQf$presAtmIrgaMfcSamp, setQf$tempIrgaMfcSamp, 
                                                 setQf$sensIrgaMfcSamp))
        
        rpt$rtioMoleWetH2o <- na.omit(data.frame(setQf$rtioMoleWetH2o, setQf$asrpH2o,
                                                 setQf$presIrga, setQf$tempIrga,
                                                 setQf$envHut, setQf$valvAux, 
                                                 setQf$heatInlt, setQf$frt00IrgaMfcSamp, 
                                                 setQf$frtIrgaMfcSamp, setQf$presAtmIrgaMfcSamp, 
                                                 setQf$tempIrgaMfcSamp, setQf$sensIrgaMfcSamp))
      }#close if statement of TypeMeas == "samp"
      
      if (TypeMeas == "vali"){
        rpt$rtioMoleDryH2o <- na.omit(data.frame(setQf$rtioMoleDryH2o, setQf$asrpH2o,
                                                 setQf$rtioMoleWetH2o, setQf$presIrga,
                                                 setQf$tempIrga, setQf$envHut, 
                                                 setQf$valvAux, setQf$frt00IrgaMfcSamp, 
                                                 setQf$frtIrgaMfcSamp, setQf$presAtmIrgaMfcSamp, 
                                                 setQf$tempIrgaMfcSamp, setQf$sensIrgaMfcSamp,
                                                 setQf$frt00MfcVali, setQf$frtMfcVali, 
                                                 setQf$presAtmMfcVali, setQf$tempMfcVali, 
                                                 setQf$sensMfcVali))
        
        rpt$rtioMoleWetH2o <- na.omit(data.frame(setQf$rtioMoleWetH2o, setQf$asrpH2o,
                                                 setQf$presIrga, setQf$tempIrga, 
                                                 setQf$envHut, setQf$valvAux, 
                                                 setQf$frt00IrgaMfcSamp, setQf$frtIrgaMfcSamp, 
                                                 setQf$presAtmIrgaMfcSamp, setQf$tempIrgaMfcSamp, 
                                                 setQf$sensIrgaMfcSamp,setQf$frt00MfcVali, 
                                                 setQf$frtMfcVali, setQf$presAtmMfcVali, 
                                                 setQf$tempMfcVali, setQf$sensMfcVali))
        
      }#close if statement of TypeMeas == "vali"
      rpt$pres <- na.omit(data.frame(setQf$presIrga))
      
      rpt$frt00 <- na.omit(data.frame(setQf$frt00IrgaMfcSamp, setQf$frtIrgaMfcSamp, 
                                      setQf$presAtmIrgaMfcSamp, setQf$tempIrgaMfcSamp, 
                                      setQf$sensIrgaMfcSamp)) 
      
      rpt$temp <- na.omit(data.frame(setQf$tempIrga))
    }#close if statement of dp01 == "irgaH2o"
    #remove setQf
    setQf <- NULL
  }##close if statement of dp01 %in% c("irgaCo2", "irgaH2o")
  
#isoCo2 ####################################################################################
  if (dp01 == "isoCo2") {
    
    #check if data are exist
    #external quality flags from heatInlt
    if (!("heatInlt" %in% names(qfInput)) || length(which(!is.na(qfInput$crdCo2$qfRngTemp))) == 0){
      qfInput$heatInlt <- as.data.frame(matrix(-1, ncol = 1, nrow = length(qfInput$crdCo2$qfRngRtioMoleDryCo2)))
      names(qfInput$heatInlt) <- "qfHeat"}
    
    #external quality flags from mfcVali
    if (!("mfcVali" %in% names(qfInput)) || length(which(!is.na(qfInput$crdCo2$qfRngTemp))) == 0){
      qfInput$mfcVali <- as.data.frame(matrix(-1, ncol = 13, nrow = length(qfInput$crdCo2$qfRngRtioMoleDryCo2)))
      names(qfInput$mfcVali) <- c("qfRngFrt00", "qfStepFrt00", "qfPersFrt00", "qfRngFrt", "qfStepFrt", "qfPersFrt",
                                  "qfRngPresAtm", "qfStepPresAtm", "qfPersPresAtm", "qfRngTemp", "qfStepTemp", "qfPersTemp",
                                  "qfFrt00")}
    #replace -1 if all qf in crdCo2 are NA
    if (length(which(!is.na(qfInput$crdCo2$qfRngTemp))) == 0){
      qfInput$crdCo2[,1:length(qfInput$crdCo2)] <- -1
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
    # setQf$exisCo2 <- data.frame("qfExisCo2" = qfInput$crdCo2$qfExisCo2)
    # setQf$exisH2o <- data.frame("qfExisH2o" = qfInput$crdCo2$qfExisH2o)
    
    #setQf from heatInlt
    setQf$heatInlt <- data.frame("qfHeat" = qfInput$heatInlt$qfHeat)
    
    #setQf from mfcVali
    setQf$frt00MfcVali <- data.frame("qfRngFrt00" = qfInput$mfcVali$qfRngFrt00, 
                                     "qfStepFrt00" = qfInput$mfcVali$qfStepFrt00, 
                                     "qfPersFrt00" = qfInput$mfcVali$qfPersFrt00)
    
    setQf$frtMfcVali <- data.frame("qfRngFrt" = qfInput$mfcVali$qfRngFrt,
                                   "qfStepFrt" = qfInput$mfcVali$qfStepFrt,
                                   "qfPersFrt" = qfInput$mfcVali$qfPersFrt)
    
    setQf$presAtmMfcVali <- data.frame("qfRngPresAtm" = qfInput$mfcVali$qfRngPresAtm, 
                                       "qfStepPresAtm" = qfInput$mfcVali$qfStepPresAtm,
                                       "qfPersPresAtm" = qfInput$mfcVali$qfPersPresAtm)
    
    setQf$tempMfcVali <- data.frame("qfRngTemp" = qfInput$mfcVali$qfRngTemp,
                                    "qfStepTemp" = qfInput$mfcVali$qfStepTemp,
                                    "qfPersTemp" = qfInput$mfcVali$qfPersTemp)
    
    setQf$sensMfcVali <- data.frame("qfFrt00" = qfInput$mfcVali$qfFrt00)
    
    
    #define qf which use only sampling period
    if (TypeMeas == "samp") {
      #if nat all idGas = NA or all qfRngTemp = NA
      if (length(which(!is.na(qfInput$crdCo2$qfRngTemp))) == 0){
        #grouping qulity flags that related to isoCo2 L1 sub-data product  
        rpt$rtioMoleWetCo2 <- na.omit(data.frame(setQf$rtioMoleWetCo2, setQf$dlta13CCo2,
                                                 setQf$rtioMoleWet12CCo2, setQf$presCrdCo2, 
                                                 setQf$tempCrdCo2, setQf$tempWbox,
                                                 setQf$sensCrdCo2, setQf$heatInlt)[which(idGas == 105),])
        
        rpt$rtioMoleDryCo2 <- na.omit(data.frame(setQf$rtioMoleDryCo2, setQf$dlta13CCo2,
                                                 setQf$rtioMoleWet12CCo2, setQf$presCrdCo2, 
                                                 setQf$tempCrdCo2, setQf$tempWbox,
                                                 setQf$sensCrdCo2, setQf$heatInlt)[which(idGas == 105),])
        
        rpt$rtioMoleWet12CCo2 <- na.omit(data.frame(setQf$rtioMoleWet12CCo2, setQf$presCrdCo2, 
                                                    setQf$tempCrdCo2, setQf$tempWbox,
                                                    setQf$sensCrdCo2, setQf$heatInlt)[which(idGas == 105),])
        
        rpt$rtioMoleDry12CCo2 <- na.omit(data.frame(setQf$rtioMoleDry12CCo2, setQf$rtioMoleWet13CCo2,
                                                    setQf$presCrdCo2, setQf$tempCrdCo2, 
                                                    setQf$tempWbox, setQf$sensCrdCo2, 
                                                    setQf$heatInlt)[which(idGas == 105),])
        
        rpt$rtioMoleWet13CCo2 <- na.omit(data.frame(setQf$rtioMoleWet13CCo2, setQf$presCrdCo2, 
                                                    setQf$tempCrdCo2, setQf$tempWbox,
                                                    setQf$sensCrdCo2, setQf$heatInlt)[which(idGas == 105),])
        
        rpt$rtioMoleDry13CCo2 <- na.omit(data.frame(setQf$rtioMoleDry13CCo2, setQf$rtioMoleWet13CCo2,
                                                    setQf$presCrdCo2, setQf$tempCrdCo2, 
                                                    setQf$tempWbox, setQf$sensCrdCo2, 
                                                    setQf$heatInlt)[which(idGas == 105),])
        
        rpt$dlta13CCo2 <- na.omit(data.frame(setQf$dlta13CCo2, setQf$presCrdCo2,
                                             setQf$tempCrdCo2, setQf$tempWbox,
                                             setQf$sensCrdCo2, setQf$heatInlt)[which(idGas == 105),])
        
        rpt$rtioMoleWetH2o <- na.omit(data.frame(setQf$rtioMoleWetH2o, setQf$presCrdCo2,
                                                 setQf$tempCrdCo2, setQf$tempWbox,
                                                 setQf$sensCrdCo2, setQf$heatInlt)[which(idGas == 11),])
        
        rpt$rtioMoleDryH2o <- na.omit(data.frame(setQf$rtioMoleDryH2o, setQf$rtioMoleWetH2o, 
                                                 setQf$presCrdCo2, setQf$tempCrdCo2, 
                                                 setQf$tempWbox, setQf$sensCrdCo2, 
                                                 setQf$heatInlt)[which(idGas == 11),])
        
        rpt$temp <- na.omit(data.frame(setQf$tempCrdCo2, setQf$sensCrdCo2))
        
        rpt$pres <- na.omit(data.frame(setQf$presCrdCo2, setQf$sensCrdCo2))
      } else {
        #grouping qulity flags that related to isoCo2 L1 sub-data product  
        rpt$rtioMoleWetCo2 <- na.omit(data.frame(setQf$rtioMoleWetCo2, setQf$dlta13CCo2,
                                                 setQf$rtioMoleWet12CCo2, setQf$presCrdCo2, 
                                                 setQf$tempCrdCo2, setQf$tempWbox,
                                                 setQf$sensCrdCo2, setQf$heatInlt))
        
        rpt$rtioMoleDryCo2 <- na.omit(data.frame(setQf$rtioMoleDryCo2, setQf$dlta13CCo2,
                                                 setQf$rtioMoleWet12CCo2, setQf$presCrdCo2, 
                                                 setQf$tempCrdCo2, setQf$tempWbox,
                                                 setQf$sensCrdCo2, setQf$heatInlt))
        
        rpt$rtioMoleWet12CCo2 <- na.omit(data.frame(setQf$rtioMoleWet12CCo2, setQf$presCrdCo2, 
                                                    setQf$tempCrdCo2, setQf$tempWbox,
                                                    setQf$sensCrdCo2, setQf$heatInlt))
        
        rpt$rtioMoleDry12CCo2 <- na.omit(data.frame(setQf$rtioMoleDry12CCo2, setQf$rtioMoleWet13CCo2,
                                                    setQf$presCrdCo2, setQf$tempCrdCo2, 
                                                    setQf$tempWbox, setQf$sensCrdCo2, 
                                                    setQf$heatInlt))
        
        rpt$rtioMoleWet13CCo2 <- na.omit(data.frame(setQf$rtioMoleWet13CCo2, setQf$presCrdCo2, 
                                                    setQf$tempCrdCo2, setQf$tempWbox,
                                                    setQf$sensCrdCo2, setQf$heatInlt))
        
        rpt$rtioMoleDry13CCo2 <- na.omit(data.frame(setQf$rtioMoleDry13CCo2, setQf$rtioMoleWet13CCo2,
                                                    setQf$presCrdCo2, setQf$tempCrdCo2, 
                                                    setQf$tempWbox, setQf$sensCrdCo2, 
                                                    setQf$heatInlt))
        
        rpt$dlta13CCo2 <- na.omit(data.frame(setQf$dlta13CCo2, setQf$presCrdCo2,
                                             setQf$tempCrdCo2, setQf$tempWbox,
                                             setQf$sensCrdCo2, setQf$heatInlt))
        
        rpt$rtioMoleWetH2o <- na.omit(data.frame(setQf$rtioMoleWetH2o, setQf$presCrdCo2,
                                                 setQf$tempCrdCo2, setQf$tempWbox,
                                                 setQf$sensCrdCo2, setQf$heatInlt))
        
        rpt$rtioMoleDryH2o <- na.omit(data.frame(setQf$rtioMoleDryH2o, setQf$rtioMoleWetH2o, 
                                                 setQf$presCrdCo2, setQf$tempCrdCo2, 
                                                 setQf$tempWbox, setQf$sensCrdCo2, 
                                                 setQf$heatInlt))
        
        rpt$temp <- na.omit(data.frame(setQf$tempCrdCo2, setQf$sensCrdCo2))
        
        rpt$pres <- na.omit(data.frame(setQf$presCrdCo2, setQf$sensCrdCo2))  
      }# close else statement
      
    }#close if statement of TypeMeas == "samp"
    
    #define qf which use only validation period
    if (TypeMeas == "vali") { 
      #if nat all idGas = NA or all qfRngTemp = NA
      if (length(which(!is.na(qfInput$crdCo2$qfRngTemp))) == 0){
        #grouping qulity flags that related to isoCo2 L1 sub-data product  
        rpt$rtioMoleWetCo2 <- na.omit(data.frame(setQf$rtioMoleWetCo2, setQf$dlta13CCo2,
                                                 setQf$rtioMoleWet12CCo2, setQf$presCrdCo2, 
                                                 setQf$tempCrdCo2, setQf$tempWbox,
                                                 setQf$sensCrdCo2, setQf$frt00MfcVali, 
                                                 setQf$frtMfcVali, setQf$presAtmMfcVali, 
                                                 setQf$tempMfcVali,setQf$sensMfcVali)[which(idGas == 105),])
        
        rpt$rtioMoleDryCo2 <- na.omit(data.frame(setQf$rtioMoleDryCo2, setQf$dlta13CCo2,
                                                 setQf$rtioMoleWet12CCo2, setQf$presCrdCo2, 
                                                 setQf$tempCrdCo2, setQf$tempWbox,
                                                 setQf$sensCrdCo2, setQf$frt00MfcVali, 
                                                 setQf$frtMfcVali, setQf$presAtmMfcVali, 
                                                 setQf$tempMfcVali, setQf$sensMfcVali)[which(idGas == 105),])
        
        rpt$rtioMoleWet12CCo2 <- na.omit(data.frame(setQf$rtioMoleWet12CCo2, setQf$presCrdCo2, 
                                                    setQf$tempCrdCo2, setQf$tempWbox,
                                                    setQf$sensCrdCo2, setQf$frt00MfcVali, 
                                                    setQf$frtMfcVali, setQf$presAtmMfcVali, 
                                                    setQf$tempMfcVali, setQf$sensMfcVali)[which(idGas == 105),])
        
        rpt$rtioMoleDry12CCo2 <- na.omit(data.frame(setQf$rtioMoleDry12CCo2, setQf$rtioMoleWet13CCo2,
                                                    setQf$presCrdCo2, setQf$tempCrdCo2, 
                                                    setQf$tempWbox, setQf$sensCrdCo2, 
                                                    setQf$frt00MfcVali, setQf$frtMfcVali, 
                                                    setQf$presAtmMfcVali, setQf$tempMfcVali, 
                                                    setQf$sensMfcVali)[which(idGas == 105),])
        
        rpt$rtioMoleWet13CCo2 <- na.omit(data.frame(setQf$rtioMoleWet13CCo2, setQf$presCrdCo2, 
                                                    setQf$tempCrdCo2, setQf$tempWbox,
                                                    setQf$sensCrdCo2, setQf$frt00MfcVali, 
                                                    setQf$frtMfcVali, setQf$presAtmMfcVali, 
                                                    setQf$tempMfcVali, setQf$sensMfcVali)[which(idGas == 105),])
        
        rpt$rtioMoleDry13CCo2 <- na.omit(data.frame(setQf$rtioMoleDry13CCo2, setQf$rtioMoleWet13CCo2,
                                                    setQf$presCrdCo2, setQf$tempCrdCo2, 
                                                    setQf$tempWbox, setQf$sensCrdCo2, 
                                                    setQf$frt00MfcVali, setQf$frtMfcVali, 
                                                    setQf$presAtmMfcVali, setQf$tempMfcVali, 
                                                    setQf$sensMfcVali)[which(idGas == 105),])
        
        rpt$dlta13CCo2 <- na.omit(data.frame(setQf$dlta13CCo2, setQf$presCrdCo2,
                                             setQf$tempCrdCo2, setQf$tempWbox,
                                             setQf$sensCrdCo2, setQf$frt00MfcVali, 
                                             setQf$frtMfcVali, setQf$presAtmMfcVali, 
                                             setQf$tempMfcVali, setQf$sensMfcVali)[which(idGas == 105),])
        
        rpt$rtioMoleWetH2o <- na.omit(data.frame(setQf$rtioMoleWetH2o, setQf$presCrdCo2,
                                                 setQf$tempCrdCo2, setQf$tempWbox,
                                                 setQf$sensCrdCo2, setQf$frt00MfcVali, 
                                                 setQf$frtMfcVali, setQf$presAtmMfcVali, 
                                                 setQf$tempMfcVali, setQf$sensMfcVali)[which(idGas == 11),])
        
        rpt$rtioMoleDryH2o <- na.omit(data.frame(setQf$rtioMoleDryH2o, setQf$rtioMoleWetH2o, 
                                                 setQf$presCrdCo2, setQf$tempCrdCo2, 
                                                 setQf$tempWbox, setQf$sensCrdCo2, 
                                                 setQf$frt00MfcVali, setQf$frtMfcVali, 
                                                 setQf$presAtmMfcVali, setQf$tempMfcVali, 
                                                 setQf$sensMfcVali)[which(idGas == 11),])
        
        rpt$temp <- na.omit(data.frame(setQf$tempCrdCo2, setQf$sensCrdCo2))
        
        rpt$pres <- na.omit(data.frame(setQf$presCrdCo2, setQf$sensCrdCo2))
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
      qfInput$envHut <- as.data.frame(matrix(-1, ncol = 1, nrow = length(qfInput$crdH2o$qfRngRtioMoleDryH2o)))
      names(qfInput$envHut) <- "qfRh"}
    
    #external quality flags from heatInlt
    if (!("heatInlt" %in% names(qfInput)) || length(which(!is.na(qfInput$crdH2o$qfRngTemp))) == 0){
      qfInput$heatInlt <- as.data.frame(matrix(-1, ncol = 1, nrow = length(qfInput$crdH2o$qfRngRtioMoleDryH2o)))
      names(qfInput$heatInlt) <- "qfHeat"}
    
    #replace -1 if all qf in crdH2o are NA
    if (length(which(!is.na(qfInput$crdH2o$qfRngTemp))) == 0){
      qfInput$crdH2o[,1:length(qfInput$crdH2o)] <- -1
    }
    
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
    #setQf of envHut
    setQf$envHut <- data.frame("qfRh" = qfInput$envHut$qfRh)
    
    #setQf of heatInlt
    setQf$heatInlt <- data.frame("qfHeat" = qfInput$heatInlt$qfHeat)
    
    #define qf which use only sampling period
    if (TypeMeas == "samp") {     
      #grouping qulity flags that related to isopH2o L1 sub-data product  
      rpt$rtioMoleDryH2o <- na.omit(data.frame(setQf$rtioMoleDryH2o, setQf$rtioMoleWetH2o,
                                               setQf$presCrdH2o, setQf$tempCrdH2o,
                                               setQf$tempWbox,  setQf$sensCrdH2o,
                                               setQf$envHut, setQf$heatInlt))
      
      rpt$rtioMoleWetH2o <- na.omit(data.frame(setQf$rtioMoleWetH2o, setQf$presCrdH2o, 
                                               setQf$tempCrdH2o, setQf$tempWbox,  
                                               setQf$sensCrdH2o, setQf$envHut, 
                                               setQf$heatInlt))
      
      rpt$dlta18OH2o <- na.omit(data.frame(setQf$dlta18OH2o, setQf$presCrdH2o, 
                                           setQf$tempCrdH2o, setQf$tempWbox,  
                                           setQf$sensCrdH2o, setQf$envHut, 
                                           setQf$heatInlt))
      
      rpt$dlta2HH2o <- na.omit(data.frame(setQf$dlta2HH2o, setQf$presCrdH2o, 
                                          setQf$tempCrdH2o, setQf$tempWbox,  
                                          setQf$sensCrdH2o, setQf$envHut, 
                                          setQf$heatInlt))
      
      rpt$pres <- na.omit(data.frame(setQf$presCrdH2o, qfSensStus = setQf$sensCrdH2o$qfSensStus))
      
      rpt$temp <- na.omit(data.frame(setQf$tempCrdH2o, qfSensStus = setQf$sensCrdH2o$qfSensStus)) 
    }#close if statement of TypeMeas == "samp"
    
    #define qf which use only validation period
    if (TypeMeas == "vali") {     
      #grouping qulity flags that related to isopH2o L1 sub-data product  
      rpt$rtioMoleDryH2o <- na.omit(data.frame(setQf$rtioMoleDryH2o, setQf$rtioMoleWetH2o,
                                               setQf$presCrdH2o, setQf$tempCrdH2o,
                                               setQf$tempWbox,  setQf$sensCrdH2o,
                                               setQf$envHut))
      
      rpt$rtioMoleWetH2o <- na.omit(data.frame(setQf$rtioMoleWetH2o, setQf$presCrdH2o, 
                                               setQf$tempCrdH2o, setQf$tempWbox,  
                                               setQf$sensCrdH2o, setQf$envHut))
      
      rpt$dlta18OH2o <- na.omit(data.frame(setQf$dlta18OH2o, setQf$presCrdH2o, 
                                           setQf$tempCrdH2o, setQf$tempWbox,  
                                           setQf$sensCrdH2o, setQf$envHut))
      
      rpt$dlta2HH2o <- na.omit(data.frame(setQf$dlta2HH2o, setQf$presCrdH2o, 
                                          setQf$tempCrdH2o, setQf$tempWbox,  
                                          setQf$sensCrdH2o, setQf$envHut))
      
      rpt$pres <- na.omit(data.frame(setQf$presCrdH2o, qfSensStus = setQf$sensCrdH2o$qfSensStus))
      
      rpt$temp <- na.omit(data.frame(setQf$tempCrdH2o, qfSensStus = setQf$sensCrdH2o$qfSensStus))  
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
