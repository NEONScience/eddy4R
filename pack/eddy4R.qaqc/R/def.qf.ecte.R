##############################################################################################
#' @title Definition function: Create fake flags for ECTE sensors

#' @author 
#' David Durden \email{ddurden@battelleecology.org}

#' @description 
#' Workflow. File to create fake flags for soni to test the QAQC test
#' @param TimeBgn is the beginning time of the period to generate flags.
#' @param TimeEnd is the end time of the period to generate flags.
#' @param Freq is the measurement frequency used to generate flags.
#' @param Sens is ECTE sensor assembly name for which the flags are being generated.
#' @param PcntQf is percentage of observations that should be flagged.

#' @return Currently none.

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007.

#' @keywords NEON, QAQC, SOM

#' @examples 
#' TimeBgn <- "2016-04-24 02:00:00.000"
#' TimeEnd <- "2016-04-24 02:29:59.950"
#' Freq <- 20
#' Sens <- "soni"
#' PcntQf <- 0.05
#' qfSens <- def.qf.ecte(TimeBgn = TimeBgn, TimeEnd = TimeEnd, Freq = Freq, Sens = Sens, PcntQf = PcntQf)

#' @seealso Currently none
 
#' @export

# changelog and author contributions / copyrights
#   Dave Durden (2016-11-15)
#     original creation
#   Natchaya P-Durden (2016-12-05)
#     edited term name
#   Natchaya P-Durden (2018-04-03)
#     update @param format
#   Dave Durden (2021-08-23)
#     update to new ECTE dp01 names
##############################################################################################

def.qf.ecte <- function(
  TimeBgn,
  TimeEnd,
  Freq = 20,
  Sens = c("soni","irgaTurb","mfcSampTurb","amrs")[1],
  PcntQf = 0.05
  ) {
  
 
  #Check that TimeBgn is initialized; otherwise return error  
  if(base::is.null(TimeBgn)) {
    stop("Input 'TimeBgn' is required")
  }
  #Check that TimeEnd is initialized; otherwise return error  
  if(base::is.null(TimeEnd)) {
    stop("Input 'TimeEnd' is required")
  }
  
  # Check Freq
  if(!base::is.numeric(Freq) || (base::length(Freq) != 1)) {
    stop("Input parameter Freq must be single number.")
  }
  
   # make sure that fractional seconds can be seen from the console
  #options(digits.secs=3)
  
  # Create regularized timestamp
  timeRglr <- seq.POSIXt(
    from = base::as.POSIXlt(TimeBgn, format="%Y-%m-%d %H:%M:%OS", tz="UTC"),
    to = base::as.POSIXlt(TimeEnd, format="%Y-%m-%d %H:%M:%OS", tz="UTC"),
    by = 1/Freq
  )

################################################################################################################################ 
  #Start list of variable names:
  if(Sens == "soni"){
    varName <- c("veloSoni", "veloXaxs","veloYaxs","veloZaxs","tempSoni")
    qfNameSens <- c("qfSoniUnrs", "qfSoniData", "qfSoniTrig", "qfSoniComm", "qfSoniCode", "qfSoniTemp", "qfSoniSgnlPoor", "qfSoniSgnlHigh","qfSoniSgnlLow" )}
  
  if(Sens == "irgaTurb"){
    varName <- c("asrpCo2", "asrpH2o", "densMoleCo2", "densMoleH2o", "tempRefe",  "presAtm","presDiff", "potCool", "rtioMoleDryCo2", "rtioMoleDryH2o", "tempIn","tempOut", "powrH2oSamp", "powrH2oRefe", "powrCo2Samp", "powrCo2Refe","ssiCo2", "ssiH2o","tempMean", "presSum")
    qfNameSens <- c("qfIrgaHead", "qfIrgaTempOut", "qfIrgaTempIn", "qfIrgaAux", "qfIrgaPres", "qfIrgaChop", "qfIrgaDetc", "qfIrgaPll","qfIrgaSync", "qfIrgaAgc" )}
  
  if(Sens == "mfcSampTurb"){
    varName <- c("presAtm", "temp", "frt", "frt00", "frtSet00")
    qfNameSens <- NULL}
  
  if(Sens == "amrs"){
    varName <- c("angXaxs", "angYaxs", "angZaxs", "accXaxs", "accYaxs", "accZaxs", "accXaxsDiff", "accYaxsDiff", "accZaxsDiff", "avelXaxs", "avelYaxs", "avelZaxs")
    qfNameSens <- c("qfAmrsVal", "qfAmrsFilt", "qfAmrsVelo", "qfAmrsRng")}
################################################################################################################################
  
  # Name of prefixes for qaqc plausibility tests
  qfNamePlau <- c("qfRng","qfStep", "qfPers", "qfNull", "qfSpk" )
  
  #varNameUp <- paste(toupper(substr(varName, 1, 1)), substr(varName, 2, nchar(varName)), sep="")
  
  # Combining the plausibility prefixes with the sensor data stream names
  qfNameSensPlau <- as.vector(outer(qfNamePlau, paste(toupper(substr(varName, 1, 1)), substr(varName, 2, nchar(varName)), sep=""), paste, sep = ""))
  
  # Combining sensor specific quality flag names with plausibilty flags for sensor streams
  qfName <- c(qfNameSens,qfNameSensPlau)
  
  # Creating randomized flags for all the streams at the percentage specified in PcntQf
  qfSensDf <- data.frame(replicate(length(qfName), rbinom(n = length(timeRglr), size = 1, prob = PcntQf)))
  
  #Add names to the dataframe
  names(qfSensDf) <- qfName
  
  return(qfSensDf)
}