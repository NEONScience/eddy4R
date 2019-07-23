##############################################################################################
#' @title Wrapper function: Output unit conversion for ECTE 

#' @author 
#' David Durden \email{eddy4R.info@gmail.com}

#' @description Function wrapper. Convert a list of data to eddy4r output units using def.unit.conv function, with special attention to variable with the mean removed that are translated between units (i.e. variance for temperature when converting K to C). 

#' @param inpList Required. A named list of data frames of type numeric, containing the data to be converted.
#' @param MethMeas A vector of class "character" containing the name of measurement method (eddy-covariance turbulent exchange or storage exchange), MethMeas = c("ecte", "ecse"). Defaults to "ecse". [-]
#' @param MethType Required. A character string containing the type of data to be converted. Defauts to \code{MethType} = c("Data").  

#' @return A list, \code{rpt}, with data, qfqm, or uncertainty output with the correct output units

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007.

#' @keywords output, HDF5

#' @examples Currently none.

#' @seealso Currently none.

#' @export

# changelog and author contributions / copyrights 
#   David Durden (2018-02-12)
#     original creation
#   Natchaya P-Durden (2018-03-28)
#     updated function header
#   Natchaya P-Durden (2018-11-28)
#     adding unit conversion for irgaTurb validation data
#   Natchaya P-Durden (2018-11-29)
#     update if else logic
#   Natchaya P-Durden (2019-01-04)
#     adding units for rtioMoleDryCo2Cor and rtioMoleDryCo2Raw
#   Natchaya P-Durden (2019-04-23)
#     adding units conversion for ecse
#   Natchaya P-Durden (2019-07-23)
#     converting time format in ecse qfqm
############################################################################################

wrap.unit.conv.out.ec <- function(
  inpList,
  MethMeas = c("ecte", "ecse")[1],
  MethType = c("Data","Ucrt","Qfqm", "Vali")[1]
  ){

#Putting MethType to lowercase always
MethType <- base::tolower(MethType)
  
rpt <- inpList 
#working and output attribute lists
wrkAttr <- base::list()
outAttr <- base::list()

#unit conversion for ecte#################################################################################
if(MethMeas == "ecte"){
if(MethType == "data"){
outAttr$soni <- base::list(
  "veloXaxsErth"= c("mean" = "m s-1", "min" = "m s-1", "max" = "m s-1", "vari" = "m s-1", "numSamp" = "NA", "timeBgn" = "NA", "timeEnd" = "NA"), 
  "veloYaxsErth"= c("mean" = "m s-1", "min" = "m s-1", "max" = "m s-1", "vari" = "m s-1", "numSamp" = "NA", "timeBgn" = "NA", "timeEnd" = "NA"), 
  "veloZaxsErth"= c("mean" = "m s-1", "min" = "m s-1", "max" = "m s-1", "vari" = "m s-1", "numSamp" = "NA", "timeBgn" = "NA", "timeEnd" = "NA"), 
  "veloXaxsYaxsErth"= c("mean" = "m s-1", "min" = "m s-1", "max" = "m s-1", "vari" = "m s-1", "numSamp" = "NA", "timeBgn" = "NA", "timeEnd" = "NA"), 
  "angZaxsErth"= c("mean" = "deg", "min" = "deg", "max" = "deg", "vari" = "deg", "numSamp" = "NA", "timeBgn" = "NA", "timeEnd" = "NA"), 
  "tempSoni"= c("mean" = "C", "min" = "C", "max" = "C", "vari" = "C", "numSamp" = "NA", "timeBgn" = "NA", "timeEnd" = "NA"), 
  "tempAir"= c("mean" = "C", "min" = "C", "max" = "C", "vari" = "C", "numSamp" = "NA", "timeBgn" = "NA", "timeEnd" = "NA"))

outAttr$amrs <- base::list(
  "angNedXaxs"= c("mean" = "deg", "min" = "deg", "max" = "deg", "vari" = "deg", "numSamp" = "NA", "timeBgn" = "NA", "timeEnd" = "NA"), 
  "angNedYaxs"= c("mean" = "deg", "min" = "deg", "max" = "deg", "vari" = "deg", "numSamp" = "NA", "timeBgn" = "NA", "timeEnd" = "NA"), 
  "angNedZaxs"= c("mean" = "deg", "min" = "deg", "max" = "deg", "vari" = "deg", "numSamp" = "NA", "timeBgn" = "NA", "timeEnd" = "NA"))

outAttr$co2Turb <- base::list(
  "rtioMoleDryCo2"= c("mean" = "umolCo2 mol-1Dry", "min" = "umolCo2 mol-1Dry", "max" = "umolCo2 mol-1Dry", "vari" = "umolCo2 mol-1Dry", "numSamp" = "NA", "timeBgn" = "NA", "timeEnd" = "NA"), 
  "rtioMoleDryCo2Cor"= c("mean" = "umolCo2 mol-1Dry", "min" = "umolCo2 mol-1Dry", "max" = "umolCo2 mol-1Dry", "vari" = "umolCo2 mol-1Dry", "numSamp" = "NA", "timeBgn" = "NA", "timeEnd" = "NA"),
  "rtioMoleDryCo2Raw"= c("mean" = "umolCo2 mol-1Dry", "min" = "umolCo2 mol-1Dry", "max" = "umolCo2 mol-1Dry", "vari" = "umolCo2 mol-1Dry", "numSamp" = "NA", "timeBgn" = "NA", "timeEnd" = "NA"),
  "densMoleCo2"= c("mean" = "umolCo2 m-3", "min" = "umolCo2 m-3", "max" = "umolCo2 m-3", "vari" = "umolCo2 m-3", "numSamp" = "NA", "timeBgn" = "NA", "timeEnd" = "NA"), 
  "presAtm"= c("mean" = "kPa", "min" = "kPa", "max" = "kPa", "vari" = "kPa", "numSamp" = "NA", "timeBgn" = "NA", "timeEnd" = "NA"), 
  "presSum"= c("mean" = "kPa", "min" = "kPa", "max" = "kPa", "vari" = "kPa", "numSamp" = "NA", "timeBgn" = "NA", "timeEnd" = "NA"), 
  "frt00Samp"= c("mean" = "dm3 min-1", "min" = "dm3 min-1", "max" = "dm3 min-1", "vari" = "dm3 min-1", "numSamp" = "NA", "timeBgn" = "NA", "timeEnd" = "NA"), 
  "tempAve"= c("mean" = "C", "min" = "C", "max" = "C", "vari" = "C", "numSamp" = "NA", "timeBgn" = "NA", "timeEnd" = "NA"))

outAttr$h2oTurb <- base::list(
  "rtioMoleDryH2o"= c("mean" = "mmolH2o mol-1Dry", "min" = "mmolH2o mol-1Dry", "max" = "mmolH2o mol-1Dry", "vari" = "mmolH2o mol-1Dry", "numSamp" = "NA", "timeBgn" = "NA", "timeEnd" = "NA"), 
  "densMoleH2o"= c("mean" = "mmolH2o m-3", "min" = "mmolH2o m-3", "max" = "mmolH2o m-3", "vari" = "mmolH2o m-3", "numSamp" = "NA", "timeBgn" = "NA", "timeEnd" = "NA"), 
  "tempDew"= c("mean" = "C", "min" = "C", "max" = "C", "vari" = "C", "numSamp" = "NA", "timeBgn" = "NA", "timeEnd" = "NA"),
  "presAtm"= c("mean" = "kPa", "min" = "kPa", "max" = "kPa", "vari" = "kPa", "numSamp" = "NA", "timeBgn" = "NA", "timeEnd" = "NA"), 
  "presSum"= c("mean" = "kPa", "min" = "kPa", "max" = "kPa", "vari" = "kPa", "numSamp" = "NA", "timeBgn" = "NA", "timeEnd" = "NA"), 
  "frt00Samp"= c("mean" = "dm3 min-1", "min" = "dm3 min-1", "max" = "dm3 min-1", "vari" = "dm3 min-1", "numSamp" = "NA", "timeBgn" = "NA", "timeEnd" = "NA"), 
  "tempAve"= c("mean" = "C", "min" = "C", "max" = "C", "vari" = "C", "numSamp" = "NA", "timeBgn" = "NA", "timeEnd" = "NA"))
}

if(MethType == "ucrt"){
  outAttr$soni <- base::list(
    "veloXaxsErth"= c("mean" = "m s-1", "vari" = "m s-1", "se" = "m s-1", "timeBgn" = "NA", "timeEnd" = "NA"), 
    "veloYaxsErth"= c("mean" = "m s-1", "vari" = "m s-1", "se" = "m s-1", "timeBgn" = "NA", "timeEnd" = "NA"), 
    "veloZaxsErth"= c("mean" = "m s-1", "vari" = "m s-1", "se" = "m s-1", "timeBgn" = "NA", "timeEnd" = "NA"),
    "veloXaxsYaxsErth"= c("mean" = "m s-1", "vari" = "m s-1", "se" = "m s-1", "timeBgn" = "NA", "timeEnd" = "NA"), 
    "angZaxsErth"= c("mean" = "deg", "vari" = "deg", "se" = "deg", "timeBgn" = "NA", "timeEnd" = "NA"), 
    "tempSoni"= c("mean" = "C", "vari" = "C", "se" = "C", "timeBgn" = "NA", "timeEnd" = "NA"), 
    "tempAir"= c("mean" = "C", "vari" = "C", "se" = "C", "timeBgn" = "NA", "timeEnd" = "NA"))
  
  outAttr$amrs <- base::list(
    "angNedXaxs"= c("mean" = "deg", "vari" = "deg", "se" = "deg", "timeBgn" = "NA", "timeEnd" = "NA"), 
    "angNedYaxs"= c("mean" = "deg", "vari" = "deg", "se" = "deg", "timeBgn" = "NA", "timeEnd" = "NA"), 
    "angNedZaxs"= c("mean" = "deg", "vari" = "deg", "se" = "deg", "timeBgn" = "NA", "timeEnd" = "NA"))
  
  outAttr$co2Turb <- base::list(
    "rtioMoleDryCo2"= c("mean" = "umolCo2 mol-1Dry", "vari" = "umolCo2 mol-1Dry", "se" = "umolCo2 mol-1Dry", "timeBgn" = "NA", "timeEnd" = "NA"), 
    "rtioMoleDryCo2Cor"= c("mean" = "umolCo2 mol-1Dry", "vari" = "umolCo2 mol-1Dry", "se" = "umolCo2 mol-1Dry", "timeBgn" = "NA", "timeEnd" = "NA"), 
    "rtioMoleDryCo2Raw"= c("mean" = "umolCo2 mol-1Dry", "vari" = "umolCo2 mol-1Dry", "se" = "umolCo2 mol-1Dry", "timeBgn" = "NA", "timeEnd" = "NA"), 
    "densMoleCo2"= c("mean" = "umolCo2 m-3", "vari" = "umolCo2 m-3", "se" = "umolCo2 m-3", "timeBgn" = "NA", "timeEnd" = "NA"), 
    "presAtm"= c("mean" = "kPa", "vari" = "kPa", "se" = "kPa", "timeBgn" = "NA", "timeEnd" = "NA"), 
    "presSum"= c("mean" = "kPa", "vari" = "kPa", "se" = "kPa", "timeBgn" = "NA", "timeEnd" = "NA"), 
    "frt00Samp"= c("mean" = "dm3 min-1", "vari" = "dm3 min-1", "se" = "dm3 min-1", "timeBgn" = "NA", "timeEnd" = "NA"), 
    "tempAve"= c("mean" = "C", "vari" = "C", "se" = "C", "timeBgn" = "NA", "timeEnd" = "NA"))
  
  outAttr$h2oTurb <- base::list(
    "rtioMoleDryH2o"= c("mean" = "mmolH2o mol-1Dry",  "vari" = "mmolH2o mol-1Dry", "se" = "mmolH2o mol-1Dry", "timeBgn" = "NA", "timeEnd" = "NA"), 
    "densMoleH2o"= c("mean" = "mmolH2o m-3", "vari" = "mmolH2o m-3", "se" = "mmolH2o m-3", "timeBgn" = "NA", "timeEnd" = "NA"), 
    "tempDew"= c("mean" = "C", "vari" = "C", "se" = "C", "timeBgn" = "NA", "timeEnd" = "NA"),
    "presAtm"= c("mean" = "kPa", "vari" = "kPa", "se" = "kPa", "timeBgn" = "NA", "timeEnd" = "NA"), 
    "presSum"= c("mean" = "kPa", "vari" = "kPa", "se" = "kPa", "timeBgn" = "NA", "timeEnd" = "NA"), 
    "frt00Samp"= c("mean" = "dm3 min-1", "vari" = "dm3 min-1", "se" = "dm3 min-1", "timeBgn" = "NA", "timeEnd" = "NA"), 
    "tempAve"= c("mean" = "C", "vari" = "C", "se" = "C", "timeBgn" = "NA", "timeEnd" = "NA"))
}

if(MethType == "vali"){
  outAttr$co2Turb <- base::list(
    "rtioMoleDryCo2Vali"= c("mean" = "umolCo2 mol-1Dry", "min" = "umolCo2 mol-1Dry", "max" = "umolCo2 mol-1Dry", "vari" = "umolCo2 mol-1Dry", "numSamp" = "NA", "rtioMoleDryCo2Refe" = "umolCo2 mol-1Dry", "timeBgn" = "NA", "timeEnd" = "NA")) 
}

#assign output attributes
outAttr$vari <- base::list(
  "veloXaxsErth"= "m2 s-2", 
  "veloYaxsErth"= "m2 s-2", 
  "veloZaxsErth"= "m2 s-2",
  "veloXaxsYaxsErth"= "m2 s-2", 
  "angZaxsErth"= "deg2", 
  "tempSoni"= "C2", 
  "tempAir"= "C2", 
  "angNedXaxs"= "deg2", 
  "angNedYaxs"= "deg2", 
  "angNedZaxs"= "deg2",
  "rtioMoleDryCo2"= "umol2Co2 mol-2Dry", 
  "rtioMoleDryCo2Cor"= "umol2Co2 mol-2Dry",
  "rtioMoleDryCo2Raw"= "umol2Co2 mol-2Dry",
  "rtioMoleDryCo2Vali"= "umol2Co2 mol-2Dry", 
  "densMoleCo2"= "umol2Co2 m-6", 
  "presAtm"= "kPa2", 
  "presSum"= "kPa2", 
  "frt00Samp"= "dm6 min-2", 
  "tempAve"= "C2",
  "rtioMoleDryH2o"= "mmol2H2o mol-2Dry", 
  "densMoleH2o"= "mmol2H2o m-6", 
  "tempDew"= "C2"
  )

#test <- outList

for(idxDp in base::names(rpt)){
  #idxDp <- names(rpt)[1] #for testing
  for(idxVar in base::names(rpt[[idxDp]])){
    #idxVar <- names(rpt[[idxDp]])[7] #for testing
    if (MethType != "vali"){
    baseAttr <- base::attributes(rpt[[idxDp]][[idxVar]])$unit} else{
      baseAttr <- base::attributes(rpt[[idxDp]][[idxVar]])$unit[1]
    }
    
    if(MethType == "data"){
      wrkAttr[[idxDp]][[idxVar]] <- c("mean"= baseAttr, "min" = baseAttr, "max" = baseAttr, "vari" = baseAttr, "numSamp" = "NA", "timeBgn" = "NA", "timeEnd" = "NA")
    
    #Only variables with conversion factors (i.e. multiplied differences should be converted using the unit conversion tool. Temp has an offset applied, this should not be performed for any variable with the mean removed, i.e. vari, se, etc.)
    if(base::grepl(pattern = "temp", x = idxVar)){wrkAttr[[idxDp]][[idxVar]]["vari"] <- "C"}
    }
    
    if(MethType == "ucrt"){
      wrkAttr[[idxDp]][[idxVar]] <- c("mean"= baseAttr, "vari" = baseAttr, "se" = baseAttr, "timeBgn" = "NA", "timeEnd" = "NA")
#Only variables with conversion factors (i.e. multiplied differences should be converted using the unit conversion tool. Temp has an offset applied, this should not be performed for any variable with the mean removed, i.e. vari, se, etc.)
      if(base::grepl(pattern = "temp", x = idxVar)){wrkAttr[[idxDp]][[idxVar]][wrkAttr[[idxDp]][[idxVar]] == baseAttr] <- "C"}
    }
    
    if(MethType == "qfqm"){
      # Create qfqm list of units
      wrkAttr[[idxDp]][[idxVar]] <- base::rep_len(baseAttr, length.out = length(rpt[[idxDp]][[idxVar]]))
      # Add names to units vector
      names(wrkAttr[[idxDp]][[idxVar]]) <- names(rpt[[idxDp]][[idxVar]])
      #Apply NA for all qf and time variables
      wrkAttr[[idxDp]][[idxVar]][base::grep(pattern = "qf|time", x = names(wrkAttr[[idxDp]][[idxVar]]))] <- "NA"
      #Apply dimensionless fraction for all qm variables
      wrkAttr[[idxDp]][[idxVar]][base::grep(pattern = "qm", x = names(wrkAttr[[idxDp]][[idxVar]]))] <- "-"
      
      # Apply output attributes
      outAttr[[idxDp]][[idxVar]] <- wrkAttr[[idxDp]][[idxVar]]
      
    }
    
    if(MethType == "vali"){
      #refeName <- names(outAttr[[idxDp]][[idxVar]][6])
      nameAttr <- c("mean", "min", "max", "vari", "numSamp" , names(outAttr[[idxDp]][[idxVar]][6]), "timeBgn", "timeEnd" )
      #wrkAttr[[idxDp]][[idxVar]] <- c("mean"= baseAttr, "min" = baseAttr, "max" = baseAttr, "vari" = baseAttr, "numSamp" = baseAttr,  refeName = baseAttr, "timeBgn" = "NA", "timeEnd" = "NA")
      wrkAttr[[idxDp]][[idxVar]] <- c(baseAttr, baseAttr, baseAttr, baseAttr, baseAttr,  baseAttr, "NA", "NA")
      names(wrkAttr[[idxDp]][[idxVar]]) <- nameAttr
    }
    
    #To apply unit conversion to variance, we need to take sqrt first
    base::try(rpt[[idxDp]][[idxVar]]$vari <- base::sqrt(rpt[[idxDp]][[idxVar]]$vari), silent = TRUE)
    base::attributes(rpt[[idxDp]][[idxVar]])$unit <- wrkAttr[[idxDp]][[idxVar]]
    #Applying unit conversion#
    rpt[[idxDp]][[idxVar]] <- base::suppressWarnings(eddy4R.base::def.unit.conv(data = rpt[[idxDp]][[idxVar]], unitFrom = attributes(rpt[[idxDp]][[idxVar]])$unit, unitTo = outAttr[[idxDp]][[idxVar]], MethGc = FALSE))
    
    #Convert units values back to variance
    base::try(rpt[[idxDp]][[idxVar]]$vari <- (rpt[[idxDp]][[idxVar]]$vari)^2, silent = TRUE)
    
    # Apply variance units
    base::try(attributes(rpt[[idxDp]][[idxVar]])$unit[names(attributes(rpt[[idxDp]][[idxVar]])$unit) == "vari"] <- outAttr$vari[[idxVar]], silent = TRUE)
    
  } #End loop around idxVar
} #End loop around idxDp
}#End of ecte

#unit conversion for ecse#################################################################################
if(MethMeas == "ecse"){
  #unit for vari
  outAttr$vari <- c("frt00" = "dm6 min-2",
                    "pres" = "kPa2", 
                    "presEnvHut" = "kPa2", 
                    "rhEnvHut" = "%", 
                    "rtioMoleDryCo2" = "umol2Co2 mol-2", 
                    "rtioMoleWetCo2" = "umol2Co2 mol-2", 
                    "rtioMoleDryCo2Refe" = "umol2Co2 mol-2", 
                    "temp" = "C2", 
                    "tempEnvHut" = "C2", 
                    "rtioMoleDryH2o" = "mmol2H2o mol-2", 
                    "rtioMoleWetH2o" = "mmol2H2o mol-2", 
                    "rtioMoleWetH2oEnvHut" = "mmol2H2o mol-2", 
                    "rtioMoleWet12CCo2"  = "umol2Co2 mol-2", 
                    "rtioMoleDry12CCo2" = "umol2Co2 mol-2", 
                    "rtioMoleWet13CCo2" = "umol2Co2 mol-2", 
                    "rtioMoleDry13CCo2" = "umol2Co2 mol-2", 
                    "dlta13CCo2" = "permill2", 
                    "dlta13CCo2Refe" = "permill2", 
                    "dlta18OH2o" = "permill2", 
                    "dlta2HH2o" = "permill2", 
                    "dlta18OH2oRefe" = "permill2", 
                    "dlta2HH2oRefe" = "permill2") 
  
  if(MethType == "data"){
    #define working unit attributes
    #dp01
    wrkAttr$frt00 <- c("mean" = "m3 s-1", "min" = "m3 s-1", "max" = "m3 s-1", "vari" = "m3 s-1", "numSamp" = "NA", "timeBgn" = "NA", "timeEnd" = "NA")
    wrkAttr$pres <- c("mean" = "Pa", "min" = "Pa", "max" = "Pa", "vari" = "Pa", "numSamp" = "NA", "timeBgn" = "NA", "timeEnd" = "NA")
    wrkAttr$presEnvHut <- c("mean" = "Pa", "min" = "Pa", "max" = "Pa", "vari" = "Pa", "numSamp" = "NA", "timeBgn" = "NA", "timeEnd" = "NA")
    wrkAttr$rhEnvHut <- c("mean" = "-", "min" = "-", "max" = "-", "vari" = "-", "numSamp" = "NA", "timeBgn" = "NA", "timeEnd" = "NA")
    wrkAttr$rtioMoleDryCo2 <- c("mean" = "molCo2 mol-1", "min" = "molCo2 mol-1", "max" = "molCo2 mol-1", "vari" = "molCo2 mol-1", "numSamp" = "NA", "timeBgn" = "NA", "timeEnd" = "NA")
    wrkAttr$rtioMoleWetCo2 <- c("mean" = "molCo2 mol-1", "min" = "molCo2 mol-1", "max" = "molCo2 mol-1", "vari" = "molCo2 mol-1", "numSamp" = "NA", "timeBgn" = "NA", "timeEnd" = "NA")
    wrkAttr$rtioMoleDryCo2Refe <- c("mean" = "molCo2 mol-1", "min" = "molCo2 mol-1", "max" = "molCo2 mol-1", "vari" = "molCo2 mol-1", "numSamp" = "NA", "timeBgn" = "NA", "timeEnd" = "NA")
    wrkAttr$temp <- c("mean" = "K", "min" = "K", "max" = "K", "vari" = "C", "numSamp" = "NA", "timeBgn" = "NA", "timeEnd" = "NA")#with means removed unit of wrk vari of temp is equal to out
    wrkAttr$tempEnvHut <- c("mean" = "K", "min" = "K", "max" = "K", "vari" = "C", "numSamp" = "NA", "timeBgn" = "NA", "timeEnd" = "NA")
    wrkAttr$rtioMoleDryH2o <- c("mean" = "molH2o mol-1", "min" = "molH2o mol-1", "max" = "molH2o mol-1", "vari" = "molH2o mol-1", "numSamp" = "NA", "timeBgn" = "NA", "timeEnd" = "NA")
    wrkAttr$rtioMoleWetH2o <- c("mean" = "molH2o mol-1", "min" = "molH2o mol-1", "max" = "molH2o mol-1", "vari" = "molH2o mol-1", "numSamp" = "NA", "timeBgn" = "NA", "timeEnd" = "NA")
    wrkAttr$rtioMoleWetH2oEnvHut <- c("mean" = "molH2o mol-1", "min" = "molH2o mol-1", "max" = "molH2o mol-1", "vari" = "molH2o mol-1", "numSamp" = "NA", "timeBgn" = "NA", "timeEnd" = "NA")
    wrkAttr$rtioMoleWet12CCo2 <- c("mean" = "molCo2 mol-1", "min" = "molCo2 mol-1", "max" = "molCo2 mol-1", "vari" = "molCo2 mol-1", "numSamp" = "NA", "timeBgn" = "NA", "timeEnd" = "NA")
    wrkAttr$rtioMoleDry12CCo2 <- c("mean" = "molCo2 mol-1", "min" = "molCo2 mol-1", "max" = "molCo2 mol-1", "vari" = "molCo2 mol-1", "numSamp" = "NA", "timeBgn" = "NA", "timeEnd" = "NA")
    wrkAttr$rtioMoleWet13CCo2 <- c("mean" = "molCo2 mol-1", "min" = "molCo2 mol-1", "max" = "molCo2 mol-1", "vari" = "molCo2 mol-1", "numSamp" = "NA", "timeBgn" = "NA", "timeEnd" = "NA")
    wrkAttr$rtioMoleDry13CCo2 <- c("mean" = "molCo2 mol-1", "min" = "molCo2 mol-1", "max" = "molCo2 mol-1", "vari" = "molCo2 mol-1", "numSamp" = "NA", "timeBgn" = "NA", "timeEnd" = "NA")
    wrkAttr$dlta13CCo2 <- c("mean" = "permill", "min" = "permill", "max" = "permill", "vari" = "permill", "numSamp" = "NA", "timeBgn" = "NA", "timeEnd" = "NA")
    wrkAttr$dlta13CCo2Refe <- c("mean" = "permill", "min" = "permill", "max" = "permill", "vari" = "permill", "numSamp" = "NA", "timeBgn" = "NA", "timeEnd" = "NA")
    wrkAttr$dlta18OH2o <- c("mean" = "permill", "min" = "permill", "max" = "permill", "vari" = "permill", "numSamp" = "NA", "timeBgn" = "NA", "timeEnd" = "NA")
    wrkAttr$dlta2HH2o <- c("mean" = "permill", "min" = "permill", "max" = "permill", "vari" = "permill", "numSamp" = "NA", "timeBgn" = "NA", "timeEnd" = "NA")
    wrkAttr$dlta18OH2oRefe <- c("mean" = "permill", "min" = "permill", "max" = "permill", "vari" = "permill", "numSamp" = "NA", "timeBgn" = "NA", "timeEnd" = "NA")
    wrkAttr$dlta2HH2oRefe <- c("mean" = "permill", "min" = "permill", "max" = "permill", "vari" = "permill", "numSamp" = "NA", "timeBgn" = "NA", "timeEnd" = "NA")
    #dp02
    wrkAttr$rateRtioMoleDryCo2 <- c("mean" = "molCo2 mol-1 s-1", "timeBgn" = "NA", "timeEnd" = "NA")
    wrkAttr$rateRtioMoleDryH2o <- c("mean" = "molH2o mol-1 s-1", "timeBgn" = "NA", "timeEnd" = "NA")
    wrkAttr$rateTemp <- c("mean" = "K s-1", "timeBgn" = "NA", "timeEnd" = "NA")
    #dp03
    wrkAttr$dp03$rateRtioMoleDryCo2 <- c(rep("molCo2 mol-1 s-1", time = ncol(rpt$dp03$data$co2Stor$rateRtioMoleDryCo2)-2), "NA", "NA")
    wrkAttr$dp03$rateRtioMoleDryH2o <- c(rep("molH2o mol-1 s-1", time = ncol(rpt$dp03$data$h2oStor$rateRtioMoleDryH2o)-2), "NA", "NA")
    wrkAttr$dp03$rateTemp <- c(rep("K s-1", time = ncol(rpt$dp03$data$tempStor$rateTemp)-2), "NA", "NA")
    #dp04
    wrkAttr$fluxCo2 <- c("mean" = "molCo2 mol-1 m s-1", "timeBgn" = "NA", "timeEnd" = "NA")
    wrkAttr$fluxH2o <- c("mean" = "molH2o mol-1 m s-1", "timeBgn" = "NA", "timeEnd" = "NA")
    wrkAttr$fluxTemp <- c("mean" = "K m s-1", "timeBgn" = "NA", "timeEnd" = "NA")
    
    #define output unit attributes
    #dp01
    outAttr$frt00 <- c("mean" = "dm3 min-1", "min" = "dm3 min-1", "max" = "dm3 min-1", "vari" = "dm3 min-1", "numSamp" = "NA", "timeBgn" = "NA", "timeEnd" = "NA")
    outAttr$pres <- c("mean" = "kPa", "min" = "kPa", "max" = "kPa", "vari" = "kPa", "numSamp" = "NA", "timeBgn" = "NA", "timeEnd" = "NA")
    outAttr$presEnvHut <- c("mean" = "kPa", "min" = "kPa", "max" = "kPa", "vari" = "kPa", "numSamp" = "NA", "timeBgn" = "NA", "timeEnd" = "NA")
    outAttr$rhEnvHut <- c("mean" = "%", "min" = "%", "max" = "%", "vari" = "%", "numSamp" = "NA", "timeBgn" = "NA", "timeEnd" = "NA")
    outAttr$rtioMoleDryCo2 <- c("mean" = "umolCo2 mol-1", "min" = "umolCo2 mol-1", "max" = "umolCo2 mol-1", "vari" = "umolCo2 mol-1", "numSamp" = "NA", "timeBgn" = "NA", "timeEnd" = "NA")
    outAttr$rtioMoleWetCo2 <- c("mean" = "umolCo2 mol-1", "min" = "umolCo2 mol-1", "max" = "umolCo2 mol-1", "vari" = "umolCo2 mol-1", "numSamp" = "NA", "timeBgn" = "NA", "timeEnd" = "NA")
    outAttr$rtioMoleDryCo2Refe <- c("mean" = "umolCo2 mol-1", "min" = "umolCo2 mol-1", "max" = "umolCo2 mol-1", "vari" = "umolCo2 mol-1", "numSamp" = "NA", "timeBgn" = "NA", "timeEnd" = "NA")
    outAttr$temp <- c("mean" = "C", "min" = "C", "max" = "C", "vari" = "C", "numSamp" = "NA", "timeBgn" = "NA", "timeEnd" = "NA")
    outAttr$tempEnvHut <- c("mean" = "C", "min" = "C", "max" = "C", "vari" = "C", "numSamp" = "NA", "timeBgn" = "NA", "timeEnd" = "NA")
    outAttr$rtioMoleDryH2o <- c("mean" = "mmolH2o mol-1", "min" = "mmolH2o mol-1", "max" = "mmolH2o mol-1", "vari" = "mmolH2o mol-1", "numSamp" = "NA", "timeBgn" = "NA", "timeEnd" = "NA")
    outAttr$rtioMoleWetH2o <- c("mean" = "mmolH2o mol-1", "min" = "mmolH2o mol-1", "max" = "mmolH2o mol-1", "vari" = "mmolH2o mol-1", "numSamp" = "NA", "timeBgn" = "NA", "timeEnd" = "NA")
    outAttr$rtioMoleWetH2oEnvHut <- c("mean" = "mmolH2o mol-1", "min" = "mmolH2o mol-1", "max" = "mmolH2o mol-1", "vari" = "mmolH2o mol-1", "numSamp" = "NA", "timeBgn" = "NA", "timeEnd" = "NA")
    outAttr$rtioMoleWet12CCo2 <- c("mean" = "umolCo2 mol-1", "min" = "umolCo2 mol-1", "max" = "umolCo2 mol-1", "vari" = "umolCo2 mol-1", "numSamp" = "NA", "timeBgn" = "NA", "timeEnd" = "NA")
    outAttr$rtioMoleDry12CCo2 <- c("mean" = "umolCo2 mol-1", "min" = "umolCo2 mol-1", "max" = "umolCo2 mol-1", "vari" = "umolCo2 mol-1", "numSamp" = "NA", "timeBgn" = "NA", "timeEnd" = "NA")
    outAttr$rtioMoleWet13CCo2 <- c("mean" = "umolCo2 mol-1", "min" = "umolCo2 mol-1", "max" = "umolCo2 mol-1", "vari" = "umolCo2 mol-1", "numSamp" = "NA", "timeBgn" = "NA", "timeEnd" = "NA")
    outAttr$rtioMoleDry13CCo2 <- c("mean" = "umolCo2 mol-1", "min" = "umolCo2 mol-1", "max" = "umolCo2 mol-1", "vari" = "umolCo2 mol-1", "numSamp" = "NA", "timeBgn" = "NA", "timeEnd" = "NA")
    outAttr$dlta13CCo2 <- c("mean" = "permill", "min" = "permill", "max" = "permill", "vari" = "permill", "numSamp" = "NA", "timeBgn" = "NA", "timeEnd" = "NA")
    outAttr$dlta13CCo2Refe <- c("mean" = "permill", "min" = "permill", "max" = "permill", "vari" = "permill", "numSamp" = "NA", "timeBgn" = "NA", "timeEnd" = "NA")
    outAttr$dlta18OH2o <- c("mean" = "permill", "min" = "permill", "max" = "permill", "vari" = "permill", "numSamp" = "NA", "timeBgn" = "NA", "timeEnd" = "NA")
    outAttr$dlta2HH2o <- c("mean" = "permill", "min" = "permill", "max" = "permill", "vari" = "permill", "numSamp" = "NA", "timeBgn" = "NA", "timeEnd" = "NA")
    outAttr$dlta18OH2oRefe <- c("mean" = "permill", "min" = "permill", "max" = "permill", "vari" = "permill", "numSamp" = "NA", "timeBgn" = "NA", "timeEnd" = "NA")
    outAttr$dlta2HH2oRefe <- c("mean" = "permill", "min" = "permill", "max" = "permill", "vari" = "permill", "numSamp" = "NA", "timeBgn" = "NA", "timeEnd" = "NA")
    #dp02; use K from dp02 up
    outAttr$rateRtioMoleDryCo2 <- c("mean" = "umolCo2 mol-1 s-1", "timeBgn" = "NA", "timeEnd" = "NA")
    outAttr$rateRtioMoleDryH2o <- c("mean" = "mmolH2o mol-1 s-1", "timeBgn" = "NA", "timeEnd" = "NA")
    outAttr$rateTemp <- c("mean" = "K s-1", "timeBgn" = "NA", "timeEnd" = "NA")
    #dp03
    outAttr$dp03$rateRtioMoleDryCo2 <- c(rep("umolCo2 mol-1 s-1", time = ncol(rpt$dp03$data$co2Stor$rateRtioMoleDryCo2)-2), "NA", "NA")
    outAttr$dp03$rateRtioMoleDryH2o <- c(rep("mmolH2o mol-1 s-1", time = ncol(rpt$dp03$data$h2oStor$rateRtioMoleDryH2o)-2), "NA", "NA")
    outAttr$dp03$rateTemp <- c(rep("K s-1", time = ncol(rpt$dp03$data$tempStor$rateTemp)-2), "NA", "NA")
    #dp04
    outAttr$fluxCo2 <- c("mean" = "umolCo2 mol-1 m s-1", "timeBgn" = "NA", "timeEnd" = "NA")
    outAttr$fluxH2o <- c("mean" = "mmolH2o mol-1 m s-1", "timeBgn" = "NA", "timeEnd" = "NA")
    outAttr$fluxTemp <- c("mean" = "K m s-1", "timeBgn" = "NA", "timeEnd" = "NA")
    
    #performing dp01 unit conversion
    print(paste0(format(Sys.time(), "%F %T"), ": dataset ", Date, ": performing dp01 unit conversion")) 
    for(idxDp in base::names(rpt$dp01$data)[which(!(names(rpt$dp01$data) %in% c("tempAirTop", "tempAirLvl")))]) {
      #idxDp <- base::names(rpt$dp01$data)[1]
      
      for(idxLvl in base::names(rpt$dp01$data[[idxDp]])){
        #idxLvl <- base::names(rpt$dp01$data[[idxDp]])[1]
        for(idxVar in base::names(rpt$dp01$data[[idxDp]][[idxLvl]])){
          #idxVar <- base::names(rpt$dp01$data[[idxDp]][[idxLvl]])[2]
          #Add a fix here to remove se in /data
          rpt$dp01$data[[idxDp]][[idxLvl]][[idxVar]]$se <- NULL
          #square root varince data before convert the unit
          rpt$dp01$data[[idxDp]][[idxLvl]][[idxVar]]$vari <- sqrt(rpt$dp01$data[[idxDp]][[idxLvl]][[idxVar]]$vari)
          #Apply unit attributes to mean, min, max, vari, numSamp, timeBgn, timeEnd
          attributes(rpt$dp01$data[[idxDp]][[idxLvl]][[idxVar]])$unit <- wrkAttr[[idxVar]]
          names(attributes(rpt$dp01$data[[idxDp]][[idxLvl]][[idxVar]])$unit) <- attributes(rpt$dp01$data[[idxDp]][[idxLvl]][[idxVar]])$names
          #perform unit conversion
          rpt$dp01$data[[idxDp]][[idxLvl]][[idxVar]] <- base::suppressWarnings(eddy4R.base::def.unit.conv(data = rpt$dp01$data[[idxDp]][[idxLvl]][[idxVar]],
                                                                                                          unitFrom = attributes(rpt$dp01$data[[idxDp]][[idxLvl]][[idxVar]])$unit,
                                                                                                          unitTo = outAttr[[idxVar]],
                                                                                                          MethGc = FALSE))
          
          #calculate variance
          rpt$dp01$data[[idxDp]][[idxLvl]][[idxVar]]$vari <- (rpt$dp01$data[[idxDp]][[idxLvl]][[idxVar]]$vari)^2
          #format time
          rpt$dp01$data[[idxDp]][[idxLvl]][[idxVar]]$timeBgn <- as.character(rpt$dp01$data[[idxDp]][[idxLvl]][[idxVar]]$timeBgn)
          rpt$dp01$data[[idxDp]][[idxLvl]][[idxVar]]$timeEnd <- as.character(rpt$dp01$data[[idxDp]][[idxLvl]][[idxVar]]$timeEnd)
          #unit transfer
          attributes(rpt$dp01$data[[idxDp]][[idxLvl]][[idxVar]])$unit <-
            attributes(rpt$dp01$data[[idxDp]][[idxLvl]][[idxVar]])$unit
          #apply variance units
          base::try(attributes(rpt$dp01$data[[idxDp]][[idxLvl]][[idxVar]])$unit[names(attributes(rpt$dp01$data[[idxDp]][[idxLvl]][[idxVar]])$unit) == "vari"] <- outAttr$vari[[idxVar]], silent = TRUE)
          
        }
      }
      base::gc(verbose=FALSE) # clean up memory 
    }; rm(idxVar)
    
    #performing dp02 unit conversion
    print(paste0(format(Sys.time(), "%F %T"), ": dataset ", Date, ": performing dp02 unit conversion"))
    for(idxDp in base::names(rpt$dp02$data)) {
      #idxDp <- base::names(rpt$dp02$data)[1]
      
      for(idxLvl in base::names(rpt$dp02$data[[idxDp]])){
        #idxLvl <- base::names(rpt$dp02$data[[idxDp]])[2]
        for(idxVar in base::names(rpt$dp02$data[[idxDp]][[idxLvl]])){
          #idxVar <- base::names(rpt$dp02$data[[idxDp]][[idxLvl]])[1]
          #Apply unit attributes to mean, timeBgn, timeEnd
          attributes(rpt$dp02$data[[idxDp]][[idxLvl]][[idxVar]])$unit <- wrkAttr[[idxVar]]
          names(attributes(rpt$dp02$data[[idxDp]][[idxLvl]][[idxVar]])$unit) <- attributes(rpt$dp02$data[[idxDp]][[idxLvl]][[idxVar]])$names
          #perform unit conversion
          rpt$dp02$data[[idxDp]][[idxLvl]][[idxVar]] <- base::suppressWarnings(eddy4R.base::def.unit.conv(data = rpt$dp02$data[[idxDp]][[idxLvl]][[idxVar]],
                                                                                                          unitFrom = attributes(rpt$dp02$data[[idxDp]][[idxLvl]][[idxVar]])$unit,
                                                                                                          unitTo = outAttr[[idxVar]],
                                                                                                          MethGc = FALSE))
          
          
          #format time
          rpt$dp02$data[[idxDp]][[idxLvl]][[idxVar]]$timeBgn <- as.character(rpt$dp02$data[[idxDp]][[idxLvl]][[idxVar]]$timeBgn)
          rpt$dp02$data[[idxDp]][[idxLvl]][[idxVar]]$timeEnd <- as.character(rpt$dp02$data[[idxDp]][[idxLvl]][[idxVar]]$timeEnd)
          
          #unit transfer
          attributes(rpt$dp02$data[[idxDp]][[idxLvl]][[idxVar]])$unit <-
            attributes(rpt$dp02$data[[idxDp]][[idxLvl]][[idxVar]])$unit
          
        }
      }
      base::gc(verbose=FALSE) # clean up memory 
    }; rm(idxVar)
    
    #performing dp03 unit conversion
    print(paste0(format(Sys.time(), "%F %T"), ": dataset ", Date, ": performing dp03 unit conversion"))
    for(idxDp in base::names(rpt$dp03$data)) {
      #idxDp <- base::names(rpt$dp03$data)[1]
      
      for(idxVar in base::names(rpt$dp03$data[[idxDp]])){
        #idxVar <- base::names(rpt$dp03$data[[idxDp]])[1]
        #Apply unit attributes to mean, timeBgn, timeEnd
        attributes(rpt$dp03$data[[idxDp]][[idxVar]])$unit <- wrkAttr$dp03[[idxVar]]
        names(attributes(rpt$dp03$data[[idxDp]][[idxVar]])$unit) <- attributes(rpt$dp03$data[[idxDp]][[idxVar]])$names
        #perform unit conversion
        rpt$dp03$data[[idxDp]][[idxVar]] <- base::suppressWarnings(eddy4R.base::def.unit.conv(data = rpt$dp03$data[[idxDp]][[idxVar]],
                                                                                              unitFrom = attributes(rpt$dp03$data[[idxDp]][[idxVar]])$unit,
                                                                                              unitTo = outAttr$dp03[[idxVar]],
                                                                                              MethGc = FALSE))
        
        
        #format time
        rpt$dp03$data[[idxDp]][[idxVar]]$timeBgn <- as.character(rpt$dp03$data[[idxDp]][[idxVar]]$timeBgn)
        rpt$dp03$data[[idxDp]][[idxVar]]$timeEnd <- as.character(rpt$dp03$data[[idxDp]][[idxVar]]$timeEnd)
        
        #unit transfer
        attributes(rpt$dp03$data[[idxDp]][[idxVar]])$unit <-
          attributes(rpt$dp03$data[[idxDp]][[idxVar]])$unit[1]
        
      }
      base::gc(verbose=FALSE) # clean up memory
      
    }; rm(idxVar)
    
    #performing dp04 unit conversion
    print(paste0(format(Sys.time(), "%F %T"), ": dataset ", Date, ": performing dp04 unit conversion"))
    for(idxDp in base::names(rpt$dp04$data)) {
      #idxDp <- base::names(rpt$dp04$data)[3]
      
      for(idxVar in base::names(rpt$dp04$data[[idxDp]])){
        #idxVar <- base::names(rpt$dp04$data[[idxDp]])[1]
        #Apply unit attributes to mean, timeBgn, timeEnd
        attributes(rpt$dp04$data[[idxDp]][[idxVar]])$unit <- wrkAttr[[idxDp]]
        names(attributes(rpt$dp04$data[[idxDp]][[idxVar]])$unit) <- attributes(rpt$dp04$data[[idxDp]][[idxVar]])$names
        #perform unit conversion
        rpt$dp04$data[[idxDp]][[idxVar]] <- base::suppressWarnings(eddy4R.base::def.unit.conv(data = rpt$dp04$data[[idxDp]][[idxVar]],
                                                                                              unitFrom = attributes(rpt$dp04$data[[idxDp]][[idxVar]])$unit,
                                                                                              unitTo = outAttr[[idxDp]],
                                                                                              MethGc = FALSE))
        
        
        #format time
        rpt$dp04$data[[idxDp]][[idxVar]]$timeBgn <- as.character(rpt$dp04$data[[idxDp]][[idxVar]]$timeBgn)
        rpt$dp04$data[[idxDp]][[idxVar]]$timeEnd <- as.character(rpt$dp04$data[[idxDp]][[idxVar]]$timeEnd)
        
        #unit transfer
        attributes(rpt$dp04$data[[idxDp]][[idxVar]])$unit <-
          attributes(rpt$dp04$data[[idxDp]][[idxVar]])$unit
      }
      base::gc(verbose=FALSE) # clean up memory
    }
  }#end of MethType = "data"
  
  if(MethType == "qfqm"){
    #performing dp01 and dp02 unit transfer
    print(paste0(format(Sys.time(), "%F %T"), ": dataset ", Date, ": transfering dp01 and dp02 qfqm unit ")) 
    for(idxDataLvl in c("dp01", "dp02")){
    for(idxDp in base::names(rpt[[idxDataLvl]]$qfqm)[which(!(names(rpt[[idxDataLvl]]$data) %in% c("tempAirTop", "tempAirLvl")))]) {
      for(idxLvl in base::names(rpt[[idxDataLvl]]$qfqm[[idxDp]])){
        for(idxVar in base::names(rpt[[idxDataLvl]]$qfqm[[idxDp]][[idxLvl]])){
          # Create qfqm list of units
          wrkAttr[[idxVar]] <- c(rep("-", time = length(rpt[[idxDataLvl]]$qfqm[[idxDp]][[idxLvl]][[idxVar]])))
          #Add names to units vector
          names(wrkAttr[[idxVar]]) <- names(rpt[[idxDataLvl]]$qfqm[[idxDp]][[idxLvl]][[idxVar]])
          #Apply NA for all qf and time variables
          wrkAttr[[idxVar]][base::grep(pattern = "qf|time", x = names(wrkAttr[[idxVar]]))] <- "NA"
          #Apply unit attributes to rpt
          attributes(rpt[[idxDataLvl]]$qfqm[[idxDp]][[idxLvl]][[idxVar]])$unit <- wrkAttr[[idxVar]]
          #format time
          rpt[[idxDataLvl]]$qfqm[[idxDp]][[idxLvl]][[idxVar]]$timeBgn <- as.character(rpt[[idxDataLvl]]$qfqm[[idxDp]][[idxLvl]][[idxVar]]$timeBgn)
          rpt[[idxDataLvl]]$qfqm[[idxDp]][[idxLvl]][[idxVar]]$timeEnd <- as.character(rpt[[idxDataLvl]]$qfqm[[idxDp]][[idxLvl]][[idxVar]]$timeEnd)
        }
      }
      base::gc(verbose=FALSE) # clean up memory 
    }; rm(idxVar)
    }#end of dp01 and dp02
    
    #performing dp03 and dp04 unit transfer
    print(paste0(format(Sys.time(), "%F %T"), ": dataset ", Date, ": transfering dp03 and dp04 qfqm unit ")) 
    for(idxDataLvl in c("dp03", "dp04")){
      for(idxDp in base::names(rpt[[idxDataLvl]]$qfqm)) {
        for(idxVar in base::names(rpt[[idxDataLvl]]$qfqm[[idxDp]])){
          #Apply unit attributes to rpt
          if(idxDataLvl == "dp03"){
            attributes(rpt[[idxDataLvl]]$qfqm[[idxDp]][[idxVar]])$unit <- "NA"  
          }else{
            # Create qfqm list of units
            wrkAttr[[idxVar]] <- c(rep("-", time = length(rpt[[idxDataLvl]]$qfqm[[idxDp]][[idxVar]])))
            #Add names to units vector
            names(wrkAttr[[idxVar]]) <- names(rpt[[idxDataLvl]]$qfqm[[idxDp]][[idxVar]])
            #Apply NA for all qf and time variables
            wrkAttr[[idxVar]][base::grep(pattern = "qf|time", x = names(wrkAttr[[idxVar]]))] <- "NA"
            #Apply unit attributes to rpt
            attributes(rpt[[idxDataLvl]]$qfqm[[idxDp]][[idxVar]])$unit <- wrkAttr[[idxVar]]
          }
          #format time
          rpt[[idxDataLvl]]$qfqm[[idxDp]][[idxVar]]$timeBgn <- as.character(rpt[[idxDataLvl]]$qfqm[[idxDp]][[idxVar]]$timeBgn)
          rpt[[idxDataLvl]]$qfqm[[idxDp]][[idxVar]]$timeEnd <- as.character(rpt[[idxDataLvl]]$qfqm[[idxDp]][[idxVar]]$timeEnd)
        }
        base::gc(verbose=FALSE) # clean up memory 
      }; rm(idxVar)
    }#end of dp03 and dp04
  }#End of MethType == "qfqm"
  
  if(MethType == "ucrt"){
    #dp01
    wrkAttr$frt00 <- c("mean" = "m3 s-1", "vari" = "m3 s-1", "se" = "m3 s-1", "timeBgn" = "NA", "timeEnd" = "NA")
    wrkAttr$pres <- c("mean" = "Pa", "vari" = "Pa", "se" = "Pa", "timeBgn" = "NA", "timeEnd" = "NA")
    wrkAttr$presEnvHut <- c("mean" = "Pa", "vari" = "Pa", "se" = "Pa", "timeBgn" = "NA", "timeEnd" = "NA")
    wrkAttr$rhEnvHut <- c("mean" = "-", "vari" = "-", "se" = "-", "timeBgn" = "NA", "timeEnd" = "NA")
    wrkAttr$rtioMoleDryCo2 <- c("mean" = "molCo2 mol-1", "vari" = "molCo2 mol-1", "se" = "molCo2 mol-1", "timeBgn" = "NA", "timeEnd" = "NA")
    wrkAttr$rtioMoleWetCo2 <- c("mean" = "molCo2 mol-1", "vari" = "molCo2 mol-1", "se" = "molCo2 mol-1", "timeBgn" = "NA", "timeEnd" = "NA")
    wrkAttr$temp <- c("mean" = "C", "vari" = "C", "se" = "C", "NA", "NA")#with means removed unit of wrk mean, vari, and se of temp is equal to out
    wrkAttr$tempEnvHut <- c("mean" = "C", "vari" = "C", "se" = "C", "timeBgn" = "NA", "timeEnd" = "NA")
    wrkAttr$rtioMoleDryH2o <- c("mean" = "molH2o mol-1", "vari" = "molH2o mol-1", "se" = "molH2o mol-1", "timeBgn" = "NA", "timeEnd" = "NA")
    wrkAttr$rtioMoleWetH2o <- c("mean" = "molH2o mol-1", "vari" = "molH2o mol-1", "se" = "molH2o mol-1", "timeBgn" = "NA", "timeEnd" = "NA")
    wrkAttr$rtioMoleWetH2oEnvHut <- c("mean" = "molH2o mol-1", "vari" = "molH2o mol-1", "se" = "molH2o mol-1", "timeBgn" = "NA", "timeEnd" = "NA")
    wrkAttr$rtioMoleWet12CCo2 <- c("mean" = "molCo2 mol-1", "vari" = "molCo2 mol-1", "se" = "molCo2 mol-1", "timeBgn" = "NA", "timeEnd" = "NA")
    wrkAttr$rtioMoleDry12CCo2 <- c("mean" = "molCo2 mol-1", "vari" = "molCo2 mol-1", "se" = "molCo2 mol-1", "timeBgn" = "NA", "timeEnd" = "NA")
    wrkAttr$rtioMoleWet13CCo2 <- c("mean" = "molCo2 mol-1", "vari" = "molCo2 mol-1", "se" = "molCo2 mol-1", "timeBgn" = "NA", "timeEnd" = "NA")
    wrkAttr$rtioMoleDry13CCo2 <- c("mean" = "molCo2 mol-1", "vari" = "molCo2 mol-1", "se" = "molCo2 mol-1", "timeBgn" = "NA", "timeEnd" = "NA")
    wrkAttr$dlta13CCo2 <- c("mean" = "permill", "vari" = "permill", "se" = "permill", "timeBgn" = "NA", "timeEnd" = "NA")
    wrkAttr$dlta18OH2o <- c("mean" = "permill", "vari" = "permill", "se" = "permill", "timeBgn" = "NA", "timeEnd" = "NA")
    wrkAttr$dlta2HH2o <- c("mean" = "permill", "vari" = "permill", "se" = "permill", "timeBgn" = "NA", "timeEnd" = "NA")
    
    #define output unit attributes
    #dp01
    outAttr$frt00 <- c("mean" = "dm3 min-1", "vari" = "dm3 min-1", "se" = "dm3 min-1", "timeBgn" = "NA", "timeEnd" = "NA")
    outAttr$pres <- c("mean" = "kPa", "vari" = "kPa", "se" = "kPa", "timeBgn" = "NA", "timeEnd" = "NA")
    outAttr$presEnvHut <- c("mean" = "kPa", "vari" = "kPa", "se" = "kPa", "timeBgn" = "NA", "timeEnd" = "NA")
    outAttr$rhEnvHut <- c("mean" = "%", "vari" = "%", "se" = "%", "timeBgn" = "NA", "timeEnd" = "NA")
    outAttr$rtioMoleDryCo2 <- c("mean" = "umolCo2 mol-1", "vari" = "umolCo2 mol-1", "se" = "umolCo2 mol-1", "timeBgn" = "NA", "timeEnd" = "NA")
    outAttr$rtioMoleWetCo2 <- c("mean" = "umolCo2 mol-1", "vari" = "umolCo2 mol-1", "se" = "umolCo2 mol-1", "timeBgn" = "NA", "timeEnd" = "NA")
    outAttr$temp <- c("mean" = "C", "vari" = "C", "se" = "C", "timeBgn" = "NA", "timeEnd" = "NA")
    outAttr$tempEnvHut <- c("mean" = "C", "vari" = "C", "se" = "C", "timeBgn" = "NA", "timeEnd" = "NA")
    outAttr$rtioMoleDryH2o <- c("mean" = "mmolH2o mol-1", "vari" = "mmolH2o mol-1", "se" = "mmolH2o mol-1", "timeBgn" = "NA", "timeEnd" = "NA")
    outAttr$rtioMoleWetH2o <- c("mean" = "mmolH2o mol-1", "vari" = "mmolH2o mol-1", "se" = "mmolH2o mol-1", "timeBgn" = "NA", "timeEnd" = "NA")
    outAttr$rtioMoleWetH2oEnvHut <- c("mean" = "mmolH2o mol-1", "vari" = "mmolH2o mol-1", "se" = "mmolH2o mol-1", "timeBgn" = "NA", "timeEnd" = "NA")
    outAttr$rtioMoleWet12CCo2 <- c("mean" = "umolCo2 mol-1", "vari" = "umolCo2 mol-1", "se" = "umolCo2 mol-1", "timeBgn" = "NA", "timeEnd" = "NA")
    outAttr$rtioMoleDry12CCo2 <- c("mean" = "umolCo2 mol-1", "vari" = "umolCo2 mol-1", "se" = "umolCo2 mol-1", "timeBgn" = "NA", "timeEnd" = "NA")
    outAttr$rtioMoleWet13CCo2 <- c("mean" = "umolCo2 mol-1", "vari" = "umolCo2 mol-1", "se" = "umolCo2 mol-1", "timeBgn" = "NA", "timeEnd" = "NA")
    outAttr$rtioMoleDry13CCo2 <- c("mean" = "umolCo2 mol-1", "vari" = "umolCo2 mol-1", "se" = "umolCo2 mol-1", "timeBgn" = "NA", "timeEnd" = "NA")
    outAttr$dlta13CCo2 <- c("mean" = "permill", "vari" = "permill", "se" = "permill", "timeBgn" = "NA", "timeEnd" = "NA")
    outAttr$dlta18OH2o <- c("mean" = "permill", "vari" = "permill", "se" = "permill", "timeBgn" = "NA", "timeEnd" = "NA")
    outAttr$dlta2HH2o <- c("mean" = "permill", "vari" = "permill", "se" = "permill", "timeBgn" = "NA", "timeEnd" = "NA")
    
    #performing dp01 ucrt unit conversion
    print(paste0(format(Sys.time(), "%F %T"), ": dataset ", Date, ": performing dp01 ucrt unit conversion")) 
    #transfer units
    for(idxDp in base::names(rpt$dp01$ucrt)) {
      #idxDp <- base::names(rpt$dp01$ucrt)[1]
      
      for(idxLvl in base::names(rpt$dp01$ucrt[[idxDp]])){
        #idxLvl <- base::names(rpt$dp01$ucrt[[idxDp]])[1]
        for(idxVar in base::names(rpt$dp01$ucrt[[idxDp]][[idxLvl]])){
          #idxVar <- base::names(rpt$dp01$ucrt[[idxDp]][[idxLvl]])[2]
          #square root varince data before convert the unit
          rpt$dp01$ucrt[[idxDp]][[idxLvl]][[idxVar]]$vari <- sqrt(rpt$dp01$ucrt[[idxDp]][[idxLvl]][[idxVar]]$vari)
          #Apply unit attributes to mean, min, max, vari, numSamp, timeBgn, timeEnd
          attributes(rpt$dp01$ucrt[[idxDp]][[idxLvl]][[idxVar]])$unit <- wrkAttr[[idxVar]]
          names(attributes(rpt$dp01$ucrt[[idxDp]][[idxLvl]][[idxVar]])$unit) <- attributes(rpt$dp01$ucrt[[idxDp]][[idxLvl]][[idxVar]])$names
          # perform unit conversion
          rpt$dp01$ucrt[[idxDp]][[idxLvl]][[idxVar]] <- base::suppressWarnings(eddy4R.base::def.unit.conv(data = rpt$dp01$ucrt[[idxDp]][[idxLvl]][[idxVar]],
                                                                                                          unitFrom = attributes(rpt$dp01$ucrt[[idxDp]][[idxLvl]][[idxVar]])$unit,
                                                                                                          unitTo = outAttr[[idxVar]],
                                                                                                          MethGc = FALSE))
          
          
          #calculate variance
          rpt$dp01$ucrt[[idxDp]][[idxLvl]][[idxVar]]$vari <- (rpt$dp01$ucrt[[idxDp]][[idxLvl]][[idxVar]]$vari)^2
          #format time
          rpt$dp01$ucrt[[idxDp]][[idxLvl]][[idxVar]]$timeBgn <- as.character(rpt$dp01$ucrt[[idxDp]][[idxLvl]][[idxVar]]$timeBgn)
          rpt$dp01$ucrt[[idxDp]][[idxLvl]][[idxVar]]$timeEnd <- as.character(rpt$dp01$ucrt[[idxDp]][[idxLvl]][[idxVar]]$timeEnd)
          #unit transfer
          attributes(rpt$dp01$ucrt[[idxDp]][[idxLvl]][[idxVar]])$unit <-
            attributes(rpt$dp01$ucrt[[idxDp]][[idxLvl]][[idxVar]])$unit
          #apply variance units
          base::try(attributes(rpt$dp01$ucrt[[idxDp]][[idxLvl]][[idxVar]])$unit[names(attributes(rpt$dp01$ucrt[[idxDp]][[idxLvl]][[idxVar]])$unit) == "vari"] <- outAttr$vari[[idxVar]], silent = TRUE)
          
        }
      }
      base::gc(verbose=FALSE) # clean up memory  
    }; rm(idxVar)
  }#end of ucrt
}#End of ecse
base::return(rpt) #Returning list from function
} #End of wrapper function
