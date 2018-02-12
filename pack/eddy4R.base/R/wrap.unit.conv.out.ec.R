##############################################################################################
#' @title Wrapper function: Output unit conversion for ECTE 

#' @author 
#' David Durden \email{eddy4R.info@gmail.com}

#' @description Function wrapper. Convert a list of data to eddy4r output units using def.unit.conv function, with special attention to variable with the mean removed that are translated between units (i.e. variance for temperature when converting K to C). 

#' @param inpList Required. A named list of data frames of type numeric, containing the data to be converted.
#' @param MethType Required. A character string containing the type of data to be converted (Defauts to \code(MethType) = "data").  

#' @return A list, \code(rpt), with data, qfqm, or uncertainty output with the correct output units
#' 
#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007.

#' @keywords output, HDF5

#' @examples Currently none.


#' @seealso Currently none.

#' @export

# changelog and author contributions / copyrights 
#   David Durden (2018-02-12)
#     original creation
############################################################################################

wrap.unit.conv.out.ec <- function(
  inpList,
  MethType = c("data","ucrt","qfqm")[1]
  ){

rpt <- inpList 
outAttr <- base::list()
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
else if(MethType == "ucrt"){
  
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

#test <- outList


wrkAttr <- base::list()
for(idxDp in base::names(rpt)){
  #idxDp <- names(rpt)[1] #for testing
  for(idxVar in base::names(rpt[[idxDp]])){
    #idxVar <- names(rpt[[idxDp]])[7] #for testing
    baseAttr <- base::attributes(rpt[[idxDp]][[idxVar]])$unit
    
    if(MethType == "data"){
      wrkAttr[[idxDp]][[idxVar]] <- c("mean"= baseAttr, "min" = baseAttr, "max" = baseAttr, "vari" = baseAttr, "numSamp" = "NA", "timeBgn" = "NA", "timeEnd" = "NA")
    
    #Only variables with conversion factors (i.e. multiplied differences should be converted using the unit conversion tool. Temp has an offset applied, this should not be performed for any variable with the mean removed, i.e. vari, se, etc.)
    if(base::grepl(pattern = "temp", x = idxVar)){wrkAttr[[idxDp]][[idxVar]]["vari"] <- "C"}
    }
    else if(MethType == "ucrt"){
      wrkAttr[[idxDp]][[idxVar]] <- c("mean"= baseAttr, "vari" = baseAttr, "se" = baseAttr, "timeBgn" = "NA", "timeEnd" = "NA")
#Only variables with conversion factors (i.e. multiplied differences should be converted using the unit conversion tool. Temp has an offset applied, this should not be performed for any variable with the mean removed, i.e. vari, se, etc.)
      if(base::grepl(pattern = "temp", x = idxVar)){wrkAttr[[idxDp]][[idxVar]][wrkAttr[[idxDp]][[idxVar]] == baseAttr] <- "C"}
    }  
    #To apply unit conversion to variance, we need to take sqrt first
    base::try(rpt[[idxDp]][[idxVar]]$vari <- base::sqrt(rpt[[idxDp]][[idxVar]]$vari))
    base::attributes(rpt[[idxDp]][[idxVar]])$unit <- wrkAttr[[idxDp]][[idxVar]]
    #Applying unit conversion#
    rpt[[idxDp]][[idxVar]] <- base::suppressWarnings(eddy4R.base::def.unit.conv(data = rpt[[idxDp]][[idxVar]], unitFrom = attributes(rpt[[idxDp]][[idxVar]])$unit, unitTo = outAttr[[idxDp]][[idxVar]], MethGc = FALSE))
    
    base::try(rpt[[idxDp]][[idxVar]]$vari <- (rpt[[idxDp]][[idxVar]]$vari)^2)
    
  } #End loop around idxVar
} #End loop around idxDp

base::return(rpt) #Returning list from function
} #End of wrapper function