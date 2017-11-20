#' @title Defination Function to read ECSE threshold table from CI-Parameter-Repo

#' @author 
#' Natchaya Pingintha-Durden \email{ndurden@battelleecology.org}

#' @description Defination Function to read ECSE threshold table from CI-Parameter-Repo

#' @param \code

#' @return

#' @references


#' @keywords 

#' @examples

#' @seealso Currently none

#' @export

# changelog and author contributions / copyrights
#   Natchaya P-Durden (2017-02-14)
#     original creation
#   Dave Durden (2017-06-18)
#     modularizing and adapting for ECTE & ECSE
##############################################################################################
def.para.thrs <- function(
  
  dataIn, #input threshold data table
  site # site name
  
) {
  #site <- c("Bart")
  dataIn <- data.frame(dataIn)
  #get NEOn default value
  dataDflt <- dataIn[which(dataIn$Loc == "Neon"),]
  
  if (site %in% c("Neon")) {
    RngMin <- dataDflt$Rng.Min
    RngMax <- dataDflt$Rng.Max
    DiffStepMax <- dataDflt$Step.DiffMax
    DiffPersMin <- dataDflt$Pers.DiffMin
    TintPers <- dataDflt$Pers.TimeDiff}
  else {
  dataSite <- dataIn[which(dataIn$Loc == site),]
  
  if (dataSite$Rng.Min %in% c("Dflt")) {RngMin <- dataDflt$Rng.Min} else {RngMin <- dataSite$Rng.Min}
  if (dataSite$Rng.Max %in% c("Dflt")) {RngMax <- dataDflt$Rng.Max} else {RngMin <- dataSite$Rng.Max}
  if (dataSite$Step.DiffMax %in% c("Dflt")) {DiffStepMax <- dataDflt$Step.DiffMax} else {DiffStepMax <- dataSite$Step.DiffMax}
  if (dataSite$Pers.DiffMin %in% c("Dflt")) {DiffPersMin <- dataDflt$Pers.DiffMin} else {DiffPersMin <- dataSite$Pers.DiffMin}
  if (dataSite$Pers.TimeDiff %in% c("Dflt")) {TintPers <- dataDflt$Pers.TimeDiff} else {TintPers <- dataSite$Pers.TimeDiff}
  }
  
  dataOut <- data.frame(RngMin=RngMin, RngMax=RngMax, DiffStepMax=DiffStepMax, DiffPersMin=DiffPersMin, TintPers=TintPers)
  
  return(dataOut)  
}
