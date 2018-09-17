#' @title Definition function to read threshold table from CI-Parameter-Repo

#' @author 
#' Natchaya Pingintha-Durden \email{ndurden@battelleecology.org}
#' David Durden \email{ddurden@battelleecology.org}

#' @description Definition function to read threshold table from CI-Parameter-Repo

#' @param dataInp data.frame containing the threshold data from the CI-Parameter-Repo
#' @param site The site for which the parameters are being pulled

#' @return A data.frame consisting of the threshold values to be used for the provided site.

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007. \cr

#' @keywords threshold

#' @examples Currently none

#' @seealso Currently none

#' @export

# changelog and author contributions / copyrights
#   Natchaya P-Durden (2017-02-14)
#     original creation
#   Dave Durden (2017-06-18)
#     modularizing and adapting for ECTE & ECSE
#   Dave Durden (2017-11-23)
#     modularizing the code to grab all thresholds passed in the data.frame
#   Natchaya P-Durden (2018-03-28)
#     updated function header
#   Ke Xu (2018-04-19)
#     applied term name convention; replaced dataIn by dataInp
##############################################################################################
def.para.thsh <- function(
  
  dataInp, #input threshold data table
  site # site name
  
) {

  #site <- c("Bart")
  dataInp <- data.frame(dataInp)
  #get NEON default value
  dataDflt <- dataInp[which(dataInp$Loc == "Neon"),]
  
  #Check if the site is set to NEON or a specific site
  if (site %in% c("Neon")) {
    #Set output data to the NEON default values
    dataOut <- dataDflt
    }
  else {
  #Grab the site specific threshold values
  dataSite <- dataInp[grep(pattern = site,x = dataInp$Loc, ignore.case = TRUE),]
  #Check if the site specific values is set to "Dflt", then grab the default NEON-wide values
  dataOutList <- ifelse(dataSite == "Dflt", dataDflt, dataSite) 
  #Set the list names
  names(dataOutList) <- names(dataSite)
  #Convert list to dataframe
  dataOut <- data.frame(dataOutList, stringsAsFactors = FALSE)
  }
  #Remove the columns not needed for the calculations
  dataOut <- dataOut[,!names(dataOut) %in% c("Loc","LocCfgr","PrdBgn","PrdEnd")]
  #Update the column names to the proper naming convention for dp0p HDF5 output file
  names(dataOut) <- gsub(pattern = ".", replacement = "$", x = names(dataOut),fixed = TRUE)
  #Return output data
  return(dataOut) 
}
