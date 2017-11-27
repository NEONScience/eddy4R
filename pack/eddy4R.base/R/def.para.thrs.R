#' @title Definition Function to read threshold table from CI-Parameter-Repo

#' @author 
#' Natchaya Pingintha-Durden \email{ndurden@battelleecology.org}
#' David Durden \email{ddurden@battelleecology.org}

#' @description Defination Function to read ECSE threshold table from CI-Parameter-Repo

#' @param dataIn data.frame containing the threshold data from the CI-Parameter-Repo
#' @param site The site for which the parameters are being pulled

#' @return A data.frame consisting of the threshold values to be used for the provided site.

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
#   Dave Durden (2017-11-23)
#     modularizing the code to grab all thresholds passed in the data.frame
##############################################################################################
def.para.thrs <- function(
  
  dataIn, #input threshold data table
  site # site name
  
) {

  #site <- c("Bart")
  dataIn <- data.frame(dataIn)
  #get NEON default value
  dataDflt <- dataIn[which(dataIn$Loc == "Neon"),]
  
  #Check if the site is set to NEON or a specific site
  if (site %in% c("Neon")) {
    #Set output data to the NEON default values
    dataOut <- dataDflt
    }
  else {
  #Grab the site specific threshold values
  dataSite <- dataIn[grep(pattern = site,x = dataIn$Loc, ignore.case = TRUE),]
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
