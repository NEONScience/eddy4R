##############################################################################################
#' @title Definition function: to remove high frequency data points that have failed quality flags

#' @author 
#' Dave Durden \email{ddurden@battelleecology.org}

#' @description 
#' Definition function  to remove high frequency data points that have failed quality flags from a data.frame
#' @param dfData Input data.frame for data to be removed from based on quality flags
#' @param dfQf Input data.frame of quality flags
#' Switch for quality flag determination for the LI7200, diag01 provides ones for passing quality by default the equals "lico". The "qfqm" method changes the ones to zeros to match the NEON QFQM logic.
#' 
#' @return A dataframe (\code{qfIrga}) of sensor specific irga quality flags as described in NEON.DOC.000807.

#' @references 
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007. \cr
#' NEON Algorithm Theoretical Basis Document:Eddy Covariance Turbulent Exchange Subsystem Level 0 to Level 0’ data product conversions and calculations (NEON.DOC.000807) \cr
#' Licor LI7200 reference manual

#' @keywords NEON, qfqm, quality

#' @examples 
#' Currently none

#' @seealso Currently none

#' @export

# changelog and author contributions / copyrights
#   Dave Durden (2017-10-27)
#     original creation
##############################################################################################


def.qf.rmv.data <- function(dfData, dfQf, Sens = NULL){
  
  #Create a list to hold all the output
  rpt <- list()
  
  rpt$dfData <- dfData
  #List of variables to check for flags to remove bad data
  rpt$listVar <- names(dfData[!names(dfData) %in% c("time", "idx")])
  
  #If a sensor (Sens) is included, check for sensor specific flags to perform filtering of data    
  if(!is.null(Sens) && length(Sens) == 1){ 
    qfSens <- paste0("qf",toupper(substr(Sens, 1, 1)),substr(Sens,2,nchar(Sens)))
  } else{
    qfSens <- NULL  
  }
  
  # Grab only qf that are integers  
  if(is.ffdf(dfQf)){
    qfName <-  names(dfQf[vmode(dfQf) == "integer"])
  }else{
    qfName <-  names(Filter(is.integer, dfQf))  
  }
  
  # A list of all the flags to be included in the data removal  
  rpt$listFlag <- lapply(listVar, function(x){ names(dfQf[,grep(pattern = paste(x,qfSens, sep ="|"),x = qfName, ignore.case = TRUE, value = TRUE)])
  })
  
  # Replace the flagged data with NaN, and calculate the total number of bad data points
  rpt$numBad <-  lapply(listVar, function(x){ 
    tmp <- dfQf[,grep(pattern = paste(x,qfSens, sep ="|"), x = qfName, ignore.case = TRUE, value = TRUE)]
    pos <-  which(rowSums(tmp == 1) > 0)
    #numBad[[x]] <<- length(pos)
    rpt$dfData[[x]][pos] <<- NaN 
    length(pos)
  })
  
}