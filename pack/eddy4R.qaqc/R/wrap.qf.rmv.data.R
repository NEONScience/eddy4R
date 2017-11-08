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


wrap.qf.rmv.data <- function(inpList){
  
  #Initialize reporting list
  rpt <- list()
  
  #Determine the sensors that have data and quality flags
  sens <- intersect(names(inpList$data), names(inpList$qfqm))
  
  #test <- def.qf.rmv.data(dfData = inpList$data[[var[2]]], dfQf = inpList$qfqm[[var[2]]], Sens = var[2])
  # Determine quality flags to apply to each stream, quantify flags, and remove bad data across all sensors
  rpt <- lapply(sens, function(x){ def.qf.rmv.data(dfData = inpList$data[[x]], inpList$qfqm[[x]], Sens = x)
  })
  
  #Apply names to the output list
  names(rpt) <- sens
  
}