##############################################################################################
#' @title Definition function: to remove high frequency data points that have failed quality flags

#' @author 
#' Dave Durden \email{ddurden@battelleecology.org}

#' @description 
#' Definition function  to remove high frequency data points that have failed quality flags from a data.frame
#' @param inpList List consisting of \code{ff::ffdf} file-backed objects, in the format provided by function \code{eddy4R.base::wrap.neon.read.hdf5.eddy()}. Of types numeric and integer.
#' 
#' @return The returned object consistes of \code{inpList}, with the bad high frequency quality flagged data replaced with NaN's.

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
  rpt <- inpList
  
  #Determine the sensors that have data and quality flags
  sens <- intersect(names(inpList$data), names(inpList$qfqm))
  
  #test <- def.qf.rmv.data(dfData = inpList$data[[var[2]]], dfQf = inpList$qfqm[[var[2]]], Sens = var[2])
  # Determine quality flags to apply to each stream, quantify flags, and remove bad data across all sensors
  outList <- lapply(sens, function(x){ def.qf.rmv.data(dfData = inpList$data[[x]][], inpList$qfqm[[x]], Sens = x)
  })
  
  #Apply names to the output list
  names(outList) <- sens
  
  #Applying the bad quality flags to the reported output
  lapply(names(outList), function(x) {
    rpt$data[[x]] <<- as.ffdf(outList[[x]]$dfData) 
  })
  
  #Return the list of information with bad data removed
  return(rpt)
}