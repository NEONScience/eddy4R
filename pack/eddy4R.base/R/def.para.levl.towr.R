##############################################################################################
#' @title Definition function: Determine tower top measurement level from input dp0p ECTE HDF5 file

#' @author 
#' David Durden \email{ddurden@battelleecology.org}

#' @description 
#' Definition function. Function determines tower top measurement level from input dp0p ECTE HDF5 file structure

#' @param FileIn is the input dp0p ECTE HDF5 file where the parameters are being read from.

#' @return \code{LevlTowr} is returned that indicates the Horizontal and Vertical indices of the tower top measurement level. 

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007.
#' NEON Algorithm Theoretical Basis Document:Eddy Covariance Turbulent Exchange Subsystem Level 0 to Level 0’ data product conversions and calculations (NEON.DOC.000823)
#' NEON Algorithm Theoretical Basis Document:Eddy Covariance Turbulent Exchange Subsystem Level 1 data product calculations (NEON.DOC.000807)

#' @keywords NEON, HDF5, eddy-covariance, ECTE

#' @examples 


#' @seealso Currently none

#' @export

# changelog and author contributions / copyrights
#   Dave Durden (2016-03-12)
#     original creation

##############################################################################################################
#Start of function call to determine tower top level
##############################################################################################################

def.para.levl.towr <- function(
  FileIn
){
  
  if(!base::file.exists(FileIn)) {
    stop("Input file does not exist")
  } 
  
  
  listPara <- rhdf5::h5ls(FileIn, datasetinfo = FALSE)
  
  LevlTowr <- sub("_30m","",unique(listPara[grep("_30m",listPara$name),"name"]))
  
  return(LevlTowr)
  
}
  