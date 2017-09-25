##############################################################################################
#' @title Definition function: Determine tower top measurement level from input dp0p ECTE HDF5 file

#' @author 
#' David Durden \email{ddurden@battelleecology.org}

#' @description 
#' Definition function. Function determines tower top measurement level from input dp0p ECTE HDF5 file structure

#' @param FileInp is the input dp0p ECTE HDF5 file where the parameters are being read from.

#' @return \code{LevlTowr} is returned that indicates the Horizontal and Vertical indices of the tower top measurement level. 

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007. \cr
#' NEON Algorithm Theoretical Basis Document:Eddy Covariance Turbulent Exchange Subsystem Level 0 to Level 0’ data product conversions and calculations (NEON.DOC.000823) \cr
#' NEON Algorithm Theoretical Basis Document:Eddy Covariance Turbulent Exchange Subsystem Level 1 data product calculations (NEON.DOC.000807)

#' @keywords NEON, HDF5, eddy-covariance, ECTE

#' @examples 
#' Currently none

#' @seealso Currently none

#' @export

# changelog and author contributions / copyrights
#   Dave Durden (2016-03-12)
#     original creation

##############################################################################################################
#Start of function call to determine tower top level
##############################################################################################################

def.para.levl.towr <- function(
  FileInp
){
  
  if(!base::file.exists(FileInp)) {
    stop("Input file does not exist")
  } 
  
  #Grab a list of all the data levels in the HDF5
  listPara <- rhdf5::h5ls(FileInp, datasetinfo = FALSE)
  
  #Grab the group level with the _30m aggregation, remove that part of the string
  LevlTowr <- sub("_30m","",unique(listPara[grep("_30m",listPara$name),"name"])) #This must be a ECTE dp0p HDF5 file to work
  
  
  #Throw an error if length of the returned value is not 1
  if(!base::length(LevlTowr) == 1) {
    stop("Input file is not standard dp0p HDF5 structure")
    #LevlTowr <- unique(listPara[grep("000_",listPara$name),"name"]) #Second way to determine
  }
  
  return(LevlTowr)
  
}
  