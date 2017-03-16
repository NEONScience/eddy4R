##############################################################################################
#' @title Definition function: Determine the workflow variables

#' @author 
#' David Durden \email{ddurden@battelleecology.org}

#' @description 
#' Definition function. Function to determine the workflow variables by either reading them in from the environmental variables, defining them explicitly, or defining them by default values

#' @param 

#' @return \code{ParaFlow} is a list returned that indicates the workflow control parameters, including \code{ParaFlow$DirInp}, \code{ParaFlow$DirMnt}, \code{ParaFlow$DirOut}, \code{ParaFlow$DirTmp}, \code{ParaFlow$DirWrk}, \code{ParaFlow$Loc}, \code{ParaFlow$DirFilePara}, \code{ParaFlow$FileDp0p}, \code{ParaFlow$Read}, \code{ParaFlow$VersDp}, \code{ParaFlow$VersEddy}. 

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
#Start of function call to determine workflow parameters
##############################################################################################################

def.para.flow <- function(
  ParaFlow = list(
  DirInp = "NA", 
  DirMnt  = "NA",
  DirOut  = "NA",
  DirTmp  = "NA",
  DirWrk  = "NA",
  Loc  = NULL,
  DirFilePara  = NULL,
  FileDp0p  = NULL,
  Read  = "hdf5",
  VersDp  = c("001","004")[1],
  VersEddy  = "cybi"),
  ...
){
  
  #Grab the 
 # ParaFlow <- as.list(match.call(expand.dots = T))[-1]

  print(ParaFlow$DirInp)
 # Para
  
  #ParaFlow <- list(ls())
  return(ParaFlow)
  
}
