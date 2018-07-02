##############################################################################################
#' @title Definition function: calculate ecse dp04

#' @author 
#' Ke Xu \email{kxu@battelleecology.org}

#' @description 
#' Definition function. Function calculates ecse dp04

#' @param \code{dataInp} Input data. 
#' @param \code{lvlTowr} the tower levels

#' @return \code{rpt} is list returned that consists of the intepolated data. 

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007. \cr
#' NEON Algorithm Theoretical Basis Document:Eddy Covariance Turbulent Exchange Subsystem Level 0 to Level 0’ data product conversions and calculations (NEON.DOC.000823) \cr
#' NEON Algorithm Theoretical Basis Document:Eddy Covariance Turbulent Exchange Subsystem Level 1 data product calculations (NEON.DOC.000807)

#' @keywords ECSE, dp04, storage flux

#' @examples 
#' Currently none

#' @seealso Currently none

#' @export

# changelog and author contributions / copyrights
#   Ke Xu (2018-06-29)
#     original creation
##############################################################################################################
#Start of function call
##############################################################################################################

def.ecse.dp04 <- function(
  dataInp,
  lvlTowr
){
  
  rpt <- list()
  
  #assign time to rpt
  rpt$timeBgn <- dataInp$timeBgn
  rpt$timeEnd <- dataInp$timeEnd 
  
  
  dataInp$timeBgn <- NULL
  dataInp$timeEnd <- NULL
  dataInp <- as.matrix(dataInp)
  
  
  #mean of dp03*(tower height - displacement height)
  rpt$mean <- rowMeans(dataInp) * (max(as.numeric(Para$Flow$Site$DistZaxsLvlMeasTow)))
  
  
  return(rpt)
}