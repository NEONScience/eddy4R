##############################################################################################
#' @title Definition function: spatial intepolation of data at discontinous levels into spatially continous data 

#' @author 
#' Ke Xu \email{kxu@battelleecology.org}

#' @description 
#' Definition function. Function spatial intepolation of data at discontinous levels into spatially continous data 

#' @param \code{dataInp} Input data. 
#' @param \code{methItpl} intepolation method: linear intepolation is the default method. 
#' @param \code{resoSpacOut} the output spatial resolution
#' @param \code{lvlTowr} the tower levels

#' @return \code{rpt} is list returned that consists of the intepolated data. 

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007. \cr
#' NEON Algorithm Theoretical Basis Document:Eddy Covariance Turbulent Exchange Subsystem Level 0 to Level 0’ data product conversions and calculations (NEON.DOC.000823) \cr
#' NEON Algorithm Theoretical Basis Document:Eddy Covariance Turbulent Exchange Subsystem Level 1 data product calculations (NEON.DOC.000807)

#' @keywords space, spatial, intepolate, ECSE

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

def.itpl.spac <- function(
  dataInp,
  methItpl,
  resoSpacOut,
  lvlTowr
){
  
  
  lvlTowr <- as.numeric(lvlTowr)
  
  #dp03 output vertical standard
  #vertical resolution is 0.1 m
  spacStad <- c(1:(max(lvlTowr)/resoSpacOut)) * resoSpacOut
  
  
  #determine which datapoints to assess     
  whrLgth <- length(which(!is.na(dataInp)))
  
  #more than 2 values (minimum required by approx() function)
  if(whrLgth >= 2) {
    
    if(methItpl == "linear"){
      rpt <- zoo::na.approx(object=as.vector(dataInp), x=#dataInp$timeFrac
                                  lvlTowr
                                , xout=spacStad
                                , method = "linear", 
                                na.rm=TRUE#,#if you want begining and end to be constant fitted, na.rm=TRUE
                                #rule=1, f=0
      )
      
      rpt <- c(rep(rpt[1], length(spacStad) - length(rpt)), rpt)
    }
    
    
  } else {
    
    if(whrLgth == 1) rpt <- rep(dataInp[which(!is.na(dataInp))], length(spacStad))
    
    if(whrLgth == 0) rpt <- rep(NaN, length(spacStad))
  }
  
  return(rpt)
}