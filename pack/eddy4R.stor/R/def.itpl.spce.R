##############################################################################################
#' @title Definition function: spatial intepolation of data at discontinous levels into spatially continous data 

#' @author 
#' Ke Xu \email{kxu@battelleecology.org}

#' @description 
#' Definition function. Function spatial intepolation of data at discontinous levels into spatially continous data 

#' @param \code{dataInp} Input data. 
#' @param \code{methItpl} intepolation method: "linear" or "constant", linear intepolation is the default method. Reference for constant spatial intepolation: Montagnani et al., 2018, Estimating the storage term in eddy covariance measurements: the ICOS methodology 
#' @param \code{resoSpceOut} the output spatial resolution
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
#   Ke Xu (2018-07-07)
#     apply eddy4R terms: whr-> set; spac -> spce
#   Ke Xu (2018-08-30)
#     add rectangular interpolation method (Montagnani et al., 2018, Estimating the storage term in eddy covariance measurements: the ICOS methodology)
##############################################################################################################
#Start of function call
##############################################################################################################

def.itpl.spce <- function(
  dataInp,
  methItpl = "linear",
  resoSpceOut,
  lvlTowr
){
  
  
  lvlTowr <- as.numeric(lvlTowr)
  
  #dp03 output vertical standard
  #vertical resolution is 0.1 m
  spceStad <- c(1:(max(lvlTowr)/resoSpceOut)) * resoSpceOut
  
  
  #determine which datapoints to assess     
  setLgth <- length(which(!is.na(dataInp)))
  
  #more than 2 values (minimum required by approx() function)
  if(setLgth >= 2) {
    
    if(methItpl == "linear"){
      rpt <- zoo::na.approx(object=as.vector(dataInp), x=#dataInp$timeFrac
                              lvlTowr
                            , xout=spceStad
                            , method = "linear", 
                            na.rm=TRUE,#if you want begining and end to be constant fitted, na.rm=TRUE
                            rule=2#, f=0
      )
      
      #rpt <- c(rep(rpt[1], length(spceStad) - length(rpt)), rpt)
    } 
    
    if(methItpl == "constant"){
      rpt <- rev(zoo::na.approx(object=rev(as.vector(dataInp)), x=#dataInp$timeFrac
                                  lvlTowr
                                , xout=spceStad
                                , method = methItpl, 
                                na.rm=TRUE,#if you want begining and end to be constant fitted, na.rm=TRUE, rule=2 set as the closest data
                                rule=2#, f=0
      ))
      
    } 
    
    
  } else {
    
    if(setLgth == 1) rpt <- rep(dataInp[which(!is.na(dataInp))], length(spceStad))
    
    if(setLgth == 0) rpt <- rep(NaN, length(spceStad))
  }
  
  return(rpt)
}