##############################################################################################
#' @title Definition function: Calculate water vapor pressure from saturated water vapor pressure and relative humidity

#' @author 
#' Stefan Metzger \email{eddy4R.info@gmail.com} \cr
#' Natchaya Pingintha-Durden \email{ndurden@battelleecology.org}

#' @description Calculate water vapor pressure from saturated water vapor pressure and relative humidity.

#' @param presH2oSat Either a vector or an object of class numeric of saturated water vapor pressure and of the same length as \code{rh}. [Pa]
#' @param rh Either a vector or an object of class numeric of relative humidity and of the same length as \code{presH2oSat}. [-]

#' @return Water vapor pressure and of the same length as \code{presH2oSat} and \code{rh}. [Pa]

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

#' @keywords humidity conversion

#' @examples 
#' def.pres.h2o.pres.h2o.sat.rh(presH2oSat = 3160.057, rh = 0.65)
#' def.pres.h2o.pres.h2o.sat.rh(presH2oSat = c(422.19, 1701.67, 6265.31), rh = c(0.10, 0.45, 0.80))

#' @seealso Currently none

#' @export

# changelog and author contributions / copyrights
#   Stefan Metzger (2014-11-11)
#     original creation
#   Stefan Metzger (2015-11-28)
#     re-formualtion as function() to allow packaging
#   Natchaya P-Durden (2016-04-08)
#     initail naming convention for eddy4R
#   Natchaya P-Durden (2016-11-27)
#     rename function to def.pres.h2o.pres.h2o.sat.rh()
#   Natchaya P-Durden (2018-04-03)
#     update @param format
##############################################################################################

def.pres.h2o.pres.h2o.sat.rh <- function(presH2oSat, rh) {
  #calculation
  presH2o <- presH2oSat * rh
  
  #return reported object
  return(presH2o)
  
  # end function def.pres.h2o.pres.h2o.sat.rh()
}

