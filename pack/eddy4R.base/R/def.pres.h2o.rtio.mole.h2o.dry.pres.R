##############################################################################################
#' @title Definition function: Calculate water vapor pressure from dry mole fraction and static pressure

#' @author 
#' Stefan Metzger \email{eddy4R.info@gmail.com} \cr
#' Natchaya Pingintha-Durden \email{ndurden@battelleecology.org}

#' @description Calculate water vapor pressure from dry mole fraction and static pressure.

#' @param rtioMoleDryH2o Either a vector or an object of class numeric of water dry mole fraction and of the same length as \code{pres}. [kmol kmol-1]
#' @param pres Either a vector or an object of class numeric of static pressure and of the same length as \code{rtioMoleDryH2o}. [Pa]

#' @return Water vapor pressure and of the same length as \code{rtioMoleDryH2o} and \code{pres}. [Pa]

#' @references 
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

#' @keywords humidity conversion

#' @examples 
#' def.pres.h2o.rtio.mole.h2o.dry.pres(rtioMoleDryH2o = 6.52*1e-6, pres = 83000) 
#' def.pres.h2o.rtio.mole.h2o.dry.pres(rtioMoleDryH2o = c(6.52*1e-6, 11.617*1e-6, 4.931*1e-6), pres = c(83000, 65000, 110000)) 

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
#     rename function to def.pres.h2o.rtio.mole.h2o.dry.pres()
#   Natchaya P-Durden (2018-04-03)
#     update @param format
##############################################################################################

def.pres.h2o.rtio.mole.h2o.dry.pres <- function(rtioMoleDryH2o, pres)  {
  #calculation
  presH2o <- pres * rtioMoleDryH2o / (1 + rtioMoleDryH2o)
  
  #return reported object
  return(presH2o)
  
  # end function def.pres.h2o.rtio.mole.h2o.dry.pres()
}
