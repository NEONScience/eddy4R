##############################################################################################
#' @title Definition function: Calculate water vapor wet mass fraction (specific humidity) from water vapor pressure and static pressure

#' @author 
#' Stefan Metzger \email{eddy4R.info@gmail.com} \cr
#' Natchaya Pingintha-Durden \email{ndurden@battelleecology.org}

#' @description Calculate specific humidity from water vapor pressure and static pressure.

#' @param presH2o Either a vector or an object of class numeric of measured water vapor pressure and of the same length as \code{pres}. [Pa]
#' @param pres Either a vector or an object of class numeric of static pressure and of the same length as \code{presH2o}. [Pa]

#' @return Specific humidity and of the same length as \code{presH2o} and \code{pres}. [kg kg-1]

#' @references 
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007.

#' @keywords humidity conversion, specific, ratio

#' @examples
#' def.rtio.mass.h2o.wet.pres.h2o.pres(presH2o = 2212.04 , pres = 65000)
#' def.rtio.mass.h2o.wet.pres.h2o.pres(presH2o = c(2212.04, 348.69, 87.17) , pres = c(65000, 83000, 110000))

#' @seealso Currently none

#' @export

# changelog and author contributions / copyrights
#   Stefan Metzger (2014-11-11)
#     original creation
#   Stefan Metzger (2015-11-28)
#     re-formualtion as function() to allow packaging
#   Natchaya P-Durden (2016-04-08)
#     Initail naming convention for eddy4R
#   Natchaya P-Durden (2016-11-30)
#     rename function to def.rtio.mass.h2o.wet.pres.h2o.pres()
#   Natchaya P-Durden (2018-04-03)
#     update @param format
##############################################################################################

def.rtio.mass.h2o.wet.pres.h2o.pres <- function(presH2o, pres)  {
  #calculation
  rtioMassH2oWet <- (eddy4R.base::IntlNatu$RtioMolmH2oDry * presH2o)/(pres - ((1-eddy4R.base::IntlNatu$RtioMolmH2oDry) * presH2o))
  
  #return reported object
  return(rtioMassH2oWet)
  
  # end function def.rtio.mass.h2o.wet.pres.h2o.pres()
}
