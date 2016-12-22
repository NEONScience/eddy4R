##############################################################################################
#' @title Calculate absolute humidity from water vapor pressure and ambient temperature

#' @author 
#' Stefan Metzger \email{eddy4R.info@gmail.com} \cr
#' Natchaya Pingintha-Durden

#' @description Calculate absolute humidity from water vapor pressure and ambient temperature.

#' @param \code{presH2o} Either a vector or an object of class numeric of measured water vapor pressure and of the same length as \code{temp}. [Pa]
#' @param \code{temp} Either a vector or an object of class numeric of measured air temperature and of the same length as \code{presH2o}. [K]

#' @return Absolute humidity and of the same length as \code{presH2o} and \code{temp}. [kg m-3]

#' @references
#' License: Terms of use of the NEON FIU algorithm repository dated 2015-01-16

#' @keywords humidity conversion

#' @examples 
#' def.dens.mass.h2o.press.h2o.temp(presH2o = 2054.04, temp = 298.15)
#' def.dens.mass.h2o.press.h2o.temp(presH2o = c(42.22, 348.70, 2054.04), temp = c(265.15, 278.15, 298.15))

#' @seealso Currently none

#' @export

# changelog and author contributions / copyrights
#   Stefan Metzger (2014-11-11)
#     original creation
#   Stefan Metzger (2015-11-28)
#     re-formualtion as function() to allow packaging
#   Natchaya P-Durden (2016-11-27)
#   rename function to def.dens.mass.h2o.press.h2o.temp()
##############################################################################################

def.dens.mass.h2o.press.h2o.temp <- function(presH2o, temp)  {
  #calculation
  densMassH2o <- (presH2o * eddy4R.base::Intl.Natu$MolmH2o) / (eddy4R.base::Intl.Natu$Rg * 1e3 * ( temp))
  
  #return reported object
  return(densMassH2o)
  
  # end function def.dens.mass.h2o.press.h2o.temp()
}
