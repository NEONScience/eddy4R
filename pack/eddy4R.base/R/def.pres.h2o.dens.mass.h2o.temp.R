##############################################################################################
#' @title Definition function: Calculate water vapor pressure from absolute humidity and ambient temperature

#' @author 
#' Stefan Metzger \email{eddy4R.info@gmail.com} \cr
#' Natchaya Pingintha-Durden \email{ndurden@battelleecology.org}

#' @description Calculate water pressure from absolute humidity and ambient temperature.

#' @param \code{densMassH2o} Either a vector or an object of class numeric of absolute humidity and of the same length as \code{temp}. [kg m-3]
#' @param \code{temp} Either a vector or an object of class numeric of measured air temperature and of the same length as \code{densMassH2o}. [K]

#' @return Water vapor pressure and of the same length as \code{densMassH2o} and \code{temp}. [Pa]

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

#' @keywords humidity conversion

#' @examples 
#' def.pres.h2o.dens.mass.h2o.temp(densMassH2o = 1.493*1e-2, temp = 298.15)
#' def.pres.h2o.dens.mass.h2o.temp(densMassH2o = c(3.451*1e-4, 2.717*1e-3, 1.493*1e-2), temp = c(265.15, 278.15, 298.15))

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
#     rename function to def.pres.h2o.dens.mass.h2o.temp()
##############################################################################################

def.pres.h2o.dens.mass.h2o.temp <- function(densMassH2o, temp)  {
  #calculation
  presH2o <- (densMassH2o * eddy4R.base::IntlNatu$Rg * 1e3 * temp) / eddy4R.base::IntlNatu$MolmH2o
  
  #return reported object
  return(presH2o)
  
  # end function def.pres.h2o.dens.mass.h2o.temp()
}
