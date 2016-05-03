##############################################################################################
#' @title Calculate water vapor pressure from absolute humidity and ambient temperature

#' @author 
#' Stefan Metzger \email{eddy4R.info@gmail.com} \cr
#' Natchaya Pingintha-Durden

#' @description Calculate water pressure from absolute humidity and ambient temperature.

#' @param \code{densMassH2o} Either a vector or an object of class numeric of absolute humidity and of the same length as \code{temp}. [kg m-3]
#' @param \code{temp} Either a vector or an object of class numeric of measured air temperature and of the same length as \code{densMassH2o}. [K]

#' @return Water vapor pressure and of the same length as \code{densMassH2o} and \code{temp}. [Pa]

#' @references
#' License: Terms of use of the NEON FIU algorithm repository dated 2015-01-16

#' @keywords humidity conversion

#' @examples 
#' def.pres.h2o.temp(densMassH2o = 1.493*1e-2, temp = 298.15)
#' def.pres.h2o.temp(densMassH2o = c(3.451*1e-4, 2.717*1e-3, 1.493*1e-2), temp = c(265.15, 278.15, 298.15))

#' @seealso Currently none

#' @export

# changelog and author contributions / copyrights
#   Stefan Metzger (2014-11-11)
#     original creation
#   Stefan Metzger (2015-11-28)
#     re-formualtion as function() to allow packaging
#   Natchaya P-Durden (2016-04-08)
#     Initail naming convention for eddy4R
##############################################################################################

def.pres.h2o.temp <- function(densMassH2o, temp)  {
  #calculation
  presH2o <- (densMassH2o * eddy4R.base::Natu$Rg * 1e3 * temp) / eddy4R.base::Natu$MolmH2o
  
  #return reported object
  return(presH2o)
  
  # end function def.pres.h2o.temp()
}
