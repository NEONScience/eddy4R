##############################################################################################
#' @title Calculate water vapor wet mass fraction (specific humidity) from water vapor pressure and static pressure

#' @author 
#' Stefan Metzger \email{eddy4R.info@gmail.com} \cr
#' Natchaya Pingintha-Durden

#' @description Calculate specific humidity from water vapor pressure and static pressure.

#' @param \code{presH2o} Either a vector or an object of class numeric of measured water vapor pressure and of the same length as \code{pres}. [Pa]
#' @param \code{pres} Either a vector or an object of class numeric of static pressure and of the same length as \code{presH2o}. [Pa]

#' @return Specific humidity and of the same length as \code{presH2o} and \code{pres}. [kg kg-1]

#' @references 
#' License: Terms of use of the NEON FIU algorithm repository dated 2015-01-16

#' @keywords humidity conversion, specific, ratio

#' @examples
#' def.rtio.mass.h2o(presH2o = 2212.04 , pres = 65000)
#' def.rtio.mass.h2o(presH2o = c(2212.04, 348.69, 87.17) , pres = c(65000, 83000, 110000))

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

def.rtio.mass.h2o <- function(presH2o, pres)  {
  #calculation
  rtioMassH2oWet <- (eddy4R.base::Natu$RtioMolmH2oDry * presH2o)/(pres - ((1-eddy4R.base::Natu$RtioMolmH2oDry) * presH2o))
  
  #return reported object
  return(rtioMassH2oWet)
  
  # end function def.rtio.mass.h2o()
}
