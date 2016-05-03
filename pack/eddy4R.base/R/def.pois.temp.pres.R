##############################################################################################
#' @title Poisson's equation (adiabatic change) - temperature as function of pressure change

#' @author 
#' Stefan Metzger \email{eddy4R.info@gmail.com} \cr
#' Natchaya P-Durden

#' @description Poisson's equation (adiabatic change) - temperature as function of pressure change.

#' @param \code{pres01} Measured air pressure [same unit as reference pressure]
#' @param \code{pres02} Reference pressure [same unit as reference pressure]
#' @param \code{temp01} Measured air temperature [K]
#' @param \code{Kppa} Ratio of specific gas constant to specific heat at constant pressure. Default as KppaDry [-]
#' 
#' @return Temperature at refernce pressure [K]

#' @references Currently none

#' @keywords Currently none

#' @examples temp02 <- def.pois.temp.pres(temp01 = 298.15, pres01 = 845, pres02 = 1000, Kppa = eddy4R.base::Natu$KppaDry)

#' @seealso Currently none

#' @export

# changelog and author contributions / copyrights
#   Stefan Metzger (2012-04-18)
#     original creation
#   Stefan Metzger (2015-11-28)
#     re-formualtion as function() to allow packaging
#   Natchaya P-Durden (2016-02-26)
#     Initail naming convention for eddy4R
##############################################################################################

def.pois.temp.pres <- function(temp01, pres01, pres02, Kppa = eddy4R.base::Natu$KppaDry)   temp02 = temp01 * (pres02/pres01)^Kppa
  