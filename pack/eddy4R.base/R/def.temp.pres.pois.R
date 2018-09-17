##############################################################################################
#' @title Definition function: Poisson's equation (adiabatic change) - temperature as function of pressure change

#' @author 
#' Stefan Metzger \email{eddy4R.info@gmail.com} \cr
#' Natchaya Pingintha-Durden \email{ndurden@battelleecology.org}

#' @description Poisson's equation (adiabatic change) - temperature as function of pressure change.

#' @param pres01 Measured air pressure [same unit as reference pressure]
#' @param pres02 Reference pressure [same unit as reference pressure]
#' @param temp01 Measured air temperature [K]
#' @param Kppa Ratio of specific gas constant to specific heat at constant pressure. Default as KppaDry [-]
#' 
#' @return Temperature at refernce pressure [K]

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

#' @keywords Currently none

#' @examples temp02 <- def.temp.pres.pois(temp01 = 298.15, pres01 = 845, pres02 = 1000, Kppa = eddy4R.base::IntlNatu$KppaDry)

#' @seealso Currently none

#' @export

# changelog and author contributions / copyrights
#   Stefan Metzger (2012-04-18)
#     original creation
#   Stefan Metzger (2015-11-28)
#     re-formualtion as function() to allow packaging
#   Natchaya P-Durden (2016-02-26)
#     initail naming convention for eddy4R
#   Natchaya P-Durden (2016-11-27)
#     rename function to def.temp.pres.pois()
#   Natchaya P-Durden (2018-04-03)
#     update @param format
##############################################################################################

def.temp.pres.pois <- function(temp01, pres01, pres02, Kppa = eddy4R.base::IntlNatu$KppaDry)   temp02 = temp01 * (pres02/pres01)^Kppa

  