##############################################################################################
#' @title Definition function: Poisson's equation (adiabatic change) - density as function of temperature change

#' @author 
#' Stefan Metzger \email{eddy4R.info@gmail.com} \cr
#' Natchaya Pingintha-Durden \email{ndurden@battelleecology.org}

#' @description Poisson's equation (adiabatic change) - density as function of temperature change.

#' @param \code{dens01} Measured density, Amount per volume [same unit as returned density, e.g. kg/m3 or kmol/m3].
#' @param \code{temp01} Measured air temperature [K]
#' @param \code{temp02} Reference temperature [K]
#' @param \code{Kppa} Ratio of specific gas constant to specific heat at constant pressure. Default as KppaDry [-]

#' @return Densities at reference temperature [same unit as measured density]

#' @references 
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

#' @keywords Currently none

#' @examples dens02 <- def.dens.temp.pois(dens01 = 1.056, temp01 = 298.15, temp02 = 288.15, Kppa = eddy4R.base::IntlNatu$KppaDry)

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
#     rename function to def.dens.temp.pois()
##############################################################################################

def.dens.temp.pois <- function(dens01, temp01, temp02, Kppa = eddy4R.base::IntlNatu$KppaDry)  dens02 <- dens01 * (temp02/temp01)^((1-Kppa)/Kppa)
