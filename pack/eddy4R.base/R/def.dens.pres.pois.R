##############################################################################################
#' @title Definition function: Poisson's equation (adiabatic change) - density as function of pressure change

#' @author 
#' Stefan Metzger \email{eddy4R.info@gmail.com} \cr
#' Natchaya Pingintha-Durden \email{ndurden@battelleecology.org}

#' @description Poisson's equation (adiabatic change) - density as function of pressure change.

#' @param \code{dens01} Measured density, Amount per volume [same unit as returned density, e.g. kg/m3 or kmol/m3].
#' @param \code{pres01} Measured air pressure [same unit as reference pressure]
#' @param \code{pres02} Reference pressure [same unit as measured air pressure]
#' @param \code{Kppa} Ratio of specific gas constant to specific heat at constant pressure. Default as KppaDry [-]

#' @return Densities at reference pressure [same unit as measured density]

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

#' @keywords Currently none

#' @examples dens02 <- def.dens.pres.pois(dens01 = 1.056, pres01 = 845, pres02 = 1000, Kppa = eddy4R.base::IntlNatu$KppaDry)

#' @seealso Currently none

#' @export
#' 
# changelog and author contributions / copyrights
#   Stefan Metzger (2012-04-18)
#     original creation
#   Stefan Metzger (2015-11-28)
#     re-formualtion as function() to allow packaging
#   Natchaya P-Durden (2016-02-26)
#     initail naming convention for eddy4R
#   Natchaya P-Durden (2016-11-27)
#     rename function to def.dens.pres.pois()
##############################################################################################

def.dens.pres.pois <- function(dens01, pres01, pres02, Kppa = eddy4R.base::IntlNatu$KppaDry)   dens02 = dens01 * (pres02/pres01)^(1-Kppa)
