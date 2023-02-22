##############################################################################################
#' @title Definition function: Calculation of the virtual temperature

#' @author
#' Stefan Metzger \email{eddy4R.info@gmail.com}

#' @description Function definition. Calculation of the virtual temperature.
#' The virtual temperature is the temperature of a moist air parcel at which a theoretical 
#' dry air parcel would have a total pressure and density equal to the moist parcel of air.

#' @param tempAir A vector containing the air temperature, of class "numeric". [K]
#' @param presH2o A vector containing the water vapor partial pressure, of class "numeric". [Pa]
#' @param presAtm A vector containing the atmospheric pressure, of class "numeric". [Pa]

#' @return 
#' The returned object is the the virtual temperature. [K]

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007
#' http://en.wikipedia.org/wiki/Virtual_temperature

#' @keywords temperature, pressure, virtual

#' @examples
#' Example 1, this will cause an error message due to tempAir, presH2o, presAtm have no units:
#' def.temp.virt.temp.air.pres.h2o.pres.atm(tempAir = 268, presH2o = 30, presAtm = 93344)
#' Example 2, assign values and units to variables first, the function should run ok.
#' tempAir <- 268
#' presH2o <- 30
#' presAtm <- 93344
#' attributes(tempAir)$unit <- "K"
#' attributes(presH2o)$unit <- "Pa"
#' attributes(presAtm)$unit <- "Pa"
#' def.temp.virt.temp.air.pres.h2o.pres.atm(tempAir, presH2o, presAtm)

#' @seealso Currently none.

#' @export

# changelog and author contributions / copyrights
#   Stefan Metzger (2011-03-04)
#     original creation
#   Stefan Metzger (2021-11-16)
#     update to eddy4R terminology and modularize into definition function
###############################################################################################

# molar density of the mixture of dry air and water vapor
def.temp.virt.temp.air.pres.h2o.pres.atm <- function(
  
  # air temperature
  tempAir,
  
  # water vapor partial pressure
  presH2o,
  
  # atmospheric pressure
  presAtm
  
) {
  
  # test for presence of unit attribute
  
  if(!("unit" %in% names(attributes(tempAir)))) {
    
    stop("def.temp.virt.temp.air.pres.h2o.pres.atm(): tempAir is missing unit attribute.")
    
  }
  
  
  if(!("unit" %in% names(attributes(presH2o)))) {
    
    stop("def.temp.virt.temp.air.pres.h2o.pres.atm(): presH2o is missing unit attribute.")
    
  }
  
  if(!("unit" %in% names(attributes(presAtm)))) {
    
    stop("def.temp.virt.temp.air.pres.h2o.pres.atm(): presAtm is missing unit attribute.")
    
  }
  
  # test for correct units of input variables
  if(attributes(tempAir)$unit != "K" || attributes(presH2o)$unit != "Pa" || attributes(presAtm)$unit != "Pa") {
    
    stop("def.temp.virt.temp.air.pres.h2o.pres.atm(): input units are not matching internal units, please check.")
    
  }
  
  
  # calculate the virtual temperature
  tempVirt <- tempAir / (1 - ((presH2o / presAtm) * (1 - eddy4R.base::IntlNatu$RtioMolmH2oDry)) )
  
  # alternate formulation based on specific humidity (retained for future extensibility): tempVirt <- tempAir * (1 + 0.61 * q)

  # assign output unit
  attributes(tempVirt)$unit <- "K"
  
  # return results
  return(tempVirt) 
  
}
