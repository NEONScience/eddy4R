##############################################################################################
#' @title Definition function: Calculation of the latent heat of vaporization

#' @author
#' Stefan Metzger \email{eddy4R.info@gmail.com}

#' @description Function definition. Calculation of the latent heat of vaporization.

#' @param tempAir A vector containing the air temperature, of class "numeric". [K]

#' @return 
#' The returned object is the latent heat of vaporization. [J kg-1] == [m2 s-2]

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007
#' Foken 2008, Eq 2.55

#' @keywords temperature, gas phase, liquid phase, heat

#' @examples
#' Example 1, this will cause an error message due to tempAir has no unit:
#' def.heat.h2o.gas.temp(tempAir = 268)
#' Example 2, assign values and units to variables first, the function should run ok.
#' tempAir <- 268
#' attributes(tempAir)$unit <- "K"
#' def.heat.h2o.gas.temp(tempAir)

#' @seealso Currently none.

#' @export

# changelog and author contributions / copyrights
#   Stefan Metzger (2011-03-04)
#     original creation
#   Stefan Metzger (2021-11-17)
#     update to eddy4R terminology and modularize into definition function
###############################################################################################

# latent heat of vaporization
def.heat.h2o.gas.temp <- function(
  
  # air temperature
  tempAir
  
) {
  
  # test for presence of unit attribute
  
  if(!("unit" %in% names(attributes(tempAir)))) {
    
    stop("def.heat.h2o.gas.temp(): tempAir is missing unit attribute.")
    
  }
  
  # test for correct units of input variables
  if(attributes(tempAir)$unit != "K") {
    
    stop("def.heat.h2o.gas.temp(): input units are not matching internal units, please check.")
    
  }
  
  
  # calculate the latent heat of vaporization
  heatH2oGas <- 2500827 - 2360 * eddy4R.base::def.unit.conv(data=as.numeric(tempAir), unitFrom="K", unitTo="C")
  
  # assign output unit
  attributes(heatH2oGas)$unit <- "J kg-1"
  
  # return results
  return(heatH2oGas) 
  
}
