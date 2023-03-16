##############################################################################################
#' @title Definition function: Calculation of the volumetric heat capacity

#' @author
#' Stefan Metzger \email{eddy4R.info@gmail.com}

#' @description Function definition. Calculation of the volumetric heat capacity

#' @param densMoleAirDry A vector containing the dry air mole density, of class "numeric". [mol m-3]
#' @param densMoleH2o A vector containing the H2O mole density, of class "numeric". [mol m-3]

#' @return 
#' The returned object is the volumetric heat capacity. [kg m-1 s-2 K-1] = specific heat [J kg-1 K-1] x mole density [mol m-3] x mole mass [kg mol-1]

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

#' @keywords sensible heat flux, temperature, heat

#' @examples
#' Example 1, this will cause an error message due to densMoleAirDry, densMoleH2o have no units:
#' def.heat.air.wet(densMoleAirDry = 41.9054, densMoleH2o = 0.01342256)
#' Example 2, assign values and units to variables first, the function should run ok.
#' densMoleAirDry = 41.9054; attributes(densMoleAirDry)$unit <- "mol m-3"
#' densMoleH2o = 0.01342256; attributes(densMoleH2o)$unit <- "mol m-3"
#' def.heat.air.wet(densMoleAirDry, densMoleH2o)

#' @seealso Currently none.

#' @export

# changelog and author contributions / copyrights
#   Stefan Metzger (2011-03-04)
#     original creation
#   Stefan Metzger (2022-03-22)
#     update to eddy4R terminology and modularize into definition function
###############################################################################################

# volumetric heat capacity
def.heat.air.wet <- function(

  # dry air mole density
  densMoleAirDry,

  # H2O mole density
  densMoleH2o
  
) {
  
  # test for presence of unit attribute
  
    # densMoleAirDry
    if(!("unit" %in% names(attributes(densMoleAirDry)))) {
      
      stop("def.heat.air.wet(): densMoleAirDry is missing unit attribute.")
      
    }

    # densMoleH2o
    if(!("unit" %in% names(attributes(densMoleH2o)))) {
      
      stop("def.heat.air.wet(): densMoleH2o is missing unit attribute.")
      
    }
  
  # test for correct units of input variables
  if(attributes(densMoleAirDry)$unit != "mol m-3" | attributes(densMoleH2o)$unit != "mol m-3") {
    
    stop("def.heat.air.wet(): input units are not matching internal units, please check.")
    
  }
  
  # calculate the volumetric heat capacity
  heatAirWet <- 
    # dry air 
    eddy4R.base::IntlNatu$CpDry * densMoleAirDry * eddy4R.base::IntlNatu$MolmDry + 
    # water vapor  
    eddy4R.base::IntlNatu$CpH2o * densMoleH2o * eddy4R.base::IntlNatu$MolmH2o    
  
  # assign output unit
  base::attr(heatAirWet, which = "unit") <- "kg m-1 s2 K-1"
  
  # return results
  base::return(heatAirWet)
  
}
