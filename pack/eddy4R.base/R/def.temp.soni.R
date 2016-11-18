
##############################################################################################
#' @title Calculation of sonic temperature from speed of sound

#' @author Stefan Metzger \email{eddy4R.info@gmail.com}
#' Hongyan Luo \email{eddy4R.info@gmail.com}

#' @description Function definition. Calculation of sonic temperature from speed of sound using Eq. (9) in Appendix C of CSAT3 Three Dimensional Sonic Anemometer Instruction manual

#' @param \code{veloSoni} A vector containing speed of sound, of class "numeric". [m s-1]
#' @param \code{GammaDry} A vector containing the ratio of specific heat of dry air at constant pressure to that at constant volume, of class "numeric". [dimensionless]
#' @param \code{Rg} A vector containing universal gas constant. [kg m2 s-2 K-1 kmol-1]
#' @param \code{Molm} A vector containing molar mass of dry air. [kg kmol-1]

#' @return 
#' The returned object is sonic temperature  

#' @references
#' Currently none.

#' @keywords temperature, sonic anemometer

#' @examples
#' Example 1, this will cause an error message due to densMoleAir and densMoleH2o have no units: 
#' def.dens.mole.air.dry(densMoleAir = 41.38, densMoleH2o = 286)

#' Example 2, assign values and units to variables first, the function should run ok.
#' densMoleAir = 86000
#' densMoleH2o = 286
#' attributes(densMoleAir)$unit = "mol m-3"
#' attributes(densMoleH2o)$unit = "mol m-3"
#' def.dens.mole.air.dry(densMoleAir, densMoleH2o)

#' @seealso Currently none.

#' @export

# changelog and author contributions / copyrights
#   Stefan Metzger (2016-09-06)
#     original creation
#   Hongyan Luo (2016-11-17)
#     adjust to eddy4R coding style
###############################################################################################

# sonic temperature from speed of sound 
data$soni$T_SONIC <- ff::as.ff(data$soni$veloSoni^2 / eddy4R.base::Natu$GmmaDry / 
                                 (eddy4R.base::Natu$Rg / eddy4R.base::Natu$MolmDry))
base::attr(x = data$soni$T_SONIC, which = "unit") <- "K"




def.temp.soni <- function(
  
  # speed of sound 
  veloSoni,
  
  # water vapor mole density of the air mixture
  densMoleH2o
  
) {
  
  # test for presence of unit attribute
  
  if(!("unit" %in% names(attributes(densMoleAir)))) {
    
    stop("def.dens.mole.air.dry(): densMoleAir is missing unit attribute.")
    
  }
  
  
  if(!("unit" %in% names(attributes(densMoleH2o)))) {
    
    stop("def.dens.mole.air.dry(): densMoleH2o is missing unit attribute.")
    
  }
  
  # test for correct units of input variables
  # if(attributes(densMoleAir)$unit != "mol m-3" || attributes(densMoleH2o)$unit != "mol m-3") {
  
  #  stop("def.dens.mole.air.dry(): input units are not matching internal units, please check.")
  
  #}
  
  
  # calculate the molar density of the dry air alone
  
  densMoleAirDry <- (densMoleAir - densMoleH2o)
  
  
  # assign output unit
  attributes(densMoleAirDry)$unit <- "mol m-3"
  
  # return results
  return(densMoleAirDry) 
  
}
