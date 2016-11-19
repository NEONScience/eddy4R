
##############################################################################################
#' @title Calculation of sonic temperature from speed of sound

#' @author Stefan Metzger \email{eddy4R.info@gmail.com}
#' Hongyan Luo \email{eddy4R.info@gmail.com}

#' @description Function definition. Calculation of sonic temperature from speed of sound using Eq. (9) in Appendix C of CSAT3 Three Dimensional Sonic Anemometer Instruction manual

#' @param \code{veloSoni} A vector containing speed of sound, of class "numeric". [m s-1]
#' @param \code{GammaDry} A vector containing the ratio of specific heat of dry air at constant pressure to that at constant volume, of class "numeric". [dimensionless]
#' @param \code{Rg} A vector containing universal gas constant. [kg m2 s-2 K-1 mol-1]
#' @param \code{Molm} A vector containing molar mass of dry air. [kg mol-1]

#' @return 
#' The returned object is sonic temperature  

#' @references
#' Currently none.

#' @keywords temperature, sonic anemometer

#' @examples
#' Example 1, this will cause an error message due to veloSoni has no units: 
#' def.temp.soni(veloSoni = 344, GammaDry = 1.4, Rg = 8.314, Molm = 0.028964)

#' Example 2, assign values and units to variables first, the function should run ok.
#' veloSoni = 344
#' GammaDry = 1.4
#' Rg = 8.314
#' Molm = 0.028964
#' attributes(veloSoni)$unit = "m s-1"
#' attributes(GammaDry)$unit = "-"
#' attributes(Rg)$unit = "kg m2 s-2 K-1 mol-1"
#' attributes(Molm)$unit = "kg mol-1"
#' def.temp.soni(veloSoni, GammaDry, Rg, Molm)

#' @seealso Currently none.

#' @export

# changelog and author contributions / copyrights
#   Stefan Metzger (2016-09-06)
#     original creation
#   Hongyan Luo (2016-11-17)
#     adjust to eddy4R coding style
###############################################################################################

# Function to calculate sonic temperature from speed of sound 
def.temp.soni <- function(
  
  # speed of sound 
  veloSoni,
  
  # ratio of specific heat of dry air
  GammaDry,
  
  # universal gas constant
  Rg,
  
  # molar mass of air constituent (dry air in this case)
  Molm
  
) {
  
  # test for presence of unit attribute
  
  if(!("unit" %in% names(attributes(veloSoni)))) {
    
    stop("def.temp.soni(): veloSoni is missing unit attribute.")
    
  }
  
 
  # test for correct units of input variables
   if(attributes(veloSoni)$unit != "m s-1" ) {
  
    stop("def.temp.soni(): input units are not matching internal units, please check.")
  
  }
  
  
  # calculate the sonic tempertaure
  
  tempSoni <- (veloSoni^2/GammaDry/(Rg/Molm))
  
  
  # assign output unit
  attributes(tempSoni)$unit <- "K"
  
  # return results
  return(tempSoni) 
  
}
