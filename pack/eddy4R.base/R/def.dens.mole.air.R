
##############################################################################################
#' @title Definition function: Calculation of the molar density of the mixture of dry air and water vapor

#' @author Stefan Metzger \email{eddy4R.info@gmail.com}
#' Hongyan Luo \email{eddy4R.info@gmail.com}

#' @description Function definition. Calculation of the molar density of the misxture of dry air and water vapor

#' @param \code{presSum} A vector containing the total pressure of the air mixture, of class "numeric". [Pa]
#' @param \code{tempMean} A vector containing the mean temperatrue of the air mixture, of class "numeric". [K]

#' @return 
#' The returned object is the the molar density of the mixture of dry air and water vapor  

#' @references
#' Currently none.

#' @keywords mole density, temperature, pressure

#' @examples
#' Example 1, this will cause an error message due to tempIn and tempOut have no units: 
#' def.dens.mole.air(presSum = 86000, tempMean = 286)

#' Example 2, assign values and units to variables first, the function should run ok.
#' presSum = 86000
#' tempMean = 286
#' attributes(presSum)$unit = "Pa"
#' attributes(tempMean)$unit = "K"
#' def.dens.mole.air(presSum, tempMean)

#' @seealso Currently none.

#' @export

# changelog and author contributions / copyrights
#   Stefan Metzger (2016-09-06)
#     original creation
#   Hongyan Luo (2016-11-14)
#     adjust to eddy4R coding style
###############################################################################################

# molar density of the mixture of dry air and water vapor
def.dens.mole.air <- function(
  
  # total pressure of the air mixture 
  presSum,
  
  # mean temperatrue of the air mixture
  tempMean
  
 ) {
  
  # test for presence of unit attribute

  if(!("unit" %in% names(attributes(presSum)))) {
    
    stop("def.dens.mole.air(): presSum is missing unit attribute.")
    
  }
  

  if(!("unit" %in% names(attributes(tempMean)))) {
    
    stop("def.dens.mole.air(): tempMean is missing unit attribute.")
    
  }
  
  # test for correct units of input variables
  if(attributes(presSum)$unit != "Pa" || attributes(tempMean)$unit != "K") {
    
    stop("def.dens.mole.air(): input units are not matching internal units, please check.")
    
  }
  
 
    # calculate the molar density of the mixture of dry air and water vapor
    
    densMoleAir <- presSum/eddy4R.base::IntlNatu$Rg/tempMean
                   
  
  # assign output unit
  attributes(densMoleAir)$unit <- "mol m-3"
  
  # return results
  return(densMoleAir) 
  
}
