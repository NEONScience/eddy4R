
##############################################################################################
#' @title Definition function: Calculation of the molar density of the dry air alone

#' @author Stefan Metzger \email{eddy4R.info@gmail.com}
#' Hongyan Luo \email{eddy4R.info@gmail.com}

#' @description Function definition. Calculation of the molar density of the mdry air alone

#' @param \code{densMoleAir} A vector containing the mole density of the air mixture (includes dry air and water vapor), of class "numeric". [mol m-3]
#' @param \code{densMoleH2o} A vector containing the water vapor mole density of the air mixture, of class "numeric". [molH2o m-3]

#' @return 
#' The returned object is the the molar density of the dry air alone  

#' @references
#' Currently none.

#' @keywords mole density

#' @examples
#' Example 1, this will cause an error message due to densMoleAir and densMoleH2o have no units: 
#' def.dens.mole.air.dry(densMoleAir = 37.9, densMoleH2o = 0.3)

#' Example 2, assign values and units to variables first, the function should run ok.
#' densMoleAir = 37.9
#' densMoleH2o = 0.3
#' attributes(densMoleAir)$unit = "mol m-3"
#' attributes(densMoleH2o)$unit = "molH2o m-3"
#' def.dens.mole.air.dry(densMoleAir, densMoleH2o)

#' @seealso Currently none.

#' @export

# changelog and author contributions / copyrights
#   Stefan Metzger (2016-09-06)
#     original creation
#   Hongyan Luo (2016-11-15)
#     adjust to eddy4R coding style
###############################################################################################

# Function to calculate the molar density of the dry air alone
def.dens.mole.air.dry <- function(
  
  # mole density of the air mixture (includes dry air and water vapor) 
  densMoleAir,
  
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
  if(attributes(densMoleAir)$unit != "mol m-3" || attributes(densMoleH2o)$unit != "molH2o m-3") {
    
    stop("def.dens.mole.air.dry(): input units are not matching internal units, please check.")
    
  }
  
  
  # calculate the molar density of the dry air alone
  
  densMoleAirDry <- (densMoleAir - densMoleH2o)
  
  
  # assign output unit
  attributes(densMoleAirDry)$unit <- "mol m-3"
  
  # return results
  return(densMoleAirDry) 
  
}
