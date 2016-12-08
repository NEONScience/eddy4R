
##############################################################################################
#' @title Calculation of sonic temperature from speed of sound

#' @author Stefan Metzger \email{eddy4R.info@gmail.com}
#' Hongyan Luo \email{eddy4R.info@gmail.com}

#' @description Function definition. Calculation of sonic temperature from speed of sound using Eq. (9) in Appendix C of CSAT3 Three Dimensional Sonic Anemometer Instruction manual

#' @param \code{veloSoni} A vector containing speed of sound, of class "numeric". [m s-1]

#' @return 
#' The returned object is sonic temperature  

#' @references
#' CSAT3 Three Dimensional Sonic Anemometer Instruction Mannual (Revisoin 2/15). Logan, Utah, USA. Campbell Scientifc, Inc. (2015)

#' @keywords temperature, sonic anemometer

#' @examples
#' Example 1, this will cause an error message due to veloSoni has no units: 
#' def.temp.soni(veloSoni = 344)

#' Example 2, assign values and units to variables first, the function should run ok.
#' veloSoni = 344
#' attributes(veloSoni)$unit = "m s-1"
#' def.temp.soni(veloSoni)

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
  veloSoni
  
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
  
  tempSoni <- (veloSoni^2/eddy4R.base::Natu$GmmaDry/(eddy4R.base::Natu$Rg/eddy4R.base::Natu$MolmDry))
  
  
  # assign output unit
  attributes(tempSoni)$unit <- "K"
  
  # return results
  return(tempSoni) 
  
}
