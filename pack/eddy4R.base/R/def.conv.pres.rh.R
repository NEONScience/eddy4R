##############################################################################################
#' @title Calculation of RH from water vapor partial pressure and saturation pressure 
#' @author Stefan Metzger \email{eddy4R.info@gmail.com}
#' Hongyan Luo \email{eddy4R.info@gmail.com}

#' @description Function definition. Calculation of RH from water vapor partial pressure and saturation pressure  

#' @param \code{presH2o} A vector containing the water vapor partial pressure, of class "numeric". [Pa]
#' @param \code{presH2oSat} A vector containing the water vapor saturation pressure, of class "numeric". [Pa]

#' @return 
#' The returned object is the RH calculated from water vapor partial pressure and saturation pressure    

#' @references
#' Currently none.

#' @keywords RH, water vapor,partial pressure, saturation pressure, irga

#' @examples
#' Example 1, this will cause an error message due to presH2o and presH2oSat have no units: 
#' def.conv.pres.rh(presH2o = 1020, presH2oSat = 2500)

#' Example 2, assign values and units to variables first, the function should run ok.
#' presH2o = 1020
#' presH2oSat = 2500
#' attributes(presH2o)$unit = "Pa"
#' attributes(presH2oSat)$unit = "Pa"
#' def.temp.mean(presH2o, presH2oSat)

#' @seealso Currently none.

#' @export

# changelog and author contributions / copyrights
#   Stefan Metzger (2016-09-06)
#     original creation
#   Hongyan Luo (2016-10-23)
#     adjust to eddy4R coding style
###############################################################################################
# Calculation of RH
def.conv.pres.rh <- function(
  
  # water vapor partial pressure 
  presH2o,
  
  # temperatrue at IRGA cell outlet
  presH2oSat
  
) {
  
  # test for correct units of input variables
  
  if(attributes(presH2o)$unit != "Pa" || attributes(presH2oSat)$unit != "Pa") {
    
    stop("def.conv.pres.rh(): input units are not matching internal units, please check.")
    
  } else {
    
    # calculate the RH
    rh <- (presH2o / presH2oSat * 100)
    
    # assign output unit
    attributes(rh)$unit <- "percent"
    
    # return results
    return(rh) 
    
  }
}


