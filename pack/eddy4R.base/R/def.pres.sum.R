##############################################################################################
#' @title Calculation of total pressure in LI-7200 IRGA cell

#' @author Stefan Metzger \email{eddy4R.info@gmail.com}
#' Hongyan Luo \email{eddy4R.info@gmail.com}

#' @description Function definition. Calculation of total pressure in LI-7200 IRGA cell

#' @param \code{presAtmBox} A vector containing the atmospheric pressure measured at LI-7550 box, of class "numeric". [Pa]
#' @param \code{presGageCell} A vector containing the cell pressure measured at LI-7200 IRGA cell, of class "numeric". [Pa]

#' @return 
#' The returned object is the total pressure in LI-7200 IRGA cell calculated from the atmospheric pressure (presAtmBox) and the cell pressure (presGageCell).  

#' @references
#' Currently none.

#' @keywords pressure, irga

#' @examples
#' Example 1, this will cause an error message due to presAtmBox and presGageCell have no units: 
#' def.pres.sum(presAtmBox = 993.8, presGageCell = -1.109)

#' Example 2, assign valuea and units to variables first, the function should run ok.
#' presAtmBox = 993.8
#' presGageCell = -1.109
#' attributes(presAtmBox)$unit = "Pa"
#' attributes(presGageCell)$unit = "Pa"
#' def.pres.sum(presAtmBox, presGageCell)


#' @seealso Currently none.

#' @export

# changelog and author contributions / copyrights
#   Stefan Metzger (2016-09-06)
#     original creation
#   Hongyan Luo (2016-10-13)
#     adjust to eddy4R coding style
###############################################################################################
# total pressure in irga cell
def.pres.sum <- function(
  
  # atmospheric pressure measured at LI-7550 box 
  presAtmBox,
  
  # cell pressure measured at LI-7200 IRGA cell
  presGageCell
  
) {
  
  # test for correct units of input variables
  
  if(attributes(presAtmBox)$unit != "Pa" || attributes(presGageCell)$unit != "Pa") {
    
    stop("def.pres.sum(): input units are not matching internal units, please check.")
    
  } else {
    
    # calculate total pressure in LI-7200 IRGA cell
    presSum <- (presAtmBox + presGageCell)
    
    # assign output unit
    attributes(presSum)$unit <- "Pa"
    
    # return results
    return(presSum) 
    
  }
}
