##############################################################################################
#' @title Calculation of total pressure in LI-7200 IRGA cell

#' @author Stefan Metzger \email{eddy4R.info@gmail.com}
#' Hongyan Luo \email{eddy4R.info@gmail.com}

#' @description Function definition. Calculation of total pressure in LI-7200 IRGA cell

#' @param \code{presStbl} A vector containing the atmospheric pressure measured at LI-7550 box. This is use as the static base to calculate the differential pressure between IRGA cell and atmospheric pressure. Class of "numeric". [Pa]
#' @param \code{presDiff} A vector containing the differential pressure between IRGA cell and atmospheric pressure. Class of "numeric". [Pa]

#' @return 
#' The returned object is the total pressure in LI-7200 IRGA cell calculated by summing the atmospheric pressure (presStbl) and the differential pressure (presDiff).  

#' @references
#' Currently none.

#' @keywords pressure, irga

#' @examples
#' Example 1, this will cause an error message due to presStbl and presDiff have no units: 
#' def.pres.sum(presStbl = 993.8, presDiff = -1.109)

#' Example 2, assign valuea and units to variables first, the function should run ok.
#' presStbl = 993.8
#' presDiff = -1.109
#' attributes(presStbl)$unit = "Pa"
#' attributes(presDiff)$unit = "Pa"
#' def.pres.sum(presStbl, presDiff)


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
  presStbl,
  
  # cell pressure measured at LI-7200 IRGA cell
  presDiff
  
) {
  
  # test for correct units of input variables
  
  if(attributes(presStbl)$unit != "Pa" || attributes(presDiff)$unit != "Pa") {
    
    stop("def.pres.sum(): input units are not matching internal units, please check.")
    
  } else {
    
    # calculate total pressure in LI-7200 IRGA cell
    presSum <- (presStbl + presDiff)
    
    # assign output unit
    attributes(presSum)$unit <- "Pa"
    
    # return results
    return(presSum) 
    
  }
}
