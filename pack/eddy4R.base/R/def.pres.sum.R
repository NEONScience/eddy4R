##############################################################################################
#' @title Definition function: Calculation of total pressure in LI-7200 IRGA cell

#' @author
#' Stefan Metzger \email{eddy4R.info@gmail.com} \cr
#' Hongyan Luo \email{hluo@battelleecology.org}

#' @description Function definition. Calculation of total pressure from static pressure and differential pressure

#' @param \code{presAtm} A vector containing the atmospheric pressure (or static pressure), of class "numeric". [Pa]
#' @param \code{presDiff} A vector containing the differential pressure, of class "numeric". [Pa]

#' @return 
#' The returned object is the total pressure calculated by summing the static pressure (presAtm) and the differential pressure (presDiff).  

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007.

#' @keywords pressure

#' @examples
#' Example 1, this will cause an error message due to presAtm and presDiff have no units: 
#' def.pres.sum(presAtm = 99380, presDiff = -1109)

#' Example 2, assign valuea and units to variables first, the function should run ok.
#' presAtm = 99380
#' presDiff = -1109
#' attributes(presAtm)$unit = "Pa"
#' attributes(presDiff)$unit = "Pa"
#' def.pres.sum(presAtm, presDiff)


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
  presAtm,
  
  # cell pressure measured at LI-7200 IRGA cell
  presDiff
  
) {
  
  # test if units exist for input variables
  if(!("unit" %in% names(attributes(presAtm)))) {
    
    stop("def.pres.sum(): presAtm is missing unit attribute.")
  }
  
  if(!("unit" %in% names(attributes(presDiff)))) {
    
    stop("def.pres.sum(): presDiff is missing unit attribute.")
  }
  
  
  
    # test for correct units of input variables
  
  if(attributes(presAtm)$unit != "Pa" || attributes(presDiff)$unit != "Pa") {
    
    stop("def.pres.sum(): input units are not matching internal units, please check.")
    
  } 
    
    # calculate total pressure in LI-7200 IRGA cell
    presSum <- (presAtm + presDiff)
    
    # assign output unit
    attributes(presSum)$unit <- "Pa"
    
    # return results
    return(presSum) 
    
  
}
