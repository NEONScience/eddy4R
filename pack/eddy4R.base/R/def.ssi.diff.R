##############################################################################################
#' @title Definition function: Calculate delta(or difference) of signal strength for LI-7200 IRGA

#' @author
#' Stefan Metzger \email{eddy4R.info@gmail.com} \cr
#' Hongyan Luo \email{hluo@battelleecology.org}

#' @description Function definition. Calculation of signal strength difference for LI-7200 IRGA.

#' @param ssiCo2 A vector containing the CO2 signal strength, of class "numeric". [unitless]
#' @param ssiH2o A vector containing the H2O signal strength, of class "numeric". [unitless]

#' @return 
#' The returned object is the signal strength difference between the CO2 signal strength and the H2O signal strength.  

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007.

#' @keywords signal strength, irga

#' @examples
#' example 1 (This will give error message becuase ssiCo2 and ssiH2o have no units):
#' def.ssi.diff(ssiCo2 = 0.550, ssiH2o =0.561) 

#' example 2 (Assign the units and values to the variable before run function, which shoudl work fine.)
#' ssiCo2 =0.550
#' ssiH2o = 0.561 
#' attributes(ssiCo2)$unit <- "-"
#' attributes(ssiH2o)$unit <- "-"
#' def.ssi.diff(ssiCo2, ssiH2o)

#' @seealso Currently none.

#' @export

# changelog and author contributions / copyrights
#   Stefan Metzger (2016-09-06)
#     original creation
#   Hongyan Luo (2016-10-10)
#     adjust to eddy4R coding style
#   Natchaya P-Durden (2018-04-03)
#     update @param format
##############################################################################################

def.ssi.diff <- function(
  # CO2 signal strength 
  ssiCo2,
  # H2O signal strength
  ssiH2o
) {
  
  # test for presence of unit attribute
  # CO2 signal strength
  if(!("unit" %in% names(attributes(ssiCo2)))) {
    
    stop("def.ssi.diff(): ssiCo2 is missing unit attribute.")
    
  }
  
  # H2O signal strength
  if(!("unit" %in% names(attributes(ssiH2o)))) {
    
    stop("def.ssi.diff(): ssiH2o is missing unit attribute.")
    
  }
  
   # test for correct units of input variables
  
  if(attributes(ssiCo2)$unit != "-" || attributes(ssiH2o)$unit != "-") {
    
    stop("def.ssi.diff(): input units are not matching internal units, please check.")
    
  } else {
    
    # calculate signal strength difference
    ssiDiff <- ssiCo2 - ssiH2o
    
    # assign output unit
    attributes(ssiDiff)$unit <- "-"
    
    # return results
    return(ssiDiff) 
    
  }
}

