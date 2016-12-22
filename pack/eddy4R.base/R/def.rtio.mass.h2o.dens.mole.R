#############################################################################################
#' @title Calculation of the wet mass fraction (specifc humidity) from mole density of water vapor and mole density of dry air

#' @author Stefan Metzger \email{eddy4R.info@gmail.com}
#' Hongyan Luo \email{eddy4R.info@gmail.com}

#' @description Function definition. Calculate the wet mass fraction (specific humidity) of air mixture from mole density of water vapor and mole density of dry air 

#' @param \code{densMoleH2o} A vector containing mole density of water vapor, of class "numeric". [molH2o m-3]
#' @param \code{densMoleAirDry} A vector containing the mole density of dry air, of class "numeric". [mol m-3]

#' @return 
#' The returned object is wet mass fraction (specific humidity)  

#' @references
#' Currently none.

#' @keywords specific humidity, mole density

#' @examples
#' Example 1, this will cause an error message due to densMoleH2o and densMoleAirDry have no units: 
#' def.rtio.mass.h2o.dens.mole(densMoleH2o = 0.3, densMoleAirDry = 41.1)

#' Example 2, assign values and units to variables first, the function should run ok.
#' densMoleH2o = 0.3
#' densMoleAirDry = 41.1
#' attributes(densMoleH2o)$unit = "molH2o m-3"
#' attributes(densMoleAirDry)$unit = "mol m-3"
#' def.rtio.mass.h2o.dens.mole (densMoleH2o, densMoleAirDry)

#' @seealso Currently none.

#' @export

# changelog and author contributions / copyrights
#   Stefan Metzger (2016-09-06)
#     original creation
#   Hongyan Luo (2016-11-20)
#     adjust to eddy4R coding style
###############################################################################################

# Function to calculate the wet mass fraction (specific humidity) of air misxture from mole density of water vapor and mole density of dry air 
def.rtio.mass.h2o.dens.mole <- function(
  
  # mole density of water vapor  
  densMoleH2o,
  
  # mole density of dry air  
  densMoleAirDry  
  
) {
  
  # test for presence of unit attribute
  
  # densMoleH2o
  if(!("unit" %in% names(attributes(densMoleH2o)))) {
    
    stop("def.rtio.mass.h2o.dens.mole(): densMoleH2o is missing unit attribute.")
    
  }
  
  
  # densMoleAirDry
  if(!("unit" %in% names(attributes(densMoleAirDry)))) {
    
    stop("def.rtio.mass.h2o.dens.mole(): densMoleAirDry is missing unit attribute.")
    
  }
  
  
  # test for correct units of input variables
  if(attributes(densMoleH2o)$unit != "molH2o m-3" || attributes(densMoleAirDry)$unit != "mol m-3") {
    
    stop("def.rtio.mass.h2o.dens.mole(): input units are not matching internal units, please check.")
    
  }
  
  
  # calculate the sonic tempertaure
  
  rtioMassH2o <- densMoleH2o * eddy4R.base::Intl.Natu$MolmH2o /
    (densMoleAirDry * eddy4R.base::Intl.Natu$MolmDry + densMoleH2o * eddy4R.base::Intl.Natu$MolmH2o)

# assign output unit
attributes(rtioMassH2o)$unit <- "kg kg-1"

# return results
return(rtioMassH2o) 

}