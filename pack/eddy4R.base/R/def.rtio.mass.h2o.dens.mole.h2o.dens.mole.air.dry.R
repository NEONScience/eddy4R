#############################################################################################
#' @title Calculation of the wet mass fraction (specifc humidity) from mole density of water vapor and mole density of dry air

#' @author Stefan Metzger \email{eddy4R.info@gmail.com}
#' Hongyan Luo \email{eddy4R.info@gmail.com}

#' @description Function definition. Calculate the wet mass fraction (specific humidity) of air misxture from mole density of water vapor and mole density of dry air 

#' @param \code{densMoleH2o} A vector containing mole density of water vapor, of class "numeric". [mol m-3]
#' @param \code{densMoleAirDry} A vector containing the mole density of dry air, of class "numeric". [mol m-3]

#' @return 
#' The returned object is swet mass fraction (specific humidity)  

#' @references
#' Currently none.

#' @keywords specific humidity, mole density

#' @examples
#' Example 1, this will cause an error message due to densMoleH2o and densMoleAirDry have no units: 
#' def.rtio.mass.h2o.dens.mole.h2o.dens.mole.air.dry(densMoleH2o = , densMoleAirDry = )

#' Example 2, assign values and units to variables first, the function should run ok.
#' densMoleH2o = 
#' densMoleAirDry =
#' attributes(densMoleH2o)$unit = "mol m-3"
#' attributes(densMoleAirDry)$unit = "mol m-3"
#' def.temp.soni(densMoleH2o, densMoleAirDry)

#' @seealso Currently none.

#' @export

# changelog and author contributions / copyrights
#   Stefan Metzger (2016-09-06)
#     original creation
#   Hongyan Luo (2016-11-20)
#     adjust to eddy4R coding style
###############################################################################################

# wet mass fraction (specific humidity)
data$irga$FW_mass_H2O_7200 <- ff::as.ff(data$irga$rhoMoleH2O * eddy4R.base::Natu$MolmH2o /
                                          (data$irga$rho_mole_dry_7200 * eddy4R.base::Natu$MolmDry + data$irga$rhoMoleH2O * eddy4R.base::Natu$MolmH2o))
base::attr(x = data$irga$FW_mass_H2O_7200, which = "unit") <- "kg kg-1"