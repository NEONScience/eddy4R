
##############################################################################################
#' @title Calculation of the molar density of the dry air alone

#' @author Stefan Metzger \email{eddy4R.info@gmail.com}
#' Hongyan Luo \email{eddy4R.info@gmail.com}

#' @description Function definition. Calculation of the molar density of the mdry air alone

#' @param \code{densMoleAir} A vector containing the mole density of the air mixture (includes dry air and water vapor), of class "numeric". [mol m-3]
#' @param \code{densMoleH2o} A vector containing the water vapor mole density of the air mixture, of class "numeric". [mol m-3]

#' @return 
#' The returned object is the the molar density of the dry air alone  

#' @references
#' Currently none.

#' @keywords mole density

#' @examples
#' Example 1, this will cause an error message due to densMoleAir and densMoleH2o have no units: 
#' def.dens.mole.air.dry(densMoleAir = 41.38, densMoleH2o = 286)

#' Example 2, assign values and units to variables first, the function should run ok.
#' densMoleAir = 86000
#' densMoleH2o = 286
#' attributes(densMoleAir)$unit = "mol m-3"
#' attributes(densMoleH2o)$unit = "mol m-3"
#' def.dens.mole.air.dry(densMoleAir, densMoleH2o)

#' @seealso Currently none.

#' @export

# changelog and author contributions / copyrights
#   Stefan Metzger (2016-09-06)
#     original creation
#   Hongyan Luo (2016-11-15)
#     adjust to eddy4R coding style
###############################################################################################

# molar density of dry air alone
data$irga$rho_mole_dry_7200 <- ff::as.ff(data$irga$rho_mole_air_7200 - data$irga$densMoleH2o)
base::attr(x = data$irga$rho_mole_dry_7200, which = "unit") <- "mol m-3"