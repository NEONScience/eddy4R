
##############################################################################################
#' @title Calculation of the molar density of the mixture of dry air and water vapor

#' @author Stefan Metzger \email{eddy4R.info@gmail.com}
#' Hongyan Luo \email{eddy4R.info@gmail.com}

#' @description Function definition. Calculation of the molar density of the misxture of dry air and water vapor

#' @param \code{presSum} A vector containing the total pressure of the air mixture, of class "numeric". [Pa]
#' @param \code{tempMean} A vector containing the mean temperatrue of the air mixtureture, of class "numeric". [K]

#' @return 
#' The returned object is the the molar density of the mixture of dry air and water vapor  

#' @references
#' Currently none.

#' @keywords mole density, temperature, pressure

#' @examples
#' Example 1, this will cause an error message due to tempIn and tempOut have no units: 
#' def.temp.mean.7200(tempIn = 293, tempOut = 294)

#' Example 2, assign values and units to variables first, the function should run ok.
#' tempIn = 293
#' tempOut = 294
#' attributes(tempIn)$unit = "K"
#' attributes(tempOut)$unit = "K"
#' def.temp.mean.7200(tempIn, tempOut)

#' @seealso Currently none.

#' @export

# changelog and author contributions / copyrights
#   Stefan Metzger (2016-09-06)
#     original creation
#   Hongyan Luo (2016-11-14)
#     adjust to eddy4R coding style
###############################################################################################

# molar density of dry air and water vapor
data$irga$rho_mole_air_7200 <- ff::as.ff(data$irga$p_cell_7200 / eddy4R.base::Natu$Rg / data$irga$T_cell_7200)
base::attr(x = data$irga$rho_mole_air_7200, which = "unit") <- "mol m-3"