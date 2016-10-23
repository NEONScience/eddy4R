##############################################################################################
#' @title Calculation of RH from vapor pressure and saturated vapor pressure 
#' @author Stefan Metzger \email{eddy4R.info@gmail.com}
#' Hongyan Luo \email{eddy4R.info@gmail.com}

#' @description Function definition. Calculation of RH from vapor pressure and saturated vapor pressure 

#' @param \code{tempCellIn} A vector containing the tempertaure measured at IRGA cell inlet, of class "numeric". [K]
#' @param \code{tempCellOut} A vector containing the tempertaure measured at IRGA cell outlet, of class "numeric". [K]

#' @return 
#' The returned object is the RH from vapor pressure and saturated vapor pressure   

#' @references
#' Currently none.

#' @keywords RH, vapor pressure, irga

#' @examples
#' Example 1, this will cause an error message due to tempCellIn and tempCellOut have no units: 
#' def.temp.mean(tempCellIn = 293, tempCellOut = 294)

#' Example 2, assign values and units to variables first, the function should run ok.
#' tempCellIn = 293
#' tempCellOut = 294
#' attributes(tempCellIn)$unit = "K"
#' attributes(tempCellOut)$unit = "K"
#' def.temp.mean(tempCellIn, tempCellOut)

#' @seealso Currently none.

#' @export

# changelog and author contributions / copyrights
#   Stefan Metzger (2016-09-06)
#     original creation
#   Hongyan Luo (2016-10-23)
#     adjust to eddy4R coding style
###############################################################################################
data$irga$presH2o / data$irga$presH2oSat * 100