##############################################################################################
#' @title Calculation of the average temperature in LI-7200 irga cell

#' @author Stefan Metzger \email{eddy4R.info@gmail.com}
#' Hongyan Luo \email{eddy4R.info@gmail.com}

#' @description Function definition. Calculation of the average temperature in LI-7200 IRGA cell

#' @param \code{tempIn} A vector containing the tempertaure measured at IRGA cell inlet, of class "numeric". [K]
#' @param \code{tempOut} A vector containing the tempertaure measured at IRGA cell outlet, of class "numeric". [K]

#' @return 
#' The returned object is the the average temperature in LI-7200 IRGA cell calculated from the temperatrue at IRGA cell inlet and the temperature at IRGA cell outlet.  

#' @references
#' Currently none.

#' @keywords temperature, irga

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
#   Hongyan Luo (2016-10-23)
#     adjust to eddy4R coding style
###############################################################################################

# average temperature in irga cell 
data$irga$T_cell_7200 <- ff::as.ff(0.2 * data$irga$tempCellIn + 0.8 * data$irga$tempCellOut)
base::attr(x = data$irga$T_cell_7200, which = "unit") <- base::attr(x = data$irga$tempCellIn, which = "unit")
