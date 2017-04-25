##############################################################################################
#' @title Definition function: Validation flag for IRGA

#' @author 
#' Natchaya Pingintha-Durden \email{ndurden@battelleecology.org}

#' @description 
#' Definition function to generate the validation flags for IRGA from the IRGA sampling mass flow controller flow rate set point \code{frtSet00}. Flag indicating when the sensor is operated under validation period (1 = validation period, 0 = normal operating condition, -1 = NA).

#' @param \code{frtSet00} The flow rate set point measured by the IRGA sampling mass flow controller. [dm3 s-1]

#' @return A vector class of numeric (\code{qfIrgaVali}) of IRGA validation flags. Flag indicating when the sensor is operated under validation period (1 = validation period, 0 = normal operating condition, -1 = NA) [-]

#' @references 
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007.

#' @keywords NEON, IRGA, qfqm

#' @examples 
#' qfIrgaVali <- def.qf.irga.vali(frtSet00 = 0.000241)
#' data <- data.frame(frtSet00 = c(0, 0, 0, -1, -1, -1, 0.000242, 0.000242))
#' data$qfIrgaVali <- def.qf.irga.vali(frtSet00 = data$frtSet00)

#' @seealso Currently none

#' @export

# changelog and author contributions / copyrights
#   Natchaya Pingintha-Durden (2017-04-25)
#     original creation
##############################################################################################

def.qf.irga.vali <- function(frtSet00){
  
  #convert low and high frtSet00 from dm3 min-1 (LPM) to the internal unit (dm3 s-1)
  critLow <- eddy4R.base::def.unit.conv(data=8, unitFrom = "dm3 min-1", unitTo = "intl")
  critHigh <- eddy4R.base::def.unit.conv(data=15, unitFrom = "dm3 min-1", unitTo = "intl")
  
  #determine the flag (1=validation period, 0=normal operating condition, else = -1)
  qfIrgaVali <- as.integer(ifelse(frtSet00 == 0, 1,
                            ifelse( frtSet00 >= critLow & frtSet00 <= critHigh, 0, -1)))
  #output
  return(qfIrgaVali)
  #end of def.qf.irga.vali()
}
  