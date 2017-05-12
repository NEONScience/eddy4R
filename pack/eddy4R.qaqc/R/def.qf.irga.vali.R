##############################################################################################
#' @title Definition function: Validation flag for IRGA

#' @author 
#' Natchaya Pingintha-Durden \email{ndurden@battelleecology.org}
#' David Durden \email{ddurden@battelleecology.org}

#' @description 
#' Definition function to generate the validation flags for IRGA from the IRGA sampling mass flow controller flow rate set point or from the IRGA validation soleniod valves. Flag indicating when the sensor is operated under validation period (1 = validation period, 0 = normal operating condition, -1 = NA).

#' @param data A data.frame containing the L0p input IRGA sampling mass flow controller data or the IRGA validation soleniod valves data at native resolution. Of type numeric or integer. [User-defined]
#' @param Sens A vector of class "character" containing the name of sensor used to determine the flag (IRGA sampling mass flow controller data or the IRGA validation soleniod valves), Sens = c("irgaMfcSamp", "irgaSndValiNema"). Defaults to "irgaMfcSamp". [-]

#' @return A vector class of integer (\code{qfIrgaVali}) of IRGA validation flags. Flag indicating when the sensor is operated under validation period (1 = validation period, 0 = normal operating condition, -1 = NA) [-]

#' @references 
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007.

#' @keywords NEON, IRGA, qfqm

#' @examples 
#' data <- list()
#' #generate test data for irgaMfcSamp
#' data$irgaMfcSamp <- data.frame(frt = c(0.0003621, 0.00035066, NA, NA, 0, 0),
#' frt00 = c(0.00020, 0.0001983, NA, NA, 0, 0), 
#' frtSet00 = c(0.00020, 0.00020, NA, NA, 0, 0), 
#' presAtm = c(57200, 58500, NA, NA, 58600, 58600), 
#' temp = c(300, 301, NA, NA, 299, 300))
#' #generate test data for irgaSndValiNema
#' data$irgaSndValiNema <- data.frame(qfGas01 = c(rep(0, 25)),
#' qfGas02 = c(rep(0, 5), rep(1, 5), rep(0, 15)),
#' qfGas03 = c(rep(0, 10), rep(1, 5), rep(0, 10)),
#' qfGas04 = c(rep(0, 15), rep(1, 5), rep(0, 5)),
#' qfGas05 = c(rep(0, 20), rep(NA, 5)))
#' #determine the flag using irgaMfcSamp
#' data$qfqm$irga$qfIrgaVali <- def.qf.irga.vali(data = data$irgaMfcSamp, Sens = "irgaMfcSamp")
#' #determine the flag using irgaSndValiNema
#' data$qfqm$irgaqfIrgaVali <- def.qf.irga.vali(data = data$irgaSndValiNema, Sens = "irgaSndValiNema")

#' @seealso Currently none

#' @export

# changelog and author contributions / copyrights
#   Natchaya Pingintha-Durden (2017-04-25)
#     original creation
#   David Durden (2017-04-26)
#     Added unit attribute
#   Natchaya Pingintha-Durden (2017-05-12)
#     revised the original by adding the sensor option to indicate qfIrgaVali
##############################################################################################

def.qf.irga.vali <- function(
  data,
  Sens = c("irgaMfcSamp", "irgaSndValiNema") [1]
){
  
  if (Sens %in% "irgaMfcSamp"){
    #check if frtSet00 existing in the input data
    if (is.null(data$frtSet00)){
      base::stop("Missing the flow rate set point (frtSet00) data")
    }#close if statement of is.null()
    
    #convert low and high frtSet00 from dm3 min-1 (LPM) to the internal unit (dm3 s-1)
    critLow <- eddy4R.base::def.unit.conv(data=8, unitFrom = "dm3 min-1", unitTo = "intl")
    critHigh <- eddy4R.base::def.unit.conv(data=15, unitFrom = "dm3 min-1", unitTo = "intl")
    
    #determine the flag (1=validation period, 0=normal operating condition, else = -1)
    qfIrgaVali <- as.integer(ifelse(is.na(data$frtSet00), -1,
                                    ifelse(data$frtSet00 >= critLow & data$frtSet00 <= critHigh, 0, 1)))
    
    }#close if statement of Sens %in% "irgaMfcSamp"
  
  if (Sens %in% "irgaSndValiNema"){
    #check if frtSet00 existing in the input data
    if (is.null(data$qfGas01 | data$qfGas02 | data$qfGas03 | data$qfGas04 | data$qfGas05)){
      base::stop("Missing the one or more of IRGA validation soleniod valves data")
    }#close if statement of is.null()
    
    #determine the flag (1=validation period, 0=normal operating condition, else = -1)
    qfIrgaVali <- as.integer(ifelse(is.na(data$qfGas01) | is.na(data$qfGas02) |
                                      is.na(data$qfGas03) | is.na(data$qfGas04) | is.na(data$qfGas05), -1,
                                    ifelse(data$qfGas01 == 1| data$qfGas02 == 1|
                                             data$qfGas03 == 1| data$qfGas04 == 1| data$qfGas05 == 1, 1, 0)))
    
  }#close if statement of Sens %in% "irgaSndValiNema"
  
  #Add unit attribute to the output  
  attr(qfIrgaVali, which = "unit") <- "NA"
  
  #output
  return(qfIrgaVali)
  #end of def.qf.irga.vali()
}
  