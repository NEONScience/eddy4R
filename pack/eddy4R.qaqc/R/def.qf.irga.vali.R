##############################################################################################
#' @title Definition function: Validation flag for IRGA

#' @author 
#' Natchaya Pingintha-Durden \email{ndurden@battelleecology.org} \cr
#' David Durden \email{ddurden@battelleecology.org}

#' @description 
#' Definition function to generate the validation flags for IRGA from the IRGA sampling mass flow controller flow rate set point or from the IRGA validation soleniod valves. Flag indicating when the sensor is operated under validation period (1 = validation period, 0 = normal operating condition, -1 = NA).

#' @param data A data.frame containing the L0p input IRGA sampling mass flow controller data or the IRGA validation soleniod valves data at native resolution. Of type numeric or integer. [User-defined]
#' @param Sens A vector of class "character" containing the name of sensor used to determine the flag (IRGA sampling mass flow controller data or the IRGA validation soleniod valves), Sens = c("irgaMfcSamp", "irgaSndValiNema"). Defaults to "irgaMfcSamp". [-]
#' @param qfGas A vector of class "character" containing the name of quality flag of reference gas, qfGas = c("qfGas01", "qfGas02", "qfGas03", "qfGas04", "qfGas05"). This parameter needs to provide when Sens = "irgaSndValiNema". Defaults to NULL. [-]

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
#' data$qfqm$irga$qfIrgaVali <- def.qf.irga.vali(data = data$irgaSndValiNema, Sens = "irgaSndValiNema")

#' @seealso Currently none

#' @export

# changelog and author contributions / copyrights
#   Natchaya Pingintha-Durden (2017-04-25)
#     original creation
#   David Durden (2017-04-26)
#     Added unit attribute
#   Natchaya Pingintha-Durden (2017-05-12)
#     revised the original by adding the sensor option to indicate qfIrgaVali
#   David Durden (2017-04-26)
#     Adapted to work with ff objects and output 20Hz flag if irgaSndValiNema is used
#   Natchaya P-Durden (2018-11-09)
#     Adjusted function to determine qf for each validation gas
##############################################################################################

def.qf.irga.vali <- function(
  data,
  Sens = c("irgaMfcSamp", "irgaSndValiNema") [1],
  qfGas = NULL
){
  
  if (Sens %in% "irgaMfcSamp"){
    #check if frtSet00 existing in the input data
    if (is.null(data$frtSet00)){
      base::stop("Missing the flow rate set point (frtSet00) data")
    }#close if statement of is.null()
    
    #convert low and high frtSet00 from dm3 min-1 (LPM) to the internal unit (dm3 s-1)
    critLow <- eddy4R.base::def.unit.conv(data=8, unitFrom = "dm3 min-1", unitTo = "intl")
    critHigh <- eddy4R.base::def.unit.conv(data=15, unitFrom = "dm3 min-1", unitTo = "intl")
    
    #Check if object passed is an ff object
    if(is.ffdf(data)){
      #create a vector of zero's
    qfIrgaVali <- rep(0L, length(data$frtSet00))
    #Find indices where irgaMfcSamp$frtSet00 is less than or greater than the critical thresholds
    idx <- ffwhich(data, frtSet00 < critLow | frtSet00 > critHigh)
    #Find indices where irgaMfcSamp$frtSet00 is NA
    idxNa <- ffwhich(data, is.na(frtSet00))
    #Fill indices where irgaMfcSamp$frtSet00 is less than or greater than the critical thresholds with a thrown flag (qfIrgaVali = 1)
    qfIrgaVali[idx[]] <- 1L
    #Fill indices where irgaMfcSamp$frtSet00 is missing data with a thrown flag (qfIrgaVali = -1)
    qfIrgaVali[idxNa[]] <- -1L
    } else {
    #determine the flag (1=validation period, 0=normal operating condition, else = -1)
    qfIrgaVali <- as.integer(ifelse(is.na(data$frtSet00), -1,
                                    ifelse(data$frtSet00 >= critLow & data$frtSet00 <= critHigh, 0, 1)))
    
    }}#close if statement of Sens %in% "irgaMfcSamp"
  
  if (Sens %in% "irgaSndValiNema"){
    #check if frtSet00 existing in the input data
    if (is.null(data$qfGas01 | data$qfGas02 | data$qfGas03 | data$qfGas04 | data$qfGas05)){
      base::stop("Missing the one or more of IRGA validation soleniod valves data")
    }#close if statement of is.null()
    
    #Check if object passed is an ff object
    if(is.ffdf(data)){
      #create a vector of zero's
      qfIrgaVali <- rep(0L, length(data[[qfGas]]))
      #Find indices where validation solenoid are open qfGas.. == 1
      idx <- ffwhich(data, data[[qfGas]] == 1)
      #Find indices where qfGas.. is NA
      idxNa <- ffwhich(data, is.na(data[[qfGas]]))
      #Fill indices where qfGas.. is equal to 1 indicating open validation valves with a flag (qfIrgaVali = 1)
      qfIrgaVali[idx[]] <- 1L
      #fill the indices where irgaSndValiNema has missing data
      qfIrgaVali[idxNa[]] <- -1L
      } else {
    
    
    #determine the flag (1=validation period, 0=normal operating condition, else = -1)
    qfIrgaVali <- as.integer(ifelse(is.na(data[[qfGas]]), -1,
                                    ifelse(data[[qfGas]] == 1, 1, 0)))
    
    }
    
    #Test that input data for irgaSndValiNema was the right length
    if(!length(qfIrgaVali) == 17280) { stop("qfIrgaVali is not the appropriate length for 0.2 Hz input data")}
    #Convert from 0.2 Hz to 20 Hz
    qfIrgaVali <- rep(qfIrgaVali, each = 100) #TODO: this could be implemented better to handle different input data
    
    }#close if statement of Sens %in% "irgaSndValiNema"
  
  
  
  #Add unit attribute to the output  
  attr(qfIrgaVali, which = "unit") <- "NA"
  
  #output
  return(qfIrgaVali)
  #end of def.qf.irga.vali()
}
  