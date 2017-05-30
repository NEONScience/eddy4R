##############################################################################################
#' @title Definition function: Signal strength flag for IRGA

#' @author 
#' David Durden \email{ddurden@battelleecology.org}

#' @description 
#' Definition function to generate the signal strength flags for the IRGA from the diagnostic output quality metric \code{qfIrgaAgc}. Flag indicating when the sensor is operating with low signal strength using 50 percent as the default threshold (1 = when qfIrgaAgc <= 0.50, 0 = when qfIrgaAgc >= 0.50, -1 = NA).

#' @param qfIrgaAgc The quality metric derived from the IRGA diagnostics to determine signal strength. Presented as a dimensionless fraction. [-]
#' @param critThsh The critical threshold value for the \code{qfIrgaAgc} value to throw the flag for low signal strength (defaults to 0.50 or 50 percent).

#' @return A vector class of integer (\code{qfIrgaAgcOut}) of IRGA AGC flags. Flag indicating when the sensor is operating with low signal strength using 50 percent as the default threshold (1 = when qfIrgaAgc <= 0.50, 0 = when qfIrgaAgc >= 0.50, -1 = NA). [-]

#' @references 
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007.

#' @keywords NEON, IRGA, qfqm

#' @examples 
#' qfIrgaAgc <- rnorm(10,0.70,0.15)
#' qfIrgaAgc[c(2,8)] <- NA
#' attributes(qfIrgaAgc)$unit <- "-"
#' 
#' qfIrgaAgcOut <- def.qf.irga.agc(qfIrgaAgc = qfIrgaAgc)
#' 

#' @seealso Currently none

#' @export

# changelog and author contributions / copyrights
#   David Durden (2017-04-26)
#     original creation
#   David Durden (2017-05-22)
#     updating to work with ff objects
##############################################################################################

def.qf.irga.agc <- function(qfIrgaAgc, critThsh = 0.50){
  
  #Check that the input is numeric
  if(!base::is.numeric(qfIrgaAgc) & !is.ff(qfIrgaAgc)) {
    stop("Input parameter qfIrgaAgc must be numeric or an ff object.")
  }
  
 # if(!attributes(qfIrgaAgc)$unit == "-") {
 #   stop("Input parameter qfIrgaAgc must be in units of dimensionless fractions.")
 # }
  
  #Check if object passed is an ff object
  if(is.ff(qfIrgaAgc)){
    #create a vector of zero's
    qfIrgaAgcOut <- rep(0L, length(qfIrgaAgc))
    #Find indices where qfIrgaAgc is less than threshold
    idx <- ffwhich(qfIrgaAgc, qfIrgaAgc < critThsh)
    #Find indices where qfIrgaAgc is NA
    idxNa <- ffwhich(qfIrgaAgc, is.na(qfIrgaAgc))
    #Fill indices where qfIrgaAgc is less than threshold with a thrown flag (qfIrgaAgc = 1)
    qfIrgaAgcOut[idx[]] <- 1L
    #Fill indices where qfIrgaAgc is missing data with a thrown flag (qfIrgaAgc = -1)
    qfIrgaAgcOut[idxNa[]] <- -1L
  } else {
  
  
  #determine the flag (1=irga AGC value less than threshold indicating the signal strength is low, 0= irga AGC value was above threshold indicating the signal strength was sufficient for measurements, else = -1)
  qfIrgaAgcOut <- as.integer(ifelse(is.na(qfIrgaAgc), -1,
                                  ifelse( qfIrgaAgc >= critThsh, 0, 1)))
  
  }
   # Add unit attribute to the output  
  attr(qfIrgaAgcOut, which = "unit") <- "NA"
  
  #output
  return(qfIrgaAgcOut)
  #end of def.qf.irga.agc()
}
