##############################################################################################
#' @title Definition function: Signal strength flag for IRGA

#' @author 
#' David Durden \email{ddurden@battelleecology.org}

#' @description 
#' Definition function to generate the signal strength flags for the IRGA from the diagnostic output quality metric \code{qfIrgaAgc}. Flag indicating when the sensor is operating with low signal strength using 50% as the default threshold (1 = when qfIrgaAgc <= 0.50, 0 = when qfIrgaAgc >= 0.50, -1 = NA).

#' @param qfIrgaAgc The quality metric derived from the IRGA diagnostics to determine signal strength. Presented as a dimensionless fraction. [-]
#' @param critThsh The critical threshold value for the \code{qfIrgaAgc} value to throw the flag for low signal strength (defaults to 0.50 or 50%).

#' @return A vector class of integer (\code{qfIrgaVali}) of IRGA AGC flags. Flag indicating when the sensor is operating with low signal strength using 50% as the default threshold (1 = when qfIrgaAgc <= 0.50, 0 = when qfIrgaAgc >= 0.50, -1 = NA). [-]

#' @references 
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007.

#' @keywords NEON, IRGA, qfqm

#' @examples 
#' qfIrgaAgc <- rnorm(10,0.70,0.15)
#' qfIrgaAgc[c(2,8)] <- NA
#' attributes(qfIrgaAgc)$unit <- "-"
#' 
#' qfIrgaAgc <- def.qf.irga.agc(qfIrgaAgc = qfIrgaAgc)
#' 

#' @seealso Currently none

#' @export

# changelog and author contributions / copyrights
#   David Durden (2017-04-26)
#     original creation
##############################################################################################

def.qf.irga.agc <- function(qfIrgaAgc, critThsh = 0.50){
  
  #Check that the input is numeric
  if(!base::is.numeric(qfIrgaAgc)) {
    stop("Input parameter qfIrgaAgc must be numeric.")
  }
  
 # if(!attributes(qfIrgaAgc)$unit == "-") {
 #   stop("Input parameter qfIrgaAgc must be in units of dimensionless fractions.")
 # }
  
  #determine the flag (1=validation period, 0=normal operating condition, else = -1)
  qfIrgaAgc <- as.integer(ifelse(is.na(qfIrgaAgc), -1,
                                  ifelse( qfIrgaAgc >= critThsh, 0, 1)))
  
  # Add unit attribute to the output  
  attr(qfIrgaAgc, which = "unit") <- "NA"
  
  #output
  return(qfIrgaAgc)
  #end of def.qf.irga.agc()
}
