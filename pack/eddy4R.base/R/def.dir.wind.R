##############################################################################################
#' @title Definition function: Wind direction mean and variance calculation using vector averaging
#'
#'  
#' @author
#' David Durden \email{ddurden@@battelleecology.org} 

#' @description Function defintion. Calculate the mean and variance of the wind direction.
#' 
#' @param inp numeric vector containing instantaneous wind directions
#' @param MethVari This is a character string to determine the method to calculate the wind direction variance. The methods include "02StepRad", "DistAngMin", and "Yama"



#' @return Mean and variance of the wind direction in meteorological coordinate system [rad]

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007. \cr
#' Yamartino, R. J. (1984) A Comparison of Several "Single-Pass" Estimators of the Standard Deviation of Wind Direction. Journal of Applied Meteorology and Climatology, 23, 1362-1366.


#' @keywords wind direction, sonic anemometer, vector averaging

#' @examples 
#' #Instantaneous wind direction
#' windDirInst <- rnorm(3600, 180, 10)
#' def.dir.wind(windDirInst, MethVari = "DistAngMin")

#' @seealso Currently none

#' @export

# changelog and author contributions / copyrights
#   David Durden (2017-06-08)
#     original creation

##############################################################################################

def.dir.wind <- function(
  inp,
  MethVari = c("02StepRad", "DistAngMin", "Yama")[3]
){
  
  
  # check whether data is numeric
  if(!base::is.numeric(inp)){
    base::stop("Input must be numeric")
  }
  
  # initialize object for reported variables
  rpt <- list()

  #Determine number of samples
  rpt$numSamp <- base::length(base::which(!base::is.na(inp)))
  #Calculate the mean velocity along the Xaxs
  veloXaxsAgr <- base::mean(cos(inp), na.rm = TRUE)
  #Calculate the mean velocity along the Yaxs
  veloYaxsAgr <- base::mean(sin(inp), na.rm = TRUE)
  #Calculate the mean wind direction using vector averages
  rpt$mean <- ((2*pi + base::atan2(veloYaxsAgr, veloXaxsAgr))%%(2*pi))
  
  
  ##########################################################################
  #Variance calculations
  ##########################################################################
  if(MethVari == "02StepRad"){
  # 2-step approach
  # Determine the minimum absolute difference for variance calculation
  diff01 <- base::abs(inp - rpt$mean)
  # Determine the minimum absolute difference from the other direction for variance calculation
  diff02 <- 2*pi - base::abs(inp - rpt$mean)
  # Determine the minimum difference
  diffOut <- base::pmin(diff01,diff02)
  # Calcluate the Variance
  rpt$vari <- stats::var(diffOut)
  }
  
  
  if(MethVari == "DistAngMin"){
  # Minimum angular distance technique in ATBD
  distAng <- base::abs(base::acos(cos(inp - rpt$mean)))
  # Correct the angles sign
  distAngCor <- base::ifelse(inp >= rpt$mean & inp < (rpt$mean + pi), distAng, -distAng)
  # Average the angular distance
  distAngAgr <- base::mean(distAngCor, na.rm = TRUE)
  #Calculate angular distance variance
  rpt$vari <- stats::var(distAngCor, na.rm = TRUE)
  }
  
  if(MethVari == "Yama"){
  # Yamartino method
  # Determine the squared estimator
  estSq <- 1 - (veloYaxsAgr^2 + veloXaxsAgr^2)
  # Determine the estimator
  est <- base::sqrt(estSq)
  # standard deviation of the wind direction
  sdDirWindEst <- base::asin(est)*(1.0 + (2/base::sqrt(3)*est^3))
  # Calculate variance of the wind direction
  rpt$vari <- sdDirWindEst^2
  }
  
  # standard error
  rpt$se <- base::sqrt(rpt$vari)/base::sqrt(rpt$numSamp)
  
  #Convert output to a dataframe
  rpt <- base::data.frame(stringsAsFactors = FALSE, base::t(unlist(rpt)))
  
  # assign units for each variable
  base::lapply(names(rpt), function(x) {base::attr(rpt[[x]], which = "unit") <<- "rad"})
  
  return(rpt)
}
