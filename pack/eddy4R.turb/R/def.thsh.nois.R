##############################################################################################
#' @title Detection limit for fluxes

#' @author
#' Stefan Metzger \email{eddy4R.info@gmail.com} \ cr
#' Hongyan Luo \email{eddy4R.info@gmail.com}

#' @description 
#' Function defintion. Detection limit for fluxes.

#' @param \code{data} A vector containing the input data (such as w_met) of class "numeric" or "integer". [user-defined]
#' @param \code{dataFlux} A vector containing the input data of actual fluxes. Class "numeric" or "integer". [user-defined]
#' @param \code{AlgBase} c("mean", "trnd", "ord03") algorithm used to determine base state, where \cr
#' "mean" is the simple algorithmic mean, \cr
#' "trnd" is the least squares linear (1st order) trend, and \cr
#' "ord03" is the least squares 3rd order polynomial fit
#' @param \code{whrVar} Specific column in \code{data} containing the variables (fluxes) to be performed detection limit calculation. Of class "numeric" (column number) or "character" (column name). Defaults to NULL. [-] 
#' @param \code{corTempPot} A logical indicating whether or not to use potential temperature in flux calculation. Defaults to TRUE. [-]
#' @param \code{presTempPot} A vector containing the air pressure data that will be used in the calculation when \code{corTempPot}=TRUE. Of class "numeric" or "integer" and of the same length as \code{data} or single entry. [Pa]
#'  @param \code{CoefRng} A parameter of confidence level for detection limit. Class "numeric". [-]
#'  @param \code{ConfEnd} A parameter of criterion to stop iteration (0.01 indicates 1% change among subsequent runs). Class "numeric". [-]


#' @return Currently none

#' @references 
#' Billesbach, D.P. (2011) Estimating uncertainties in individual eddy covariance flux measurements: A comparison of methods and proposed new method. Agricultural and Forest Meteorology, 151, 394-405.

#' @keywords eddy-covariance, turbulent flux, detection limit

#' @examples Currently none

#' @seealso Currently none

#' @export

# changelog and author contributions / copyrights
#   Stefan Metzger (2015-03-18)
#     original creation
#   Stefan Metzger (2015-11-29)
#     added Roxygen2 tags
#   Hongyan Luo (2016-07-20)
#     adjust to eddy4R coding style
##############################################################################################

############################################################
#DETECTION LIMIT FOR FLUXES
############################################################
#determine noise level of fluxes using "random shuffle" of w_met (Billesbach, 2011)

def.thsh.nois <- function(
  #data set
  data,
  #actual (correct) fluxes
  dataFlux,
  #what to use as basis to determine fluctuations
  AlgBase,
  #use potential quantities?
  corTempPot=TRUE,
  #pressure level for potential quantities
  presTempPot=NULL,
  #which entries are fluxes?
  whrVar,
  #confidence level for detection limit
  CoefRng=0.95,
  #criterion to stop iteration (0.01 = 1% change among subsequent realizations)
  CritEnd=0.01
) {
  
  
  ###
  #start loop around sample size of manipulations
  noisBgn <- 1e5 #initial noise
  critReps <- FALSE #initial criterion. When FALSE, continue iteration; when true, stop iteration
  reps <- 0
  while(reps < 3 | critReps == FALSE) {
    ###
    
    #keep counting
    reps <- reps + 1
    
    #randomize vertical wind speed
    veloZaxs <- base::sample(x=1:base::nrow(data), size=base::nrow(data), replace=FALSE)
    data$w_met <- data$w_met[veloZaxs]
    
    #calculate fluxes
    flux <- eddy4R.turb::REYNflux_FD_mole_dry(
      data=data,
      AlgBase=AlgBase,
      FcorPOT=corTempPot,
      FcorPOTl=presTempPot
    )
    
    #store output
    if(reps == 1) noisVar <- flux$mn[, whrVar]
    if(reps > 1)  noisVar <- base::rbind(noisVar, flux$mn[, whrVar])
    
    #distributions stats
    med <- base::sapply(1:ncol(noisVar), function(x) eddy4R.base::def.med.mad(noisVar[,x]))
    #average offset/level/location of noise
    noisOfst <- med[1,]
    base::names(noisOfst) <- whrVar
    #actual dispersion of noise
    noisSd <- med[2,]
    base::names(noisSd) <- whrVar
    #detection limit (recast of signal-to-noise criterion after Park et al., 2013), at provided confidence level
    coefOut <- stats::qnorm((1 - CoefRng)/2, lower.tail = FALSE)
    nois <- base::abs(noisOfst) + coefOut * noisSd
    base::names(nois) <- whrVar
    #signal to noise ratio
    rtioMeasNois <- (base::abs(dataFlux$mn[, whrVar]) - base::abs(noisOfst) ) / noisSd
    base::names(rtioMeasNois) <- whrVar
    
    #stop criterion: change in signal to noise ratio < 10%
    crit <- base::abs( (nois - noisBgn) / noisBgn )
    #condition1: crit has to consist of finite values
    if(base::length(base::which(base::is.infinite(base::unlist(crit)))) == 0) {
      #condition 2: change in signal to noise ratio < 1% between steps
      if(base::length(base::which(base::abs(crit) < CritEnd)) == base::length(crit)) critReps <- TRUE else critReps <- FALSE
    } else {
      critReps <- FALSE
    }
    
    #save posterior as prior for next loop
    noisBgn <- nois
    
    ###
    if(reps%%10 == 0) base::print(base::paste("Iteration ", reps, " of flux noise determination finished.", sep=""))
  }
  base::print(base::paste("Flux noise determination completed after ", reps, " iterations.", sep=""))
  #end loop around sample size of manipulations
  ###
  
  
  
  #save to list
  noisData <- base::list()
  #noise location
  noisData$mn <- noisOfst
  #noisData$noisOfst <- noisOfst # This should be the format in the future
  #noise location
  noisData$sd <- noisSd
  # noisData$noisSd <- noisSd # This should be the format in the future
  #detection limit
  noisData$dl <- nois
  # noisData$nois <- nois # This should be the format in the future
  #signal-to-noise ratio 
  noisData$sn <- rtioMeasNois
  # noisData$rtioMeasNois <- rtioMeasNois # This should be the format in the future
  
  #clean up
  rm(critReps, crit, data, med, noisVar, reps, flux, veloZaxs)
  
  #return results
  return(noisData)
  
}
