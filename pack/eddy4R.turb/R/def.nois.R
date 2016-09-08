##############################################################################################
#' @title Determination of noise and detection limit for eddy-covariance turbulent fluxes

#' @author
#' Stefan Metzger \email{eddy4R.info@gmail.com} \ cr
#' Hongyan Luo \email{eddy4R.info@gmail.com}

#' @description 
#' Function defintion. Determine the noise bias, noise dispersion, detection limit and signal-to-noise ratio for turbulent fluxes using the "random shuffle" technique (Billesbach, 2011).

#' @param \code{dataTest} A dataframe containing the input data, of class "numeric". [user-defined]
#' @param \code{dataRefe} A vector containing the reference fluxes, of class "numeric". [user-defined]
#' @param \code{idxFlux} Names of columnns in \code{dataRefe} that contain the fluxes for which the detection limit calculation is performed, of class "character" (column name). [-]
#' @param \code{AlgBase} c("mean", "trnd", "ord03") algorithm used to determine base state, where \cr
#' "mean" is the simple algorithmic mean, \cr
#' "trnd" is the least squares linear (1st order) trend, and \cr
#' "ord03" is the least squares 3rd order polynomial fit.
#' @param \code{corTempPot} Logical value indicating whether or not to use potential temperature in the flux calculation. Defaults to TRUE. [-]
#' @param \code{presTempPot} A vector containing the air pressure data that will be used in the calculation when \code{corTempPot} = TRUE. Of class "numeric" or "integer" and of the same length as \code{dataTest} or single entry. [Pa]
#' @param \code{ConfLevl} The confidence level at which the detection limit is calculated. Of class "numeric", defaults to 0.95. [-]
#' @param \code{CritMax} The stop criterion for the iteration. Of class "numeric", defaults to 0.01 (i.e., 1 percent change among subsequent runs). [-]

#' @return A list containing the noise bias, noise dispersion, detection limit and signal-to-noise ratio.

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
#   Stefan Metzger (2016-08-31)
#     eddy4R coding style suggestions
##############################################################################################

############################################################
# DETECTION LIMIT FOR FLUXES
############################################################
# determine noise level of fluxes using "random shuffle" of the vertical wind (Billesbach, 2011)

def.nois <- function(
  # data set
  dataTest,
  # actual (correct) fluxes
  dataRefe,
  # which entries are fluxes?
  idxFlux,
  # what to use as basis to determine fluctuations
  AlgBase,
  # use potential quantities?
  corTempPot = TRUE,
  # pressure level for potential quantities
  presTempPot = NULL,
  # confidence level for detection limit
  ConfLevl = 0.95,
  # criterion to stop iteration (0.01 = 1% change among subsequent realizations)
  CritMax = 0.01
) {
  
  
  ###
  # start loop around sample size of manipulations
  noisBgn <- 1e5         # prior for cut-off criterion of iteration
  critReps <- FALSE      # initial criterion. When FALSE, continue iteration; when TRUE, stop iteration
  reps <- 0              # count number of iterations
  while(reps < 3 | critReps == FALSE) {
  ###
    
    # keep counting
    reps <- reps + 1
    
    # randomize vertical wind speed
    idx <- base::sample(x = 1:base::nrow(dataTest), size = base::nrow(dataTest), replace = FALSE)
    dataTest$w_met <- dataTest$w_met[idx]
    
    # calculate fluxes
    flux <- eddy4R.turb::REYNflux_FD_mole_dry(
      data = dataTest,
      AlgBase = AlgBase,
      FcorPOT = corTempPot,
      FcorPOTl = presTempPot,
      PltfEc=PltfEc,
      flagCh4 = flagCh4
    )
    
    # store output
    if(reps == 1) noisTmp <- flux$mn[,idxFlux]
    if(reps > 1)  noisTmp <- base::rbind(noisTmp, flux$mn[, idxFlux])
    
    # distributions stats
    med <- base::sapply(1:ncol(noisTmp), function(x) eddy4R.base::def.med.mad(noisTmp[,x]))
    
    # average offset/level/bias of noise
    noisBias <- med[1,]
    base::names(noisBias) <- idxFlux
    
    # actual dispersion of noise
    noisSd <- med[2,]
    base::names(noisSd) <- idxFlux
    
    # detection limit (recast of signal-to-noise criterion after Park et al., 2013), at provided confidence level
    confVar <- stats::qnorm((1 - ConfLevl)/2, lower.tail = FALSE)
    noisMax <- base::abs(noisBias) + confVar * noisSd
    base::names(noisMax) <- idxFlux
    
    # signal to noise ratio
    rtioMeasNois <- (base::abs(dataRefe$mn[,idxFlux]) - base::abs(noisBias) ) / noisSd
    base::names(rtioMeasNois) <- idxFlux
    
    # stop criterion: change in detection limit between subsequent iterations
    crit <- base::abs( (noisMax - noisBgn) / noisBgn )
    
    # condition 1: crit has to consist of finite values
    if(base::length(base::which(base::is.infinite(base::unlist(crit)))) == 0) {
      
      # condition 2: change in detection limit < CritMax between subsequent iterations
      if(base::length(base::which(base::abs(crit) < CritMax)) == base::length(crit)) critReps <- TRUE else critReps <- FALSE
      
    } else {
      
      critReps <- FALSE
      
    }
    
    # save posterior as prior for next loop
    noisBgn <- noisMax
    
  ###
  if(reps%%10 == 0) base::print(base::paste("Iteration ", reps, " of flux noise determination finished.", sep = ""))
  }
  base::print(base::paste("Flux noise determination completed after ", reps, " iterations.", sep = ""))
  # end loop around sample size of manipulations
  ###
  
  
  
  # save to list
  noisRpt <- base::list()
  
    # noise bias
    noisRpt$mn <- noisBias
    # noisRpt$Bias <- noisBias # This should be the format in the future
    
    # noise dispersion
    noisRpt$sd <- noisSd
    # noisRpt$sd <- noisSd # This should be the format in the future
    
    # detection limit
    noisRpt$dl <- noisMax
    # noisRpt$max <- noisMax # This should be the format in the future
    
    # signal-to-noise ratio 
    noisRpt$sn <- rtioMeasNois
    # noisRpt$rtio <- rtioMeasNois # This should be the format in the future
  
  # clean up
  rm(critReps, crit, dataTest, med, noisTmp, reps, flux, idx)
  
  # return results
  return(noisRpt)
  
}
