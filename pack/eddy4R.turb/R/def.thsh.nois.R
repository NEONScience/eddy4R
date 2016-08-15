##############################################################################################
#' @title Detection limit for fluxes

#' @author
#' Stefan Metzger \email{eddy4R.info@gmail.com} \ cr
#' Hongyan Luo \email{eddy4R.info@gmail.com}

#' @description 
#' Function defintion. Detection limit for fluxes.

#' @param \code{data} A vector containing the input data of class "numeric" or "integer". [user-defined]
#' @param \code{data} A vector containing the input data of actual fluxes. Class "numeric" or "integer". [user-defined]
#' @param \code{AlgBase} c("mean", "trnd", "ord03") algorithm used to determine base state, where \cr
#' "mean" is the simple algorithmic mean, \cr
#' "trnd" is the least squares linear (1st order) trend, and \cr
#' "ord03" is the least squares 3rd order polynomial fit
#' @param \code{whrVar} Specific column in \code{data} containing the variables (fluxes) to be performed detection limit calculation. Of class "numeric" (column number) or "character" (column name). Defaults to NULL. [-] 
#' @param \code{corTempPot} A logical indicating whether or not to use potential temperature in flux calculation. Defaults to TRUE. [-]
#' @param \code{presTempPot} A vector containing the air pressure data that will be used in the calculation when \code{corTempPot}=TRUE. Of class "numeric" or "integer" and of the same length as \code{data} or single entry. [Pa]
#'  @param \code{ConfRng} A parameter of confidence level for detection limit. Class "numeric". [-]
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

def.thsh.nois.R <- function(
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
  ConfRng=0.95,
  #criterion to stop iteration (0.01 = 1% change among subsequent realizations)
  CoefEnd=0.01
) {
  
  
  ###
  #start loop around sample size of manipulations
  noise_dete_prior <- 1e5
  crit <- FALSE
  r <- 0
  while(r < 3 | crit == FALSE) {
    ###
    
    #keep counting
    r <- r + 1
    
    #randomize w_met
    whr_random <- sample(x=1:nrow(data), size=nrow(data), replace=FALSE)
    data$w_met <- data$w_met[whr_random]
    
    #calculate fluxes
    REYN_noise <- REYNflux_FD_mole_dry(
      data=eddy.data_random,
      AlgBase=AlgBase,
      FcorPOT=FcorPOT,
      FcorPOTl=FcorPOTl
    )
    
    #store output
    if(r == 1) NOISE <- REYN_noise$mn[, whrVar]
    if(r > 1)  NOISE <- rbind(NOISE, REYN_noise$mn[, whrVar])
    
    #distributions stats
    MEma <- sapply(1:ncol(NOISE), function(x) def.med.mad(NOISE[,x]))
    #average offset/level/location of noise
    noise_loca <- MEma[1,]
    names(noise_loca) <- whrVar
    #actual dispersion of noise
    noise_disp <- MEma[2,]
    names(noise_disp) <- whrVar
    #detection limit (recast of signal-to-noise criterion after Park et al., 2013), at provided confidence level
    conf_fac <- qnorm((1 - ConfRng)/2, lower.tail = FALSE)
    noise_dete <- abs(noise_loca) + conf_fac * noise_disp
    names(noise_dete) <- whrVar
    #signal to noise ratio
    noise_s2n <- ( abs(dataFlux$mn[, whrVar]) - abs(noise_loca) ) / noise_disp
    names(noise_s2n) <- whrVar
    
    #stop criterion: change in signal to noise ratio < 10%
    CRIT <- abs( (noise_dete - noise_dete_prior) / noise_dete_prior )
    #condition1: CRIT has to consist of finite values
    if(length(which(is.infinite(unlist(CRIT)))) == 0) {
      #condition 2: change in signal to noise ratio < 1% between steps
      if(length(which(abs(CRIT) < CoefEnd)) == length(CRIT)) crit <- TRUE else crit <- FALSE
    } else {
      crit <- FALSE
    }
    
    #save posterior as prior for next loop
    noise_dete_prior <- noise_dete
    
    ###
    if(r%%10 == 0) print(paste("Iteration ", r, " of flux noise determination finished.", sep=""))
  }
  print(paste("Flux noise determination completed after ", r, " iterations.", sep=""))
  #end loop around sample size of manipulations
  ###
  
  
  
  #save to list
  noise <- list()
  #noise location
  noise$mn <- noise_loca
  #noise location
  noise$sd <- noise_disp
  #detection limit
  noise$dl <- noise_dete
  #signal-to-noise ratio
  noise$sn <- noise_s2n
  
  #clean up
  rm(crit, CRIT, data, MEma, NOISE, r, REYN_noise, whr_random)
  
  #return results
  return(noise)
  
}
