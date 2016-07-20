##############################################################################################
#' @title Detection limit for fluxes

#' @author
#' Stefan Metzger \email{eddy4R.info@gmail.com} \ cr
#' Hongyan Luo \email{eddy4R.info@gmail.com}

#' @description 
#' Function defintion. Detection limit for fluxes.

#' @param Currently none

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

NOISE_rs <- function(
  #data set
  eddy.data_random=eddy.data_loc,
  #actual (correct) fluxes
  REYN_loc=REYN_loc,
  #what to use as basis to determine fluctuations
  AlgBase=AlgBase,
  #use potential quantities?
  FcorPOT=FcorPOT,
  #pressure level for potential quantities
  FcorPOTl=FcorPOTl,
  #which entries are fluxes?
  whr_flux=whr_flux,
  #confidence level for detection limit
  conf_level=0.95,
  #criterion to stop iteration (0.01 = 1% change among subsequent realizations)
  crit_iter=0.01
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
    whr_random <- sample(x=1:nrow(eddy.data_random), size=nrow(eddy.data_random), replace=FALSE)
    eddy.data_random$w_met <- eddy.data_random$w_met[whr_random]
    
    #calculate fluxes
    REYN_noise <- REYNflux_FD_mole_dry(
      data=eddy.data_random,
      AlgBase=AlgBase,
      FcorPOT=FcorPOT,
      FcorPOTl=FcorPOTl
    )
    
    #store output
    if(r == 1) NOISE <- REYN_noise$mn[, whr_flux]
    if(r > 1)  NOISE <- rbind(NOISE, REYN_noise$mn[, whr_flux])
    
    #distributions stats
    MEma <- sapply(1:ncol(NOISE), function(x) def.med.mad(NOISE[,x]))
    #average offset/level/location of noise
    noise_loca <- MEma[1,]
    names(noise_loca) <- whr_flux
    #actual dispersion of noise
    noise_disp <- MEma[2,]
    names(noise_disp) <- whr_flux
    #detection limit (recast of signal-to-noise criterion after Park et al., 2013), at provided confidence level
    conf_fac <- qnorm((1 - conf_level)/2, lower.tail = FALSE)
    noise_dete <- abs(noise_loca) + conf_fac * noise_disp
    names(noise_dete) <- whr_flux
    #signal to noise ratio
    noise_s2n <- ( abs(REYN_loc$mn[, whr_flux]) - abs(noise_loca) ) / noise_disp
    names(noise_s2n) <- whr_flux
    
    #stop criterion: change in signal to noise ratio < 10%
    CRIT <- abs( (noise_dete - noise_dete_prior) / noise_dete_prior )
    #condition1: CRIT has to consist of finite values
    if(length(which(is.infinite(unlist(CRIT)))) == 0) {
      #condition 2: change in signal to noise ratio < 1% between steps
      if(length(which(abs(CRIT) < crit_iter)) == length(CRIT)) crit <- TRUE else crit <- FALSE
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
  rm(crit, CRIT, eddy.data_random, MEma, NOISE, r, REYN_noise, whr_random)
  
  #return results
  return(noise)
  
}
