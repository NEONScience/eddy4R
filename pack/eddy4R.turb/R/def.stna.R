##############################################################################################
#' @title Stationarity tests

#' @author
#' Stefan Metzger \email{eddy4R.info@gmail.com}

#' @description 
#' Function defintion. Stationarity tests.

#' @param Currently none

#' @return Currently none

#' @references Currently none

#' @keywords eddy-covariance, turbulent flux

#' @examples Currently none

#' @seealso Currently none

#' @export

# changelog and author contributions / copyrights
#   Stefan Metzger (2015-03-18)
#     original creation
#   Stefan Metzger (2015-11-29)
#     added Roxygen2 tags
##############################################################################################

############################################################
#STATIONARITY TESTS
############################################################

REYNstat_FD_mole_dry <- function(
  data,  		#data frame with EC data
  type=c(1, 2, 3)[2],	#analysis with trend (1) or internal stationarity (2) or both (3)
  whr_flux, #for which fluxes to perform stationarity test?
  NOsusa=6,		#number of subsamples for trend==FALSE
  FcorPOT,
  FcorPOTl=FcorPOTl
) {
  
  #-----------------------------------------------------------
  #BASIC SETUP
  
  #fluxes including trend
  tren <- REYNflux_FD_mole_dry(
    data=data,
    AlgBase="mean",
    FcorPOT=FcorPOT,
    FcorPOTl=FcorPOTl
  )
  
  
  ###
  if(type %in% c(1, 3)) {
    ###
    
    #-----------------------------------------------------------
    #TREND EFFECT
    
    #fluxes after trend removal
    detr <- REYNflux_FD_mole_dry(
      data=data,
      AlgBase="trnd",
      FcorPOT=FcorPOT,
      FcorPOTl=FcorPOTl
    )
    
    #deviation [%]
    crit_t <- ((detr$mn - tren$mn) / tren$mn * 100)[whr_flux]
    
    #clean up
    rm(detr)
    
    
    ###
  } else crit_t <- NULL
  ###
  
  
  ###
  if(type %in% c(2, 3)) {
    ###
    
    #-----------------------------------------------------------
    #INTERNAL INSTATIONARITIES
    
    #class boundaries
    sampBO <- round(seq(1, nrow(data), length.out=NOsusa + 1))
    sampBO[length(sampBO)] <- sampBO[length(sampBO)] + 1
    
    #list with indexes of subsamples
    sampLU <- sapply(1:(length(sampBO) - 1), function(x) seq(sampBO[x], sampBO[x + 1] - 1))
    
    #results for the subsamples
    sampRE <- sapply(1:NOsusa, function(x) REYNflux_FD_mole_dry(
      data=data[sampLU[[x]],],
      AlgBase="mean",
      FcorPOT=FcorPOT,
      FcorPOTl=FcorPOTl
    )$mn[,whr_flux]
    )
    sampRE <- data.frame(matrix(unlist(sampRE), ncol=length(whr_flux), byrow=TRUE))
    dimnames(sampRE)[[2]] <- whr_flux
    
    #stationarity criteria
    crit_i <- (colMeans(sampRE) - tren$mn[whr_flux]) / tren$mn[whr_flux] * 100
    
    #clean up
    rm(tren, NOsusa, sampBO, sampLU, sampRE)
    
    ###
  } else crit_i <- NULL
  ###
  
  
  #-----------------------------------------------------------
  #AGGREGATE AND RETURN RESULTS
  
  #aggregate results
  crit <- list()
  if(!is.null(crit_t)) crit$trend=crit_t
  if(!is.null(crit_i)) crit$subsa=crit_i
  
  #return results
  return(crit)
  
}
