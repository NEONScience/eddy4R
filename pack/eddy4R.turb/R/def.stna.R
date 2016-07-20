##############################################################################################
#' @title Stationarity tests

#' @author
#' Stefan Metzger \email{eddy4R.info@gmail.com} \cr
#' Natchaya Pingintha-Durden

#' @description 
#' Function defintion. Stationarity tests based on Vickers and Mahrt (1997) and Foken and Wichura (1996).

#' @param \code{data} A vector containing the input data. Of class "numeric" or "integer". [user-defined]
#' @param \code{MethStna} A Vector containing the stationarity test methods. \code{MethStna} = c(1,2,3), where 1 is calculating using trend method (Vickers and Mahrt, 1997) , 2 is calculating using internal stationarity method (Foken and Wichura, 1996) , and 3 is calculating using both methods. Defaults to 2. [-]
#' @param \code{whrVar} Specific column in \code{data} containing the variables to be performed stationarity test. Of class "numeric" (column number) or "character" (column name). Defaults to NULL. [-] 
#' @param \code{NumSubSamp}  An object of class "numeric" or "integer" containing the number of sub sample over averaing period. For example, \code{NumSubSamp} = 6 if a 30 min averaging period is subsetted into 5 minute intervals. Defaults to 6. [-]
#' @param \code{corTempPot} A logical indicating whether or not to use potential temperature in flux calculation. Defaults to TRUE. [-]
#' @param \code{presTempPot} A vector containing the air pressure data that will be used in the calculation when \code{corTempPot}=TRUE. Of class "numeric" or "integer".[Pa]

#' @return Currently none

#' @references
#' Foken, T. and Wichura, B.: Tools for quality assessment of surface-based flux measurements, Agricultural and Forest Meteorology, 78, 83-105, (1996) \cr
#' Vickers, D. and Mahrt, L.: Quality control and flux sampling problems for tower and aircraft data, Journal of Atmospheric and Oceanic Technology, 14, 512-526, 1997. \cr

#' @keywords eddy-covariance, stationarity, turbulent flux

#' @examples
#' #input data
#' doy <- c(115.01, 115.03, 115.05, 115.07, 115.09, 115.11, 115.14, 115.16, 115.18,  115.20) # day of year
#' pres <- c(99422.008, 99464.440, 99509.482, 99533.279, 99572.300, 99606.239, 99610.400, 99630.021, 99657.557, 99670.128) # pressure
#' veloFric <- c(0.498, 0.695, 0.656, 0.468, 0.446, 0.295, 0.566, 0.567, 0.600, 0.528)# ustar
#' fluxSenh <- c(-35.254, -71.593, -56.082, -28.539, -36.199, -9.943, -68.417, -44.583, -32.294, -53.629) # Sensible heat flux
#' fluxLath <- c(10.885, 55.163, 22.086, 32.500, 6.868, -0.247, 4.003, 23.445, 11.954, 30.837) # Latent heat flux
#' 
#' data <- data.frame(doy, pres, veloFric, fluxSenh, fluxLath) # generating data frame
#' out <-def.stna(data=data, MethStna=2,whrVar=c("veloFric", "fluxSenh", "fluxLath"), NumSubSamp=6, corTempPot=TRUE, presTempPot="pres")

#' @seealso Currently none

#' @export

# changelog and author contributions / copyrights
#   Stefan Metzger (2015-03-18)
#     original creation
#   Stefan Metzger (2015-11-29)
#     added Roxygen2 tags
#   Natchaya Pingintha-Durden (2015-07-15)
#     Initail naming convention for eddy4R
##############################################################################################
#STATIONARITY TESTS

def.stna <- function(
  data,  		#data frame with EC data
  MethStna=c(1, 2, 3)[2],	#analysis with trend (1) or internal stationarity (2) or both (3)
  whrVar, #for which fluxes to perform stationarity test?
  NumSubSamp=6,		#number of subsamples for trend==FALSE
  corTempPot=TRUE,
  presTempPot=NULL
) {
  
  #-----------------------------------------------------------
  #BASIC SETUP
  
  #fluxes including trend
  tren <- REYNflux_FD_mole_dry(
    data=data,
    AlgBase="mean",
    FcorPOT=corTempPot,
    FcorPOTl=presTempPot
  )
  
  
  ###
  if(MethStna %in% c(1, 3)) {
    ###
    
    #-----------------------------------------------------------
    #TREND EFFECT
    
    #fluxes after trend removal
    detr <- REYNflux_FD_mole_dry(
      data=data,
      AlgBase="trnd",
      FcorPOT=corTempPot,
      FcorPOTl=presTempPot
    )
    
    #deviation [%]
    rptStna01 <- ((detr$mn - tren$mn) / tren$mn * 100)[whrVar]
    
    #clean up
    rm(detr)
    
    
    ###
  } else rptStna01 <- NULL
  ###
  
  
  ###
  if(MethStna %in% c(2, 3)) {
    ###
    
    #-----------------------------------------------------------
    #INTERNAL INSTATIONARITIES
    
    #class boundaries
    sampBou <- base::round(base::seq(1, nrow(data), length.out=NumSubSamp + 1))
    sampBou[length(sampBou)] <- sampBou[length(sampBou)] + 1
    
    #list with indexes of subsamples
    idxSubsamp <- base::sapply(1:(length(sampBou) - 1), function(x) base::seq(sampBou[x], sampBou[x + 1] - 1))
    
    #results for the subsamples
    outSubsamp <- base::sapply(1:NumSubSamp, function(x) REYNflux_FD_mole_dry(
      data=data[idxSubsamp[[x]],],
      AlgBase="mean",
      FcorPOT=corTempPot,
      FcorPOTl=presTempPot
    )$mn[,whrVar]
    )
    outSubsamp <- data.frame(base::matrix(unlist(outSubsamp), ncol=length(whrVar), byrow=TRUE))
    dimnames(outSubsamp)[[2]] <- whrVar
    
    #stationarity criteria
    rptStna02 <- (base::colMeans(outSubsamp) - tren$mn[whrVar]) / tren$mn[whrVar] * 100
    
    #clean up
    rm(tren, NumSubSamp, sampBou, idxSubsamp, outSubsamp)
    
    ###
  } else rptStna02 <- NULL
  ###
  
  
  #-----------------------------------------------------------
  #AGGREGATE AND RETURN RESULTS
  
  #aggregate results
  rpt <- list()
  if(!is.null(rptStna01)) crit$trend=rptStna01
  if(!is.null(rptStna02)) crit$subsa=rptStna02
  
  #return results
  return(rpt)
  
}
