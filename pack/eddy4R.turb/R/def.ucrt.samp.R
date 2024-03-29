##############################################################################################
#' @title Definition function: Statistical errors for scalars, variances, and fluxes

#' @author
#' Stefan Metzger \email{eddy4R.info@gmail.com}
#' Kenny Pratt

#' @description 
#' Function defintion. Statistical errors for scalars, variances, and fluxes.

#' @param \code{data} A vector of instantaneous data. If user provides vector, Moving block bootstrap technique will be implemented to provide an additional uncertainty estimate. If NULL is provided, only Lenschow technique will be performed.
#' @param \code{distIsca} A list of integral length scales for scalars, variances, and fluxes.
#' @param \code{valuMean} Mean Values.
#' @param \code{coefCorr} Flux correlation coefficient
#' @param \code{distMean} Average flight length
#' @param \code{timeFold} e-folding time

#' @return Statistical Errors for scalars, variances, and fluxes

#' @references Torrence and Compo, 1998 \cr
#' Lenschow, 1994 \cr
#' D.H. Lenschow, B.B.Stankov. (1986) Length Scales in the Convective Boundary Layer. Journal of the Atmospheric Sciences. 43:12, 1198-1209 \cr
#' J.Mann, D. Lenschow (1994) Errors in airborne flux measurements. JGR:Atmospheres, 99, 14519-14526 \cr
#' J. Bange (2002). Airborne Measurements of Turbulent Energy Exchange Between the Earth Surface and the Atmosphere. \cr
#' S. Salesky, M. Chamecki, N. Dias (2012) Estimating random error in eddy covariance fluxes and other turbulence statistics: the filtering method .  Boundary-Layer Meteorology., 144, 113-135. \cr 
#' J.Bange, F. Beyrich, D. Engelbart. (2002) Airborne measurements of turbulent fluxes during LITFASS-98: Comparison with ground measurements and remote sensing in a case study. Theor. Appl. Climatol. 73, 35-51 \cr

#' @keywords eddy-covariance, sampling uncertainty

#' @examples Currently none

#' @seealso Currently none

#' @export

# changelog and author contributions / copyrights
#   Stefan Metzger (2015-03-18)
#     original creation
#   Stefan Metzger (2015-11-29)
#     added Roxygen2 tags
#   Kenny Pratt (2017-02-08)
#     changed naming conventions to match eddy4R and added Moving Block Bootstrap technique
#   Will Drysdale (2018-01-25)
#     Added Support for other species to be defined in SiteInfo
#   Will Drysdale (2019-01-29)
#     Removed SiteInfo dependance, added switches as explicit variables
#   David Durden (2019-11-07)
#     Fixing missing water vapor variable
##############################################################################################
def.ucrt.samp <- function(
  data = NULL,           #instantaneous data
  distIsca,		    #integral scale lengths
  valuMean,		      #mean values
  coefCorr,		      #flux correlation coefficient
  distMean,		      #averaging distance ()
  timeFold = 0 ,    #e-folding time for the autocorrelation of wavelet power at
  spcs = NULL
  #each scale (Torrence and Compo, 1998, Table 1). This e-folding time is chosen
  #so that the wavelet power for a discontinuity at the
  #edge drops by a factor e-2 and ensures that the edge
  #effects are negligible beyond this point. 
  #For Morlet Wavelet sqrt(2) * scale for one side (for or aft), so 2 * sqrt(2) * scale for both sides (fore and aft)
) {
  
  #-----------------------------------------------------------
  #VARIABLES
  
  #scalar length scales
  distScalIsca <- distIsca$scal
  #lookup table for matching scalar length scales to flux correlation coefficients
  if(is.null(spcs)){
    whrDistIsca <- base::rbind(  
      c("u_star2_x", "u_hor"),
      c("u_star2_y", "v_hor"),
      c("F_H_en", "T_air"),
      c("F_LE_en", "FD_mole_H2O"),
      c("F_CH4_mass",  "FD_mole_CH4")
    )
  }else{
    whrDistIsca <- base::rbind(c("u_star2_x", "u_hor"),
                               c("u_star2_y", "v_hor"),
                               c("F_H_en", "T_air"),
                               c("F_LE_en", "FD_mole_H2O"))
    
    for(i in 1:length(spcs)){
      F_spcs_var <- def.spcs.name(spcs[i],"mole")
      F_spcs_mass <- def.spcs.name(spcs[i],"mass")
      
      whrDistIsca = base::rbind(whrDistIsca,
                                c(F_spcs_mass,F_spcs_var))}
  }
  
  #variance length scales
  distVariIsca <- distIsca$vari
  #flux length scales
  distFluxIsca <- distIsca$flux
  #matching flux correlation coefficients
  coefCorr <- coefCorr[base::match(base::dimnames(distFluxIsca)[[2]], base::dimnames(coefCorr)[[2]])]
  
  
  #-----------------------------------------------------------
  #RANDOM ERROR
  
  #variance (Lenschow, 1994 Eq. 36)
  #in case of Wavelet, the convolution of the mother Wavelet at a particular scale of interest (here: the integral length scale) 
  #is influced by a multiple of that scale fore and aft the averaging interval (e-folding time).
  #This should be considered for the random error ( + timeFold * distVariIsca in denominator)
  ucrtRandVari <- base::sqrt(2 * distVariIsca / (distMean + timeFold * distVariIsca))
  
  
  #fluxes
  
  #directly from flux length scales (Lenschow, 1986 Eq. (7) == Lenschow, 1994 Eq. (48))
  #error estimate
  #large error in F_CH4_mass due to four times lower correlation coefficient
  ucrtRandFlux <- base::data.frame(base::sqrt(2 * distFluxIsca / (distMean + timeFold * distFluxIsca) * (coefCorr^(-2) + 1)))
  
  #error reproduction
  #Bange: u_star2_x and u_star2_y are orthogonal == indepenend -> for random error we can use Gauss reproduction
  ucrtRandFlux$veloFric <- (
    (valuMean$u_star2_x *  ucrtRandFlux$u_star2_x * valuMean$u_star2_x / valuMean$u_star^2)^2 +
      (valuMean$u_star2_y *  ucrtRandFlux$u_star2_y * valuMean$u_star2_y / valuMean$u_star^2)^2
  )^(1/4) / valuMean$u_star 
  
  #upper limit from scalar length scales
  #upper limt for lenght scale following Mann (1994) Eq. (9), Bange (2002) Eq. (10)
  distMax <- base::data.frame(base::sapply(1:base::length(coefCorr), function(x) {
    #get corresponding scalar length scale from conversion table
    locScalIsca <- distScalIsca[,whrDistIsca[base::match(base::dimnames(coefCorr)[[2]][x], whrDistIsca[,1]),2]]
    base::sqrt(distScalIsca$w_hor *  locScalIsca) / base::abs(coefCorr[x])
  }
  ))
  #error estimate
  #with Mann / Bange length scale
  ucrtRandFluxMann <- base::data.frame(base::sapply(1:base::length(coefCorr), function(x) base::sqrt(2 *  distMax[x] / (distMean + timeFold * distMax[x]) * (coefCorr[x]^(-2) + 1))))
  #according to Lenschow, 1994 Eq. (49); Bange (2002) Eq. (16) says: WRONG
  #errm <- base::data.frame(base::sapply(1:base::length(coefCorr), function(x) base::abs(2 / coefCorr[x] * base::sqrt(min(distScalIsca$w_hor, distScalIsca[,x]) / (distMean + timeFold * distScalIsca[,x]))) * 100))
  #error reproduction (Bange, 2007 Eq.(3.32))
  #u_star: Bange u_star2_x and u_star2_y are orthogonal == indepenend -> for random error we can use Gauss reproduction
  ucrtRandFluxMann$veloFric <- (
    (valuMean$u_star2_x * ucrtRandFluxMann$u_star2_x * valuMean$u_star2_x / valuMean$u_star^2)^2 +
      (valuMean$u_star2_y * ucrtRandFluxMann$u_star2_y * valuMean$u_star2_y / valuMean$u_star^2)^2
  )^(1/4) / valuMean$u_star
  
  #save to list
  ucrtRand <- base::list()
  
  #sampling error in variance
  ucrtRand$vari$act=ucrtRandVari*100
  #actual flux random error (flux length scales)
  ucrtRand$flux$act= ucrtRandFlux*100
  #maximum flux random error (scalar length scales)
  ucrtRand$flux$max=ucrtRandFluxMann*100		
  
  #Below is the Moving Block Bootstrap technique to estimate sampling uncertainty for scalars, variances, and fluxes. Only run if user supplies instantaneous data
  
  if (!is.null(data)) {
    
    #scalar
    
    require(boot)
    #Compute the mean of the Relative Error from the B = 1000 bootstrap samples
    distCrit <- def.wind.mbb(data$scal)
    tmp <- boot::tsboot(data$scal,mean, 1000, l = round(distCrit), sim = "fixed")
    ucrtRandScal <- sd(tmp$t)/mean(data$scal)
    rm(tmp)
    
    #save sampling error to list
    ucrtRand$scal$act = ucrtRandScal*100
    
    #variance
    
    #Compute the mean of the Relative Error from the B = 1000 bootstrap samples
    distCrit <- def.wind.mbb(data$vari)
    tmp <- boot::tsboot(data$vari,mean, 1000, l = round(distCrit), sim = "fixed")
    ucrtRandVariMbb <- sd(tmp$t)/mean(data$vari)
    rm(tmp)
    
    #sampling error in variance using MBB
    ucrtRand$vari$mbb=ucrtRandVariMBB*100
    
    #flux
    
    #Compute the mean of the Relative Error from the B = 1000 bootstrap samples
    distCrit <- def.wind.mbb(data$flux)
    tmp <- boot::tsboot(data$flux,mean, 1000, l = round(distCrit), sim = "fixed")
    ucrtRandFluxMbb <- sd(tmp$t)/mean(data$flux)
    rm(tmp)
    
    #Sampling error using MBB
    ucrtRand$flux$mbb= ucrtRandFluxMBB*100
    
  }
  
  #clean up
  rm(ucrtRandFlux, ucrtRandVari, ucrtRandFluxMann)
  
  
  #-----------------------------------------------------------
  #SYSTEMATIC ERROR
  
  #variance (Lenschow, 1994 Eq. 36)
  ucrtSysVari <- 2 * distVariIsca / distMean 
  
  #fluxes
  
  #directly from flux length scales (Lenschow, 1994 Eq. (27) == Bange, 2007 Eq. (3.20))
  #error estimate
  ucrtSysFlux <- base::data.frame(2 * distFluxIsca / distMean)
  #error reproduction
  #u_star upper bound (no independence and randomness required)
  ucrtSysFlux$veloFric <- (
    (valuMean$u_star2_x * ucrtSysFlux$u_star2_x * valuMean$u_star2_x / valuMean$u_star^2)^2 +
      (valuMean$u_star2_y * ucrtSysFlux$u_star2_y * valuMean$u_star2_y / valuMean$u_star^2)^2
  )^(1/4) / valuMean$u_star
  
  #upper limit from scalar length scales
  #error estimate
  #with Mann / Bange length scale
  ucrtSysFluxMann <- base::data.frame(base::sapply(1:base::length(coefCorr), function(x) 2 *  distMax[x] / distMean))
  #according to Lenschow, 1994 Eq. (29); same!
  #ersm <- base::data.frame(base::sapply(1:base::length(coefCorr), function(x) base::abs(2 / coefCorr[x] * base::sqrt(distScalIsca$w_hor * distScalIsca[,x] ) / distMean) * 100))
  #error reproduction
  #u_star upper bound (no independence and randomness required)
  ucrtSysFluxMann$veloFric <- (
    (valuMean$u_star2_x * ucrtSysFluxMann$u_star2_x * valuMean$u_star2_x / valuMean$u_star^2)^2 +
      (valuMean$u_star2_y * ucrtSysFluxMann$u_star2_y * valuMean$u_star2_y / valuMean$u_star^2)^2
  )^(1/4) / valuMean$u_star
  
  #save to list
  ucrtSys <- base::list()
  #sampling error in variance
  ucrtSys$vari$act=ucrtSysVari*100
  #actual flux systematic error (flux length scales)
  ucrtSys$flux$act=ucrtSysFlux*100
  #maximum flux systematic error (scalar length scales)
  ucrtSys$flux$max=ucrtSysFluxMann*100
  
  #clean up
  rm(distScalIsca, distVariIsca, distFluxIsca, ucrtSysFlux, ucrtSysVari, ucrtSysFluxMann,  distMax)
  
  
  #-----------------------------------------------------------
  #AGGREGATE AND EXPORT RESULTS
  
  #aggregate
  rprt <- base::list(
    ran=ucrtRand,
    sys=ucrtSys
  )
  #clean up
  rm(ucrtRand, ucrtSys)
  #export
  return(rprt)
  
}