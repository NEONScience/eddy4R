##############################################################################################
#' @title Definition function: Statistical errors for scalars, variances, and fluxes

#' @author
#' Stefan Metzger \email{eddy4R.info@gmail.com}
#' Kenny Pratt

#' @description 
#' Function defintion. Statistical errors for scalars, variances, and fluxes.

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
#   Kenny Pratt (2017-02-08)
#     changed naming conventions to match eddy4R 
##############################################################################################
def.ucrt.samp <- function(
  distIsca,		    #integral scale lengths
  valuMean,		      #mean values
  coefCorr,		      #flux correlation coefficient
  distAve,		      #averaging distance ()
  timeFold = 0     #e-folding time for the autocorrelation of wavelet power at
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
  whrDistIsca <- base::rbind(  
    c("u_star2_x", "u_hor"),
    c("u_star2_y", "v_hor"),
    c("F_H_en", "T_air"),
    c("F_LE_en", "FD_mole_H2O"),
    c("F_CH4_mass",  "FD_mole_CH4")
  )
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
  ucrtRandVari <- base::sqrt(2 * distVariIsca / (distAve + timeFold * distVariIsca))
  
  #fluxes
  
  #directly from flux length scales (Lenschow, 1986 Eq. (7) == Lenschow, 1994 Eq. (48))
  #error estimate
  #large error in F_CH4_mass due to four times lower correlation coefficient
   ucrtRandFlux <- base::data.frame(base::sqrt(2 * distFluxIsca / (distAve + timeFold * distFluxIsca) * (coefCorr^(-2) + 1)))
  #error reproduction
  #Bange: u_star2_x and u_star2_y are orthogonal == indepenend -> for random error we can use Gauss reproduction
   ucrtRandFlux$u_star <- (
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
  ucrtRandFluxMann <- base::data.frame(base::sapply(1:base::length(coefCorr), function(x) base::sqrt(2 *  distMax[x] / (distAve + timeFold * distMax[x]) * (coefCorr[x]^(-2) + 1))))
  #according to Lenschow, 1994 Eq. (49); Bange (2002) Eq. (16) says: WRONG
  #errm <- base::data.frame(base::sapply(1:base::length(coefCorr), function(x) base::abs(2 / coefCorr[x] * base::sqrt(min(distScalIsca$w_hor, distScalIsca[,x]) / (distAve + timeFold * distScalIsca[,x]))) * 100))
  #error reproduction (Bange, 2007 Eq.(3.32))
  #u_star: Bange u_star2_x and u_star2_y are orthogonal == indepenend -> for random error we can use Gauss reproduction
  ucrtRandFluxMann$u_star <- (
    (valuMean$u_star2_x * ucrtRandFluxMann$u_star2_x * valuMean$u_star2_x / valuMean$u_star^2)^2 +
      (valuMean$u_star2_y * ucrtRandFluxMann$u_star2_y * valuMean$u_star2_y / valuMean$u_star^2)^2
  )^(1/4) / valuMean$u_star
  
  #save to list
  ran <- base::list()
  #sampling error in variance
  ran$vari$act=ucrtRandVari
  #actual flux random error (flux length scales)
  ran$flux$act= ucrtRandFlux		
  #maximum flux random error (scalar length scales)
  ran$flux$max=ucrtRandFluxMann		
  
  #clean up
  rm(ucrtRandFlux, ucrtRandVari, ucrtRandFluxMann)
  
  
  #-----------------------------------------------------------
  #SYSTEMATIC ERROR
  
  #variance (Lenschow, 1994 Eq. 36)
  ucrtSysVari <- 2 * distVariIsca / distAve 
  
  #fluxes
  
  #directly from flux length scales (Lenschow, 1994 Eq. (27) == Bange, 2007 Eq. (3.20))
  #error estimate
  ucrtSysFlux <- base::data.frame(2 * distFluxIsca / distAve)
  #error reproduction
  #u_star upper bound (no independence and randomness required)
  ucrtSysFlux$u_star <- (
    (valuMean$u_star2_x * ucrtSysFlux$u_star2_x * valuMean$u_star2_x / valuMean$u_star^2)^2 +
      (valuMean$u_star2_y * ucrtSysFlux$u_star2_y * valuMean$u_star2_y / valuMean$u_star^2)^2
  )^(1/4) / valuMean$u_star
  
  #upper limit from scalar length scales
  #error estimate
  #with Mann / Bange length scale
  ucrtSysFluxMann <- base::data.frame(base::sapply(1:base::length(coefCorr), function(x) 2 *  distMax[x] / distAve))
  #according to Lenschow, 1994 Eq. (29); same!
  #ersm <- base::data.frame(base::sapply(1:base::length(coefCorr), function(x) base::abs(2 / coefCorr[x] * base::sqrt(distScalIsca$w_hor * distScalIsca[,x] ) / distAve) * 100))
  #error reproduction
  #u_star upper bound (no independence and randomness required)
  ucrtSysFluxMann$u_star <- (
    (valuMean$u_star2_x * ucrtSysFluxMann$u_star2_x * valuMean$u_star2_x / valuMean$u_star^2)^2 +
      (valuMean$u_star2_y * ucrtSysFluxMann$u_star2_y * valuMean$u_star2_y / valuMean$u_star^2)^2
  )^(1/4) / valuMean$u_star
  
  #save to list
  sys <- base::list()
  #sampling error in variance
  sys$vari$act=ucrtSysVari
  #actual flux systematic error (flux length scales)
  sys$flux$act=ucrtSysFlux
  #maximum flux systematic error (scalar length scales)
  sys$flux$max=ucrtSysFluxMann
  
  #clean up
  rm(distScalIsca, distVariIsca, distFluxIsca, ucrtSysFlux, ucrtSysVari, ucrtSysFluxMann,  distMax)
  
  
  #-----------------------------------------------------------
  #AGGREGATE AND EXPORT RESULTS
  
  #aggregate
  exrt <- base::list(
    ran=ran,
    sys=sys
  )
  #clean up
  rm(ran, sys)
  #export
  return(exrt)
  
}