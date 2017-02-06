##############################################################################################
#' @title Definition function: Statistical errors for fluxes

#' @author
#' Stefan Metzger \email{eddy4R.info@gmail.com}

#' @description 
#' Function defintion. Statistical errors for fluxes.

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
#STATISTICAL ERROR FOR FLUXES
############################################################

REYNerro_FD_mole_dry <- function(
  isca,		    #integral scale lengths
  mn,		      #mean values
  rc,		      #flux correlation coefficient
  Lf,		      #averaging distance (flight length)
  COI = 0     #e-folding time for the autocorrelation of wavelet power at
  #each scale (Torrence and Compo, 1998, Table 1). This e-folding time is chosen
  #so that the wavelet power for a discontinuity at the
  #edge drops by a factor e-2 and ensures that the edge
  #effects are negligible beyond this point. 
  #For Morlet Wavelet sqrt(2) * scale for one side (for or aft), so 2 * sqrt(2) * scale for both sides (fore and aft)
) {
  
  
  #-----------------------------------------------------------
  #VARIABLES
  
  #scalar length scales
  ls <- isca$scal
  #lookup table for matching scalar length scales to flux correlation coefficients
  whr_ls <- rbind(  
    c("u_star2_x", "u_hor"),
    c("u_star2_y", "v_hor"),
    c("F_H_en", "T_air"),
    c("F_LE_en", "FD_mole_H2O"),
    c("F_CH4_mass",  "FD_mole_CH4")
  )
  #variance length scales
  lv <- isca$vari
  #flux length scales
  lf <- isca$flux
  #matching flux correlation coefficients
  rc <- rc[match(dimnames(lf)[[2]], dimnames(rc)[[2]])]
  
  
  #-----------------------------------------------------------
  #RANDOM ERROR
  
  #variance (Lenschow, 1994 Eq. 36)
  #in case of Wavelet, the convolution of the mother Wavelet at a particular scale of interest (here: the integral length scale) 
  #is influced by a multiple of that scale fore and aft the averaging interval (e-folding time).
  #This should be considered for the random error ( + COI * lv in denominator)
  erra_vari <- sqrt(2 * lv / (Lf + COI * lv)) * 100
  
  #fluxes
  
  #directly from flux length scales (Lenschow, 1986 Eq. (7) == Lenschow, 1994 Eq. (48))
  #error estimate
  #large error in F_CH4_mass due to four times lower correlation coefficient
  erra_flux <- data.frame(sqrt(2 * lf / (Lf + COI * lf) * (rc^(-2) + 1)) * 100)
  #error reproduction
  #Bange: u_star2_x and u_star2_y are orthogonal == indepenend -> for random error we can use Gauss reproduction
  erra_flux$u_star <- (
    (mn$u_star2_x * erra_flux$u_star2_x / 100 * mn$u_star2_x / mn$u_star^2)^2 +
      (mn$u_star2_y * erra_flux$u_star2_y / 100 * mn$u_star2_y / mn$u_star^2)^2
  )^(1/4) / mn$u_star * 100
  
  #upper limit from scalar length scales
  #upper limt for lenght scale following Mann (1994) Eq. (9), Bange (2002) Eq. (10)
  lf_max <- data.frame(sapply(1:length(rc), function(x) {
    #get corresponding scalar length scale from conversion table
    ls_loc <- ls[,whr_ls[match(dimnames(rc)[[2]][x], whr_ls[,1]),2]]
    sqrt(ls$w_hor * ls_loc) / abs(rc[x])
  }
  ))
  #error estimate
  #with Mann / Bange length scale
  errm_flux <- data.frame(sapply(1:length(rc), function(x) sqrt(2 * lf_max[x] / (Lf + COI * lf_max[x]) * (rc[x]^(-2) + 1)) * 100))
  #according to Lenschow, 1994 Eq. (49); Bange (2002) Eq. (16) says: WRONG
  #errm <- data.frame(sapply(1:length(rc), function(x) abs(2 / rc[x] * sqrt(min(ls$w_hor, ls[,x]) / (Lf + COI * ls[,x]))) * 100))
  #error reproduction (Bange, 2007 Eq.(3.32))
  #u_star: Bange u_star2_x and u_star2_y are orthogonal == indepenend -> for random error we can use Gauss reproduction
  errm_flux$u_star <- (
    (mn$u_star2_x * errm_flux$u_star2_x / 100 * mn$u_star2_x / mn$u_star^2)^2 +
      (mn$u_star2_y * errm_flux$u_star2_y / 100 * mn$u_star2_y / mn$u_star^2)^2
  )^(1/4) / mn$u_star * 100
  
  #save to list
  ran <- list()
  #sampling error in variance
  ran$vari$act=erra_vari
  #actual flux random error (flux length scales)
  ran$flux$act=erra_flux		
  #maximum flux random error (scalar length scales)
  ran$flux$max=errm_flux		
  
  #clean up
  rm(erra_flux, erra_vari, errm_flux)
  
  
  #-----------------------------------------------------------
  #SYSTEMATIC ERROR
  
  #variance (Lenschow, 1994 Eq. 36)
  ersa_vari <- 2 * lv / Lf * 100
  
  #fluxes
  
  #directly from flux length scales (Lenschow, 1994 Eq. (27) == Bange, 2007 Eq. (3.20))
  #error estimate
  ersa_flux <- data.frame(2 * lf / Lf * 100)
  #error reproduction
  #u_star upper bound (no independence and randomness required)
  ersa_flux$u_star <- (
    (mn$u_star2_x * ersa_flux$u_star2_x / 100 * mn$u_star2_x / mn$u_star^2)^2 +
      (mn$u_star2_y * ersa_flux$u_star2_y / 100 * mn$u_star2_y / mn$u_star^2)^2
  )^(1/4) / mn$u_star * 100
  
  #upper limit from scalar length scales
  #error estimate
  #with Mann / Bange length scale
  ersm_flux <- data.frame(sapply(1:length(rc), function(x) 2 * lf_max[x] / Lf * 100))
  #according to Lenschow, 1994 Eq. (29); same!
  #ersm <- data.frame(sapply(1:length(rc), function(x) abs(2 / rc[x] * sqrt(ls$w_hor * ls[,x] ) / Lf) * 100))
  #error reproduction
  #u_star upper bound (no independence and randomness required)
  ersm_flux$u_star <- (
    (mn$u_star2_x * ersm_flux$u_star2_x / 100 * mn$u_star2_x / mn$u_star^2)^2 +
      (mn$u_star2_y * ersm_flux$u_star2_y / 100 * mn$u_star2_y / mn$u_star^2)^2
  )^(1/4) / mn$u_star * 100
  
  #save to list
  sys <- list()
  #sampling error in variance
  sys$vari$act=ersa_vari
  #actual flux systematic error (flux length scales)
  sys$flux$act=ersa_flux
  #maximum flux systematic error (scalar length scales)
  sys$flux$max=ersm_flux
  
  #clean up
  rm(ls, lv, lf, ersa_flux, ersa_vari, ersm_flux, lf_max)
  
  
  #-----------------------------------------------------------
  #AGGREGATE AND EXPORT RESULTS
  
  #aggregate
  export <- list(
    ran=ran,
    sys=sys
  )
  #clean up
  rm(ran, sys)
  #export
  return(export)
  
}