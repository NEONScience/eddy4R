##############################################################################################
#' @title Integral length scales

#' @author
#' Stefan Metzger \email{eddy4R.info@gmail.com}

#' @description 
#' Function defintion. Integral length scales.

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
#INTEGERAL LENGTH SCALES
############################################################


INTsca <- function(
  d_xy_flow,
  depe
) {

  #fill gaps via linear interpolation
    depe <- approx(d_xy_flow, depe, xout=d_xy_flow)[[2]]

  #get rid of NAs at start and end
    dum_NA <- na.omit(data.frame(d_xy_flow=d_xy_flow, depe=depe))
    d_xy_flow <- dum_NA$d_xy_flow
    depe <- dum_NA$depe
    rm(dum_NA)

  #demeaning and detrending
    depe <- lm(depe ~ d_xy_flow)$residuals

  #path through air [m] per increment, the stepwidth of the integral scale
    incr <- max(d_xy_flow - min(d_xy_flow)) / length(d_xy_flow)

  #calculate auto-correlation function
    lag <- 10; crit <- 1
    while(crit > 0) {
      lag <- lag * 2
      ACF <- acf(depe, lag.max = lag, type = "correlation", plot = FALSE, na.action = na.fail, demean = TRUE)
      crit <- min(ACF$acf)
    }

  #integral length scale: typical size of the largest or most energy-transporting eddies.
  #a) integral over acf until first zero crossing (Bange, 2002);
  #b) first maximum of the integral (Lenschow, 1986);
    #find first zero crossing
#      whr_zero <- ACF$lag[zeroCross(ACF$acf, slope="negative")[1]]
#      whr_zero <- ACF$lag[GenKern::nearest(x=ACF$acf, xval=0)[1]]
       require(EMD)
       #data needs have extrema, otherwise it will not identify the zero crossing
       #hence attaching sin(1:10) to the end
       whr_zero <- EMD::extrema(y=c(ACF$acf, sin(1:10)))$cross[1,2]
    
    #for each cell, weight distance increment with correlation coefficient
      I <- sum(ACF$acf[1:whr_zero]) * incr
    #alternatively: all in one, assume that correlation monotonously decreases after peak
      #I <- max(cumsum(ACF$acf)) * incr

  #return result
    return(I)

}



##############################################################################################
#' @title Statistical errors for fluxes

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



##############################################################################################
#' @title Detection limit for fluxes

#' @author
#' Stefan Metzger \email{eddy4R.info@gmail.com}

#' @description 
#' Function defintion. Detection limit for fluxes.

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


##############################################################################################
#' @title Flux computation sequence

#' @author
#' Stefan Metzger \email{eddy4R.info@gmail.com}

#' @description 
#' Function wrapper. Flux computation sequence.

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
#COMPLETE FLUX COMPUTATION SEQUENCE
############################################################

REYNcomp_FD_mole_dry <- function(
  eddy.data_loc=eddy.data,
  whr_scal=c("u_hor", "v_hor", "w_hor", "T_air", "T_air_0", "T_v_0", "FD_mole_H2O", "FD_mole_CH4", "T_surface", "R_SW_down"),
  whr_flux=c("u_star2_x", "u_star2_y", "u_star", "F_H_en", "F_LE_en", "F_CH4_mass"),
  latitude,
  AlgBase=c("mean", "trnd", "ord03")[1],
  stattype=c(1, 2, 3)[3],
  NOsusa=6,
  FcorPOT=TRUE,
  FcorPOTl=NULL,
  #perform determination of detection limit, siganl-to-noise ratio etc.? 
  noise_determination=FALSE,
  #confidence level for detection limit
  conf_level=0.95,
  #criterion to stop iteration (0.01 = 1% change among subsequent realizations)
  crit_iter=0.01,
  PltfEc="airc",
  flagCh4 = TRUE
) {


#-----------------------------------------------------------
#CALCULATE FLUXES
  REYN_loc <- REYNflux_FD_mole_dry(
    data=eddy.data_loc,
    AlgBase=AlgBase,
    FcorPOT=FcorPOT,
    FcorPOTl=FcorPOTl,
    PltfEc=PltfEc,
    flagCh4 = flagCh4
  )


#-----------------------------------------------------------
#CALCULATE AERODYNAMIC ROUGHNESS LENGTH
  REYN_loc$mn$d_z_0 <- def.dist.rgh(
    distZaxsMeas=REYN_loc$mn$d_z_m,
    distObkv=REYN_loc$mn$d_L_v_0,
    veloXaxs=REYN_loc$mn$u_hor,
    veloFric=REYN_loc$mn$u_star,
    RngStblObkv=c(-2, 1)
  )


#-----------------------------------------------------------
#STATIONARITY CRITERIA [%]
  REYN_loc$stat <- def.stna(
    data=eddy.data_loc,
    MethStna=c(1, 2, 3)[stattype],
    whrVar=whr_flux,
    NumSubSamp=NOsusa,
    corTempPot=FcorPOT,
    presTempPot=FcorPOTl,
    PltfEc = PltfEc,
    flagCh4 = flagCh4
  )


#-----------------------------------------------------------
#INTEGRAL TURBULENCE CHARACTERISTICS [%]

  #data frame for deviations
    stdev <- data.frame(
      u_hor=REYN_loc$sd$u_hor,
      w_hor=REYN_loc$sd$w_hor,
      T_air=REYN_loc$sd$T_air
    )

  #data frame for scales
    scale <- data.frame(
      u_star=REYN_loc$mn$u_star,
      T_star_SL=REYN_loc$mn$T_star_SL
    )

  #calculation
    REYN_loc$itcs <- def.itc(
      stblObkv=REYN_loc$mn$sigma,  #stability
      lat=latitude,
      VarInp=c("veloXaxs","veloZaxs","temp","all")[4],
      sd=stdev,
      varScal=scale
    )

  #clean up
    rm(stdev, scale)


#-----------------------------------------------------------
#INTEGRAL LENGTH SCALES

  #scalar length scales
    #calculate scales
      isca_scal <- sapply(whr_scal, function(x) INTsca(
    	  d_xy_flow=REYN_loc$data$d_xy_flow,
    	  depe=REYN_loc$imfl[,x]
      	)
      )
      isca_scal <- as.data.frame( matrix(isca_scal, ncol=length(whr_scal)) )
      attributes(isca_scal)$names <- whr_scal

  #variance length scales
    #calculate scales
      isca_vari <- sapply(whr_scal, function(x) INTsca(
    	  d_xy_flow=REYN_loc$data$d_xy_flow,
    	  depe=REYN_loc$imfl[,x]^2
      	)
      )
      isca_vari <- as.data.frame( matrix(isca_vari, ncol=length(whr_scal)) )
      attributes(isca_vari)$names <- whr_scal

  #flux length scales
    #exclude data with > 10% immidiate flucuations not available, e.g. u_star;
    #sampling error in u_star has to be assessed by its both components u_star_x and u_star_y
      whr_not <- sapply(whr_flux, function(x) length(which(is.na(REYN_loc$imfl[,x]))) / nrow(REYN_loc$imfl))
      whr_not <- which(whr_not > 0.1)
      if(length(whr_not) == 0) whr_flux_loc <-  whr_flux else whr_flux_loc <-  whr_flux[-whr_not]

    #calculate scales    
      isca_flux <- sapply(whr_flux_loc, function(x) INTsca(
    	  d_xy_flow=REYN_loc$data$d_xy_flow,
    	  depe=REYN_loc$imfl[,x]
    	  )
      )
      isca_flux <- as.data.frame( matrix(isca_flux, ncol=length(whr_flux_loc)) )
      attributes(isca_flux)$names <- whr_flux_loc

  #assign to list
    REYN_loc$isca <- list(
                 scal=isca_scal,
                 vari=isca_vari,
                 flux=isca_flux
                 )

  #clean up
    rm(isca_scal, isca_vari, isca_flux, whr_flux_loc, whr_not)


#-----------------------------------------------------------
#STATISTICAL ERRORS FOR FLUXES
  REYN_loc$erro <- REYNerro_FD_mole_dry(
    isca=REYN_loc$isca,		#integral scale lengths
    mn=REYN_loc$mn,			#mean values
    rc=REYN_loc$cor,		#flux correlation coefficient
    Lf=REYN_loc$max$d_xy_flow		#averaging distance (length of air d_xy_flow flown through [m])
  )


#-----------------------------------------------------------
#DETECTION LIMIT

if(noise_determination == TRUE) {

  REYN_loc$noise <- def.nois (
    #data set
    dataTest=eddy.data_loc,
    #actual (correct) fluxes
    dataRefe=REYN_loc,
    #what to use as basis to determine fluctuations
    AlgBase=AlgBase,
    #use potential quantities?
    corTempPot=FcorPOT,
    #pressure level for potential quantities
    presTempPot=FcorPOTl,
    #which entries are fluxes?
    idxFlux=whr_flux,
    #confidence level for detection limit
    ConfLevl=conf_level,
    #criterion to stop iteration (0.01 = 1% change among subsequent realizations)
    CritMax=crit_iter,
    PltfEc = PltfEc,
    flagCh4 = flagCh4
  )

}


#-----------------------------------------------------------
#RETURN RESULT
  return(REYN_loc)


}
