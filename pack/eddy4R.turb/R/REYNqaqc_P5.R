##############################################################################################
#' @title Definition function: Detection limit for fluxes

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
#' @title Wrapper function: Flux computation sequence

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
  attributes(REYN_loc$data$d_xy_flow)$unit <- "m"
 
  #scalar length scales
  #calculate scales
  isca_scal <- sapply(whr_scal, function(x) def.dist.isca(
    scalEddy=REYN_loc$data$d_xy_flow,
    data=REYN_loc$imfl[,x]
  )
  )
  isca_scal <- as.data.frame( matrix(isca_scal, ncol=length(whr_scal)) )
  attributes(isca_scal)$names <- whr_scal
  
  #variance length scales
  #calculate scales
  isca_vari <- sapply(whr_scal, function(x) def.dist.isca(
    scalEddy=REYN_loc$data$d_xy_flow,
    data=REYN_loc$imfl[,x]^2
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
  isca_flux <- sapply(whr_flux_loc, function(x) def.dist.isca(
    scalEddy=REYN_loc$data$d_xy_flow,
    data=REYN_loc$imfl[,x]
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
  REYN_loc$erro <- 100*def.ucrt.samp(
    distIsca=REYN_loc$isca,		#integral scale lengths
    valuMean=REYN_loc$mn,			#mean values
    coefCorr=REYN_loc$cor,		#flux correlation coefficient
    distAve=REYN_loc$max$d_xy_flow		#averaging distance (length of air d_xy_flow flown through [m])
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
      ConfLvl=conf_level,
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