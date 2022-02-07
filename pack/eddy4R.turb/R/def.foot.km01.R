##############################################################################################
#' @title Definition function: Flux footprint after Kormann & Meixner (2001)

#' @author
#' Stefan Metzger \email{eddy4R.info@gmail.com} \cr
#' Chris Florian

# changelog and author contributions / copyrights
#   Stefan Metzger (2014-09-22)
#     original creation
#   Stefan Metzger (2015-11-28)
#     re-formualtion as function() to allow packaging
#   Chris Florian (2021-11-09)
#     modularization of function from footprints.r and initial eddy4R naming convention updates

#' @description Function for calculating one flux footprint matrix at a time for the crosswind integrated footprint Kormann & Meixner, 2001 (KM01). For the crosswind distribution, a Gaussian function is used(Pasquill, 1974; Kormann and Meixner, 2001). The matrix extent is precalculated, and cut off where the footprint function reaches 1 % of its peak (Neftel, 2008).

#' @param \code{angZaxsErth} wind direction to rotate the inertial footprint matrix [Deg]
#' @param \code{distReso} cell size of result grid [m]
#' @param \code{veloXaxsYaxs} horizontal wind speed [m/s]
#' @param \code{veloYaxsHorSd} crosswind fluctuations [m/s]
#' @param \code{veloFric} friction velocity [m/s]
#' @param \code{distZaxsMeasDisp} height of measurement - displacement [m]
#' @param \code{distObkv} Obukhov Length [m]
#' @param \code{cutoff} cellwise contribution relative to peak at which footprint will be cut off

#' @return Footprint weight matrix

#' @references
#' #'Kormann, R., and F. X. Meixner, 2001: An analytical footprint model for non-neutral stratification. Boundary-Layer Meteorol., 99, 207-224, doi:10.1023/A:1018991015119. \cr
#'Pasquill, F., Atmospheric diffusion; the dispersion of windborne material from industrial and other sources. 2d ed. Chichester [Eng.] E. Horwood; New York, Halsted Press [1974], https://search.library.wisc.edu/catalog/999623699702121. \cr
#'Neftel, A., C. Spirig, and C. Ammann, 2008: Application and test of a simple tool for operational footprint evaluations. Environ. Pollut., 152, 644-652, doi:10.1016/j.envpol.2007.06.062. \cr

#' @keywords footprint, source area

#' @examples Currently none

#' @seealso Currently none

#' @export
##############################################################################################



def.foot.km01 <- function(
  angZaxsErth,	#wind direction [B0] to rotate the inertial footprint matrix; used for both models
  distReso,	#cell size [m] of result grid; used for both models
  veloXaxsYaxs,		#horizontal wind speed [m/s]; used in KM01, and to determine z0 for K04
  veloYaxsHorSd,		#crosswind fluctuations [m/s]; used for both models to calculate the crosswind dispersion
  veloFric,	#friction velocity [m/s]; used a) for both models to characterize the shear stress, and b) to determine z0
  distZaxsMeasDisp,	#height of measurement - displacement [m]
  distObkv,	#
  cutoff=0.01	#cellwise contribution relative to peak at which footprint will be cut off
){
  
  
  #-----------------------------------------------------------
  #PREPARATION OF INPUT PARAMETERS
  
  #calculation of local stability [-]
  StblObkv <- distZaxsMeasDisp / distObkv
  if(StblObkv > 0.1) StblObkv <- 0.1
  
  #FuncVertVeloExp02 exponents of the wind velocity power law
  if (StblObkv >= 0) {
    FuncVertVeloExp02 <- 1 + 5*StblObkv
  } else {
    FuncVertVeloExp02 <- (1-16*StblObkv)^(-0.25)
  }
  FuncVertVeloExp02 <- veloFric * FuncVertVeloExp02 / 0.4 / veloXaxsYaxs
  
  #FuncVertTurbExp01 exponent of eddy diffusivity power law
  if (StblObkv >= 0) {
    FuncVertTurbExp01 <- 1 /(1+5*StblObkv)
  } else {
    FuncVertTurbExp01 <- (1-24*StblObkv)/(1-16*StblObkv)
  }
  #FuncVertCoef shape factor
  FuncVertCoef <- 2 + FuncVertVeloExp02 - FuncVertTurbExp01  
  
  #FuncVertVeloCnst constant in power law profile in the wind velocity
  FuncVertVeloCnst <- veloXaxsYaxs / (distZaxsMeasDisp^FuncVertVeloExp02)
  
  #K vertical profile of the eddy diffusivity
  if (StblObkv >= 0) {
    phi_c <- 1 + 5*StblObkv
  } else {
    phi_c <- (1-16*StblObkv)^(-0.5)
  } 
  K <- 0.4*veloFric * distZaxsMeasDisp /phi_c
  
  #FuncVertTurbCnst constant of the power law profile of the eddy diffusivity
  FuncVertTurbCnst <- K / distZaxsMeasDisp^FuncVertTurbExp01
  
  
  #-----------------------------------------------------------
  #SIZE ESTIMATION OF FOOTPRINT MATRIX, SMALLEST CONTRIBUTION cutoff % OF PEAK CONTRIBUTION (NEFTEL, 2008)
  
  #alongwind (crosswind integrated) density distribution
  
  #position of maximum along x, 290.5334 FuncVertVeloExp02
  Xmax <- function(mue, xi) xi / (1+mue)
  xmax <- Xmax(mue=(1+FuncVertVeloExp02)/FuncVertCoef, xi=(FuncVertVeloCnst*distZaxsMeasDisp^FuncVertCoef)/((FuncVertCoef^2)*FuncVertTurbCnst))
  
  #maximum value, 0.001369324
  Fmax <- function(mue, xi) ( exp(-1-mue) * (1+mue)^(1+mue) ) / (gamma(mue) * xi)
  fmax <- Fmax(mue=(1+FuncVertVeloExp02)/FuncVertCoef, xi=(FuncVertVeloCnst*distZaxsMeasDisp^FuncVertCoef)/((FuncVertCoef^2)*FuncVertTurbCnst))
  
  #crosswind integrated flux footprint
  FFPalong <- function(x, mue, xi) {
    f <- (1 / gamma(mue)) * ((xi^mue)/x^(1+mue)) * exp(-xi/x)
    return(f)
  }
  
  #length until contribution falls below 1% of fmax
  whri <- xmax; whro <- fmax	#start from distribution peak
  while(whro > fmax * cutoff) {
    whri <- whri + distReso	#use step width of landuse matrix
    whro <- FFPalong(whri, mue=(1+FuncVertVeloExp02)/FuncVertCoef, xi=(FuncVertVeloCnst*distZaxsMeasDisp^FuncVertCoef)/((FuncVertCoef^2)*FuncVertTurbCnst))	#calculate
  }
  whrx <- ceiling(whri / distReso)	#cell length necessay in X direction
  
  
  #crosswind density distribution
  
  #crosswind distribution of footprint
  FFPcross <- function(x, y, sigmav, u) {
    sigma <- sigmav * x / u
    Dy <- (1 / (sqrt(2 * pi) * sigma)) * exp((-y^2) / (2 * (sigma^2)))
    return(Dy)
  }
  
  #maximum of crosswind density distribution
  ymax <- 0
  fmax <- FFPcross(whrx * distReso, ymax, sigmav=veloYaxsHorSd, u=veloXaxsYaxs)
  
  #length until contribution falls below 1 % fmax
  whri <- ymax; whro <- fmax	#start from distribution peak
  while(whro > fmax * cutoff) {
    whri <- whri + distReso	#use step width of landuse matrix
    whro <- FFPcross(whrx * distReso, whri, sigmav=veloYaxsHorSd, u=veloXaxsYaxs)	#calculate
  }
  whry <- ceiling(whri / distReso)	#cell length necessary in Y direction
  
  
  #-----------------------------------------------------------
  #CELL ALLOCATION AND INTEGRATION
  
  #place aircraft in center cell of first row
  XRng <- c(0, 1:whrx - 0.5)* distReso	#alongwind integration boundaries with aicraft centered in 0
  YRng <- c(0, 1:whry - 0.5)* distReso	#crosswind integration boundaries with aicraft centered in 0
  Xcen <- sapply(1:(length(XRng)-1), function(x) mean(XRng[x:(x+1)]))	#alongwind cell center coordinates
  #  Ycen <- sapply(1:(length(YRng)-1), function(y) mean(XRng[y:(y+1)]))	#crosswind cell center coordinates
  
  
  #integration of alongwind footprint
  PHIalong <- sapply(1:(length(XRng)-1), function(xwhr) integrate(FFPalong, XRng[xwhr], XRng[xwhr+1], mue=(1+FuncVertVeloExp02)/FuncVertCoef, xi=(FuncVertVeloCnst*distZaxsMeasDisp^FuncVertCoef)/((FuncVertCoef^2)*FuncVertTurbCnst))$value)
  sumPHIalong <- sum(PHIalong)
  PHIalong <- PHIalong / sumPHIalong	#normalization to 1
  
  
  #integration of crosswind footprint
  
  #function for crosswind dispersion
  FFPcrossY <- function(y, sigma) {
    Dy <- (1 / (sqrt(2 * pi) * sigma)) * exp((-y^2) / (2 * (sigma^2)))
    return(Dy)
  }
  
  #alongwind distance dependence of crosswind dispersion
  FFPcrossXY <- function(x, y, sigmav, u) {
    sigma <- sigmav * x / u
    PHIcross <- sapply(1:(length(y)-1), function(ywhr) integrate(FFPcrossY, y[ywhr], y[ywhr+1], sigma)$value)
    PHIcross <- PHIcross / (2 * sum(PHIcross))	#normalisation to 0.5
    return(PHIcross)
  }
  
  #integration
  PHIcross <- t(sapply(1:length(Xcen), function(xwhr) FFPcrossXY(Xcen[xwhr], YRng, sigmav=veloYaxsHorSd, u=veloXaxsYaxs)))
  
  
  #-----------------------------------------------------------
  #COMBINE ALONG- AND CROSSWIND DENSITY DISTRIBUTIONS AND ROTATE INTO MEAN WIND
  
  #combine crosswind contributions on alongwind axis; will always yield uneven column number
  PHIcross <- cbind(matlab::fliplr(PHIcross[,2:ncol(PHIcross)]), 2*PHIcross[,1], PHIcross[,2:ncol(PHIcross)])
  #YcenLR <- c(-rev(Ycen[2:length(Ycen)]), 0, Ycen[2:length(Ycen)])
  #sapply(1:nrow(PHIcross), function(x) sum(PHIcross[x,]))
  #str(PHIcross)
  
  #combination of along- and cross component; along wind from up to down, crosswind from left to right
  PHI <- t(sapply(1:nrow(PHIcross), function(x) PHIalong[x] * PHIcross[x,]))
  #plot(PHIalong)
  #plot(PHIcross[5,])
  #contour(matlab::rot90(PHI,3), levels=c(0.0001, 0.001, 0.01))
  
  #center aicraft alongwind location in plot, pad with zeroes
  PHIc<-rbind(matrix(nrow=(nrow(PHI)-1), ncol=ncol(PHI), 0), PHI)
  #XcenUD <- c(-rev(Xcen[2:length(Xcen)]), 0, Xcen[2:length(Xcen)])
  #contour(YcenLR, XcenUD, matlab::rot90(PHIc,1), levels=NIVo, labels=NIVi, col=colorRampPalette(c("black", "red"))(length(NIVo)), asp=1)
  
  #pad with zeroes if not rectangular matrix
  if(nrow(PHIc) == ncol(PHIc)) {
    PHIcp <- PHIc
  } else {
    if(nrow(PHIc) > ncol(PHIc)) {
      PHIcp <- cbind(
        matrix(nrow=nrow(PHIc), ncol=(nrow(PHIc)-ncol(PHIc)) / 2, 0),
        PHIc,
        matrix(nrow=nrow(PHIc), ncol=(nrow(PHIc)-ncol(PHIc)) / 2, 0)
      )
    }
    if(nrow(PHIc) < ncol(PHIc)) {
      PHIcp <- rbind(
        matrix(ncol=ncol(PHIc), nrow=(ncol(PHIc)-nrow(PHIc)) / 2, 0),
        PHIc,
        matrix(ncol=ncol(PHIc), nrow=(ncol(PHIc)-nrow(PHIc)) / 2, 0)
      )
    }
  }
  #YcenLRp <- XcenUD
  #contour(YcenLRp, XcenUD, matlab::rot90(PHIcp,1), levels=NIVo, labels=NIVi, col=colorRampPalette(c("black", "red"))(length(NIVo)), asp=1)
  
  #rotate image clockwise (align footprint in mean wind)
  PHIcpr <- EBImage::rotate(PHIcp, 180-angle)@.Data
  #YcenLRpr <- ((1:ncol(PHIcpr)) - ceiling(ncol(PHIcpr) / 2)) * distReso	#new X
  #XcenUDpr <- ((1:nrow(PHIcpr)) - ceiling(nrow(PHIcpr) / 2)) * distReso	#new Y
  #contour(YcenLRpr, XcenUDpr, matlab::rot90(PHIcpr,3), levels=NIVo, labels=NIVi, col=colorRampPalette(c("black", "red"))(length(NIVo)), asp=1)
  
  
  #-----------------------------------------------------------
  #RETURN RESULTS
  
  #create result list
  export<-list(PHIcpr, sumPHIalong)
  names(export) <- c("PHI", "sum")
  
  #return list
  return(export)
  
}