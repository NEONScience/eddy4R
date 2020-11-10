##############################################################################################
#' @title Definition function: Flux footprint after Kormann & Meixner (2001)

# type (one of function defintion, function wrapper, workflow, demo): function defintion

# license: Terms of use of the NEON FIU algorithm repository dated 2015-01-16

#' @author Stefan Metzger \email{eddy4R.info@gmail.com}

# changelog and author contributions / copyrights
#   Stefan Metzger (2014-09-22)
#     original creation
#   Stefan Metzger (2015-11-28)
#     re-formualtion as function() to allow packaging

#' @description Function for calclulating one flux footprint matrix at a time for the crosswind integrated footprint Kormann & Meixner, 2001 (KM01). For the crosswind distribution, a Gaussian function is used(Pasquill, 1974; Kormann and Meixner, 2001). The matrix extent is precalculated, and cut off where the footprint function reaches 1 % of its peak (Neftel, 2008).

#' @param \code{angle} wind direction to rotate the inertial footprint matrix [Deg]
#' @param \code{Csize} cell size of result grid [m]
#' @param \code{hor} horizontal wind speed [m/s]
#' @param \code{sigV} crosswind fluctuations [m/s]
#' @param \code{ustar} friction velocity [m/s]
#' @param \code{zmeas} height of measurement - displacement [m]
#' @param \code{Lvirt} Obukhov Length [m]
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



#WHAT FOR
#calculates one footprint weight matrix at a time from analytical models
#for the crosswind integrated footprint Kormann & Meixner, 2001 (KM01) or Kljun, 2004 (K04) model can be chosen
#for the crosswind distribution the same Gaussian function is used in both cases (Pasquill, 1974; Kormann and Meixner, 2001)
#the matrix extend is precalculated, and cut off where the footprint function reaches 1 % of its peak (Neftel, 2008)
#matrix extends are uneven, with the aircraft / tower at its center
  #time consumption ~1/3 s per footprint for KM01
  #K04 is 3-4 times faster

#INPUT VARIABLES
  #angle	#wind direction [B0] to rotate the inertial footprint matrix; used for both models
  #Csize	#cell size [m] of result grid; used for both models
  #hor		#horizontal wind speed [m/s]; used in KM01, and to determine z0 for K04
  #sigV		#crosswind fluctuations [m/s]; used for both models to calculate the crosswind dispersion
  #sigW		#vertical wind fluctuations [m/s]; used for K04 to characterize the vertical transport
  #ustar	#friction velocity [m/s]; used a) for both models to characterize the shear stress, and b) to determine z0 for K04
  #Lvirt	#Obukhov length from local friction velocity and buoyancy flux [m]; used a) for KM01 to characterize the vertical transport, and b) for the universal functions to determine z0 for K04
  #zmeas	#height of measurement - displacement [m]



############################################################
#KM01 MODEL
############################################################

footKM01 <- function(
  angle,	#wind direction [B0] to rotate the inertial footprint matrix; used for both models
  Csize,	#cell size [m] of result grid; used for both models
  hor,		#horizontal wind speed [m/s]; used in KM01, and to determine z0 for K04
  sigV,		#crosswind fluctuations [m/s]; used for both models to calculate the crosswind dispersion
  ustar,	#friction velocity [m/s]; used a) for both models to characterize the shear stress, and b) to determine z0
  zmeas,	#height of measurement - displacement [m]
  Lvirt,	#
  cutoff=0.01	#cellwise contribution relative to peak at which footprint will be cut off
){


#-----------------------------------------------------------
#PREPARATION OF INPUT PARAMETERS

  #calculation of local stability zeta [-]
    zeta <- zmeas / Lvirt
	if(zeta > 0.1) zeta <- 0.1
	
  #m exponents of the wind velocity power law
    if (zeta >= 0) {
      phi_m <- 1 + 5*zeta
    } else {
      phi_m <- (1-16*zeta)^(-0.25)
    }
    m <- ustar * phi_m / 0.4 / hor

  #n exponent of eddy diffusivity power law
    if (zeta >= 0) {
      n <- 1 /(1+5*zeta)
    } else {
      n <- (1-24*zeta)/(1-16*zeta)
    }
  #r shape factor
    r <- 2 + m - n  

  #U constant in power law profile in the wind velocity
    U <- hor / (zmeas^m)

  #K vertical profile of the eddy diffusivity
    if (zeta >= 0) {
      phi_c <- 1 + 5*zeta
    } else {
      phi_c <- (1-16*zeta)^(-0.5)
    } 
    K <- 0.4*ustar * zmeas /phi_c

  #kapa constant of the power law profile of the eddy diffusivity
    kapa <- K / zmeas^n


#-----------------------------------------------------------
#SIZE ESTIMATION OF FOOTPRINT MATRIX, SMALLEST CONTRIBUTION cutoff % OF PEAK CONTRIBUTION (NEFTEL, 2008)

  #alongwind (crosswind integrated) density distribution

    #position of maximum along x, 290.5334 m
      Xmax <- function(mue, xi) xi / (1+mue)
      xmax <- Xmax(mue=(1+m)/r, xi=(U*zmeas^r)/((r^2)*kapa))

    #maximum value, 0.001369324
      Fmax <- function(mue, xi) ( exp(-1-mue) * (1+mue)^(1+mue) ) / (gamma(mue) * xi)
      fmax <- Fmax(mue=(1+m)/r, xi=(U*zmeas^r)/((r^2)*kapa))

    #crosswind integrated flux footprint
      FFPalong <- function(x, mue, xi) {
	f <- (1 / gamma(mue)) * ((xi^mue)/x^(1+mue)) * exp(-xi/x)
	return(f)
      }

    #length until contribution falls below 1% of fmax
      whri <- xmax; whro <- fmax	#start from distribution peak
      while(whro > fmax * cutoff) {
	whri <- whri + Csize	#use step width of landuse matrix
	whro <- FFPalong(whri, mue=(1+m)/r, xi=(U*zmeas^r)/((r^2)*kapa))	#calculate
      }
      whrx <- ceiling(whri / Csize)	#cell length necessay in X direction


  #crosswind density distribution

    #crosswind distribution of footprint
      FFPcross <- function(x, y, sigmav, u) {
	sigma <- sigmav * x / u
	Dy <- (1 / (sqrt(2 * pi) * sigma)) * exp((-y^2) / (2 * (sigma^2)))
	return(Dy)
      }

    #maximum of crosswind density distribution
      ymax <- 0
      fmax <- FFPcross(whrx * Csize, ymax, sigmav=sigV, u=hor)

    #length until contribution falls below 1 % fmax
      whri <- ymax; whro <- fmax	#start from distribution peak
      while(whro > fmax * cutoff) {
	whri <- whri + Csize	#use step width of landuse matrix
	whro <- FFPcross(whrx * Csize, whri, sigmav=sigV, u=hor)	#calculate
      }
      whry <- ceiling(whri / Csize)	#cell length necessay in Y direction


#-----------------------------------------------------------
#CELL ALLOCATION AND INTEGRATION

  #place aircraft in center cell of first row
    XRng <- c(0, 1:whrx - 0.5)* Csize	#alongwind integration boundaries with aicraft centered in 0
    YRng <- c(0, 1:whry - 0.5)* Csize	#crosswind integration boundaries with aicraft centered in 0
    Xcen <- sapply(1:(length(XRng)-1), function(x) mean(XRng[x:(x+1)]))	#alongwind cell center coordinates
  #  Ycen <- sapply(1:(length(YRng)-1), function(y) mean(XRng[y:(y+1)]))	#crosswind cell center coordinates


  #integration of alongwind footprint
    PHIalong <- sapply(1:(length(XRng)-1), function(xwhr) integrate(FFPalong, XRng[xwhr], XRng[xwhr+1], mue=(1+m)/r, xi=(U*zmeas^r)/((r^2)*kapa))$value)
    sumPHIalong <- sum(PHIalong)
    PHIalong <- PHIalong / sumPHIalong	#normalisation to 1


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
      PHIcross <- t(sapply(1:length(Xcen), function(xwhr) FFPcrossXY(Xcen[xwhr], YRng, sigmav=sigV, u=hor)))


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
    #YcenLRpr <- ((1:ncol(PHIcpr)) - ceiling(ncol(PHIcpr) / 2)) * Csize	#new X
    #XcenUDpr <- ((1:nrow(PHIcpr)) - ceiling(nrow(PHIcpr) / 2)) * Csize	#new Y
    #contour(YcenLRpr, XcenUDpr, matlab::rot90(PHIcpr,3), levels=NIVo, labels=NIVi, col=colorRampPalette(c("black", "red"))(length(NIVo)), asp=1)


#-----------------------------------------------------------
#RETURN RESULTS

  #create result list
    export<-list(PHIcpr, sumPHIalong)
    names(export) <- c("PHI", "sum")

  #return list
    return(export)

}



##############################################################################################
#' @title Definition function: Flux footprint after Kljun et a. (2004), Metzger et al. (2012)

# type (one of function defintion, function wrapper, workflow, demo): function defintion

# license: Terms of use of the NEON FIU algorithm repository dated 2015-01-16

#' @author Stefan Metzger \email{eddy4R.info@gmail.com}

# changelog and author contributions / copyrights
#   Stefan Metzger (2014-09-22)
#     original creation
#   Stefan Metzger (2015-11-28)
#     re-formualtion as function() to allow packaging

#' @description Flux footprint after Kljun et a. (2004), Metzger et al. (2012).

#' @param Currently none

#' @return Footprint weight matrix

#' @references Currently none

#' @keywords footprint, source area

#' @examples Currently none

#' @seealso Currently none

#' @export
##############################################################################################
############################################################
#K04 MODEL
############################################################


###---------------------------------------------------------
##WITH NEUTRAL WIND PROFILE FOR CROSSWIND DISPERSION


footK04 <- function(
  angle,
  Csize,
  sigV,
  sigW,
  ustar,
  zmeas,
  z0,
  h=1000,	#boundary layer height
  thsh = 0.8, # threshold for cumulative footprint extent
  Psi=0
){


#-----------------------------------------------------------
#PREPARATION OF INPUT PARAMETERS

  #constant parameters from pages 507 and 516
    alpha1 <- 0.8
    Ac <- 4.28	#+-0.13
    Ad <- 1.68	#+-0.11
    Af <- 0.18	#+-0.01
    Ax <- 2.59	#+-0.17
    Bup <- 3.42	#+-0.35

  #calculation of fitted parameters Eqs. (13) - (16)
    a <- Af / (Bup - log(z0))	#maximum value of the distribution
    b <- 3.70	#+-0.30
    c <- Ac * (Bup - log(z0))
    d <- Ad * (Bup - log(z0))	#maximum upwind extend (non-dimensional)


#-----------------------------------------------------------
#SIZE ESTIMATION OF FOOTPRINT MATRIX

  #scaling from non-dimensional to dimensional framework, Eqs. (17) - (18)
    scal <- zmeas * (sigW / ustar)^(-alpha1)

  #alongwind (crosswind integrated) density distribution

    #position of maximum along x
      #non-dimensional, Eq. (11)
	Xmax <- Ax * (Bup - log(z0))
      #dimensional, Eq. (17)
	xmax <- Xmax * scal

    #maximum value, Eqs. (12) - (13)
      fmax <- a

    #crosswind integrated flux footprint, Eq. (7)
      FFPalong <- function(x) {
	#non-dimensional alongwind distance, Eq. (5)
	  Xstar <- ((sigW / ustar)^alpha1) * x / zmeas
	#seperated term 
	  Lprime <- (Xstar + d) / c
	#crosswind integrated flux footprint, Eq. (7)
	  Fstar <- a * (Lprime^b) * exp(b * (1 - Lprime))
	#return result
	  return(Fstar)
      }

    #alongwind footprint extend

      #in lee (negative) direction, Eq. (10)
	whrxn <- ceiling((-(d * scal + Csize/2)) / Csize)

      #in luv (postitiv) direction until contribution falls below 1 % of fmax
	whri <- xmax; whro <- fmax	#start from distribution peak
	while(whro > fmax / 100) {
	  whri <- whri + Csize	#use step width of landuse matrix
	  whro <- FFPalong(whri)	#calculate
	}
	whrxp <- ceiling(whri / Csize)	#cell length necessay in X direction

  #crosswind density distribution

    #crosswind distribution of footprint (Heidbach, 2010)
      FFPcross <- function(
	x=0,
	y=0,
	sigV=sigV,
	sigW=sigW,
	ustar=ustar,
	zmeas=zmeas,
	z0=z0,
	h=h,
	Psi=Psi
      ) {
	#describing the friction within the air layer / column [s]
	  Tly <- 0.08 * h^2 / (h - zmeas) / ustar
	#column average transport velocity [s]
	  Ulog <- ustar / 0.4 * (log(zmeas / z0) - (zmeas - z0) / zmeas - Psi)
	#average travel time of a particle
	  tau <- sqrt((x / Ulog)^2 + ((zmeas - z0) / sigW)^2)
	#scaled crosswind fluctuations
	  sigma <- tau / (1 + sqrt(tau / (2 * Tly))) * tau / Tly * sigV
	#crosswind distribution
	  Dy <- (1 / (sqrt(2 * pi) * sigma)) * exp((-y^2) / (2 * (sigma^2)))
	#return result
	  return(Dy)
      }

    #maximum of crosswind density distribution
      ymax <- 0
      fmax <- FFPcross(
	x=whrxp * Csize,
	y=ymax,
	sigV=sigV,
	sigW=sigW,
	ustar=ustar,
	zmeas=zmeas,
	z0=z0,
	h=h,
	Psi=Psi
      )

    #crosswind footprint extend until contribution falls below 1 % fmax
      whri <- ymax; whro <- fmax	#start from distribution peak
      while(whro > fmax / 100) {
	whri <- whri + Csize	#use step width of landuse matrix
	whro <- FFPcross(
	  x=whrxp * Csize,
	  y=whri,
	  sigV=sigV,
	  sigW=sigW,
	  ustar=ustar,
	  zmeas=zmeas,
	  z0=z0,
	  h=h,
	  Psi=Psi
	)
      }
      whry <- ceiling(whri / Csize)	#cell length necessay in Y direction


#-----------------------------------------------------------
#CELL ALLOCATION AND INTEGRATION

  #place aircraft in cell center around zero

    #alongwind integration boundaries with aicraft centered in 0
      if(whrxn < 0) {
	XRng <- c((whrxn:(-1) + 0.5), 1:whrxp - 0.5) * Csize
      } else {
	XRng <- c(0, 1:whrxp - 0.5)* Csize
      }

    #crosswind integration boundaries with aicraft centered in 0
      YRng <- c(0, 1:whry - 0.5) * Csize

    #alongwind cell center coordinates
      Xcen <- sapply(1:(length(XRng)-1), function(x) mean(XRng[x:(x+1)]))

    #crosswind cell center coordinates
      Ycen <- c(0, sapply(2:(length(YRng)-1), function(y) mean(YRng[y:(y+1)])))

  #integration of alongwind footprint

    #function to integrate over, Eq. (A10) - (A11)
      gam <- function(t) t^b * exp(-t)

    #auxilary dimensionless distance
      Lhat <- (XRng / scal + d) / c

    #integrate
      Gam <- sapply(1:(length(Lhat)-1), function(xwhr) integrate(gam, b*Lhat[xwhr], b*Lhat[xwhr+1])$value)

    #cellwise alongwind footprint, Eq. (A10)
      PHIalong <- a * c * exp(b) * b^(-b) / b *Gam

    #integral over the entire footprint
      INTall <- a * c * exp(b) * b^(-b) * gamma(b)

    #percentage of alongwind footprint covered
      cover <- sum(PHIalong) / INTall * 100

    #normalisation to unity
      PHIalong <- PHIalong / sum(PHIalong)

  #integration of crosswind footprint

    #function for crosswind dispersion
      FFPcrossY <- function(y, sigma) {
	Dy <- (1 / (sqrt(2 * pi) * sigma)) * exp((-y^2) / (2 * (sigma^2)))
	return(Dy)
      }

    #alongwind distance dependence of crosswind dispersion
      FFPcrossXY <- function(
	x=0,
	y=0,
	sigV=sigV,
	sigW=sigW,
	ustar=ustar,
	zmeas=zmeas,
	z0=z0,
	h=h,
	Psi=Psi
      ) {
	#describing the friction within the air layer / column [s]
	  Tly <- 0.08 * h^2 / (h - zmeas) / ustar
	#column average transport velocity [s]
	  Ulog <- ustar / 0.4 * (log(zmeas / z0) - (zmeas - z0) / zmeas - Psi)
	#average travel time of a particle
	  tau <- sqrt((x / Ulog)^2 + ((zmeas - z0) / sigW)^2)
	#scaled crosswind fluctuations
	  sigma <- tau / (1 + sqrt(tau / (2 * Tly))) * tau / Tly * sigV
	#call function for crosswind dispersion (integration slightly increases density towards the outside)
	  #PHIcross <- FFPcrossY(Ycen, sigma)
	  PHIcross <- sapply(1:(length(y)-1), function(ywhr) integrate(FFPcrossY, y[ywhr], y[ywhr+1], sigma)$value)
	#normalisation to 0.5
	  PHIcross <- PHIcross / (2 * sum(PHIcross))
	#return result
	  return(PHIcross)
      }

    #integration, output: top -> bottom == upwind -> downwind, left -> right == alongwind axis -> outside
      PHIcross <- t(sapply(1:length(Xcen), function(xwhr) FFPcrossXY(
	x=Xcen[xwhr],
	y=YRng,
	sigV=sigV,
	sigW=sigW,
	ustar=ustar,
	zmeas=zmeas,
	z0=z0,
	h=h,
	Psi=Psi
      )))


#-----------------------------------------------------------
#COMBINE ALONG- AND CROSSWIND DENSITY DISTRIBUTIONS AND ROTATE INTO MEAN WIND

  #combine crosswind contributions on alongwind axis; will always yield uneven column number; rows sum to unity
    PHIcross <- cbind(matlab::fliplr(PHIcross[,2:ncol(PHIcross)]), 2*PHIcross[,1], PHIcross[,2:ncol(PHIcross)])
    #YcenLR <- c(-rev(Ycen[2:length(Ycen)]), 0, Ycen[2:length(Ycen)])
    #sapply(1:nrow(PHIcross), function(x) sum(PHIcross[x,]))
    #str(PHIcross)

  #combination of along- and cross component; along wind from up to down, crosswind from left to right; sums to unity
    PHI <- t(sapply(1:nrow(PHIcross), function(x) PHIalong[x] * PHIcross[x,]))
    #plot(PHIalong)
    #plot(PHIcross[5,])
    #NIVo <- c(1e-4, 1e-3, 1e-2)
    #contour(matlab::rot90(PHI,3), levels=c(1e-4, 1e-3, 1e-2))

  #center aicraft alongwind location in plot, pad with zeroes
    pads <- length(which(Xcen > 0)) - length(which(Xcen < 0))
    PHIc<- rbind(matrix(nrow=pads, ncol=ncol(PHI), 0), PHI)
    #XcenUD <- seq(-max(Xcen), max(Xcen), by=Csize)
    #contour(YcenLR, XcenUD, matlab::rot90(PHIc,1), levels=NIVo, col=colorRampPalette(c("black", "red"))(length(NIVo)), asp=1)

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
    #contour(YcenLRp, XcenUD, matlab::rot90(PHIcp,1), levels=NIVo, col=colorRampPalette(c("black", "red"))(length(NIVo)), asp=1)

  # upwind distance of 80% cumulative flux footprint
    PHIcpr <- matlab::flipud(PHIcp)
    #PHIcpr <- EBImage::rotate(PHIcp, 180-0)@.Data
    f80 <- rev(rowSums(PHIcpr))

      # upwind distance of peak
      fx <- (which(f80 == max(f80)) - (length(f80) - 1) / 2 + 1) *   Csize
      names(fx) <- ("fx")

    f80 <- cumsum(f80)
    f80 <- f80/max(f80, na.rm=TRUE)
    f80 <- (which(f80 > thsh)[1] - (length(f80) - 1) / 2 + 1) *   Csize
    names(f80) <- ("f80")

  # one-sided cross-wind distance of 80% cumulative flux footprint
    fy <- rev(colSums(PHIcpr))
    fy <- fy[((length(fy) - 1) / 2 + 1):length(fy)]
    fy <- cumsum(fy)
    fy <- fy/max(fy, na.rm=TRUE)
    fy <- which(fy > thsh)[1] * Csize
    names(fy) <- ("fy")
    
  # rotate image clockwise (align footprint in mean wind)
  # specify output.dim, so that dimensions remain odd-numbered, and the tower at the center
  # explicitly specifying output.origin for some reason leads to a shift, hence commented out
    PHIcpr <- EBImage::rotate(x = PHIcp,
                              angle = 180 - angle,
                              output.dim = base::rep(nrow(PHIcp),2),
                              # output.origin = base::rep(((nrow(PHIcp) - 1) / 2 + 1), 2)
                              )@.Data
    PHIcpr <- PHIcpr / sum(sum(PHIcpr))

    # contour(
    #   x = 1:nrow(PHIcp),
    #   y = 1:nrow(PHIcp),
    #   z = PHIcpr)
    # points(x = ((nrow(PHIcp) - 1) / 2 + 1), y = ((nrow(PHIcp) - 1) / 2 + 1), col=2)
    # nrow(PHIcp)
    
#     #attempted workaround for cell shifts of rotate() function in Windows
#     #for some reason rotate shifts 1 cell back and 1 cell right in Windows
#       if(.Platform$OS.type == "windows") {
#         #fix before rotation?
#           #PHIcp1 <- cbind(rep(0, nrow(PHIcp)), PHIcp[,1:(ncol(PHIcp)-1)])
#           #PHIcp1 <- rbind(rep(0, ncol(PHIcp1)), PHIcp1[1:(nrow(PHIcp1)-1),])
#         PHIcpr <- EBImage::rotate(PHIcp1, 180-angle)@.Data
#         #fix after rotation?
#           #PHIcpr <- cbind(PHIcpr[,2:ncol(PHIcpr)], rep(0, nrow(PHIcpr)))
#           #PHIcpr <- rbind(PHIcpr[2:nrow(PHIcpr),], rep(0, ncol(PHIcpr)))
#   
#           image(PHIcpr)
#           points(x=0.5, y=0.5)
#         
#         #none of the above work consistently
#         # -> for now calculate footprint matrices in Linux only;
#         # -> on the longer run, find different package to perform rotation also in Windows;
#       }

    #YcenLRpr <- ((1:ncol(PHIcpr)) - ceiling(ncol(PHIcpr) / 2)) * Csize  #new X
    #XcenUDpr <- ((1:nrow(PHIcpr)) - ceiling(nrow(PHIcpr) / 2)) * Csize	#new Y
    #contour(YcenLRpr, XcenUDpr, matlab::rot90(PHIcpr,3), levels=NIVo, col=colorRampPalette(c("black", "red"))(length(NIVo)), asp=1)
    #points(c(0,0), pch=3)



#-----------------------------------------------------------
#RETURN RESULTS

  #create result list
    export<-list(
      PHI=PHIcpr, 
      cover=cover,
      f80=f80,
      fx=fx,
      fy=fy
    )

  #return list
    return(export)

}
