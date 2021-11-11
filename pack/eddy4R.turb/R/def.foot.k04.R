##############################################################################################
#' @title Definition function: Flux footprint after Kljun et a. (2004), Metzger et al. (2012)

#' @author 
#' #' Stefan Metzger \email{eddy4R.info@gmail.com} \cr
#' Chris Florian

# changelog and author contributions / copyrights
#   Stefan Metzger (2014-09-22)
#     original creation
#   Stefan Metzger (2015-11-28)
#     re-formualtion as function() to allow packaging
#   Chris Florian (2021-11-10)
#     modularization of function from footprints.r and initial eddy4R naming convention updates

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


def.foot.k04 <- function(
  angZaxsErth,
  distReso,
  veloYaxsHorSd,
  veloZaxsHorSd,
  veloFric,
  distZaxsMeasDisp,
  distZaxsRgh,
  distZaxsAbl=1000,	#boundary layer height
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
  a <- Af / (Bup - log(distZaxsRgh))	#maximum value of the distribution
  b <- 3.70	#+-0.30
  c <- Ac * (Bup - log(distZaxsRgh))
  d <- Ad * (Bup - log(distZaxsRgh))	#maximum upwind extend (non-dimensional)
  
  
  #-----------------------------------------------------------
  #SIZE ESTIMATION OF FOOTPRINT MATRIX
  
  #scaling from non-dimensional to dimensional framework, Eqs. (17) - (18)
  scal <- distZaxsMeasDisp * (veloZaxsHorSd / veloFric)^(-alpha1)
  
  #alongwind (crosswind integrated) density distribution
  
  #position of maximum along x
  #non-dimensional, Eq. (11)
  Xmax <- Ax * (Bup - log(distZaxsRgh))
  #dimensional, Eq. (17)
  xmax <- Xmax * scal
  
  #maximum value, Eqs. (12) - (13)
  fmax <- a
  
  #crosswind integrated flux footprint, Eq. (7)
  FFPalong <- function(x) {
    #non-dimensional alongwind distance, Eq. (5)
    Xstar <- ((veloZaxsHorSd / veloFric)^alpha1) * x / distZaxsMeasDisp
    #seperated term 
    Lprime <- (Xstar + d) / c
    #crosswind integrated flux footprint, Eq. (7)
    Fstar <- a * (Lprime^b) * exp(b * (1 - Lprime))
    #return result
    return(Fstar)
  }
  
  #alongwind footprint extend
  
  #in lee (negative) direction, Eq. (10)
  whrxn <- ceiling((-(d * scal + distReso/2)) / distReso)
  
  #in luv (postitiv) direction until contribution falls below 1 % of fmax
  whri <- xmax; whro <- fmax	#start from distribution peak
  while(whro > fmax / 100) {
    whri <- whri + distReso	#use step width of landuse matrix
    whro <- FFPalong(whri)	#calculate
  }
  whrxp <- ceiling(whri / distReso)	#cell length necessay in X direction
  
  #crosswind density distribution
  
  #crosswind distribution of footprint (Heidbach, 2010)
  FFPcross <- function(
    x=0,
    y=0,
    veloYaxsHorSd=veloYaxsHorSd,
    veloZaxsHorSd=veloZaxsHorSd,
    veloFric=veloFric,
    distZaxsMeasDisp=distZaxsMeasDisp,
    distZaxsRgh=distZaxsRgh,
    distZaxsAbl=distZaxsAbl,
    Psi=Psi
  ) {
    #describing the friction within the air layer / column [s]
    Tly <- 0.08 * distZaxsAbl^2 / (distZaxsAbl - distZaxsMeasDisp) / veloFric
    #column average transport velocity [s]
    Ulog <- veloFric / 0.4 * (log(distZaxsMeasDisp / distZaxsRgh) - (distZaxsMeasDisp - distZaxsRgh) / distZaxsMeasDisp - Psi)
    #average travel time of a particle
    tau <- sqrt((x / Ulog)^2 + ((distZaxsMeasDisp - distZaxsRgh) / veloZaxsHorSd)^2)
    #scaled crosswind fluctuations
    sigma <- tau / (1 + sqrt(tau / (2 * Tly))) * tau / Tly * veloYaxsHorSd
    #crosswind distribution
    Dy <- (1 / (sqrt(2 * pi) * sigma)) * exp((-y^2) / (2 * (sigma^2)))
    #return result
    return(Dy)
  }
  
  #maximum of crosswind density distribution
  ymax <- 0
  fmax <- FFPcross(
    x=whrxp * distReso,
    y=ymax,
    veloYaxsHorSd=veloYaxsHorSd,
    veloZaxsHorSd=veloZaxsHorSd,
    veloFric=veloFric,
    distZaxsMeasDisp=distZaxsMeasDisp,
    distZaxsRgh=distZaxsRgh,
    distZaxsAbl=distZaxsAbl,
    Psi=Psi
  )
  
  #crosswind footprint extend until contribution falls below 1 % fmax
  whri <- ymax; whro <- fmax	#start from distribution peak
  while(whro > fmax / 100) {
    whri <- whri + distReso	#use step width of landuse matrix
    whro <- FFPcross(
      x=whrxp * distReso,
      y=whri,
      veloYaxsHorSd=veloYaxsHorSd,
      veloZaxsHorSd=veloZaxsHorSd,
      veloFric=veloFric,
      distZaxsMeasDisp=distZaxsMeasDisp,
      distZaxsRgh=distZaxsRgh,
      distZaxsAbl=distZaxsAbl,
      Psi=Psi
    )
  }
  whry <- ceiling(whri / distReso)	#cell length necessay in Y direction
  
  
  #-----------------------------------------------------------
  #CELL ALLOCATION AND INTEGRATION
  
  #place aircraft in cell center around zero
  
  #alongwind integration boundaries with aicraft centered in 0
  if(whrxn < 0) {
    XRng <- c((whrxn:(-1) + 0.5), 1:whrxp - 0.5) * distReso
  } else {
    XRng <- c(0, 1:whrxp - 0.5)* distReso
  }
  
  #crosswind integration boundaries with aicraft centered in 0
  YRng <- c(0, 1:whry - 0.5) * distReso
  
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
    veloYaxsHorSd=veloYaxsHorSd,
    veloZaxsHorSd=veloZaxsHorSd,
    veloFric=veloFric,
    distZaxsMeasDisp=distZaxsMeasDisp,
    distZaxsRgh=distZaxsRgh,
    distZaxsAbl=distZaxsAbl,
    Psi=Psi
  ) {
    #describing the friction within the air layer / column [s]
    Tly <- 0.08 * distZaxsAbl^2 / (distZaxsAbl - distZaxsMeasDisp) / veloFric
    #column average transport velocity [s]
    Ulog <- veloFric / 0.4 * (log(distZaxsMeasDisp / distZaxsRgh) - (distZaxsMeasDisp - distZaxsRgh) / distZaxsMeasDisp - Psi)
    #average travel time of a particle
    tau <- sqrt((x / Ulog)^2 + ((distZaxsMeasDisp - distZaxsRgh) / veloZaxsHorSd)^2)
    #scaled crosswind fluctuations
    sigma <- tau / (1 + sqrt(tau / (2 * Tly))) * tau / Tly * veloYaxsHorSd
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
    veloYaxsHorSd=veloYaxsHorSd,
    veloZaxsHorSd=veloZaxsHorSd,
    veloFric=veloFric,
    distZaxsMeasDisp=distZaxsMeasDisp,
    distZaxsRgh=distZaxsRgh,
    distZaxsAbl=distZaxsAbl,
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
  #XcenUD <- seq(-max(Xcen), max(Xcen), by=distReso)
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
  fx <- (which(f80 == max(f80)) - (length(f80) - 1) / 2 + 1) *   distReso
  names(fx) <- ("fx")
  
  f80 <- cumsum(f80)
  f80 <- f80/max(f80, na.rm=TRUE)
  f80 <- (which(f80 > thsh)[1] - (length(f80) - 1) / 2 + 1) *   distReso
  names(f80) <- ("f80")
  
  # one-sided cross-wind distance of 80% cumulative flux footprint
  fy <- rev(colSums(PHIcpr))
  fy <- fy[((length(fy) - 1) / 2 + 1):length(fy)]
  fy <- cumsum(fy)
  fy <- fy/max(fy, na.rm=TRUE)
  fy <- which(fy > thsh)[1] * distReso
  names(fy) <- ("fy")
  
  # rotate image clockwise (align footprint in mean wind)
  # specify output.dim, so that dimensions remain odd-numbered, and the tower at the center
  # explicitly specifying output.origin for some reason leads to a shift, hence commented out
  PHIcpr <- EBImage::rotate(x = PHIcp,
                            angZaxsErth = 180 - angZaxsErth,
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
  #         PHIcpr <- EBImage::rotate(PHIcp1, 180-angZaxsErth)@.Data
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
  
  #YcenLRpr <- ((1:ncol(PHIcpr)) - ceiling(ncol(PHIcpr) / 2)) * distReso  #new X
  #XcenUDpr <- ((1:nrow(PHIcpr)) - ceiling(nrow(PHIcpr) / 2)) * distReso	#new Y
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