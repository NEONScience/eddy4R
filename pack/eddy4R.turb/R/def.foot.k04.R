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
#   Chris Florian (2021-11-24)
#     update remaining terms to eddy4R naming convention
#   Chris Florian (2021-02-07)
#     update to call base functions from namespace

#' @description Flux footprint after Kljun et a. (2004), Metzger et al. (2012).

#' @param
#' @param \code{angZaxsErth} wind direction to rotate the inertial footprint matrix [Deg]
#' @param \code{distReso} cell size of result grid [m]
#' @param \code{veloXaxsYaxs} horizontal wind speed [m/s]
#' @param \code{veloYaxsHorSd} crosswind fluctuations [m/s]
#' @param \code{veloFric} friction velocity [m/s]
#' @param \code{distZaxsMeasDisp} height of measurement - displacement [m]
#' @param \code{distObkv} Obukhov Length [m]
#' @param \code{distZaxsAbl}	boundary layer height
#' @param \code{thsh} threshold for cumulative footprint extent
#' @param \code{univFunc} integral over the stability-dependent universal function to make the log-wind-profile applicable to different atmospheric stratifications; from def.func.univ()

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
  univFunc=0
){
  
  
  #-----------------------------------------------------------
  #PREPARATION OF INPUT PARAMETERS
  
  #constant parameters from pages 507 and 516
  para01 <- 0.8 #alpha1
  para02 <- 4.28	#+-0.13; Ac
  para03 <- 1.68	#+-0.11; Ad
  para04 <- 0.18	#+-0.01; Af
  para05 <- 2.59	#+-0.17; Ax
  para06 <- 3.42	#+-0.35; Bup
  
  #calculation of fitted parameters Eqs. (13) - (16)
  paraFit01 <- para04 / (para06 - base::log(distZaxsRgh))	#maximum value of the distribution; a
  paraFit02 <- 3.70	#+-0.30; b
  paraFit03 <- para02 * (para06 - base::log(distZaxsRgh)) #c
  paraFit04 <- para03 * (para06 - base::log(distZaxsRgh))	#maximum upwind extend (non-dimensional); d
  
  
  #-----------------------------------------------------------
  #SIZE ESTIMATION OF FOOTPRINT MATRIX
  
  #scaling from non-dimensional to dimensional framework, Eqs. (17) - (18)
  scal <- distZaxsMeasDisp * (veloZaxsHorSd / veloFric)^(-para01)
  
  #alongwind (crosswind integrated) density distribution
  
  #position of maximum along x
  #non-dimensional, Eq. (11)
  distFootXaxsMaxNorm <- para05 * (para06 - base::log(distZaxsRgh))
  #dimensional, Eq. (17)
  distFootXaxsMax <- distFootXaxsMaxNorm * scal
  
  #maximum value, Eqs. (12) - (13)
  wghtFootYaxsMax <- paraFit01
  
  #crosswind integrated flux footprint, Eq. (7)
  def.wght.foot.xaxs <- function(x) {
    #non-dimensional alongwind distance, Eq. (5)
    distFootXaxsNorm01 <- ((veloZaxsHorSd / veloFric)^para01) * x / distZaxsMeasDisp
    #seperated term 
    distFootXaxsNorm02 <- (distFootXaxsNorm01 + paraFit04) / paraFit03
    #crosswind integrated flux footprint, Eq. (7)
    wghtFootXaxs <- paraFit01 * (distFootXaxsNorm02^paraFit02) * base::exp(paraFit02 * (1 - distFootXaxsNorm02))
    #return result
    return(wghtFootXaxs)
  }
  
  #alongwind footprint extend
  
  #in lee (negative) direction, Eq. (10)
  numCellXaxsEnd <- base::ceiling((-(paraFit04 * scal + distReso/2)) / distReso)
  
  #in luv (postitiv) direction until contribution falls below 1 % of wghtFootYaxsMax
  distFootXaxs <- distFootXaxsMax 
  wghtFootXaxsDistMax <- wghtFootYaxsMax	#start from distribution peak
  while(wghtFootXaxsDistMax > wghtFootYaxsMax / 100) {
    distFootXaxs <- distFootXaxs + distReso	#use step width of landuse matrix
    wghtFootXaxsDistMax <- def.wght.foot.xaxs(whri)	#calculate
  }
  #clean up distFootXaxs
  base::rm(distFootXaxs)
  
  numCellXaxsBgn <- base::ceiling(whri / distReso)	#cell length necessay in X direction
  
  #crosswind density distribution
  
  #crosswind distribution of footprint (Heidbach, 2010)
  def.wght.foot.yaxs <- function(
    distFootXaxs=0,
    distFootYaxs=0,
    veloYaxsHorSd=veloYaxsHorSd,
    veloZaxsHorSd=veloZaxsHorSd,
    veloFric=veloFric,
    distZaxsMeasDisp=distZaxsMeasDisp,
    distZaxsRgh=distZaxsRgh,
    distZaxsAbl=distZaxsAbl,
    univFunc=univFunc
  ) {
    #describing the friction within the air layer / column [s]
    timeFric <- 0.08 * distZaxsAbl^2 / (distZaxsAbl - distZaxsMeasDisp) / veloFric
    #column average transport velocity [s]
    veloLog <- veloFric / 0.4 * (log(distZaxsMeasDisp / distZaxsRgh) - (distZaxsMeasDisp - distZaxsRgh) / distZaxsMeasDisp - univFunc)
    #average travel time of a particle
    timeZaxs <- base::sqrt((distFootXaxs / veloLog)^2 + ((distZaxsMeasDisp - distZaxsRgh) / veloZaxsHorSd)^2)
    #scaled crosswind fluctuations
    veloYaxsHorSdScal <- timeZaxs / (1 + base::sqrt(timeZaxs / (2 * timeFric))) * timeZaxs / timeFric * veloYaxsHorSd
    #crosswind distribution
    wghtFootYaxs <- (1 / (base::sqrt(2 * pi) * veloYaxsHorSdScal)) * base::exp((-distFootYaxs^2) / (2 * (veloYaxsHorSdScal^2)))
    #return result
    return(wghtFootYaxs)
  }
  
  #maximum of crosswind density distribution
  ymax <- 0
  wghtFootYaxsMax <- def.wght.foot.yaxs(
    distFootXaxs=numCellXaxsBgn * distReso,
    distFootYaxs=ymax,
    veloYaxsHorSd=veloYaxsHorSd,
    veloZaxsHorSd=veloZaxsHorSd,
    veloFric=veloFric,
    distZaxsMeasDisp=distZaxsMeasDisp,
    distZaxsRgh=distZaxsRgh,
    distZaxsAbl=distZaxsAbl,
    univFunc=univFunc
  )
  
  #crosswind footprint extend until contribution falls below 1 % wghtFootYaxsMax
  whri <- ymax; wghtFootXaxsDistMax <- wghtFootYaxsMax	#start from distribution peak
  while(wghtFootXaxsDistMax > wghtFootYaxsMax / 100) {
    whri <- whri + distReso	#use step width of landuse matrix
    wghtFootXaxsDistMax <- def.wght.foot.yaxs(
      distFootXaxs=numCellXaxsBgn * distReso,
      distFootYaxs=whri,
      veloYaxsHorSd=veloYaxsHorSd,
      veloZaxsHorSd=veloZaxsHorSd,
      veloFric=veloFric,
      distZaxsMeasDisp=distZaxsMeasDisp,
      distZaxsRgh=distZaxsRgh,
      distZaxsAbl=distZaxsAbl,
      univFunc=univFunc
    )
  }
  numCellYaxs <- base::ceiling(whri / distReso)	#cell length necessay in Y direction
  
  
  #-----------------------------------------------------------
  #CELL ALLOCATION AND INTEGRATION
  
  #place aircraft in cell center around zero
  
  #alongwind integration boundaries with aicraft centered in 0
  if(numCellXaxsEnd < 0) {
    distXaxs <- c((numCellXaxsEnd:(-1) + 0.5), 1:numCellXaxsBgn - 0.5) * distReso
  } else {
    distXaxs <- c(0, 1:numCellXaxsBgn - 0.5)* distReso
  }
  
  #crosswind integration boundaries with aicraft centered in 0
  distYaxs <- c(0, 1:numCellYaxs - 0.5) * distReso
  
  #alongwind cell center coordinates
  distXaxsCntr <- base::sapply(1:(length(distXaxs)-1), function(x) mean(distXaxs[x:(x+1)]))
  
  #crosswind cell center coordinates
  distYaxsCntr <- c(0, base::sapply(2:(length(distYaxs)-1), function(y) mean(distYaxs[y:(y+1)])))
  
  #integration of alongwind footprint
  
  #function to integrate over, Eq. (A10) - (A11)
  def.wght.foot.xaxs.itgr.xaxs.norm <- function(idxCellXaxs) idxCellXaxs^paraFit02 * exp(-idxCellXaxs)
  
  #auxilary dimensionless distance
  distXaxsNorm <- (distXaxs / scal + paraFit04) / paraFit03
  
  #integrate
  wghtFootXaxsItgrNorm <- base::sapply(1:(base::length(distXaxsNorm)-1), function(numCellXaxs) stats::integrate(def.wght.foot.xaxs.itgr.xaxs.norm, paraFit02*distXaxsNorm[numCellXaxs], paraFit02*distXaxsNorm[numCellXaxs+1])$value)
  
  #cellwise alongwind footprint, Eq. (A10)
  wghtFootXaxsItgr <- paraFit01 * paraFit03 * base::exp(paraFit02) * paraFit02^(-paraFit02) / paraFit02 *wghtFootXaxsItgrNorm
  
  #integral over the entire footprint
   wghtFootXaxsAll <- paraFit01 * paraFit03 * base::exp(paraFit02) * paraFit02^(-paraFit02) * base::gamma(paraFit02)
  
  #percentage of alongwind footprint covered
   qiFootXaxsFrac <- base::sum(wghtFootXaxsItgr) /  wghtFootXaxsAll * 100
  
  #normalisation to unity
  wghtFootXaxsItgr <- wghtFootXaxsItgr / base::sum(wghtFootXaxsItgr)
  
  #integration of crosswind footprint
  
  #function for crosswind dispersion
  def.wght.foot.yaxs.itgr.yaxs <- function(y, veloYaxsHorSdScal) {
    wghtFootYaxs <- (1 / (sqrt(2 * pi) * veloYaxsHorSdScal)) * base::exp((-y^2) / (2 * (veloYaxsHorSdScal^2)))
    return(wghtFootYaxs)
  }
  
  #alongwind distance dependence of crosswind dispersion
  def.wght.foot.yaxs.itgr.xaxs.yaxs <- function(
    distFootXaxs=0,
    distFootYaxs=0,
    veloYaxsHorSd=veloYaxsHorSd,
    veloZaxsHorSd=veloZaxsHorSd,
    veloFric=veloFric,
    distZaxsMeasDisp=distZaxsMeasDisp,
    distZaxsRgh=distZaxsRgh,
    distZaxsAbl=distZaxsAbl,
    univFunc=univFunc
  ) {
    #describing the friction within the air layer / column [s]
    timeFric <- 0.08 * distZaxsAbl^2 / (distZaxsAbl - distZaxsMeasDisp) / veloFric
    #column average transport velocity [s]
    veloLog <- veloFric / 0.4 * (log(distZaxsMeasDisp / distZaxsRgh) - (distZaxsMeasDisp - distZaxsRgh) / distZaxsMeasDisp - univFunc)
    #average travel time of a particle
    timeZaxs <- sqrt((distFootXaxs / veloLog)^2 + ((distZaxsMeasDisp - distZaxsRgh) / veloZaxsHorSd)^2)
    #scaled crosswind fluctuations
    veloYaxsHorSdScal <- timeZaxs / (1 + base::sqrt(timeZaxs / (2 * timeFric))) * timeZaxs / timeFric * veloYaxsHorSd
    #call function for crosswind dispersion (integration slightly increases density towards the outside)
    #wghtFootYaxsItgr <- def.wght.foot.yaxs.itgr.yaxs(distYaxsCntr, veloYaxsHorSdScal)
    wghtFootYaxsItgr <- base::sapply(1:(length(distFootYaxs)-1), function(numCellYaxs) stats::integrate(def.wght.foot.yaxs.itgr.yaxs, distFootYaxs[numCellYaxs], distFootYaxs[numCellYaxs+1], veloYaxsHorSdScal)$value)
    #normalisation to 0.5
    wghtFootYaxsItgr <- wghtFootYaxsItgr / (2 * base::sum(wghtFootYaxsItgr))
    #return result
    return(wghtFootYaxsItgr)
  }
  
  #integration, output: top -> bottom == upwind -> downwind, left -> right == alongwind axis -> outside
  wghtFootYaxsItgr <- t(base::sapply(1:base::length(distXaxsCntr), function(numCellXaxs) def.wght.foot.yaxs.itgr.xaxs.yaxs(
    x=distXaxsCntr[numCellXaxs],
    y=distYaxs,
    veloYaxsHorSd=veloYaxsHorSd,
    veloZaxsHorSd=veloZaxsHorSd,
    veloFric=veloFric,
    distZaxsMeasDisp=distZaxsMeasDisp,
    distZaxsRgh=distZaxsRgh,
    distZaxsAbl=distZaxsAbl,
    univFunc=univFunc
  )))
  
  
  #-----------------------------------------------------------
  #COMBINE ALONG- AND CROSSWIND DENSITY DISTRIBUTIONS AND ROTATE INTO MEAN WIND
  
  #combine crosswind contributions on alongwind axis; will always yield uneven column number; rows sum to unity
  wghtFootYaxsItgr <- base::cbind(matlab::fliplr(wghtFootYaxsItgr[,2:base::ncol(wghtFootYaxsItgr)]), 2*wghtFootYaxsItgr[,1], wghtFootYaxsItgr[,2:base::ncol(wghtFootYaxsItgr)])
  #YcenLR <- c(-rev(distYaxsCntr[2:length(distYaxsCntr)]), 0, distYaxsCntr[2:length(distYaxsCntr)])
  #sapply(1:nrow(wghtFootYaxsItgr), function(x) sum(wghtFootYaxsItgr[x,]))
  #str(wghtFootYaxsItgr)
  
  #combination of along- and cross component; along wind from up to down, crosswind from left to right; sums to unity
  wghtFootXaxsYaxsItgr <- t(sapply(1:base::nrow(wghtFootYaxsItgr), function(x) wghtFootXaxsItgr[x] * wghtFootYaxsItgr[x,]))
  #plot(wghtFootXaxsItgr)
  #plot(wghtFootYaxsItgr[5,])
  #NIVo <- c(1e-4, 1e-3, 1e-2)
  #contour(matlab::rot90(wghtFootXaxsYaxsItgr,3), levels=c(1e-4, 1e-3, 1e-2))
  
  #center aicraft alongwind location in plot, pad with zeroes
  numCellXaxsDiff <- base::length(base::which(distXaxsCntr > 0)) - base::length(base::which(distXaxsCntr < 0))
  wghtFootXaxsYaxsItgrCntr<- base::rbind(matrix(nrow=numCellXaxsDiff, ncol=base::ncol(wghtFootXaxsYaxsItgr), 0), wghtFootXaxsYaxsItgr)
  #XcenUD <- seq(-max(distXaxsCntr), max(distXaxsCntr), by=distReso)
  #contour(YcenLR, XcenUD, matlab::rot90(wghtFootXaxsYaxsItgrCntr,1), levels=NIVo, col=colorRampPalette(c("black", "red"))(length(NIVo)), asp=1)
  
  #pad with zeroes if not rectangular matrix
  if(base::nrow(wghtFootXaxsYaxsItgrCntr) == base::ncol(wghtFootXaxsYaxsItgrCntr)) {
    wghtFootXaxsYaxsItgrCntrSq  <- wghtFootXaxsYaxsItgrCntr
  } else {
    if(base::nrow(wghtFootXaxsYaxsItgrCntr) > base::ncol(wghtFootXaxsYaxsItgrCntr)) {
      wghtFootXaxsYaxsItgrCntrSq  <- base::cbind(
        base::matrix(nrow=base::nrow(wghtFootXaxsYaxsItgrCntr), ncol=(base::nrow(wghtFootXaxsYaxsItgrCntr)-base::ncol(wghtFootXaxsYaxsItgrCntr)) / 2, 0),
        wghtFootXaxsYaxsItgrCntr,
        base::matrix(nrow=base::nrow(wghtFootXaxsYaxsItgrCntr), ncol=(base::nrow(wghtFootXaxsYaxsItgrCntr)-base::ncol(wghtFootXaxsYaxsItgrCntr)) / 2, 0)
      )
    }
    if(base::nrow(wghtFootXaxsYaxsItgrCntr) < base::ncol(wghtFootXaxsYaxsItgrCntr)) {
      wghtFootXaxsYaxsItgrCntrSq  <- base::rbind(
        base::matrix(ncol=ncol(wghtFootXaxsYaxsItgrCntr), nrow=(base::ncol(wghtFootXaxsYaxsItgrCntr)-base::nrow(wghtFootXaxsYaxsItgrCntr)) / 2, 0),
        wghtFootXaxsYaxsItgrCntr,
        base::matrix(ncol=ncol(wghtFootXaxsYaxsItgrCntr), nrow=(base::ncol(wghtFootXaxsYaxsItgrCntr)-base::nrow(wghtFootXaxsYaxsItgrCntr)) / 2, 0)
      )
    }
  }
  #YcenLRp <- XcenUD
  #contour(YcenLRp, XcenUD, matlab::rot90(wghtFootXaxsYaxsItgrCntrSq ,1), levels=NIVo, col=colorRampPalette(c("black", "red"))(length(NIVo)), asp=1)
  
  # upwind distance of 80% cumulative flux footprint
  wghtFootXaxsYaxsItgrCntrSqRot <- matlab::flipud(wghtFootXaxsYaxsItgrCntrSq )
  #wghtFootXaxsYaxsItgrCntrSqRot <- EBImage::rotate(wghtFootXaxsYaxsItgrCntrSq , 180-0)@.Data
  distFootXaxsThshFootCum <- base::rev(rowSums(wghtFootXaxsYaxsItgrCntrSqRot))
  
  # upwind distance of peak
  distFootXaxsMax <- (base::which(distFootXaxsThshFootCum == base::max(distFootXaxsThshFootCum)) - (length(distFootXaxsThshFootCum) - 1) / 2 + 1) *   distReso
  base::names(distFootXaxsMax) <- ("distFootXaxsMax")
  
  distFootXaxsThshFootCum <- base::cumsum(distFootXaxsThshFootCum)
  distFootXaxsThshFootCum <- distFootXaxsThshFootCum/base::max(distFootXaxsThshFootCum, na.rm=TRUE)
  distFootXaxsThshFootCum <- (base::which(distFootXaxsThshFootCum > thsh)[1] - (base::length(distFootXaxsThshFootCum) - 1) / 2 + 1) *   distReso
  base::names(distFootXaxsThshFootCum) <- ("distFootXaxsThshFootCum")
  
  # one-sided cross-wind distance of 80% cumulative flux footprint
  distFootYaxsThshFootCum <- base::rev(base::colSums(wghtFootXaxsYaxsItgrCntrSqRot))
  distFootYaxsThshFootCum <- distFootYaxsThshFootCum[((base::length(distFootYaxsThshFootCum) - 1) / 2 + 1):base::length(distFootYaxsThshFootCum)]
  distFootYaxsThshFootCum <- base::cumsum(distFootYaxsThshFootCum)
  distFootYaxsThshFootCum <- distFootYaxsThshFootCum/base::max(distFootYaxsThshFootCum, na.rm=TRUE)
  distFootYaxsThshFootCum <- base::which(distFootYaxsThshFootCum > thsh)[1] * distReso
  base::names(distFootYaxsThshFootCum) <- ("distFootYaxsThshFootCum")
  
  # rotate image clockwise (align footprint in mean wind)
  # specify output.dim, so that dimensions remain odd-numbered, and the tower at the center
  # explicitly specifying output.origin for some reason leads to a shift, hence commented out
  wghtFootXaxsYaxsItgrCntrSqRot <- EBImage::rotate(x = wghtFootXaxsYaxsItgrCntrSq ,
                            angZaxsErth = 180 - angZaxsErth,
                            output.dim = base::rep(nrow(wghtFootXaxsYaxsItgrCntrSq ),2),
                            # output.origin = base::rep(((nrow(wghtFootXaxsYaxsItgrCntrSq ) - 1) / 2 + 1), 2)
  )@.Data
  wghtFootXaxsYaxsItgrCntrSqRot <- wghtFootXaxsYaxsItgrCntrSqRot / base::sum(base::sum(wghtFootXaxsYaxsItgrCntrSqRot))
  
  # contour(
  #   x = 1:nrow(wghtFootXaxsYaxsItgrCntrSq ),
  #   y = 1:nrow(wghtFootXaxsYaxsItgrCntrSq ),
  #   z = wghtFootXaxsYaxsItgrCntrSqRot)
  # points(x = ((nrow(wghtFootXaxsYaxsItgrCntrSq ) - 1) / 2 + 1), y = ((nrow(wghtFootXaxsYaxsItgrCntrSq ) - 1) / 2 + 1), col=2)
  # nrow(wghtFootXaxsYaxsItgrCntrSq )
  
  #     #attempted workaround for cell shifts of rotate() function in Windows
  #     #for some reason rotate shifts 1 cell back and 1 cell right in Windows
  #       if(.Platform$OS.type == "windows") {
  #         #fix before rotation?
  #           #PHIcp1 <- cbind(rep(0, nrow(wghtFootXaxsYaxsItgrCntrSq )), wghtFootXaxsYaxsItgrCntrSq [,1:(ncol(wghtFootXaxsYaxsItgrCntrSq )-1)])
  #           #PHIcp1 <- rbind(rep(0, ncol(PHIcp1)), PHIcp1[1:(nrow(PHIcp1)-1),])
  #         wghtFootXaxsYaxsItgrCntrSqRot <- EBImage::rotate(PHIcp1, 180-angZaxsErth)@.Data
  #         #fix after rotation?
  #           #wghtFootXaxsYaxsItgrCntrSqRot <- cbind(wghtFootXaxsYaxsItgrCntrSqRot[,2:ncol(wghtFootXaxsYaxsItgrCntrSqRot)], rep(0, nrow(wghtFootXaxsYaxsItgrCntrSqRot)))
  #           #wghtFootXaxsYaxsItgrCntrSqRot <- rbind(wghtFootXaxsYaxsItgrCntrSqRot[2:nrow(wghtFootXaxsYaxsItgrCntrSqRot),], rep(0, ncol(wghtFootXaxsYaxsItgrCntrSqRot)))
  #   
  #           image(wghtFootXaxsYaxsItgrCntrSqRot)
  #           points(x=0.5, y=0.5)
  #         
  #         #none of the above work consistently
  #         # -> for now calculate footprint matrices in Linux only;
  #         # -> on the longer run, find different package to perform rotation also in Windows;
  #       }
  
  #YcenLRpr <- ((1:ncol(wghtFootXaxsYaxsItgrCntrSqRot)) - ceiling(ncol(wghtFootXaxsYaxsItgrCntrSqRot) / 2)) * distReso  #new X
  #XcenUDpr <- ((1:nrow(wghtFootXaxsYaxsItgrCntrSqRot)) - ceiling(nrow(wghtFootXaxsYaxsItgrCntrSqRot) / 2)) * distReso	#new Y
  #contour(YcenLRpr, XcenUDpr, matlab::rot90(wghtFootXaxsYaxsItgrCntrSqRot,3), levels=NIVo, col=colorRampPalette(c("black", "red"))(length(NIVo)), asp=1)
  #points(c(0,0), pch=3)
  
  
  
  #-----------------------------------------------------------
  #RETURN RESULTS
  
  #create result list
  rpt<-list(
    wghtFootXaxsYaxsItgr=wghtFootXaxsYaxsItgrCntrSqRot, 
     qiFootXaxsFrac= qiFootXaxsFrac,
    distFootXaxsThshFootCum=distFootXaxsThshFootCum,
    distFootXaxsMax=distFootXaxsMax,
    distFootYaxsThshFootCum=distFootYaxsThshFootCum
  )
  
  #return list
  return(rpt)
  
}