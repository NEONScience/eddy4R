##############################################################################################
#' @title Definition function: Plot ogives
#' 
#' @author Stefan Metzger \email{eddy4R.info@gmail.com}
#' @author David Durden 

# changelog and author contributions / copyrights
#   Stefan Metzger (2014-09-22)
#     original creation
#   Stefan Metzger (2015-11-28)
#     re-formualtion as function() to allow packaging
#   David Durden (2022-02-12)
#     terms update

#' @description Plot ogives. Note: Accumulation only valid with raw data or equidistant binned data!

#' @param FilePlot  output location and filename for plot (no default)
#' @param idepRaw one independent variable for the raw cospectra, e.g. frequencey, wavenumber...(no default) 
#' @param cospRaw raw cospectra of up to four dependent variables, same length as idepRaw (no default)
#' @param idepSmth one independent variable for the smoothed cospectra, e.g. frequencey, wavenumber (no default) 
#' @param cospSmth smoothed cospectra of up to four dependent variables, same length as idepRaw (no default) 
#' @param ThshFluxLvl Flux contribution level to evaluate e.g. corresponding wavelength (no default)
#' @param PrdFluxLvl flux averaging period to evaluate corresponding contribution level (no default) 
#' @param DscrPlot one description (character or expression) for each variable for the title (no default) 
#' @param Labx description for abscissa (NA) 
#' @param Laby description for ordinate (NA) 
#' @param Colr colors for plotting of measurement, model and evaluation range (c(1,2,1))
#' @param Limx limits for abscissa (NULL) 
#' @param Limy limits for ordinate (NULL) 
#' @param MethWght weight the cospectra for independent variable (wavelength / frequency)? (FALSE) 
#' @param MethDistWave is wavelength used as independent variable? (TRUE) 
#' @param CoefNorm normalizes max(cospectrum) to a given quantity, e.g. total flux per variable, else unity (Default = 1) 

#' @return Outputs Ogive plots to \code{FilePlot} and returns a vector or data.frame containing \code{DistWaveFluxLvl} wavelength corresponding to given contribution level and \code{FluxLvlPrd} determine flux contribution level for given wavelength for each cospectra.

#' @references 
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

#' @keywords Fast Fourier Transform, FFT, spectral, Ogive

#' @examples Currently none

#' @seealso Currently none

#' @export
##############################################################################################

########################################################
#PLOT OGIVES
##Accumulation only valid with raw data or equidistant binned data!
########################################################
def.spec.plot.ogiv <- function(
  FilePlot, #FilePlot: (no default) output location and filename for plot
  idepRaw, #idepRaw: (no default) one independent variable for the raw cospectra, e.g. frequencey, wavenumber...
  cospRaw, #cospRaw: (no default) raw cospectra of up to four dependent variables, same length as idepRaw
  idepSmth, #idepSmth: (no default) one independent variable for the smoothed cospectra, e.g. frequencey, wavenumber...
  cospSmth, #cospSmth: (no default) smoothed cospectra of up to four dependent variables, same length as idepRaw
  ThshFluxLvl, #ThshFluxLvl: (no default) flux contribution level to evaluate e.g. corresponding wavelength
  PrdFluxLvl, #PrdFluxLvl: (no default) flux averaging period to evaluate corresponding contribution level
  DscrPlot = NULL, #DscrPlot: (no default) one description (character or expression) for each variable for the title
  Labx = NULL, #Labx: (NA) description for abscissa
  Laby = NULL, #Laby: (NA) description for ordinate
  Colr = c("grey",1,1), #Colr: (c(1,2,1)) colors for plotting of measurement, model and evaluation range
  Limx = NULL, #Limx: (NULL) limits for abscissa
  Limy = NULL, #Limy: (NULL) limits for ordinate
  MethWght = F, #MethWght (F) weight the cospectra for independent variable (wavelength / frequency)?
  MethDistWave = TRUE, #MethDistWave: (T) is wavelength used as independent variable?
  CoefNorm = 1 #CoefNorm: (1) normalizes max(cospectrum) to a given quantity, e.g. total flux per variable, else unity
  ) {
  
  #prepare local variables
  idepRaw <- idepRaw
  cospRaw <- base::as.matrix(cospRaw)
  idepSmth <- idepSmth
  cospSmth <- base::as.matrix(cospSmth)
  
  #sort for increasing wavelenght (a) or decreasing frequency (b) -> always from short- to longer wavelength contributions
  if(MethDistWave == TRUE) {
    cospRaw <- base::as.matrix(cospRaw[base::sort.list(idepRaw, decreasing = FALSE),])	#first dependent variable
    idepRaw <- idepRaw[base::sort.list(idepRaw, decreasing = FALSE)]			#then independent variable
    cospSmth <- base::as.matrix(cospSmth[base::sort.list(idepSmth, decreasing = FALSE),])	#first dependent variable
    idepSmth <- idepSmth[base::sort.list(idepSmth, decreasing = FALSE)]			#then independent variable
  } else {
    cospRaw <- base::as.matrix(cospRaw[sort.list(idepRaw, decreasing = TRUE),])
    idepRaw <- idepRaw[base::sort.list(idepRaw, decreasing = TRUE)]
    cospSmth <- base::as.matrix(cospSmth[base::sort.list(idepSmth, decreasing = TRUE),])
    idepSmth <- idepSmth[base::sort.list(idepSmth, decreasing = TRUE)]
  }
  
  #weighting of the cospectrum: divide for wavelength, multiply for frequencies
  #only valid for cospectrum, not for Ogive!
  if(MethWght == TRUE) {
    if(MethDistWave==TRUE) {
      #      cospRaw <- as.matrix(cospRaw / idepRaw)
      cospSmth <- base::as.matrix(cospSmth / idepSmth)
    } else {
      #      cospRaw <- as.matrix(cospRaw * idepRaw)
      cospSmth <- base::as.matrix(cospSmth * idepSmth)
    }}
  
  #make sure that all fluxes are accumulating towards positive
  CoefCumPstv <- base::sign(base::colSums(cospRaw))	#check the sign(tendency) of the different Cospectra
  cospSmth <- base::t(CoefCumPstv * base::t(cospSmth))
  cospRaw <- base::t(CoefCumPstv * base::t(cospRaw))
  
  #normalize the smoothed cospectrum
  if(!base::is.null(CoefNorm)) {
    if(base::length(CoefNorm) == 1) CoefNorm <- base::rep(CoefNorm, base::ncol(cospRaw))
    cospSmth <- base::sapply(1:base::length(CoefNorm), function(x) base::as.matrix(cospSmth[,x] / base::max(cospSmth[,x]) * CoefNorm[x]))
  }
  
  #call graphics device
  grDevices::png(filename=FilePlot, width = 1000, height = 1000, units = "px", pointsize = 20, bg = "white")
  cexvar=3; graphics::par(mfrow=c(2,2), las=0, cex.axis=cexvar*0.4, cex.lab=cexvar*0.4, font.lab=2, mar=c(4,4,2,2), mgp=c(2.6,0.8,0), family="times", lwd=cexvar, cex.main=cexvar*0.6)
  
  
  ##loop around variables
  for(idxVar in 1:base::ncol(cospRaw)) {
    ##
    
    #Ogive: cumulated flux contribution
    ogiv <- base::cumsum(cospRaw[,idxVar])
    ogiv <- ogiv / base::max(ogiv) * CoefNorm[idxVar]
    
    #determine wavelength for given flux contribution level
    #find first value exceeding given contribution level
    zeroCros <- zeroCross(ogiv - ThshFluxLvl, slope="positive") + 1
    #flag whether zero crossing is singular (s) or multiple (m)
    if(base::length(zeroCros == 1)) flagOgiv <- "single" else flagOgiv <- "multiple"
    #wavelength corresponding to given contribution level
    DistWaveFluxLvl <- idepRaw[(zeroCros[1]-1)]
    
    #determine flux contribution level for given wavelength
    idxPrd <- GenKern::nearest(idepRaw, PrdFluxLvl)
    FluxLvlPrd <- ogiv[idxPrd]
    
    #store ogive results
    ogivTmp <- c(DistWaveFluxLvl, FluxLvlPrd)
    if(idxVar == 1) ogivOut <- ogivTmp else ogivOut <- base::cbind(ogivOut, ogivTmp)
    
    #plotting range
    if(base::is.null(Limx))  Limx <- base::range(idepRaw)
    if(base::is.null(Limy))  Limy <- base::range(c(cospSmth[,idxVar], ogiv))
    
    #plotting
   graphics::plot(cospSmth[,idxVar] ~ idepSmth, col=Colr[1], type="l", xlim=Limx, ylim=Limy, main=DscrPlot[idxVar], xlab=Labx, ylab=Laby, las=1, log="x", xaxt = "n")
   graphics::lines(ogiv ~ idepRaw, col=Colr[2])
   graphics::abline(h=0)
   graphics::abline(h=ThshFluxLvl, lty=2, col=Colr[3])
   graphics::abline(v=DistWaveFluxLvl, lty=2, col=Colr[3])
   graphics::legend(x="topleft", lty=c(1,1,1,1,2), bty="n", col=c(NA,NA,col=Colr,NA,NA,NA,NA), cex=cexvar*0.4, pt.cex=cexvar/2, pt.lwd=cexvar*2/3, xjust = 0, yjust = 0, legend = c(
      "","",
      "Cospectrum",
      "Ogive (Og)",
      base::paste(ThshFluxLvl, " max(Og)", sep=""),
      base::paste("== ", base::round(DistWaveFluxLvl,2)," X", sep="")
    ))
   graphics::legend(x="topright", bty="n", cex=cexvar*0.4, pt.cex=cexvar/2, xjust = 0, yjust = 0, legend = c(
      "","","","",
      base::paste(PrdFluxLvl, " X ==", sep=""),
      base::paste(base::round(FluxLvlPrd,2), " max(Og)", sep="")
    ))
    sfsmisc::eaxis(side=1,labels=NA,las=0)
    graphics::box()
    
    ##end loop around variables
  }
  ##
  
  
  #close graphics device
  grDevices::dev.off()
  
  #export Ogive results
  base::dimnames(ogivOut) <- base::list(c(paste(ThshFluxLvl, "(Og)==", sep=""), base::paste(PrdFluxLvl, " X==", sep="")), base::dimnames(cospRaw)[[2]])
  
  #Return output
  return(ogivOut)
  
  ########################################################
} #End of function
########################################################
