##############################################################################################
#' @title Definition function: Plot cospectra

#' @author Stefan Metzger \email{eddy4R.info@gmail.com}
#' @author David Durden 

# changelog and author contributions / copyrights
#   Stefan Metzger (2014-09-22)
#     original creation
#   Stefan Metzger (2015-11-28)
#     re-formualtion as function() to allow packaging
#   David Durden (2022-02-14)
#     terms update


#' @description Plot cospectra.

#' @param FileOut (no default) output location and filename for plot
#' @param idep (no default) one independent variable, e.g. frequencey, wavenumber...
#' @param depe (no default) spectra of up to three dependent variables, same length as idep
#' @param DscrPlot (no default) one description (character or expression) for each variable for the title
#' @param Labx (NA) description for abscissa
#' @param Laby (NA) description for ordinate
#' @param Colr (c(1,2,1)) colors for plotting of measurement, model and evaluation rang
#' @param Limx (NULL) limits for abscissa
#' @param Limy (NULL) limits for ordinate
#' @param MethWght (T) weight the spectra for independent variable (wavelength / frequency)?
#' @param MethDistWave (F) is wavelength used as independent variable?
#' @param CoefNorm (NULL) normalizes sum(cospectrum) to a given quantity, e.g. unity or the total flux per variable, else NULL
#' @param CoefScalPlot (1) allows to plot measured cospectrum with scaling factor that is different from norm
#' @param MethModlSpec (T) shall model cospetra be calculated and plotted?
#' @param paraStbl (no default) atmospheric stability to choose the reference cospectrum
#' @param FreqPeakCosp (NULL) frequency at which fCO(f) reaches its maximum value; will be determined from measured data if NULL
#' @param FreqRngEval (NULL) range to assess sum((cospectrum - model)[setFreq]) / sum(model) [percentage]; only valid for equidistant data
#' @param MethPlot logical to determine if plot should be output to FileOut

#' @return The Correction Coefficient (\code{CoefCor}) is returned if \code{FreqRngEval} is defined. Additionally, if \code{MethPlot == TRUE} plots will be generated and output to \code{FileOut}.

#' @references 
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007
#' Kaimal, J.C., Wyngaard, J.C., Izumi, Y. and Cote, O.R. (1972), Spectral characteristics of surface-layer turbulence. Q.J.R. Meteorol. Soc., 98: 563-589. https://doi.org/10.1002/qj.49709841707
#' Foken, Thomas. (2017). Micrometeorology. 10.1007/978-3-642-25440-6.
#' Massman W. (2004) Concerning the Measurement of Atmospheric Trace Gas Fluxes with Open- and Closed-Path Eddy Covariance System: The WPL Terms and Spectral Attenuation. In: Lee X., Massman W., Law B. (eds) Handbook of Micrometeorology. Atmospheric and Oceanographic Sciences Library, vol 29. Springer, Dordrecht. https://doi.org/10.1007/1-4020-2265-4_7

#' @keywords Fast Fourier Transform, FFT, spectral, cospectra

#' @examples Currently none

#' @seealso Currently none

#' @export
##############################################################################################

########################################################
#PLOT COSPECTRA
########################################################
def.spec.plot.cosp <- function(
  FileOut, #FileOut: (no default) output location and filename for plot
  idep, #idep: (no default) one independent variable, e.g. frequencey, wavenumber...
  depe, #depe: (no default) spectra of up to three dependent variables, same length as idep
  DscrPlot, #DscrPlot: (no default) one discription (character or expression) for each variable for the title
  Labx=NA, #Labx: (NA) description for abscissa
  Laby=NA, #Laby: (NA) description for ordinate
  Colr=c(1,2,1), #Colr: (c(1,2,1)) colors for plotting of measurement, model and evaluation range
  Limx=NULL, #Limx: (NULL) limits for abscissa
  Limy=NULL, #Limy: (NULL) limits for ordinate
  MethWght=TRUE, #MethWght (T) weight the spectra for independent variable (wavelength / frequency)?
  MethDistWave=FALSE, #MethDistWave: (F) is wavelength used as independent variable?
  CoefNorm=NULL, #CoefNorm: (NULL) normalizes sum(cospectrum) to a given quantity, e.g. unity or the total flux per variable, else NULL
  CoefScalPlot=1, #CoefScalPlot: (1) allows to plot measured cospectrum with scaling factor that is different from norm
  MethModlSpec=TRUE, #MethModlSpec: (T) shall model cospetra be calculated and plotted?
  paraStbl, #paraStbl: (no default) atmospheric stability to choose the reference cospectrum
  FreqPeakCosp=NULL, #FreqPeakCosp: (NULL) frequency at which fCO(f) reaches its maximum value; will be determined from measured data if NULL
  FreqRngEval=NULL, #FreqRngEval: (NULL) range to assess sum((cospectrum - model)[setFreq]) / sum(model) [%]; only valid for equidistant data
  MethPlot=TRUE #MethPlot: logical to determine if plot should be output to FileOut
) {
  
  #prepare variables
  depe <- base::as.matrix(depe)
  
  #weighting of the cospectrum: divide for wavelenght, multiply for frequencies
  if(MethWght == TRUE) {
    if(MethDistWave==TRUE) depe <- base::as.matrix(depe / idep) else depe <- base::as.matrix(depe * idep)
  }
  
  #normalize each cospectrum
  if(!base::is.null(CoefNorm)) {
    if(base::length(CoefNorm) == 1) CoefNorm <- base::rep(CoefNorm, base::ncol(depe))
    sumDepe <- base::sapply(1:ncol(depe), function(x) base::sum(depe[,x], na.rm=TRUE))
    depe <- base::sapply(1:base::length(sumDepe), function(x) base::as.matrix(depe[,x] / sumDepe[x] * CoefNorm[x]))
  }
  
  #call graphics device
  if(MethPlot == TRUE) {
    
    graphics::png(filename=FileOut, width = 1000, height = 1000, units = "px", pointsize = 20, bg = "white")
    cexvar=3; graphics::par(mfrow=c(2,2), las=0, cex.axis=cexvar*0.4, cex.lab=cexvar*0.4, font.main=1, font.lab=1,
                  mar=c(4,4,2,2), mgp=c(2.6,0.8,0), family="times", lwd=cexvar, cex.main=cexvar*0.5)
    
  }
  
  ##loop around variables
  for(idx in 1:base::ncol(depe)) {
    ##
    
    #Model cospectrum after Massman, 2005 (in Lee, 2005); continuous approximation of the Kaimal (1972) cospectra
    if(MethModlSpec == TRUE) {
      #(inertial subrange) slope parameter; 3/4 for -7/3 (cospectra), and 3/2 for -5/3 (spectra) power law
      paraSlp=3/4
      #broadness parameter; 1/2 for unstable, 7/6 for stable stratification 
      if(paraStbl <= 0) paraBrd=1/2  else paraBrd=7/6
      #frequency at which fCO(f) reaches its maximum value
      if(!base::is.null(FreqPeakCosp)) FreqPeakCosp=FreqPeakCosp else FreqPeakCosp=idep[base::which(depe[,idx] == base::max(depe[,idx]))]
      #calculate non-scaled, frequency-weighted model Cospectrum (fCo or nCo)
      modlSpec <- (idep / FreqPeakCosp) / (
        ( 1 + paraSlp * (idep / FreqPeakCosp)^(2 * paraBrd) )^( (1/(2*paraBrd)) * ((paraSlp+1)/paraSlp) )
      )
      if(MethWght == FALSE) modlSpec <- modlSpec / idep
      #individual normalisation parameter for each variable
      CoefNormVar <- base::sum(depe[,idx], na.rm=T) / base::sum(modlSpec, na.rm=T)
      modlSpec <- CoefNormVar * modlSpec
    }
    
    #Assessment of deviation(cospectrum - model) in given frequency range
    #only valid for equidistantly binned data; else multiply with bin width
    if(MethModlSpec == TRUE & !base::is.null(FreqRngEval)) {
      #range to assess    
      setFreq <- base::which(idep >= FreqRngEval[1] & idep <= FreqRngEval[2])
      #weighted or unweighted?
      if(MethWght == TRUE) {
        
        if(MethDistWave==TRUE) {
          CoefCorCalc <- 1 - ( base::sum(((depe[,idx] - modlSpec) * idep)[setFreq], na.rm=T) / base::abs( base::sum(depe[,idx] * idep, na.rm=T) ) )
        } else {
          CoefCorCalc <- 1 - ( base::sum(((depe[,idx] - modlSpec) / idep)[setFreq], na.rm=T) / base::abs( base::sum(depe[,idx] / idep, na.rm=T) ) )
        }
      } else {
        #Critical 
        critThsh <- 1
        idxCrit <- 0
        CoefCorInit <- 1
        
        while(critThsh > 1e-2) {
          idxCrit <- idxCrit + 1
          
          #CoefCorTmp <- 1 - ( sum((depe[,idx] - modlSpec * CoefCorInit)[setFreq], na.rm=T) / sum(depe[,idx], na.rm=T) )
          CoefCorTmp <- 1 - ( base::sum((depe[,idx] / CoefCorInit - modlSpec)[setFreq], na.rm=T) / base::sum(depe[,idx], na.rm=T) )
          
          critThsh <- CoefCorTmp - CoefCorInit
          CoefCorInit <- CoefCorTmp
          
        }
        CoefCorCalc <- CoefCorTmp
        
      }
      #store      
      if(idx == 1) CoefCor <- CoefCorCalc else CoefCor <- c(CoefCor, CoefCorCalc)
    }
    
    #plot cospectrum  
    
    if(MethPlot == TRUE) {
      
      graphics::plot(depe[,idx] * CoefScalPlot ~ idep, type="l", log="x", xaxt="n", yaxt="n", xlim=Limx, ylim=Limy, main=DscrPlot[idx], xlab=Labx,
           ylab=Laby, col=Colr[1])
      
      graphics::abline(h=0, col="grey")
      
      if(MethModlSpec == TRUE) graphics::lines(modlSpec ~ idep, lty=2, col=Colr[2], lwd=5)
      if(MethModlSpec == TRUE & !base::is.null(FreqRngEval)) graphics::abline(v=c(FreqRngEval), lty=2, col=Colr[3])
      
      sfsmisc::eaxis(side=1,labels=NA,las=0)
      sfsmisc::eaxis(side=2,labels=NA,las=0)
      
      graphics::legend(x="topleft", lty=c(1,2,2,1,1), bty="n", col=c(Colr,NA,NA), xjust = 0, yjust = 0,
             lwd=c(graphics::par()$lwd,5,graphics::par()$lwd,1,1), legend = c(
               "measured",
               "Massman (2005)",
               "evaluated range",
               base::paste("stability", " = ", base::round(paraStbl,1), sep=""),
               base::paste("N = ", base::length(idep), sep="")
             ))
      
      graphics::box()
      
    }
    
    ##end loop around variables
  }
  ##
  
  
  #close graphics device
  if(MethPlot == TRUE) {
    grDevices::dev.off()
  }
  
  #export results
  if(!base::is.null(FreqRngEval)) return(CoefCor)
  
  ########################################################
}
########################################################


