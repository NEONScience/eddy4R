##############################################################################################
#' @title Definition function: Determine spectral peak using an Ogive method

#' @author Stefan Metzger \email{eddy4R.info@gmail.com}
#' @author David Durden 

# changelog and author contributions / copyrights
#   Stefan Metzger (2014-09-22)
#     original creation
#   Stefan Metzger (2015-11-28)
#     re-formualtion as function() to allow packaging
#   David Durden (2022-02-09)
#     terms update

#' @description Determine spectral peak using an Ogive method.

#' @param FreqPeak frequency f at which fCO(f) reaches its maximum value
#' @param idep independent variable, preferabley f, but n is possible
#' @param depe dependent variable, spectra or cospectra
#' @param MethSpec spectrum or cospectrum?
#' @param paraStbl stability parameter
#' @param MethWght use frequency-weighted (co)spectrum?
#' @param ThshFreqRng frequency range for determining optimiality criterion
#' @param CoefCumScal cumulative flux contribution for which measured (co)-spectrum is scaled to model (co)-spectrum
#' @param FilePlot generate plot?

#' @return The optimality criterion and the cumulative (Ogive) correction coefficient is returned for the calculated independent variable (frequency/wavenumber) scaling index for the dependent variables (spectra or cospectra). If \code{FilePlot} is provided, a plot will be generated and output.

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

#' @keywords Fast Fourier Transform, FFT, spectral, Ogive

#' @examples Currently none

#' @seealso Currently none

#' @export
##############################################################################################

########################################################
#function to determine spectral peak using an Ogive method
########################################################
def.spec.peak.ogiv <- function(
  #frequency f at which fCO(f) reaches its maximum value
  FreqPeak,
  #independent variable, preferabley f, but n is possible
  idep,
  #dependent variable, spectra or cospectra
  depe,
  #spectrum or cospectrum?
  MethSpec = c("spec", "cosp")[2],
  #stability parameter
  paraStbl,
  #use frequency-weighted (co)spectrum?
  MethWght =TRUE,
  #frequency range for determining optimiality criterion
  ThshFreqRng = c(0.01, 1),
  #cumulative flux contribution for which measured (co)-spectrum is scaled to model (co)-spectrum
  CoefCumScal = 0.6,
  #generate plot?
  FilePlot = NULL
) {
  
  #generate spectral model for range of frequencies
  modlSpec <- eddy4R.turb::def.spec.modl(
    #independent variable, preferabley f, but n is possible
    Idep = idep,
    #spectrum or cospectrum?
    MethSpec  = MethSpec,
    #stability parameter
    paraStbl = paraStbl,
    #frequency f at which fCO(f) reaches its maximum value
    FreqPeak=FreqPeak,
    #output frequency-weighted (co)spectrum?
    MethWght=MethWght
  )
  #cumulate to Ogive from lowest to highest frequency
  modlSpecCum <- base::cumsum(modlSpec)
  
  #assign measured variables
  
  #dependent variable
  #frequency-weighted
  if(MethWght == TRUE) {
    depe <- idep * depe
  } 
  
  #normalize to sum of 1
  depe <- depe / base::sum(depe, na.rm=TRUE)
  #cumulate to Ogive from lowest to highest frequency
  depeCum <- base::cumsum(depe)
  #scaling factor to intersect with modelled Ogive at pre-determined level
  idxScal <- GenKern::nearest(depeCum, CoefCumScal)
  coefScal <- modlSpecCum[idxScal] / CoefCumScal
  #scale to intersect with modelled Ogive at pre-determined level
  depeCumScal <- depeCum * coefScal  
  #indices of observations in the frequency range for determining optimiality criterion
  setFreqCritOptm <- base::which(idep > ThshFreqRng[1] & idep < ThshFreqRng[2])
  
  #optimiality criterion
  #critOptm <- sd((modlSpecCum - depeCumScal)[setFreqCritOptm])  
  critOptm <- eddy4R.base::def.rmsd.diff.prcs.rsq(refe = modlSpecCum[setFreqCritOptm], test = depeCumScal[setFreqCritOptm])[1,1]
  #critOptm <- cor(x = modlSpecCum[setFreqCritOptm], y = depeCumScal[setFreqCritOptm], use = "pairwise.complete.obs")
  #critOptm <- sqrt(def.med.mad((modlSpecCum - depeCumScal)[setFreqCritOptm])[1,1]^2 + def.med.mad((modlSpecCum - depeCumScal)[setFreqCritOptm])[1,2]^2)
  
  #plotting
  if(!base::is.null(FilePlot)) {
    
    #graphics device
    grDevices::png(filename=FilePlot, width = 1000, 
        height = 1000, units = "px", pointsize = 20, bg = "white")
    cexvar=3; par(mfcol=c(2,2), las=0, cex.axis=cexvar*0.4, cex.lab=cexvar*0.4, font.main=1, font.lab=1,
                  mar=c(4,4,2,2), mgp=c(2.6,0.8,0), family="times", lwd=cexvar, cex.main=cexvar*0.5)
    
    #actual plotting
    #Ogive
    graphics::plot(modlSpecCum ~ idep, log="x", type="l", main=paste("peak = ", round(FX,2), sep=""), 
         xlab="frequency", ylab="relative contribution")
    graphics::lines(depeCum ~ idep, col=2)
    graphics::lines(depeCumScal ~ idep, col=4)
    graphics::abline(h=c(CoefCumScal, modlSpecCum[idxScal]), lty=2)
    graphics::abline(v=idep[idxScal], lty=2)
    graphics::abline(v=ThshFreqRng, lty=5)
    graphics::points(base::I(idep[idxScal]), CoefCumScal, col=2, cex=2)
    graphics::points(base::I(idep[idxScal]), base::I(modlSpecCum[idxScal]), col=4, cex=2)
    
    #close graphics device
    grDevices::dev.off()
    
  }
  
  #Create output list
  rpt <- base::list("PeakFreq" = critOptm, "CoefCorIdepScal"=idep[idxScal], "CoefCor"=1/base::max(depeCumScal, na.rm=TRUE))
  #return result
  return(rpt)
  
  ########################################################
}
########################################################